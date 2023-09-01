/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* The interpreter */

#include "scheme.h"
#include "trap.h"
#include "lookup.h"
#include "history.h"

extern void * obstack_chunk_alloc (size_t);
#define obstack_chunk_free free
extern void preserve_signal_mask (void);
extern void fixup_float_environment (void);

typedef enum
{
  ACTION_APPLY,
  ACTION_APPLY_NO_TRAP,
  ACTION_APPLY_NO_INTERRUPT,
  ACTION_EVAL,
  ACTION_EVAL_NO_TRAP,
  ACTION_RETURN,
  ACTION_RETURN_NO_TRAP,
  ACTION_DONE
} action_t;

typedef enum
{
  VARIANT_TRAP,
  VARIANT_NO_TRAP,
  VARIANT_NO_INTERRUPT
} variant_t;

static inline bool
variant_trap_p (variant_t variant)
{
  return variant == VARIANT_TRAP;
}

static inline bool
variant_interrupt_p (variant_t variant)
{
  return variant != VARIANT_NO_INTERRUPT;
}

#ifdef COMPILE_STEPPER

#define FETCH_EVAL_TRAPPER()						\
  (hunk3_ref_0 (vector_ref (fixed_objects, STEPPER_STATE)))

#define FETCH_APPLY_TRAPPER()						\
  (hunk3_ref_1 (vector_ref (fixed_objects, STEPPER_STATE)))

#define FETCH_RETURN_TRAPPER()						\
  (hunk3_ref_2 (vector_ref (fixed_objects, STEPPER_STATE)))

#endif /* COMPILE_STEPPER */

// Interpreter state

#define NULL_INTERPRETER_STATE ((interpreter_state_t*) 0)

static interpreter_state_t* interpreter_state = NULL_INTERPRETER_STATE;

void
bind_interpreter_state (interpreter_state_t* s)
{
  s->previous_state = interpreter_state;
  s->nesting_level
    = (interpreter_state == NULL_INTERPRETER_STATE)
      ? 0
      : 1 + interpreter_state->nesting_level;
  s->dstack_position = dstack_position;
  interpreter_state = s;
}

void
unbind_interpreter_state (interpreter_state_t* s)
{
  interpreter_state = s;
  unsigned long old_mask = GET_INT_MASK;
  SET_INTERRUPT_MASK (0);
  dstack_set_position (s->dstack_position);
  SET_INTERRUPT_MASK (old_mask);
  interpreter_state = s->previous_state;
}

void
abort_to_interpreter (int argument)
{
  assert (argument != 0);
  if (interpreter_state == NULL_INTERPRETER_STATE)
    {
      outf_fatal ("abort_to_interpreter: Interpreter not set up.\n");
      termination_init_error ();
    }

  interpreter_state->throw_argument = argument;
  unsigned long old_mask = GET_INT_MASK;
  SET_INTERRUPT_MASK (0);
  dstack_set_position (interpreter_state->dstack_position);
  SET_INTERRUPT_MASK (old_mask);
  obstack_free (&scratch_obstack, 0);
  obstack_init (&scratch_obstack);
  longjmp (interpreter_state->catch_env, argument);
}

jmp_buf*
interpreter_catch_env (void)
{
  return &interpreter_state->catch_env;
}

int
abort_to_interpreter_argument (void)
{
  return interpreter_state->throw_argument;
}

long prim_apply_error_code;

static inline action_t
handle_interrupt (void)
{
  setup_interrupt (PENDING_INTERRUPTS ());
  return ACTION_APPLY_NO_INTERRUPT;
}

static inline action_t
handle_error (long code, bool from_return)
{
  Do_Micro_Error (code, from_return);
  return ACTION_APPLY;
}

static inline action_t
immediate_gc (unsigned long n)
{
  REQUEST_GC (n);
  return handle_interrupt ();
}

static inline action_t
re_eval (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  SET_EXP (exp);
  SET_ENV (env);
  return ACTION_EVAL;
}

static inline action_t
eval_subproblem (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  NEW_SUBPROBLEM (exp, env);
  return re_eval (exp, env);
}

static inline action_t
eval_reduction (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  NEW_REDUCTION (exp, env);
  return re_eval (exp, env);
}

static inline action_t
single_val (SCHEME_OBJECT val)
{
  SET_VAL (val);
  return ACTION_RETURN;
}

static inline action_t
eval_error (long code, SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  if (code == PRIM_INTERRUPT)
    {
      stack_check (ENV_CONT_SIZE);
      push_env_cont (RC_EVAL_ERROR, exp, env);
      return handle_interrupt ();
    }
  SET_EXP (exp);
  SET_ENV (env);
  return handle_error (code, false);
}

static inline action_t
apply_error (long code)
{
  SET_VAL (apply_frame_proc ());
  push_cont (RC_INTERNAL_APPLY_VAL, SHARP_F);
  return handle_error (code, true);
}

static inline void
prepare_apply_interrupt (void)
{
  SCHEME_OBJECT value = apply_frame_proc ();
  push_cont (RC_INTERNAL_APPLY_VAL, SHARP_F);
  push_cont (RC_RESTORE_VALUE, value);
}

static inline action_t
apply_immediate_gc (unsigned long n)
{
  prepare_apply_interrupt ();
  return immediate_gc (n);
}

static inline action_t
apply_interrupt (void)
{
  prepare_apply_interrupt ();
  return handle_interrupt ();
}

static inline void
prepare_return_interrupt (void)
{
  re_push_cont ();
  push_cont (RC_RESTORE_VALUE, GET_VAL);
}

static inline action_t
return_error (long code)
{
  if (code == PRIM_INTERRUPT)
    {
      prepare_return_interrupt ();
      return handle_interrupt ();
    }
  re_push_cont ();
  return handle_error (code, true);
}

static inline action_t
return_immediate_gc (unsigned long n)
{
  prepare_return_interrupt ();
  return immediate_gc (n);
}

#ifdef CC_SUPPORT_P
static inline action_t
return_from_compiled_code (long code)
{
  switch (code)
    {
    case PRIM_DONE:
      return ACTION_RETURN;

    case PRIM_APPLY:
      return ACTION_APPLY;

    case PRIM_INTERRUPT:
      return handle_interrupt ();

    case PRIM_APPLY_INTERRUPT:
      return apply_interrupt ();

    case ERR_INAPPLICABLE_OBJECT:
    case ERR_WRONG_NUMBER_OF_ARGUMENTS:
      return apply_error (code);

    default:
      return handle_error (code, true);
    }
}
#endif

// Eval

static inline action_t
eval_access (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  stack_check (CONT_SIZE);
  push_cont (RC_EXECUTE_ACCESS_FINISH, exp);
  return eval_subproblem (access_env (exp), env);
}

static inline action_t
eval_assignment (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  stack_check (ENV_CONT_SIZE);
  push_env_cont (RC_EXECUTE_ASSIGNMENT_FINISH, exp, env);
  return eval_subproblem (assignment_value (exp), env);
}

static inline action_t
eval_combination (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  unsigned long nargs = combination_size (exp) - 1;
  stack_check (CONT_SIZE + 2 + nargs);
  decrement_sp (nargs);
  stack_push (make_nmv_header (nargs));
  if (nargs == 0)
    {
      stack_push (make_apply_frame_header (1));
      push_cont (RC_COMB_APPLY_FUNCTION, exp);
    }
  else
    push_env_cont (RC_COMB_SAVE_VALUE, exp, env);
  return eval_subproblem (combination_expr (exp, nargs), env);
}

static inline action_t
eval_comment (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  return eval_reduction (comment_expression (exp), env);
}

#ifdef CC_SUPPORT_P
static inline action_t
eval_compiled_entry (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  SET_EXP (exp);
  SET_ENV (env);
  return return_from_compiled_code (enter_compiled_expression ());
}
#endif

static inline action_t
eval_conditional (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  stack_check (ENV_CONT_SIZE);
  push_env_cont (RC_CONDITIONAL_DECIDE, exp, env);
  return eval_subproblem (conditional_predicate (exp), env);
}

static inline action_t
eval_definition (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  stack_check (ENV_CONT_SIZE);
  push_env_cont (RC_EXECUTE_DEFINITION_FINISH, exp, env);
  return eval_subproblem (definition_value (exp), env);
}

static inline action_t
eval_delay (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  return single_val (make_delayed (delay_object (exp), env));
}

static inline action_t
eval_disjunction (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  stack_check (ENV_CONT_SIZE);
  push_env_cont (RC_DISJUNCTION_DECIDE, exp, env);
  return eval_subproblem (disjunction_predicate (exp), env);
}

static inline action_t
eval_extended_lambda (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  return single_val (make_extended_procedure (exp, env));
}

static inline action_t
eval_lambda (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  return single_val (make_procedure (exp, env));
}

static inline action_t
eval_scode_quote (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  return single_val (scode_quote_object (exp));
}

static inline action_t
eval_sequence (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  stack_check (ENV_CONT_SIZE);
  push_env_cont (RC_EXECUTE_SEQUENCE_FINISH, exp, env);
  return eval_subproblem (sequence_1 (exp), env);
}

static inline action_t
eval_variable (SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  SCHEME_OBJECT val;
  long code = lookup_variable (env, variable_name (exp), &val);
  if (code == PRIM_DONE)
    return single_val (val);
  if (code == ERR_UNASSIGNED_VARIABLE && variable_safe_p (exp))
    return single_val (UNASSIGNED_OBJECT);
  return eval_error (code, exp, env);
}

static action_t
eval (SCHEME_OBJECT exp, SCHEME_OBJECT env, variant_t variant)
{
#ifdef COMPILE_STEPPER
  if (variant_trap_p (variant)
      && trapping
      && !WITHIN_CRITICAL_SECTION_P ()
      && FETCH_EVAL_TRAPPER () != SHARP_F)
    {
      trapping = false;
      stack_check (4);
      stack_push (GET_ENV);
      stack_push (GET_EXP);
      stack_push (FETCH_EVAL_TRAPPER ());
      stack_push (make_apply_frame_header (3));
      return ACTION_APPLY_NO_TRAP;
    }
#endif /* COMPILE_STEPPER */

#ifdef EVAL_UCODE_HOOK
  EVAL_UCODE_HOOK ();
#endif

  switch (object_type (exp))
    {
    case TC_ACCESS:
      return eval_access (exp, env);
    case TC_ASSIGNMENT:
      return eval_assignment (exp, env);
    case TC_BROKEN_HEART:
      Microcode_Termination (TERM_BROKEN_HEART);
    case TC_COMBINATION:
      return eval_combination (exp, env);
    case TC_COMMENT:
      return eval_comment (exp, env);
#ifdef CC_SUPPORT_P
    case TC_COMPILED_ENTRY:
      return eval_compiled_entry (exp, env);
#endif
    case TC_CONDITIONAL:
      return eval_conditional (exp, env);
    case TC_DEFINITION:
      return eval_definition (exp, env);
    case TC_DELAY:
      return eval_delay (exp, env);
    case TC_DISJUNCTION:
      return eval_disjunction (exp, env);
    case TC_EXTENDED_LAMBDA:
      return eval_extended_lambda (exp, env);
    case TC_LAMBDA:
      return eval_lambda (exp, env);
    case TC_MANIFEST_NM_VECTOR:
      return eval_error (ERR_EXECUTE_MANIFEST_VECTOR, exp, env);
    case TC_SCODE_QUOTE:
      return eval_scode_quote (exp, env);
    case TC_SEQUENCE:
      return eval_sequence (exp, env);
    case TC_SYNTAX_ERROR:
      return eval_error (ERR_SYNTAX_ERROR, exp, env);
    case TC_THE_ENVIRONMENT:
      return single_val (env);
    case TC_VARIABLE:
      return eval_variable (exp, env);
    default:
      return single_val (exp);
    }
}

// Apply

static inline action_t
apply_primitive (SCHEME_OBJECT proc)
{
  if (!primitive_implemented_p (proc))
    return apply_error (ERR_UNIMPLEMENTED_PRIMITIVE);

  unsigned long n_args = apply_frame_n_args ();
  if (primitive_arity (proc) == LEXPR_PRIMITIVE_ARITY)
    SET_LEXPR_ACTUALS (n_args);
  else if (primitive_arity (proc) != n_args)
    return apply_error (ERR_WRONG_NUMBER_OF_ARGUMENTS);

  // Primitives don't need header or proc
  increment_sp (2);
  SET_EXP (proc);
#ifdef ENABLE_DEBUGGING_TOOLS
  if (Primitive_Debug)
    Print_Primitive (proc);
#endif
  apply_primitive_external (proc);
#ifdef ENABLE_DEBUGGING_TOOLS
  if (Primitive_Debug)
    {
      Print_Expression (GET_VAL, "Primitive Result");
      outf_error("\n");
      outf_flush_error();
    }
#endif
  pop_primitive_frame (n_args);
  return ACTION_RETURN;
}

void
apply_primitive_external (SCHEME_OBJECT proc)
{
  void* position = dstack_position;
  SET_PRIMITIVE (proc);
  Free_primitive = Free;
  SET_VAL ((*Primitive_Procedure_Table [primitive_number (proc)]) ());
  /* If the primitive failed to unwind the dynamic stack, lose. */
  if (position != dstack_position)
    {
      outf_fatal ("\nPrimitive slipped the dynamic stack: %s\n",
                  primitive_name (proc));
      Microcode_Termination (TERM_EXIT);
    }
  SET_PRIMITIVE (SHARP_F);
  Free_primitive = 0;
}

static inline action_t
apply_procedure (SCHEME_OBJECT proc)
{
  unsigned long frame_size = apply_frame_size ();
  SCHEME_OBJECT lambda = proc_lambda (proc);
  {
    SCHEME_OBJECT names = lambda_names (lambda);
    if (! (frame_size == vector_length (names)
           || (object_type (lambda) == TC_LEXPR
               && frame_size < vector_length (names))))
      return apply_error (ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  unsigned long nwords = frame_size + 1;
  if (GC_NEEDED_P (nwords))
    return apply_immediate_gc (nwords);
  SCHEME_OBJECT* end = Free + nwords;
  SCHEME_OBJECT env = make_pointer_object (TC_ENVIRONMENT, Free);
  *Free++ = make_vector_header (frame_size);
  increment_sp (1); // discard apply_frame_header
  while (Free < end)
    *Free++ = stack_pop ();
  return eval_reduction (lambda_body (lambda), env);
}

static inline action_t
apply_extended_procedure (SCHEME_OBJECT proc)
{
  SCHEME_OBJECT lambda = proc_lambda (proc);
  unsigned long reqs = elambda_reqs (lambda);
  unsigned long opts = elambda_opts (lambda);
  unsigned long rest = elambda_rest (lambda);
  unsigned long nfixed = reqs + opts;
  unsigned long nargs = apply_frame_n_args ();

  if (nargs < reqs || (rest == 0 && nargs > nfixed))
    return apply_error (ERR_WRONG_NUMBER_OF_ARGUMENTS);

  unsigned long size = /* proc: */ 1 + nfixed + rest;
  unsigned long nwords
    = 1             // vector header
      + size
      // rest list:
      + (nargs > nfixed) ? 2 * (nargs - nfixed) : 0;
  if (GC_NEEDED_P (nwords))
    return apply_immediate_gc (nwords);
  increment_sp (1);             // discard apply-frame header
  SCHEME_OBJECT* scan = Free;
  SCHEME_OBJECT env = make_pointer_object (TC_ENVIRONMENT, scan);
  *scan++ = make_vector_header (size);
  if (nargs <= nfixed)
    {
      *scan++ = stack_pop (); // proc
      for (unsigned int i = 0; i < nargs; i += 1)
        *scan++ = stack_pop ();
      for (unsigned int i = nargs; i < nfixed; i += 1)
        *scan++ = DEFAULT_OBJECT;
      if (rest == 1)
        *scan++ = EMPTY_LIST;
    }
  else
    {
      /* assert (rest == 1) */
      SCHEME_OBJECT list
        = make_pointer_object (TC_LIST, scan + size);
      *scan++ = stack_pop (); // proc
      for (unsigned int i = 0; i < nfixed; i += 1)
        *scan++ = stack_pop ();
      *scan++ = list;
      /* Now scan == object_address (list) */
      for (unsigned int i = nfixed; i < nargs; i += 1)
        {
          *scan++ = stack_pop ();
          *scan = make_pointer_object (TC_LIST, scan + 1);
          scan += 1;
        }
      scan[-1] = EMPTY_LIST;
    }
  Free = scan;
  return eval_reduction (elambda_body (lambda), env);
}

static inline action_t
apply_control_point (SCHEME_OBJECT proc)
{
  if (apply_frame_size () != 2)
    return apply_error (ERR_WRONG_NUMBER_OF_ARGUMENTS);
  SET_VAL (*apply_frame_args ());
  unpack_control_point (proc);
  reset_history ();
  return ACTION_RETURN;
}

static inline action_t
apply_entity (SCHEME_OBJECT proc)
{
  unsigned long frame_size = apply_frame_size ();
  SCHEME_OBJECT data = entity_data (proc);
  if (VECTOR_P (data)
      && frame_size < vector_length (data)
      && vector_ref (data, frame_size) != SHARP_F
      && vector_ref (data, 0)
         == vector_ref (fixed_objects, ARITY_DISPATCHER_TAG))
    {
      set_apply_frame_proc (vector_ref (data, frame_size));
      return ACTION_APPLY_NO_INTERRUPT;
    }
  stack_set (0, entity_operator (proc));
  stack_check (1);
  stack_push (make_apply_frame_header (frame_size + 1));
  return ACTION_APPLY;
}

static inline action_t
apply_record (SCHEME_OBJECT proc)
{
  SCHEME_OBJECT applicator = record_applicator (proc);
  if (applicator == SHARP_F)
    return apply_error (ERR_INAPPLICABLE_OBJECT);
  unsigned long frame_size = apply_frame_size ();
  stack_set (0, applicator);
  stack_check (1);
  stack_push (make_apply_frame_header (frame_size + 1));
  return ACTION_APPLY;
}

#ifdef CC_SUPPORT_P
static inline action_t
apply_compiled_entry (void)
{
  guarantee_cc_return (1 + apply_frame_size ());
  return return_from_compiled_code (apply_compiled_procedure ());
}
#endif

static action_t
apply (variant_t variant)
{
#ifdef COMPILE_STEPPER
  if (variant_trap_p (variant)
      && trapping
      && !WITHIN_CRITICAL_SECTION_P ()
      && FETCH_APPLY_TRAPPER () != SHARP_F)
    {
      unsigned long frame_size = apply_frame_size ();
      stack_set (0, FETCH_APPLY_TRAPPER ());
      stack_push (make_apply_frame_header (frame_size + 1));
      trapping = false;
    }
#endif

  if (variant_interrupt_p (variant) && PENDING_INTERRUPTS_P)
    {
      unsigned long interrupts = PENDING_INTERRUPTS ();
      prepare_apply_interrupt ();
      setup_interrupt (interrupts);
    }

#ifdef APPLY_UCODE_HOOK
  APPLY_UCODE_HOOK ();
#endif

  SCHEME_OBJECT proc = apply_frame_proc ();
  switch (object_type (proc))
    {
    case TC_ENTITY:
      return apply_entity (proc);

    case TC_RECORD:
      return apply_record (proc);

    case TC_PROCEDURE:
      return apply_procedure (proc);

    case TC_CONTROL_POINT:
      return apply_control_point (proc);

    case TC_PRIMITIVE:
      return apply_primitive (proc);

    case TC_EXTENDED_PROCEDURE:
      return apply_extended_procedure (proc);

#ifdef CC_SUPPORT_P
    case TC_COMPILED_ENTRY:
      return apply_compiled_entry ();
#endif

    default:
      return apply_error (ERR_INAPPLICABLE_OBJECT);
    }
}

static inline action_t
return_comb_apply_function (void)
{
  END_SUBPROBLEM ();
  set_apply_frame_proc (GET_VAL);
  return ACTION_APPLY;
}

static inline action_t
return_comb_save_value (void)
{
  SCHEME_OBJECT env = stack_pop ();
  SCHEME_OBJECT exp = GET_EXP;
  unsigned long arg = object_datum (stack_ref (0)) - 1;
  stack_set (1 + arg, GET_VAL);
  stack_set (0, make_nmv_header (arg));
  if (arg > 0)
    push_env_cont (RC_COMB_SAVE_VALUE, exp, env);
  else
    {
      stack_push (make_apply_frame_header (combination_size (exp)));
      push_cont (RC_COMB_APPLY_FUNCTION, exp);
    }
  SCHEME_OBJECT new_exp = combination_expr (exp, arg);
  REUSE_SUBPROBLEM (new_exp, env);
  return re_eval (new_exp, env);
}

static inline action_t
return_conditional_decide (void)
{
  END_SUBPROBLEM ();
  SCHEME_OBJECT env = stack_pop ();
  SCHEME_OBJECT exp
    = (GET_VAL == SHARP_F)
      ? conditional_alternative (GET_EXP)
      : conditional_consequent (GET_EXP);
  return re_eval (exp, env);
}

static inline action_t
return_disjunction_decide (void)
{
  END_SUBPROBLEM ();
  SCHEME_OBJECT env = stack_pop ();
  return (GET_VAL != SHARP_F)
         ? ACTION_RETURN
         : eval_reduction (disjunction_alternative (GET_EXP), env);
}

static inline action_t
return_end_of_computation (void)
{
  /* Signals bottom of stack */
  interpreter_state_t* previous_state = interpreter_state->previous_state;
  if (previous_state == NULL_INTERPRETER_STATE)
    {
      termination_end_of_computation ();
      /*NOTREACHED*/
    }
  dstack_position = interpreter_state->dstack_position;
  interpreter_state = previous_state;
  return ACTION_DONE;
}

static inline action_t
return_redo_evaluation (void)
{
  SCHEME_OBJECT env = stack_pop ();
  return eval_reduction (GET_EXP, env);
}

static inline action_t
return_access_finish (void)
{
  SCHEME_OBJECT val;
  long code = lookup_variable (GET_VAL, access_name (GET_EXP), &val);
  if (code == PRIM_DONE)
    {
      END_SUBPROBLEM ();
      return single_val (val);
    }
  return return_error (code);
}

static inline action_t
return_assignment_finish (void)
{
  SCHEME_OBJECT env = stack_pop ();
  SCHEME_OBJECT variable = assignment_name (GET_EXP);
  SCHEME_OBJECT old_val;
  long code
    = (object_type (variable) == TC_VARIABLE)
      ? assign_variable (env, variable_name (variable), GET_VAL, &old_val)
      : ERR_BAD_FRAME;
  if (code == PRIM_DONE)
    {
      END_SUBPROBLEM ();
      return single_val (old_val);
    }
  stack_push (env);
  return return_error (code);
}

static inline action_t
return_definition_finish (void)
{
  SCHEME_OBJECT env = stack_pop ();
  SCHEME_OBJECT name = definition_name (GET_EXP);
  long code = define_variable (env, name, GET_VAL);
  if (code == PRIM_DONE)
    {
      END_SUBPROBLEM ();
      return single_val (name);
    }
  stack_push (env);
  return return_error (code);
}

static inline action_t
return_hardware_trap (void)
{
  /* This just reinvokes the handler */
  SCHEME_OBJECT info = stack_ref (0);
  re_push_cont ();
  SCHEME_OBJECT handler
    = VECTOR_P (fixed_objects)
      ? vector_ref (fixed_objects, TRAP_HANDLER)
      : SHARP_F;
  if (handler == SHARP_F)
    {
      outf_fatal ("There is no trap handler for recovery!\n");
      termination_trap ();
      /*NOTREACHED*/
    }
  stack_check (3);
  stack_push (info);
  stack_push (handler);
  stack_push (make_apply_frame_header (2));
  return ACTION_APPLY;
}

static inline action_t
return_normal_gc_done (void)
{
  SET_VAL (GET_EXP);
  /* Paranoia */
  if (GC_NEEDED_P (gc_space_needed))
    termination_gc_out_of_space ();
  gc_space_needed = 0;
  EXIT_CRITICAL_SECTION ({ re_push_cont (); });
  return ACTION_RETURN;
}

// The following two return codes are both used to restore a saved history
// object.  The difference is that the first does not copy the history object
// while the second does.  In both cases, GET_EXP contains the history object
// and the next item to be popped off the stack contains the offset back to the
// previous restore history return code.

static inline action_t
return_restore_dont_copy_history (void)
{
  prev_restore_history_offset = object_datum (stack_pop ());
  increment_sp (1);       // obsolete field
  history_register = object_address (GET_EXP);
  return ACTION_RETURN;
}

static inline action_t
return_restore_history (void)
{
  if (!restore_history (GET_EXP))
    return return_immediate_gc (HEAP_AVAILABLE);
  prev_restore_history_offset = (object_datum (stack_pop ()));
  increment_sp (1);       // obsolete field
  if (prev_restore_history_offset > 0)
    *(stack_end - prev_restore_history_offset)
      = MAKE_RETURN_CODE (RC_RESTORE_HISTORY);
  return ACTION_RETURN;
}

static inline action_t
return_restore_int_mask (void)
{
  SET_INTERRUPT_MASK (UNSIGNED_FIXNUM_TO_LONG (GET_EXP));
  if (GC_NEEDED_P (0))
    REQUEST_GC (0);
  if (PENDING_INTERRUPTS_P)
    {
      push_cont (RC_RESTORE_VALUE, GET_VAL);
      return handle_interrupt ();
    }
  return ACTION_RETURN;
}

static inline action_t
return_stack_marker (void)
{
  // Frame consists of the return code followed by two objects.  The first
  // object has already been popped into GET_EXP, so just pop the second
  // argument.
  increment_sp (1);
  return ACTION_RETURN;
}

static inline action_t
return_sequence_finish (void)
{
  END_SUBPROBLEM ();
  SCHEME_OBJECT env = stack_pop ();
  return eval_reduction (sequence_2 (GET_EXP), env);
}

static action_t
apply_cont (variant_t variant)
{
#ifdef COMPILE_STEPPER
  if (variant_trap_p (variant)
      && trapping
      && !WITHIN_CRITICAL_SECTION_P ()
      && FETCH_RETURN_TRAPPER () != SHARP_F)
    {
      stack_check (3);
      stack_push (GET_VAL);
      stack_push (FETCH_RETURN_TRAPPER ());
      stack_push (make_apply_frame_header (2));
      trapping = false;
    }
#endif

#ifdef POP_RETURN_UCODE_HOOK
  POP_RETURN_UCODE_HOOK ();
#endif

  restore_cont ();

#ifdef ENABLE_DEBUGGING_TOOLS
  if (!RETURN_CODE_P (GET_RET))
    {
      stack_push (GET_VAL);	/* For possible stack trace */
      re_push_cont ();
      Microcode_Termination (TERM_BAD_STACK);
    }
#endif

  switch (object_datum (GET_RET))
    {
    case RC_COMB_APPLY_FUNCTION:
      return return_comb_apply_function ();
    case RC_COMB_SAVE_VALUE:
      return return_comb_save_value ();
    case RC_CONDITIONAL_DECIDE:
      return return_conditional_decide ();
    case RC_DISJUNCTION_DECIDE:
      return return_disjunction_decide ();
    case RC_END_OF_COMPUTATION:
      return return_end_of_computation ();
    case RC_EVAL_ERROR:
      return return_redo_evaluation ();
    case RC_EXECUTE_ACCESS_FINISH:
      return return_access_finish ();
    case RC_EXECUTE_ASSIGNMENT_FINISH:
      return return_assignment_finish ();
    case RC_EXECUTE_DEFINITION_FINISH:
      return return_definition_finish ();
    case RC_HALT:
      Microcode_Termination (TERM_TERM_HANDLER);
    case RC_HARDWARE_TRAP:
      return return_hardware_trap ();
    case RC_INTERNAL_APPLY:
      return ACTION_APPLY;
    case RC_INTERNAL_APPLY_VAL:
      set_apply_frame_proc (GET_VAL);
      return ACTION_APPLY;
    case RC_JOIN_STACKLETS:
      unpack_control_point (GET_EXP);
      return ACTION_RETURN;
    case RC_NORMAL_GC_DONE:
      return return_normal_gc_done ();
    case RC_POP_RETURN_ERROR:
    case RC_RESTORE_VALUE:
      // Eliminate RC_POP_RETURN_ERROR in favor of RC_RESTORE_VALUE
      return single_val (GET_EXP);
    case RC_RESTORE_DONT_COPY_HISTORY:
      return return_restore_dont_copy_history ();
    case RC_RESTORE_HISTORY:
      return return_restore_history ();
    case RC_RESTORE_INT_MASK:
      return return_restore_int_mask ();
    case RC_STACK_MARKER:
      return return_stack_marker ();
    case RC_EXECUTE_SEQUENCE_FINISH:
      return return_sequence_finish ();
    case RC_SNAP_NEED_THUNK:
      return single_val (snap_delayed (GET_EXP, GET_VAL));

#ifdef CC_SUPPORT_P
    case RC_COMP_INTERRUPT_RESTART:
      return return_from_compiled_code (comp_interrupt_restart ());
    case RC_COMP_LOOKUP_TRAP_RESTART:
      return return_from_compiled_code (comp_lookup_trap_restart ());
    case RC_COMP_ASSIGNMENT_TRAP_RESTART:
      return return_from_compiled_code (comp_assignment_trap_restart ());
    case RC_COMP_OP_REF_TRAP_RESTART:
      return return_from_compiled_code (comp_op_lookup_trap_restart ());
    case RC_COMP_CACHE_REF_APPLY_RESTART:
      return return_from_compiled_code (comp_cache_lookup_apply_restart ());
    case RC_COMP_SAFE_REF_TRAP_RESTART:
      return return_from_compiled_code (comp_safe_lookup_trap_restart ());
    case RC_COMP_UNASSIGNED_TRAP_RESTART:
      return return_from_compiled_code (comp_unassigned_p_trap_restart ());
    case RC_COMP_LINK_CACHES_RESTART:
      return return_from_compiled_code (comp_link_caches_restart ());
    case RC_COMP_ERROR_RESTART:
      return return_from_compiled_code (comp_error_restart ());
    case RC_REENTER_COMPILED_CODE:
      return return_from_compiled_code (return_to_compiled_code ());
#endif

    default:
      return return_error (ERR_INAPPLICABLE_CONTINUATION);
    }
}

static action_t
handle_throw (int code)
{
  switch (code)
    {
    case PRIM_APPLY:
      SET_PRIMITIVE (SHARP_F);
      return ACTION_APPLY;

    case PRIM_NO_TRAP_APPLY:
      SET_PRIMITIVE (SHARP_F);
      return ACTION_APPLY_NO_TRAP;

    case PRIM_APPLY_INTERRUPT:
      SET_PRIMITIVE (SHARP_F);
      return apply_interrupt ();

    case PRIM_APPLY_ERROR:
      SET_PRIMITIVE (SHARP_F);
      return apply_error (prim_apply_error_code);

    case PRIM_DO_EXPRESSION:
      SET_PRIMITIVE (SHARP_F);
      SET_VAL (GET_EXP);
      return eval_reduction (GET_EXP, GET_ENV);

    case PRIM_NO_TRAP_EVAL:
      SET_PRIMITIVE (SHARP_F);
      SET_VAL (GET_EXP);
      NEW_REDUCTION (GET_EXP, GET_ENV);
      return ACTION_EVAL_NO_TRAP;

    case PRIM_POP_RETURN:
      SET_PRIMITIVE (SHARP_F);
      return ACTION_RETURN;

    case PRIM_RETURN_TO_C:
      SET_PRIMITIVE (SHARP_F);
      unbind_interpreter_state (interpreter_state);
      return ACTION_DONE;

    case PRIM_NO_TRAP_POP_RETURN:
      SET_PRIMITIVE (SHARP_F);
      return ACTION_RETURN_NO_TRAP;

    case PRIM_INTERRUPT:
      back_out_of_primitive ();
      return handle_interrupt ();

    default:
      back_out_of_primitive ();
      return handle_error (code, true);
    }
}

void
Interpret (void)
{
  interpreter_state_t new_state;
  bind_interpreter_state (&new_state);
  int code = (setjmp (interpreter_state->catch_env));
  preserve_signal_mask ();
  fixup_float_environment ();

  action_t action = (code == 0) ? ACTION_EVAL : handle_throw (code);
  while (true)
    switch (action)
      {
      case ACTION_APPLY:
        action = apply (VARIANT_TRAP);
        break;
      case ACTION_APPLY_NO_TRAP:
        action = apply (VARIANT_NO_TRAP);
        break;
      case ACTION_APPLY_NO_INTERRUPT:
        action = apply (VARIANT_NO_INTERRUPT);
        break;
      case ACTION_EVAL:
        action = eval (GET_EXP, GET_ENV, VARIANT_TRAP);
        break;
      case ACTION_EVAL_NO_TRAP:
        action = eval (GET_EXP, GET_ENV, VARIANT_NO_TRAP);
        break;
      case ACTION_RETURN:
        action = apply_cont (VARIANT_TRAP);
        break;
      case ACTION_RETURN_NO_TRAP:
        action = apply_cont (VARIANT_NO_TRAP);
        break;
      case ACTION_DONE:
        unbind_interpreter_state (&new_state);
        return;
      }
}
