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

/* Definitions used by the interpreter and some utilities. */

#ifndef SCM_INTERP_H
#define SCM_INTERP_H 1

#include "object.h"
#include "const.h"
#include "intrpt.h"
#include "stack.h"

static inline void
stack_check_fatal (const char* s)
{
  if (stack_overflowed_p ())
    stack_death (s);
}

static inline void
stack_check (unsigned long n)
{
  if (!stack_can_push_p (n))
    {
      stack_check_fatal ("STACK_CHECK");
      REQUEST_INTERRUPT (INT_Stack_Overflow);
    }
}

/* Note: push_cont must match the definitions in sdata.h */

static inline void
push_cont (SCHEME_OBJECT ret, SCHEME_OBJECT val)
{
  stack_push (val);
  stack_push (ret);
}

static inline void
push_cont_rc (unsigned long rc, SCHEME_OBJECT val)
{
  push_cont (MAKE_RETURN_CODE (rc), val);
}

static inline void
push_cont_env (unsigned long rc, SCHEME_OBJECT exp, SCHEME_OBJECT env)
{
  stack_push (env);
  push_cont_rc (rc, exp);
}

static inline void
save_cont (void)
{
  push_cont (GET_RET, GET_EXP);
}

static inline void
restore_cont (void)
{
  SET_RET (stack_pop ());
  SET_EXP (stack_pop ());
}

#define STACK_CHECK_FATAL stack_check_fatal
#define STACK_CHECK stack_check
#define SAVE_CONT save_cont
#define RESTORE_CONT restore_cont

#define CONT_RET(offset) (stack_ref (offset))
#define CONT_EXP(offset) (stack_ref ((offset) + 1))

#define CONT_SIZE 2
#define ENV_CONT_SIZE (CONT_SIZE + 1)
#define HISTORY_CONT_SIZE (CONT_SIZE + 2)

static inline SCHEME_OBJECT
cont_frame_ret (SCHEME_OBJECT* frame)
{
  return frame[0];
}

static inline SCHEME_OBJECT
cont_frame_rc (SCHEME_OBJECT* frame)
{
  return object_datum (cont_frame_ret (frame));
}

static inline SCHEME_OBJECT
cont_frame_exp (SCHEME_OBJECT* frame)
{
  return frame[1];
}

static inline SCHEME_OBJECT
cont_frame_env (SCHEME_OBJECT* frame)
{
  return frame[2];
}

static inline void
discard_cont_frame (void)
{
  increment_sp (2);
}

static inline void
discard_cont_env_frame (void)
{
  increment_sp (3);
}

#define make_apply_frame_header make_vector_header
#define apply_frame_header_size object_datum

static inline unsigned long
apply_frame_header_n_args (SCHEME_OBJECT header)
{
  return apply_frame_header_size (header) - 1;
}

static inline SCHEME_OBJECT
apply_frame_ptr_header (SCHEME_OBJECT* frame)
{
  return frame[0];
}

static inline SCHEME_OBJECT
apply_frame_ptr_proc (SCHEME_OBJECT* frame)
{
  return frame[1];
}

static inline SCHEME_OBJECT*
apply_frame_ptr_args (SCHEME_OBJECT* frame)
{
  return frame + 2;
}

static inline SCHEME_OBJECT
apply_frame_header (void)
{
  return apply_frame_ptr_header (stack_pointer);
}

static inline SCHEME_OBJECT
apply_frame_proc (void)
{
  return apply_frame_ptr_proc (stack_pointer);
}

static inline void
set_apply_frame_proc (SCHEME_OBJECT proc)
{
  return stack_set (1, proc);
}

static inline SCHEME_OBJECT*
apply_frame_args (void)
{
  return apply_frame_ptr_args (stack_pointer);
}

static inline unsigned long
apply_frame_size (void)
{
  return apply_frame_header_size (apply_frame_header ());
}

static inline unsigned long
apply_frame_n_args (void)
{
  return apply_frame_header_n_args (apply_frame_header ());
}

static inline void
push_apply_frame_header (unsigned long nargs)
{
  stack_push (make_apply_frame_header (nargs + 1));
}

static inline unsigned long
pop_apply_frame_header (void)
{
  return apply_frame_header_n_args (stack_pop ());
}

#define PUSH_APPLY_FRAME_HEADER push_apply_frame_header
#define POP_APPLY_FRAME_HEADER pop_apply_frame_header
#define APPLY_FRAME_HEADER_N_ARGS apply_frame_header_n_args
#define APPLY_FRAME_SIZE apply_frame_size
#define APPLY_FRAME_N_ARGS apply_frame_n_args
#define APPLY_FRAME_PROCEDURE apply_frame_proc

#define CHECK_RETURN_CODE(code, offset)					\
  ((CONT_RET (offset)) == (MAKE_RETURN_CODE (code)))

#if 0
/* Saving history is required for C_call_scheme to work correctly
   because the recursive call to Interpret() can rotate the history.  */

#define APPLY_PRIMITIVE_FROM_INTERPRETER(primitive) do			\
{									\
  SCHEME_OBJECT * APFI_saved_history = history_register;		\
  PRIMITIVE_APPLY (primitive);						\
  history_register = APFI_saved_history;				\
} while (0)

#else
/* C_call_scheme must save/restore history_register on/from the stack
   so that it will be relocated if the call to Interpret() causes a
   garbage collection. */

#define APPLY_PRIMITIVE_FROM_INTERPRETER PRIMITIVE_APPLY

#endif

/* Primitive utility macros */

#ifndef ENABLE_DEBUGGING_TOOLS
#  define PRIMITIVE_APPLY PRIMITIVE_APPLY_INTERNAL
#else
   extern void primitive_apply_internal (SCHEME_OBJECT);
#  define PRIMITIVE_APPLY primitive_apply_internal
#endif

#define PRIMITIVE_APPLY_INTERNAL(primitive) do				\
{									\
  void * PRIMITIVE_APPLY_INTERNAL_position = dstack_position;		\
  SET_PRIMITIVE (primitive);						\
  Free_primitive = Free;						\
  SET_VAL								\
    ((* (Primitive_Procedure_Table [PRIMITIVE_NUMBER (primitive)]))	\
     ());								\
  /* If the primitive failed to unwind the dynamic stack, lose. */	\
  if (PRIMITIVE_APPLY_INTERNAL_position != dstack_position)		\
    {									\
      outf_fatal ("\nPrimitive slipped the dynamic stack: %s\n",	\
		  (PRIMITIVE_NAME (primitive)));			\
      Microcode_Termination (TERM_EXIT);				\
    }									\
  Free_primitive = 0;							\
  SET_PRIMITIVE (SHARP_F);						\
} while (0)

#define POP_PRIMITIVE_FRAME(arity) (increment_sp (arity))

typedef struct interpreter_state_s * interpreter_state_t;

struct interpreter_state_s
{
  interpreter_state_t previous_state;
  unsigned int nesting_level;
  void * dstack_position;
  jmp_buf catch_env;
  int throw_argument;
};

#define interpreter_catch_dstack_position interpreter_state->dstack_position
#define interpreter_catch_env interpreter_state->catch_env
#define interpreter_throw_argument interpreter_state->throw_argument
#define NULL_INTERPRETER_STATE ((interpreter_state_t) 0)

extern void abort_to_interpreter (int) NORETURN;
extern int abort_to_interpreter_argument (void);

extern interpreter_state_t interpreter_state;
extern long prim_apply_error_code;
extern void bind_interpreter_state (interpreter_state_t);
extern void unbind_interpreter_state (interpreter_state_t);

#endif /* not SCM_INTERP_H */
