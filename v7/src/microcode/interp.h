/* -*-C-*-

$Id: interp.h,v 9.46 2002/07/02 20:50:08 cph Exp $

Copyright (c) 1987-1999, 2002 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
*/

/* Macros used by the interpreter and some utilities. */

extern void EXFUN (abort_to_interpreter, (int argument));
extern int EXFUN (abort_to_interpreter_argument, (void));

#define Regs		Registers

#define env_register (Registers[REGBLOCK_ENV])
#define val_register (Registers[REGBLOCK_VAL])
#define exp_register (Registers[REGBLOCK_EXPR])
#define ret_register (Registers[REGBLOCK_RETURN])

#define Store_Return(P) ret_register = (MAKE_OBJECT (TC_RETURN_CODE, (P)))

/* Note: Save_Cont must match the definitions in sdata.h */

#define Save_Cont()							\
{									\
  STACK_PUSH (exp_register);						\
  STACK_PUSH (ret_register);						\
}

#define Restore_Cont()							\
{									\
  ret_register = (STACK_POP ());					\
  exp_register = (STACK_POP ());					\
}

#define Stop_Trapping() Trapping = 0

/* Saving history is required for C_call_scheme to work correctly
   because the recursive call to Interpret() can rotate the history.
   */

#define APPLY_PRIMITIVE_FROM_INTERPRETER(location, primitive)		\
{									\
  SCHEME_OBJECT * APFI_saved_history = history_register;		\
  PRIMITIVE_APPLY ((location), (primitive));				\
  history_register = APFI_saved_history;				\
}

/* Internal_Will_Push is in stack.h. */

#ifdef ENABLE_DEBUGGING_TOOLS

#define Will_Push(N)							\
{									\
  SCHEME_OBJECT * Will_Push_Limit;					\
									\
  Internal_Will_Push ((N));						\
  Will_Push_Limit = (STACK_LOC (- (N)))

#define Pushed()							\
  if (sp_register < Will_Push_Limit)					\
    {									\
      Stack_Death ();							\
    }									\
}

#else

#define Will_Push(N)			Internal_Will_Push(N)
#define Pushed()

#endif

/*
  N in Will_Eventually_Push is the maximum contiguous (single return code)
  amount that this operation may take.  On the average case it may use less.
  M in Finished_Eventual_Pushing is the amount not yet pushed.
 */

#define Will_Eventually_Push(N)		Internal_Will_Push(N)
#define Finished_Eventual_Pushing(M)

/* Primitive stack operations:
   These operations hide the direction of stack growth.
   `Throw' in "stack.h", `Allocate_New_Stacklet' in "utils.c",
   `apply', `cwcc' and friends in "hooks.c", and possibly other stuff,
   depend on the direction in which the stack grows. */

#define STACK_LOCATIVE_DECREMENT(locative) (-- (locative))
#define STACK_LOCATIVE_INCREMENT(locative) ((locative) ++)
#define STACK_LOCATIVE_OFFSET(locative, offset) ((locative) + (offset))
#define STACK_LOCATIVE_REFERENCE(locative, offset) ((locative) [(offset)])
#define STACK_LOCATIVE_DIFFERENCE(x, y) ((x) - (y))

#define STACK_LOCATIVE_PUSH(locative)					\
  (* (STACK_LOCATIVE_DECREMENT (locative)))

#define STACK_LOCATIVE_POP(locative)					\
  (* (STACK_LOCATIVE_INCREMENT (locative)))

#define STACK_PUSH(object) (STACK_LOCATIVE_PUSH (sp_register)) = (object)
#define STACK_POP() (STACK_LOCATIVE_POP (sp_register))
#define STACK_LOC(offset) (STACK_LOCATIVE_OFFSET (sp_register, (offset)))
#define STACK_REF(offset) (STACK_LOCATIVE_REFERENCE (sp_register, (offset)))

/* Primitive utility macros */

#ifndef ENABLE_DEBUGGING_TOOLS

#define PRIMITIVE_APPLY PRIMITIVE_APPLY_INTERNAL

#else

extern SCHEME_OBJECT EXFUN
  (primitive_apply_internal, (SCHEME_OBJECT primitive));
#define PRIMITIVE_APPLY(loc, primitive)					\
  (loc) = (primitive_apply_internal (primitive))

#endif

#define PRIMITIVE_APPLY_INTERNAL(loc, primitive)			\
{									\
  (Registers[REGBLOCK_PRIMITIVE]) = (primitive);			\
  {									\
    /* Save the dynamic-stack position. */				\
    PTR PRIMITIVE_APPLY_INTERNAL_position = dstack_position;		\
    (loc) =								\
      ((* (Primitive_Procedure_Table [PRIMITIVE_NUMBER (primitive)]))	\
       ());								\
    /* If the primitive failed to unwind the dynamic stack, lose. */	\
    if (PRIMITIVE_APPLY_INTERNAL_position != dstack_position)		\
      {									\
	outf_fatal ("\nPrimitive slipped the dynamic stack: %s\n",	\
		    (PRIMITIVE_NAME (primitive)));			\
	Microcode_Termination (TERM_EXIT);				\
      }									\
  }									\
  (Registers[REGBLOCK_PRIMITIVE]) = SHARP_F;				\
}

#define POP_PRIMITIVE_FRAME(arity) sp_register = (STACK_LOC (arity))

typedef struct interpreter_state_s * interpreter_state_t;

struct interpreter_state_s
{
  interpreter_state_t previous_state;
  unsigned int nesting_level;
  PTR dstack_position;
  jmp_buf catch_env;
  int throw_argument;
};

#define interpreter_catch_dstack_position interpreter_state->dstack_position
#define interpreter_catch_env interpreter_state->catch_env
#define interpreter_throw_argument interpreter_state->throw_argument
#define NULL_INTERPRETER_STATE ((interpreter_state_t) NULL)

extern interpreter_state_t interpreter_state;
extern void EXFUN (bind_interpreter_state, (interpreter_state_t));
extern void EXFUN (unbind_interpreter_state, (interpreter_state_t));
