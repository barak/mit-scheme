/* -*-C-*-

$Id: interp.h,v 9.42 2000/12/05 21:23:45 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Macros used by the interpreter and some utilities. */

extern void EXFUN (abort_to_interpreter, (int argument));
extern int EXFUN (abort_to_interpreter_argument, (void));

                     /********************/
                     /* OPEN CODED RACKS */
                     /********************/

/* Move from register to static storage and back */

/* Note defined() cannot be used because VMS does not understand it. */

#ifdef In_Main_Interpreter
#ifndef ENABLE_DEBUGGING_TOOLS
#define Cache_Registers
#endif
#endif

#ifdef Cache_Registers

#define Regs		Reg_Block
#define Stack_Pointer	Reg_Stack_Pointer
#define History		Reg_History

#define Import_Registers()						\
{									\
  Reg_Stack_Pointer = Ext_Stack_Pointer;				\
  Reg_History = Ext_History;						\
}

#define Export_Registers()						\
{									\
  Ext_History = Reg_History;						\
  Ext_Stack_Pointer = Reg_Stack_Pointer;				\
}

/* Importing History is required for C_call_scheme for work correctly because
   the recursive call to Interpret() can rotate the history:
*/
#define IMPORT_REGS_AFTER_PRIMITIVE()                                   \
{                                                                       \
    Reg_History = Ext_History;                                          \
}

#define EXPORT_REGS_BEFORE_PRIMITIVE Export_Registers

#else

#define Regs		Registers
#define Stack_Pointer	Ext_Stack_Pointer
#define History		Ext_History

#define Import_Registers()
#define Export_Registers()

#define IMPORT_REGS_AFTER_PRIMITIVE()
#define EXPORT_REGS_BEFORE_PRIMITIVE()

#endif

#define Import_Val()
#define Import_Registers_Except_Val()		Import_Registers()

#define Env		Regs[REGBLOCK_ENV]
#define Val		Regs[REGBLOCK_VAL]
#define Expression	Regs[REGBLOCK_EXPR]
#define Return		Regs[REGBLOCK_RETURN]

/* Internal_Will_Push is in stack.h. */

#ifdef ENABLE_DEBUGGING_TOOLS

#define Will_Push(N)							\
{									\
  SCHEME_OBJECT *Will_Push_Limit;					\
									\
  Internal_Will_Push((N));						\
  Will_Push_Limit = (STACK_LOC (- (N)))

#define Pushed()							\
  if (Stack_Pointer < Will_Push_Limit)					\
  {									\
    Stack_Death();							\
  }									\
}

#else

#define Will_Push(N)			Internal_Will_Push(N)
#define Pushed()			/* No op */

#endif

/*
  N in Will_Eventually_Push is the maximum contiguous (single return code)
  amount that this operation may take.  On the average case it may use less.
  M in Finished_Eventual_Pushing is the amount not yet pushed.
 */

#define Will_Eventually_Push(N)		Internal_Will_Push(N)
#define Finished_Eventual_Pushing(M)	/* No op */

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

#define STACK_PUSH(object) (STACK_LOCATIVE_PUSH (Stack_Pointer)) = (object)
#define STACK_POP() (STACK_LOCATIVE_POP (Stack_Pointer))
#define STACK_LOC(offset) (STACK_LOCATIVE_OFFSET (Stack_Pointer, (offset)))
#define STACK_REF(offset) (STACK_LOCATIVE_REFERENCE (Stack_Pointer, (offset)))

/* Fetch from register */

#define Fetch_Expression()	Expression
#define Fetch_Env()		Env
#define Fetch_Return()		Return

/* Store into register */

#define Store_Expression(P)	Expression = (P)
#define Store_Env(P)		Env = (P)
#define Store_Return(P)							\
  Return = (MAKE_OBJECT (TC_RETURN_CODE, (P)))

#define Save_Env()		STACK_PUSH (Env)
#define Restore_Env()		Env = (STACK_POP ())
#define Restore_Then_Save_Env()	Env = (STACK_REF (0))

/* Note: Save_Cont must match the definitions in sdata.h */

#define Save_Cont()							\
{									\
  STACK_PUSH (Expression);						\
  STACK_PUSH (Return);							\
}

#define Restore_Cont()							\
{									\
  Return = (STACK_POP ());						\
  Expression = (STACK_POP ());						\
}

#define Stop_Trapping()							\
{									\
  Trapping = false;							\
}

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
  (Regs[REGBLOCK_PRIMITIVE]) = (primitive);				\
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
  (Regs[REGBLOCK_PRIMITIVE]) = SHARP_F;					\
}

#define POP_PRIMITIVE_FRAME(arity) Stack_Pointer = (STACK_LOC (arity))

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
