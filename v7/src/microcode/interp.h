/* -*-C-*-

$Id: interp.h,v 9.38 1993/08/03 08:29:51 gjr Exp $

Copyright (c) 1987-1993 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

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

#else

#define Regs		Registers
#define Stack_Pointer	Ext_Stack_Pointer
#define History		Ext_History

#define Import_Registers()
#define Export_Registers()

#endif

#define Import_Val()
#define Import_Registers_Except_Val()		Import_Registers()

#define IMPORT_REGS_AFTER_PRIMITIVE()
#define EXPORT_REGS_BEFORE_PRIMITIVE Export_Registers

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
  Cont_Print ();							\
}

#define Restore_Cont()							\
{									\
  Return = (STACK_POP ());						\
  Expression = (STACK_POP ());						\
  if (Cont_Debug)							\
  {									\
    Print_Return(RESTORE_CONT_RETURN_MESSAGE);				\
    Print_Expression(Fetch_Expression(),				\
		     RESTORE_CONT_EXPR_MESSAGE);			\
    printf ("\n");							\
  }									\
}

#define Cont_Print()							\
{									\
  if (Cont_Debug)							\
  {									\
    Print_Return(CONT_PRINT_RETURN_MESSAGE);				\
    Print_Expression(Fetch_Expression(),				\
		     CONT_PRINT_EXPR_MESSAGE);				\
    printf ("\n");							\
  }									\
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
