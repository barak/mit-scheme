/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/interp.h,v 9.27 1987/11/20 08:17:10 jinx Exp $
 *
 * Macros used by the interpreter and some utilities.
 *
 */

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

#define Import_Regs_After_Primitive()
#define Export_Regs_Before_Primitive()		Export_Registers()

#define Env		Regs[REGBLOCK_ENV]
#define Val		Regs[REGBLOCK_VAL]
#define Expression	Regs[REGBLOCK_EXPR]
#define Return		Regs[REGBLOCK_RETURN]

/* Internal_Will_Push is in stack.h. */

#ifdef ENABLE_DEBUGGING_TOOLS

#define Will_Push(N)							\
{									\
  Pointer *Will_Push_Limit;						\
									\
  Internal_Will_Push((N));						\
  Will_Push_Limit = Simulate_Pushing(N)

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
 * These operations hide the direction of stack growth.
 * Throw in stack.h, Allocate_New_Stacklet in utils.c, apply, cwcc and
 * friends in hooks.c, and possibly other stuff, depend on the direction in
 * which the stack grows. 
 */

#define Push(P)				*--Stack_Pointer = (P)
#define Pop()				(*Stack_Pointer++)
#define Stack_Ref(N)			(Stack_Pointer[(N)])
#define Simulate_Pushing(N)		(Stack_Pointer - (N))
#define Simulate_Popping(N)		(Stack_Pointer + (N))

#define Top_Of_Stack()			Stack_Ref(0)
#define Stack_Distance(previous_top_of_stack)	\
  ((previous_top_of_stack) -  (&Top_Of_Stack()))

/* These can be used when SP is a pointer into the stack, to make
 * stack gap operations independent of the direction of stack growth.
 * They must match Push and Pop above.
 */

#define Push_From(SP)			*--(SP)
#define Pop_Into(SP, What)		(*(SP)++) = (What)

/* Fetch from register */

#define Fetch_Expression()	Expression
#define Fetch_Env()		Env
#define Fetch_Return()		Return

/* Store into register */

#define Store_Expression(P)	Expression = (P)
#define Store_Env(P)		Env = (P)
#define Store_Return(P)							\
  Return = Make_Non_Pointer(TC_RETURN_CODE, (P))

#define Save_Env()		Push(Env)
#define Restore_Env()		Env = Pop()
#define Restore_Then_Save_Env()	Env = Top_Of_Stack()

/* Note: Save_Cont must match the definitions in sdata.h */                                

#define Save_Cont()							\
{									\
  Push(Expression);							\
  Push(Return);								\
  Cont_Print();								\
}

#define Restore_Cont()							\
{									\
  Return = Pop();							\
  Expression = Pop();							\
  if (Cont_Debug)							\
  {									\
    Print_Return(RESTORE_CONT_RETURN_MESSAGE);				\
    Print_Expression(Fetch_Expression(),				\
		     RESTORE_CONT_EXPR_MESSAGE);			\
    CRLF();								\
  }									\
}

#define Cont_Print()							\
{									\
  if (Cont_Debug)							\
  {									\
    Print_Return(CONT_PRINT_RETURN_MESSAGE);				\
    Print_Expression(Fetch_Expression(),				\
		     CONT_PRINT_EXPR_MESSAGE);				\
    CRLF();								\
  }									\
}

#define Stop_Trapping()							\
{									\
  Trapping = false;							\
  if (Return_Hook_Address != NULL)					\
  {									\
    *Return_Hook_Address = Old_Return_Code;				\
  }									\
  Return_Hook_Address = NULL;						\
}

/* Primitive utility macros */

/* The first two are only valid for implemented primitives. */

#define Internal_Apply_Primitive(primitive_code)			\
  ((*(Primitive_Procedure_Table[primitive_code]))())

#define PRIMITIVE_ARITY(primitive_code)					\
  (Primitive_Arity_Table[primitive_code])

extern long primitive_to_arity();

#define PRIMITIVE_N_PARAMETERS(primitive_code)				\
  (primitive_to_arity(primitive_code))

/* This is only valid during a primitive call. */

extern long primitive_to_arguments();

#define PRIMITIVE_N_ARGUMENTS(primitive_code)				\
  (primitive_to_arguments(primitive_code))

#define Pop_Primitive_Frame(NArgs)					\
  Stack_Pointer = Simulate_Popping(NArgs)

#define UNWIND_PROTECT(body_statement, cleanup_statement) do		\
{									\
  jmp_buf UNWIND_PROTECT_new_buf, *UNWIND_PROTECT_old_buf;		\
  int UNWIND_PROTECT_value;						\
									\
  UNWIND_PROTECT_old_buf = Back_To_Eval;				\
  Back_To_Eval = ((jmp_buf *) UNWIND_PROTECT_new_buf);			\
  UNWIND_PROTECT_value = (setjmp (*Back_To_Eval));			\
  if (UNWIND_PROTECT_value != 0)					\
    {									\
      Back_To_Eval = UNWIND_PROTECT_old_buf;				\
      cleanup_statement;						\
      longjmp ((*Back_To_Eval), UNWIND_PROTECT_value);			\
    }									\
  body_statement;							\
  Back_To_Eval = UNWIND_PROTECT_old_buf;				\
  cleanup_statement;							\
} while (0)
