/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/interp.h,v 9.33 1990/06/20 17:41:20 cph Rel $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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
  Return = MAKE_OBJECT (TC_RETURN_CODE, (P))

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
  if (Return_Hook_Address != NULL)					\
  {									\
    *Return_Hook_Address = Old_Return_Code;				\
  }									\
  Return_Hook_Address = NULL;						\
}

/* Primitive utility macros */

/* A primitive object has two components (besides the type code), a
   table index in the low 12 bits (assuming datum fields are 24 bits
   wide), and a virtual index in the upper 12 bits.  The table index
   is always guaranteed to be a valid entry into
   Primitive_Procedure_Table.  For unimplemented primitives it is the
   index of the last entry in the table, which causes an error when
   invoked.  For implemented primitives it is the real index.  The
   virtual index is 0 for implemented primitives (for histerical
   reasons), and the actual virtual index (higher than any real table
   index) for unimplemented primitives.
 */

#define PRIMITIVE_TABLE_INDEX(primitive)				\
((primitive) & HALF_DATUM_MASK)

#define PRIMITIVE_VIRTUAL_INDEX(primitive)				\
(((primitive) >> HALF_DATUM_LENGTH) & HALF_DATUM_MASK)

#define MAKE_PRIMITIVE_OBJECT(virtual, real)				\
(MAKE_OBJECT (TC_PRIMITIVE, (((virtual) << HALF_DATUM_LENGTH) | (real))))

/* Does this fail for the first unimplemented primitive if there are no
   implemented primitives?
 */

#define IMPLEMENTED_PRIMITIVE_P(primitive)				\
(PRIMITIVE_VIRTUAL_INDEX(primitive) == 0)

#define PRIMITIVE_NUMBER(primitive)					\
((IMPLEMENTED_PRIMITIVE_P(primitive))	?				\
 (PRIMITIVE_TABLE_INDEX(primitive))	:				\
 (PRIMITIVE_VIRTUAL_INDEX(primitive)))

/* This will automagically cause an error if the primitive is
   not implemented. */

#ifndef ENABLE_DEBUGGING_TOOLS

#define PRIMITIVE_APPLY PRIMITIVE_APPLY_INTERNAL

#else

extern SCHEME_OBJECT EXFUN
  (primitive_apply_internal, (SCHEME_OBJECT primitive));
#define PRIMITIVE_APPLY(loc, primitive)					\
  (loc) = (primitive_apply_internal (primitive))

#endif

extern char * EXFUN (primitive_to_name, (SCHEME_OBJECT primitive));
extern long EXFUN (primitive_to_arity, (SCHEME_OBJECT primitive));
extern long EXFUN (primitive_to_arguments, (SCHEME_OBJECT primitive));

#define PRIMITIVE_APPLY_INTERNAL(loc, primitive)			\
{									\
  (Regs[REGBLOCK_PRIMITIVE]) = (primitive);				\
  {									\
    /* Save the dynamic-stack position. */				\
    PTR PRIMITIVE_APPLY_INTERNAL_position = dstack_position;		\
    (loc) =								\
      ((*								\
	(Primitive_Procedure_Table					\
	 [PRIMITIVE_TABLE_INDEX (primitive)]))				\
       ());								\
    /* If the primitive failed to unwind the dynamic stack, lose. */	\
    if (PRIMITIVE_APPLY_INTERNAL_position != dstack_position)		\
      {									\
	fprintf (stderr, "\nPrimitive slipped the dynamic stack: %s\n",	\
		 (primitive_to_name (primitive)));			\
	fflush (stderr);						\
	Microcode_Termination (TERM_EXIT);				\
      }									\
  }									\
  (Regs[REGBLOCK_PRIMITIVE]) = SHARP_F;					\
}

/* This is only valid for implemented primitives. */

#define PRIMITIVE_ARITY(primitive)					\
  (Primitive_Arity_Table [PRIMITIVE_TABLE_INDEX (primitive)])

#define PRIMITIVE_N_PARAMETERS primitive_to_arity
#define PRIMITIVE_N_ARGUMENTS primitive_to_arguments
#define POP_PRIMITIVE_FRAME(arity) Stack_Pointer = (STACK_LOC (arity))
