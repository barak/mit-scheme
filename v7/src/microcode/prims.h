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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prims.h,v 9.24 1987/05/14 13:49:36 cph Exp $ */

/* This file contains some macros for defining primitives,
   for argument type or value checking, and for accessing
   the arguments. */

/* Definition of primitives. */

#define Define_Primitive(C_Name, Number_of_args, Scheme_Name)	\
extern Pointer C_Name();					\
Pointer C_Name()

#define Built_In_Primitive(C_Name, Number_of_args, Scheme_Name, index)	\
extern Pointer C_Name();						\
Pointer C_Name()

/* Preambles for primitive procedures.  These store the arguments into
 * local variables for fast access.
 */

#ifdef ENABLE_PRIMITIVE_PROFILING
#define primitive_entry_hook() record_primitive_entry (Fetch_Expression ())
#else
#define primitive_entry_hook() {}
#endif

#define Primitive_0_Args()	primitive_entry_hook ()

#define Primitive_1_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				primitive_entry_hook ()

#define Primitive_2_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				primitive_entry_hook ()

#define Primitive_3_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				primitive_entry_hook ()

#define Primitive_4_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				primitive_entry_hook ()

#define Primitive_5_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				fast Pointer Arg5 = Stack_Ref(4);	\
				primitive_entry_hook ()

#define Primitive_6_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				fast Pointer Arg5 = Stack_Ref(4);	\
				fast Pointer Arg6 = Stack_Ref(5);	\
				primitive_entry_hook ()

#define Primitive_7_Args()	fast Pointer Arg1 = Stack_Ref(0);	\
				fast Pointer Arg2 = Stack_Ref(1);	\
				fast Pointer Arg3 = Stack_Ref(2);	\
				fast Pointer Arg4 = Stack_Ref(3);	\
				fast Pointer Arg5 = Stack_Ref(4);	\
				fast Pointer Arg6 = Stack_Ref(5);	\
				fast Pointer Arg7 = Stack_Ref(6);	\
				primitive_entry_hook ()

#define Primitive_1_Arg()	Primitive_1_Args()

/* Various utilities */

#define Primitive_Error(Err_No)					\
{								\
  signal_error_from_primitive (Err_No);				\
}

#define Primitive_Interrupt()					\
{								\
  signal_interrupt_from_primitive ();				\
}

#define Special_Primitive_Interrupt(Local_Mask)			\
{								\
  special_interrupt_from_primitive (Local_Mask);		\
}

#define Primitive_GC(Amount)					\
{								\
  Request_GC (Amount);						\
  Primitive_Interrupt ();					\
}

#define Primitive_GC_If_Needed(Amount)				\
if (GC_Check (Amount)) Primitive_GC(Amount)

#define Range_Check(To_Where, P, Low, High, Error)		\
{								\
  To_Where = Get_Integer (P);					\
  if ((To_Where < (Low)) || (To_Where > (High)))		\
    Primitive_Error (Error);					\
}

#define Sign_Extend_Range_Check(To_Where, P, Low, High, Error)	\
{								\
  Sign_Extend ((P), To_Where);					\
  if ((To_Where < (Low)) || (To_Where > (High)))		\
    Primitive_Error (Error);					\
}

#define CHECK_ARG(argument, type_p)					\
do									\
{									\
  if (! (type_p (ARG_REF (argument))))					\
    error_wrong_type_arg (argument);					\
} while (0)

#define ARG_REF(argument) (Stack_Ref (argument - 1))

extern long arg_nonnegative_integer ();
extern long arg_index_integer ();

/* Instances of the following should be flushed. */

#define Arg_1_Type(TC)  					\
do { if ((pointer_type (Arg1)) != (TC)) error_wrong_type_arg (1); } while (0)

#define Arg_2_Type(TC)  					\
do { if ((pointer_type (Arg2)) != (TC)) error_wrong_type_arg (2); } while (0)

#define Arg_3_Type(TC)						\
do { if ((pointer_type (Arg3)) != (TC)) error_wrong_type_arg (3); } while (0)

#define Arg_4_Type(TC)  					\
do { if ((pointer_type (Arg4)) != (TC)) error_wrong_type_arg (4); } while (0)

#define Arg_5_Type(TC)  					\
do { if ((pointer_type (Arg5)) != (TC)) error_wrong_type_arg (5); } while (0)

#define Arg_6_Type(TC)						\
do { if ((pointer_type (Arg6)) != (TC)) error_wrong_type_arg (6); } while (0)

#define Arg_7_Type(TC)						\
do { if ((pointer_type (Arg7)) != (TC)) error_wrong_type_arg (7); } while (0)

#define Arg_8_Type(TC)						\
do { if ((pointer_type (Arg8)) != (TC)) error_wrong_type_arg (8); } while (0)

#define Arg_9_Type(TC)						\
do { if ((pointer_type (Arg9)) != (TC)) error_wrong_type_arg (9); } while (0)

#define Arg_10_Type(TC)						\
do { if ((pointer_type (Arg10)) != (TC)) error_wrong_type_arg (10); } while (0)


#define Arg_1_GC_Type(GCTC)                                     \
do { if ((GC_Type (Arg1)) != GCTC) error_wrong_type_arg (1); } while (0)

#define Arg_2_GC_Type(GCTC)                                     \
do { if ((GC_Type (Arg2)) != GCTC) error_wrong_type_arg (2); } while (0)

#define Arg_3_GC_Type(GCTC)                                     \
do { if ((GC_Type (Arg3)) != GCTC) error_wrong_type_arg (3); } while (0)
