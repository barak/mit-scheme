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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prims.h,v 5.2 1987/01/12 17:18:44 cph Exp $ */

/* This file contains some macros for defining primitives,
   for argument type or value checking, and for accessing
   the arguments. */

/* Definition of primitives.  See storage.c for some information. */

#define Define_Primitive(C_Name, Number_of_args, Scheme_Name)	\
extern Pointer C_Name();					\
Pointer C_Name()

#define Built_In_Primitive(C_Name, Number_of_args, Scheme_Name)	\
Define_Primitive(C_Name, Number_of_args, Scheme_Name)

extern Pointer Not_Implemented_Yet();

#define NIY(C_Name, Number_of_args, Scheme_Name)		\
Built_In_Primitive(C_Name, Number_of_args, Scheme_Name)		\
{ return Not_Implemented_Yet(Scheme_Name);			\
}

/* Preambles for primitive procedures.  These store the arguments into
 * local variables for fast access.
 */

#define Primitive_0_Args()

#define Primitive_1_Args()	fast Pointer Arg1 = Stack_Ref(0);\
				Primitive_0_Args()

#define Primitive_2_Args()      fast Pointer Arg2 = Stack_Ref(1);\
				Primitive_1_Args()

#define Primitive_3_Args()      fast Pointer Arg3 = Stack_Ref(2);\
				Primitive_2_Args()

#define Primitive_4_Args()      fast Pointer Arg4 = Stack_Ref(3);\
				Primitive_3_Args()

#define Primitive_5_Args()      fast Pointer Arg5 = Stack_Ref(4);\
				Primitive_4_Args()

#define Primitive_6_Args()      fast Pointer Arg6 = Stack_Ref(5);\
				Primitive_5_Args()

#define Primitive_7_Args()      fast Pointer Arg7 = Stack_Ref(6);\
				Primitive_6_Args()

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

#define Arg_1_Type(TC)  					\
if ((pointer_type (Arg1)) != (TC)) error_wrong_type_arg_1 ()

#define Arg_2_Type(TC)  					\
if ((pointer_type (Arg2)) != (TC)) error_wrong_type_arg_2 ()

#define Arg_3_Type(TC)						\
if ((pointer_type (Arg3)) != (TC)) error_wrong_type_arg_3 ()

#define Arg_4_Type(TC)  					\
if ((pointer_type (Arg4)) != (TC)) error_wrong_type_arg_4 ()

#define Arg_5_Type(TC)  					\
if ((pointer_type (Arg5)) != (TC)) error_wrong_type_arg_5 ()

#define Arg_6_Type(TC)						\
if ((pointer_type (Arg6)) != (TC)) error_wrong_type_arg_6 ()

#define Arg_7_Type(TC)						\
if ((pointer_type (Arg7)) != (TC)) error_wrong_type_arg_7 ()

#define Arg_8_Type(TC)						\
if ((pointer_type (Arg8)) != (TC)) error_wrong_type_arg_8 ()

#define Arg_9_Type(TC)						\
if ((pointer_type (Arg9)) != (TC)) error_wrong_type_arg_9 ()

#define Arg_10_Type(TC)						\
if ((pointer_type (Arg10)) != (TC)) error_wrong_type_arg_10 ()


#define Arg_1_GC_Type(GCTC)                                     \
if ((GC_Type (Arg1)) != GCTC) error_wrong_type_arg_1 ()

#define Arg_2_GC_Type(GCTC)                                     \
if ((GC_Type (Arg2)) != GCTC) error_wrong_type_arg_2 ()

#define Arg_3_GC_Type(GCTC)                                     \
if ((GC_Type (Arg3)) != GCTC) error_wrong_type_arg_3 ()

#define guarantee_fixnum_arg_1()				\
if (! (fixnum_p (Arg1))) error_wrong_type_arg_1 ()

#define guarantee_fixnum_arg_2()				\
if (! (fixnum_p (Arg2))) error_wrong_type_arg_2 ()

#define guarantee_fixnum_arg_3()				\
if (! (fixnum_p (Arg3))) error_wrong_type_arg_3 ()

#define guarantee_fixnum_arg_4()				\
if (! (fixnum_p (Arg4))) error_wrong_type_arg_4 ()

#define guarantee_fixnum_arg_5()				\
if (! (fixnum_p (Arg5))) error_wrong_type_arg_5 ()

#define guarantee_fixnum_arg_6()				\
if (! (fixnum_p (Arg6))) error_wrong_type_arg_6 ()

extern long guarantee_nonnegative_integer_arg_1();
extern long guarantee_nonnegative_integer_arg_2();
extern long guarantee_nonnegative_integer_arg_3();
extern long guarantee_nonnegative_integer_arg_4();
extern long guarantee_nonnegative_integer_arg_5();
extern long guarantee_nonnegative_integer_arg_6();
extern long guarantee_nonnegative_integer_arg_7();
extern long guarantee_nonnegative_integer_arg_8();
extern long guarantee_nonnegative_integer_arg_9();
extern long guarantee_nonnegative_integer_arg_10();

extern long guarantee_index_arg_1();
extern long guarantee_index_arg_2();
extern long guarantee_index_arg_3();
extern long guarantee_index_arg_4();
extern long guarantee_index_arg_5();
extern long guarantee_index_arg_6();
extern long guarantee_index_arg_7();
extern long guarantee_index_arg_8();
extern long guarantee_index_arg_9();
extern long guarantee_index_arg_10();
