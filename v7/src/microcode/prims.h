/* -*-C-*-

Copyright (c) 1986 Massachusetts Institute of Technology

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

/* File: primitive.h
 *
 * This file contains some macros for defining primitives,
 * for argument type or value checking, and for accessing
 * the arguments.
 *
 */

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
        { Back_Out_Of_Primitive();				\
          longjmp(*Back_To_Eval, Err_No);			\
        }

#define Primitive_Interrupt()					\
        { Back_Out_Of_Primitive();				\
          longjmp(*Back_To_Eval, PRIM_INTERRUPT);		\
        }

#define Primitive_GC(Amount)					\
        { Request_GC(Amount);					\
          Primitive_Interrupt();				\
        }

#define Primitive_GC_If_Needed(Amount)				\
        if (GC_Check(Amount)) Primitive_GC(Amount)

#define Arg_1_Type(TC)  					\
if (Type_Code(Arg1) != (TC)) Primitive_Error(ERR_ARG_1_WRONG_TYPE)

#define Arg_2_Type(TC)  					\
if (Type_Code(Arg2) != (TC)) Primitive_Error(ERR_ARG_2_WRONG_TYPE)

#define Arg_3_Type(TC)						\
if (Type_Code(Arg3) != (TC)) Primitive_Error(ERR_ARG_3_WRONG_TYPE)

#define Arg_4_Type(TC)  					\
if (Type_Code(Arg4) != (TC)) Primitive_Error(ERR_ARG_4_WRONG_TYPE)

#define Arg_5_Type(TC)  					\
if (Type_Code(Arg5) != (TC)) Primitive_Error(ERR_ARG_5_WRONG_TYPE)

#define Arg_6_Type(TC)						\
if (Type_Code(Arg6) != (TC)) Primitive_Error(ERR_ARG_6_WRONG_TYPE)

#define Arg_7_Type(TC)						\
if (Type_Code(Arg7) != (TC)) Primitive_Error(ERR_ARG_7_WRONG_TYPE)


#define Arg_1_GC_Type(GCTC)                                     \
if (GC_Type(Arg1) != GCTC) Primitive_Error(ERR_ARG_1_WRONG_TYPE)

#define Arg_2_GC_Type(GCTC)                                     \
if (GC_Type(Arg2) != GCTC) Primitive_Error(ERR_ARG_2_WRONG_TYPE)

#define Arg_3_GC_Type(GCTC)                                     \
if (GC_Type(Arg3) != GCTC) Primitive_Error(ERR_ARG_3_WRONG_TYPE)


/* And a procedure or two for range checking */

#define Range_Check(To_Where, P, Low, High, Error)		\
        { To_Where = Get_Integer(P);				\
          if ((To_Where < (Low)) || (To_Where > (High)))	\
            Primitive_Error(Error); }

#define Sign_Extend_Range_Check(To_Where, P, Low, High, Error)	\
        { Sign_Extend(P,To_Where);				\
          if ((To_Where < (Low)) || (To_Where > (High)))	\
            Primitive_Error(Error); }
