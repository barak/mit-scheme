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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prim.h,v 9.39 1987/12/04 22:18:35 jinx Rel $ */

/*
   Primitive declarations.

   Note that the following cannot be changed without changing
   Findprim.c.  
*/

extern Pointer (*(Primitive_Procedure_Table[]))();
extern int Primitive_Arity_Table[];
extern int Primitive_Count_Table[];
extern char *Primitive_Name_Table[];
extern long MAX_PRIMITIVE;

#define CHUNK_SIZE	20	/* Grow undefined vector by this much */

extern Pointer Undefined_Primitives;
extern Pointer Undefined_Primitives_Arity;

/* Utility macros */

#define NUMBER_OF_DEFINED_PRIMITIVES() (MAX_PRIMITIVE + 1)

#define NUMBER_OF_UNDEFINED_PRIMITIVES()		\
((Undefined_Primitives == NIL) ?			\
 0 :							\
 Get_Integer(User_Vector_Ref(Undefined_Primitives, 0)))

#define NUMBER_OF_PRIMITIVES()				\
(NUMBER_OF_UNDEFINED_PRIMITIVES() +			\
 NUMBER_OF_DEFINED_PRIMITIVES())
