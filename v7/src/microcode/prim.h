/* -*-C-*-

$Id: prim.h,v 9.44 1993/08/03 08:29:58 gjr Exp $

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

/* Primitive declarations.
   Note that the following cannot be changed without changing
   findprim.c.
 */

#ifndef SCM_PRIM_H
#define SCM_PRIM_H

typedef SCHEME_OBJECT EXFUN ((* primitive_procedure_t), (void));

extern primitive_procedure_t * Primitive_Procedure_Table;
extern int * Primitive_Arity_Table;
extern int * Primitive_Count_Table;
extern char ** Primitive_Name_Table;
extern char ** Primitive_Documentation_Table;
extern long MAX_PRIMITIVE;

extern SCHEME_OBJECT EXFUN (Prim_unimplemented, (void));

#define PRIMITIVE_NUMBER(primitive) (OBJECT_DATUM (primitive))

#define MAKE_PRIMITIVE_OBJECT(index) (MAKE_OBJECT (TC_PRIMITIVE, (index)))

#define IMPLEMENTED_PRIMITIVE_P(prim)					\
  ((Primitive_Procedure_Table[(PRIMITIVE_NUMBER (prim))])		\
   != Prim_unimplemented)

#define NUMBER_OF_PRIMITIVES()	(MAX_PRIMITIVE)

#define PRIMITIVE_ARITY(prim)						\
  (Primitive_Arity_Table [PRIMITIVE_NUMBER (prim)])

#define PRIMITIVE_DOCUMENTATION(prim)					\
  (Primitive_Documentation_Table[(PRIMITIVE_NUMBER (prim))])

#define PRIMITIVE_NAME(prim)						\
  (Primitive_Name_Table[(PRIMITIVE_NUMBER (prim))])

#define PRIMITIVE_N_PARAMETERS(prim) (PRIMITIVE_ARITY (prim))

#define PRIMITIVE_N_ARGUMENTS(prim)					\
  (((PRIMITIVE_ARITY (prim)) == LEXPR_PRIMITIVE_ARITY)			\
   ? ((long) (Regs[REGBLOCK_LEXPR_ACTUALS]))				\
   : (PRIMITIVE_ARITY (prim)))

#endif /* SCM_PRIM_H */
