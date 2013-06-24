/* -*-C-*-

$Id: prim.h,v 9.46 2001/03/08 18:00:26 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

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

/* Primitive declarations.
   Note that the following cannot be changed without changing
   findprim.c.
 */

#ifndef SCM_PRIM_H
#define SCM_PRIM_H

typedef SCHEME_OBJECT EXFUN ((*primitive_procedure_t), (void));

extern primitive_procedure_t * Primitive_Procedure_Table;
extern int * Primitive_Arity_Table;
extern int * Primitive_Count_Table;
extern CONST char ** Primitive_Name_Table;
extern CONST char ** Primitive_Documentation_Table;
extern long MAX_PRIMITIVE;

extern SCHEME_OBJECT EXFUN
  (declare_primitive,
   (CONST char *, primitive_procedure_t, int, int, CONST char *));

extern SCHEME_OBJECT EXFUN
  (install_primitive,
   (CONST char *, primitive_procedure_t, int, int, CONST char *));

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
