/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* Primitive declarations.
   Note that the following cannot be changed without changing
   findprim.c.
 */

#ifndef SCM_PRIM_H
#define SCM_PRIM_H

typedef SCHEME_OBJECT (*primitive_procedure_t) (void);

extern primitive_procedure_t * Primitive_Procedure_Table;
extern int * Primitive_Arity_Table;
extern int * Primitive_Count_Table;
extern const char ** Primitive_Name_Table;
extern const char ** Primitive_Documentation_Table;
extern unsigned long MAX_PRIMITIVE;

extern SCHEME_OBJECT declare_primitive
  (const char *, primitive_procedure_t, int, int, const char *);

extern SCHEME_OBJECT install_primitive
  (const char *, primitive_procedure_t, int, int, const char *);

extern SCHEME_OBJECT Prim_unimplemented (void);

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
   ? GET_LEXPR_ACTUALS							\
   : (PRIMITIVE_ARITY (prim)))

#endif /* SCM_PRIM_H */
