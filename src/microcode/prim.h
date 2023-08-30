/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

extern primitive_procedure_t* Primitive_Procedure_Table;
extern int* Primitive_Arity_Table;
extern int* Primitive_Count_Table;
extern const char** Primitive_Name_Table;
extern const char** Primitive_Documentation_Table;
extern unsigned long MAX_PRIMITIVE;

extern SCHEME_OBJECT declare_primitive
  (const char*, primitive_procedure_t, int, int, const char*);

extern SCHEME_OBJECT install_primitive
  (const char*, primitive_procedure_t, int, int, const char*);

extern SCHEME_OBJECT Prim_unimplemented (void);

#define primitive_number object_datum

static inline SCHEME_OBJECT
make_primitive_object (unsigned long index)
{
  return make_object (TC_PRIMITIVE, index);
}

static inline const char*
primitive_name (SCHEME_OBJECT prim)
{
  return Primitive_Name_Table [primitive_number (prim)];
}

static inline int
primitive_arity (SCHEME_OBJECT prim)
{
  return Primitive_Arity_Table [primitive_number (prim)];
}

static inline const char*
primitive_documentation (SCHEME_OBJECT prim)
{
  return Primitive_Documentation_Table [primitive_number (prim)];
}

static inline primitive_procedure_t
primitive_procedure (SCHEME_OBJECT prim)
{
  return Primitive_Procedure_Table [primitive_number (prim)];
}

static inline bool
primitive_implemented_p (SCHEME_OBJECT prim)
{
  return primitive_procedure (prim) != Prim_unimplemented;
}

static inline unsigned long
primitive_n_args (SCHEME_OBJECT prim)
{
  return (primitive_arity (prim) == LEXPR_PRIMITIVE_ARITY)
         ? GET_LEXPR_ACTUALS
         : primitive_arity (prim);
}

#define NUMBER_OF_PRIMITIVES MAX_PRIMITIVE
#define pop_primitive_frame increment_sp

#endif /* SCM_PRIM_H */
