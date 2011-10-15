/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

/* GC definitions needed by all GC-like programs. */

#ifndef SCM_GC_H
#define SCM_GC_H 1

#include "object.h"

typedef enum
{
  GC_COMPILED = -4,
  GC_VECTOR,
  GC_SPECIAL,			/* Internal GC types */
  GC_UNDEFINED,
  GC_NON_POINTER,
  GC_CELL,
  GC_PAIR,
  GC_TRIPLE,
  GC_QUADRUPLE
} gc_type_t;

#define GC_TYPE_TO_INT(type) ((int) (type))
#define GC_TYPE(object) (GC_TYPE_CODE (OBJECT_TYPE (object)))
#define GC_TYPE_CODE gc_type_code

#define GC_TYPE_NON_POINTER(object)	((GC_TYPE (object)) == GC_NON_POINTER)
#define GC_TYPE_CELL(object)		((GC_TYPE (object)) == GC_CELL)
#define GC_TYPE_PAIR(object)		((GC_TYPE (object)) == GC_PAIR)
#define GC_TYPE_TRIPLE(object)		((GC_TYPE (object)) == GC_TRIPLE)
#define GC_TYPE_QUADRUPLE(object)	((GC_TYPE (object)) == GC_QUADRUPLE)
#define GC_TYPE_UNDEFINED(object)	((GC_TYPE (object)) == GC_UNDEFINED)
#define GC_TYPE_SPECIAL(object)		((GC_TYPE (object)) == GC_SPECIAL)
#define GC_TYPE_VECTOR(object)		((GC_TYPE (object)) == GC_VECTOR)
#define GC_TYPE_COMPILED(object)	((GC_TYPE (object)) == GC_COMPILED)

typedef enum
{
  GC_POINTER_NORMAL,
  GC_POINTER_COMPILED,
  GC_POINTER_NOT
} gc_ptr_type_t;

extern gc_type_t gc_type_map [];
extern gc_type_t gc_type_code (unsigned int);
extern gc_ptr_type_t gc_ptr_type (SCHEME_OBJECT);
extern SCHEME_OBJECT * get_object_address (SCHEME_OBJECT);

#endif /* not SCM_GC_H */
