/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

/* List creation and manipulation primitives. */

#include "scheme.h"
#include "prims.h"

DEFINE_PRIMITIVE ("pair?", Prim_pair, 1, 1,
 "(OBJECT)\n\
Returns #t iff OBJECT is a pair")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PAIR_P (ARG_REF (1))));
}

SCHEME_OBJECT
cons (SCHEME_OBJECT car, SCHEME_OBJECT cdr)
{
  Primitive_GC_If_Needed (2);
  (*Free++) = car;
  (*Free++) = cdr;
  return (MAKE_POINTER_OBJECT (TC_LIST, (Free - 2)));
}

DEFINE_PRIMITIVE ("cons", Prim_cons, 2, 2,
"(OBJ1 OBJ2)\n\
Returns a newly allocated pair whose car is OBJ1 and whose cdr is OBJ2.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (cons ((ARG_REF (1)), (ARG_REF (2))));
}

DEFINE_PRIMITIVE ("car", Prim_car, 1, 1,
"(PAIR)\n\
Returns the contents of the car field of PAIR.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PAIR_P);
  PRIMITIVE_RETURN (PAIR_CAR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("cdr", Prim_cdr, 1, 1,
"(PAIR)\n\
Returns the contents of the cdr field of PAIR")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PAIR_P);
  PRIMITIVE_RETURN (PAIR_CDR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("set-car!", Prim_set_car, 2, 2,
 "(PAIR OBJECT)\n\
Stores OBJECT in the car field of PAIR and returns an unspecified value.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, PAIR_P);
  SET_PAIR_CAR ((ARG_REF (1)), (ARG_REF (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("set-cdr!", Prim_set_cdr, 2, 2,
"(PAIR OBJECT)\n\
Stores OBJECT in the cdr field of PAIR and returns an unspecified value.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, PAIR_P);
  SET_PAIR_CDR ((ARG_REF (1)), (ARG_REF (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("general-car-cdr", Prim_general_car_cdr, 2, 2,
"(OBJECT PATH)\n\
\n\
This procedure is a generalization of CAR and CDR.  PATH encodes a\n\
particular sequence of CAR and CDR operations, which this procedure\n\
executes on OBJECT.\n\
\n\
PATH is an exact non-negative integer that encodes the operations in a\n\
bitwise fashion: a zero bit represents a CDR operation, and a one bit\n\
represents a CAR.  The bits are executed LSB to MSB, and the most\n\
significant one bit, rather than being interpreted as an operation,\n\
signals the end of the sequence.\n\
\n\
For example, the following are equivalent:\n\
\n\
     (general-car-cdr OBJECT #b1011)\n\
     (cdr (car (car OBJECT)))\n\
\n\
Here is a partial table of path/operation equivalents:\n\
\n\
    #b10    cdr\n\
    #b11    car\n\
    #b100   cddr\n\
    #b101   cdar\n\
    #b110   cadr\n\
    #b111   caar\n\
    #b1000  cdddr\n\
\n\
Note that this implementation restricts PATH to the length of the\n\
machine word.  Since one bit is used as a limit marker, this means the\n\
maximum length is 31 bits for a 32-bit architecture, and 63 bits for a\n\
64-bit architecture.")
{
  PRIMITIVE_HEADER (2);
  SCHEME_OBJECT object = (ARG_REF (1));
  unsigned long path = (arg_ulong_integer (2));
  while (path > 1)
    {
      if (!PAIR_P (object))
	error_wrong_type_arg (1);
      object = (((path & 1) == 0) ? (PAIR_CDR (object)) : (PAIR_CAR (object)));
      path >>= 1;
    }
  PRIMITIVE_RETURN (object);
}

DEFINE_PRIMITIVE ("system-pair?", Prim_sys_pair, 1, 1, 0)
{
  SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  object = (ARG_REF (1));
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (GC_TYPE_PAIR (object)));
}

SCHEME_OBJECT
system_pair_cons (long type, SCHEME_OBJECT car, SCHEME_OBJECT cdr)
{
  Primitive_GC_If_Needed (2);
  (*Free++) = car;
  (*Free++) = cdr;
  return (MAKE_POINTER_OBJECT (type, (Free - 2)));
}

DEFINE_PRIMITIVE ("system-pair-cons", Prim_sys_pair_cons, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  unsigned long type = (arg_ulong_index_integer (1, N_TYPE_CODES));
  if ((GC_TYPE_CODE (type)) != GC_PAIR)
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (system_pair_cons (type, (ARG_REF (2)), (ARG_REF (3))));
}

DEFINE_PRIMITIVE ("system-pair-car", Prim_sys_pair_car, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, GC_TYPE_PAIR);
  PRIMITIVE_RETURN (PAIR_CAR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("system-pair-cdr", Prim_sys_pair_cdr, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, GC_TYPE_PAIR);
  PRIMITIVE_RETURN (PAIR_CDR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("system-pair-set-car!", Prim_sys_set_car, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, GC_TYPE_PAIR);
  SET_PAIR_CAR ((ARG_REF (1)), (ARG_REF (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("system-pair-set-cdr!", Prim_sys_set_cdr, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, GC_TYPE_PAIR);
  SET_PAIR_CDR ((ARG_REF (1)), (ARG_REF (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%tagged-object?", Prim_tagged_object_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (TAGGED_OBJECT_P (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("%make-tagged-object", Prim_make_tagged_object, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Primitive_GC_If_Needed (2);
  SCHEME_OBJECT result = (MAKE_POINTER_OBJECT (TC_TAGGED_OBJECT, Free));
  (*Free++) = (ARG_REF (1));
  (*Free++) = (ARG_REF (2));
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("%tagged-object-tag", Prim_tagged_object_tag, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, TAGGED_OBJECT_P);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_REF (1)), 0));
}

DEFINE_PRIMITIVE ("%tagged-object-datum", Prim_tagged_object_datum, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, TAGGED_OBJECT_P);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_REF (1)), 1));
}

DEFINE_PRIMITIVE ("weak-pair?", Prim_weak_pair_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (WEAK_PAIR_P (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("weak-cons", Prim_weak_cons, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Primitive_GC_If_Needed (2);
  SCHEME_OBJECT result = (MAKE_POINTER_OBJECT (TC_WEAK_CONS, Free));
  (*Free++) = (ARG_REF (1));
  (*Free++) = (ARG_REF (2));
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("weak-car", Prim_weak_car, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, WEAK_PAIR_P);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_REF (1)), 0));
}

DEFINE_PRIMITIVE ("weak-cdr", Prim_weak_cdr, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, WEAK_PAIR_P);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_REF (1)), 1));
}

DEFINE_PRIMITIVE ("weak-set-car!", Prim_weak_set_car, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, WEAK_PAIR_P);
  MEMORY_SET ((ARG_REF (1)), 0, (ARG_REF (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("weak-set-cdr!", Prim_weak_set_cdr, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, WEAK_PAIR_P);
  MEMORY_SET ((ARG_REF (1)), 1, (ARG_REF (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
