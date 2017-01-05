/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

DEFINE_PRIMITIVE ("PAIR?", Prim_pair, 1, 1,
 "(object)\n\
  Returns #t if object is a pair; otherwise returns #f.\
")
{
  SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  object = (ARG_REF (1));
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PAIR_P (object)));
}

SCHEME_OBJECT
cons (SCHEME_OBJECT car,
       SCHEME_OBJECT cdr)
{
  Primitive_GC_If_Needed (2);
  (*Free++) = car;
  (*Free++) = cdr;
  return (MAKE_POINTER_OBJECT (TC_LIST, (Free - 2)));
}

DEFINE_PRIMITIVE ("CONS", Prim_cons, 2, 2,
 "(obj1 obj2)\n\
  Returns a newly allocated pair whose car is OBJ1 and whose cdr is OBJ2.\n\
  The pair is guaranteed to be different (in the sense of EQV?) from other\n\
  previously existing object.\
 ")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (cons ((ARG_REF (1)), (ARG_REF (2))));
}

DEFINE_PRIMITIVE ("CAR", Prim_car, 1, 1,
 "(pair)\n\
  Returns the contents of the car field of PAIR.\n\
  Note that it is an error to take the CAR of an empty list.\
 ")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PAIR_P);
  PRIMITIVE_RETURN (PAIR_CAR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("CDR", Prim_cdr, 1, 1,
 "(pair)\n\
  Returns the contents of the cdr field of PAIR.\n\
  Note that it is an error to take the CDR of an empty list.\
 ")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PAIR_P);
  PRIMITIVE_RETURN (PAIR_CDR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("SET-CAR!", Prim_set_car, 2, 2,
 "(pair object)\n\
  Store OBJECT in the car field of PAIR.\n\
  The value returned by SET-CAR! is unspecified.\
 ")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, PAIR_P);
  {
    SCHEME_OBJECT pair = (ARG_REF (1));
    SCHEME_OBJECT car = (ARG_REF (2));
    SET_PAIR_CAR (pair, car);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SET-CDR!", Prim_set_cdr, 2, 2,
 "(pair object)\n\
  Store OBJECT in the cdr field of PAIR.\n\
  The value returned by SET-CDR! is unspecified.\
 ")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, PAIR_P);
  {
    SCHEME_OBJECT pair = (ARG_REF (1));
    SCHEME_OBJECT cdr = (ARG_REF (2));
    SET_PAIR_CDR (pair, cdr);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* (GENERAL-CAR-CDR LIST DIRECTIONS)
   DIRECTIONS encodes a string of CAR and CDR operations to be
   performed on LIST as follows:
     1   = NOP	101 = CDAR
     10  = CDR	110 = CADR
     11  = CAR	111 = CAAR
     100 = CDDR	... */

DEFINE_PRIMITIVE ("GENERAL-CAR-CDR", Prim_general_car_cdr, 2, 2,
 "(object path)\n\
 This procedure is a generalization of `car' and `cdr'. PATH\n\
 encodes a particular sequence of `car' and `cdr' operations, which\n\
 `general-car-cdr' executes on OBJECT. PATH is an exact\n\
 non-negative integer that encodes the operations in a bitwise\n\
 fashion: a zero bit represents a `cdr' operation, and a one bit\n\
 represents a `car'.  The bits are executed LSB to MSB, and the\n\
 most significant one bit, rather than being interpreted as an\n\
 operation, signals the end of the sequence.\n\
 \n\
 For example, the following are equivalent:\n\
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
  Note that PATH is restricted to a machine-dependent range,\n\
  usually the size of a machine word.  On many machines, this means that\n\
  the maximum length of PATH will be 30 operations (32 bits, less the\n\
  sign bit and the "end-of-sequence" bit).\
 ")
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT object = (ARG_REF (1));
    long CAR_CDR_Pattern = (arg_nonnegative_integer (2));
    while (CAR_CDR_Pattern > 1)
      {
	if (! (PAIR_P (object)))
	  error_wrong_type_arg (1);
	object =
	  (((CAR_CDR_Pattern & 1) == 0)
	   ? (PAIR_CDR (object))
	   : (PAIR_CAR (object)));
	CAR_CDR_Pattern >>= 1;
      }
    PRIMITIVE_RETURN (object);
  }
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR?", Prim_sys_pair, 1, 1, 0)
{
  SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  object = (ARG_REF (1));
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (GC_TYPE_PAIR (object)));
}

SCHEME_OBJECT
system_pair_cons (long type,
       SCHEME_OBJECT car,
       SCHEME_OBJECT cdr)
{
  Primitive_GC_If_Needed (2);
  (*Free++) = car;
  (*Free++) = cdr;
  return (MAKE_POINTER_OBJECT (type, (Free - 2)));
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR-CONS", Prim_sys_pair_cons, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    unsigned long type = (arg_ulong_index_integer (1, N_TYPE_CODES));
    if ((GC_TYPE_CODE (type)) != GC_PAIR)
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (system_pair_cons (type, (ARG_REF (2)), (ARG_REF (3))));
  }
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR-CAR", Prim_sys_pair_car, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, GC_TYPE_PAIR);
  PRIMITIVE_RETURN (PAIR_CAR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR-CDR", Prim_sys_pair_cdr, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, GC_TYPE_PAIR);
  PRIMITIVE_RETURN (PAIR_CDR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR-SET-CAR!", Prim_sys_set_car, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, GC_TYPE_PAIR);
  {
    SCHEME_OBJECT pair = (ARG_REF (1));
    SCHEME_OBJECT car = (ARG_REF (2));
    SET_PAIR_CAR (pair, car);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR-SET-CDR!", Prim_sys_set_cdr, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, GC_TYPE_PAIR);
  {
    SCHEME_OBJECT pair = (ARG_REF (1));
    SCHEME_OBJECT cdr = (ARG_REF (2));
    SET_PAIR_CDR (pair, cdr);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}
