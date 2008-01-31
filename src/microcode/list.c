/* -*-C-*-

$Id: list.c,v 9.39 2008/01/30 20:02:14 cph Exp $

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

DEFINE_PRIMITIVE ("LENGTH", Prim_length, 1, 1,
 "(list)\n\
  Returns the length of LIST.\
 ")
{
  SCHEME_OBJECT list;
  long i = 0;
  PRIMITIVE_HEADER (1);

  list = (ARG_REF (1));
  while (PAIR_P (list))
    {
      i += 1;
      list = (PAIR_CDR (list));
    }
  if (!EMPTY_LIST_P (list))
    error_wrong_type_arg (1);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (i));
}

DEFINE_PRIMITIVE ("MEMQ", Prim_memq, 2, 2,
 "(object list)\n\
  Returns the first pair of LIST whose car is OBJECT;\n\
  the returned pair is always one from which LIST is composed.\n\
  If OBJECT does not occur in LIST, `#f' (n.b.: not the\n\
  empty list) is returned.  `memq' uses `eq?' to compare OBJECT with\n\
  the elements of LIST, while `memv' uses `eqv?' and `member' uses\n\
  `equal?'.\n\
  \n\
       (memq 'a '(a b c))                      =>  (a b c)\n\
       (memq 'b '(a b c))                      =>  (b c)\n\
       (memq 'a '(b c d))                      =>  #f\n\
       (memq (list 'a) '(b (a) c))             =>  #f\n\
       (member (list 'a) '(b (a) c))           =>  ((a) c)\n\
       (memq 101 '(100 101 102))               =>  unspecified\n\
       (memv 101 '(100 101 102))               =>  (101 102)\n\
  \n\
  Although they are often used as predicates, `memq', `memv', and\n\
  `member' do not have question marks in their names because they return\n\
  useful values rather than just `#t' or `#f'.\
 ")
{
  SCHEME_OBJECT key;
  SCHEME_OBJECT list;
  SCHEME_OBJECT list_key;
  PRIMITIVE_HEADER (2);
  key = (ARG_REF (1));
  list = (ARG_REF (2));
  while (PAIR_P (list))
    {
      list_key = (PAIR_CAR (list));
      if (list_key == key)
	PRIMITIVE_RETURN (list);
      list = (PAIR_CDR (list));
    }
  if (!EMPTY_LIST_P (list))
    error_wrong_type_arg (2);
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("ASSQ", Prim_assq, 2, 2,
 "(object alist)\n\
  These procedures find the first pair in ALIST whose car field is\n\
  OBJECT, and return that pair; the returned pair is always an\n\
  *element* of ALIST, *not* one of the pairs from which ALIST is\n\
  composed.  If no pair in ALIST has OBJECT as its car, `#f' (n.b.:\n\
  not the empty list) is returned.  `assq' uses `eq?' to compare\n\
  OBJECT with the car fields of the pairs in ALIST, while `assv'\n\
  uses `eqv?' and `assoc' uses `equal?'.\n\
  \n\
       (define e '((a 1) (b 2) (c 3)))\n\
       (assq 'a e)                             =>  (a 1)\n\
       (assq 'b e)                             =>  (b 2)\n\
       (assq 'd e)                             =>  #f\n\
       (assq (list 'a) '(((a)) ((b)) ((c))))   =>  #f\n\
       (assoc (list 'a) '(((a)) ((b)) ((c))))  =>  ((a))\n\
       (assq 5 '((2 3) (5 7) (11 13)))         =>  unspecified\n\
       (assv 5 '((2 3) (5 7) (11 13)))         =>  (5 7)\n\
  \n\
  Although they are often used as predicates, `assq', `assv', and\n\
  `assoc' do not have question marks in their names because they return\n\
  useful values rather than just `#t' or `#f'.\
 ")
{
  SCHEME_OBJECT key;
  SCHEME_OBJECT alist;
  SCHEME_OBJECT association;
  SCHEME_OBJECT association_key;
  PRIMITIVE_HEADER (2);

  key = (ARG_REF (1));
  alist = (ARG_REF (2));
  while (PAIR_P (alist))
    {
      association = (PAIR_CAR (alist));
      if (! (PAIR_P (association)))
	error_wrong_type_arg (2);
      association_key = (PAIR_CAR (association));
      if (association_key == key)
	PRIMITIVE_RETURN (association);
      alist = (PAIR_CDR (alist));
    }
  if (!EMPTY_LIST_P (alist))
    error_wrong_type_arg (2);
  PRIMITIVE_RETURN (SHARP_F);
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
