/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/list.c,v 9.29 1992/01/15 02:33:17 jinx Exp $

Copyright (c) 1987-92 Massachusetts Institute of Technology

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

/* List creation and manipulation primitives. */

#include "scheme.h"
#include "prims.h"

DEFINE_PRIMITIVE ("PAIR?", Prim_pair, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PAIR_P (object)));
}

SCHEME_OBJECT
DEFUN (cons, (car, cdr),
       SCHEME_OBJECT car
       AND SCHEME_OBJECT cdr)
{
  Primitive_GC_If_Needed (2);
  (*Free++) = car;
  (*Free++) = cdr;
  return (MAKE_POINTER_OBJECT (TC_LIST, (Free - 2)));
}

DEFINE_PRIMITIVE ("CONS", Prim_cons, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (cons ((ARG_REF (1)), (ARG_REF (2))));
}

DEFINE_PRIMITIVE ("CAR", Prim_car, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PAIR_P);
  PRIMITIVE_RETURN (PAIR_CAR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("CDR", Prim_cdr, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PAIR_P);
  PRIMITIVE_RETURN (PAIR_CDR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("SET-CAR!", Prim_set_car, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, PAIR_P);
  {
    fast SCHEME_OBJECT pair = (ARG_REF (1));
    fast SCHEME_OBJECT car = (ARG_REF (2));
    SIDE_EFFECT_IMPURIFY (pair, car);
    SET_PAIR_CAR (pair, car);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SET-CDR!", Prim_set_cdr, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, PAIR_P);
  {
    fast SCHEME_OBJECT pair = (ARG_REF (1));
    fast SCHEME_OBJECT cdr = (ARG_REF (2));
    SIDE_EFFECT_IMPURIFY (pair, cdr);
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

DEFINE_PRIMITIVE ("GENERAL-CAR-CDR", Prim_general_car_cdr, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    fast SCHEME_OBJECT object = (ARG_REF (1));
    fast long CAR_CDR_Pattern = (arg_nonnegative_integer (2));
    while (CAR_CDR_Pattern > 1)
      {
	TOUCH_IN_PRIMITIVE (object, object);
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

DEFINE_PRIMITIVE ("LENGTH", Prim_length, 1, 1, 0)
{
  fast SCHEME_OBJECT list;
  fast long i = 0;
  PRIMITIVE_HEADER (1);

  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), list);
  while (PAIR_P (list))
    {
      i += 1;
      TOUCH_IN_PRIMITIVE ((PAIR_CDR (list)), list);
    }
  if (list != EMPTY_LIST)
    error_wrong_type_arg (1);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (i));
}

DEFINE_PRIMITIVE ("MEMQ", Prim_memq, 2, 2, 0)
{
  fast SCHEME_OBJECT key;
  fast SCHEME_OBJECT list;
  fast SCHEME_OBJECT list_key;
  PRIMITIVE_HEADER (2);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), key);
  TOUCH_IN_PRIMITIVE ((ARG_REF (2)), list);
  while (PAIR_P (list))
    {
      TOUCH_IN_PRIMITIVE ((PAIR_CAR (list)), list_key);
      if (list_key == key)
	PRIMITIVE_RETURN (list);
      TOUCH_IN_PRIMITIVE ((PAIR_CDR (list)), list);
    }
  if (list != EMPTY_LIST)
    error_wrong_type_arg (2);
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("ASSQ", Prim_assq, 2, 2, 0)
{
  fast SCHEME_OBJECT key;
  fast SCHEME_OBJECT alist;
  fast SCHEME_OBJECT association;
  fast SCHEME_OBJECT association_key;
  PRIMITIVE_HEADER (2);

  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), key);
  TOUCH_IN_PRIMITIVE ((ARG_REF (2)), alist);
  while (PAIR_P (alist))
    {
      TOUCH_IN_PRIMITIVE ((PAIR_CAR (alist)), association);
      if (! (PAIR_P (association)))
	error_wrong_type_arg (2);
      TOUCH_IN_PRIMITIVE ((PAIR_CAR (association)), association_key);
      if (association_key == key)
	PRIMITIVE_RETURN (association);
      TOUCH_IN_PRIMITIVE ((PAIR_CDR (alist)), alist);
    }
  if (alist != EMPTY_LIST)
    error_wrong_type_arg (2);
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR?", Prim_sys_pair, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (GC_PAIR_P (object)));
}

SCHEME_OBJECT
DEFUN (system_pair_cons, (type, car, cdr),
       long type
       AND SCHEME_OBJECT car
       AND SCHEME_OBJECT cdr)
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
    long type = (arg_index_integer (1, (MAX_TYPE_CODE + 1)));
    if ((GC_Type_Code (type)) != GC_Pair)
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (system_pair_cons (type, (ARG_REF (2)), (ARG_REF (3))));
  }
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR-CAR", Prim_sys_pair_car, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, GC_PAIR_P);
  PRIMITIVE_RETURN (PAIR_CAR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR-CDR", Prim_sys_pair_cdr, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, GC_PAIR_P);
  PRIMITIVE_RETURN (PAIR_CDR (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR-SET-CAR!", Prim_sys_set_car, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, GC_PAIR_P);
  {
    fast SCHEME_OBJECT pair = (ARG_REF (1));
    fast SCHEME_OBJECT car = (ARG_REF (2));
    SIDE_EFFECT_IMPURIFY (pair, car);
    SET_PAIR_CAR (pair, car);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SYSTEM-PAIR-SET-CDR!", Prim_sys_set_cdr, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, GC_PAIR_P);
  {
    fast SCHEME_OBJECT pair = (ARG_REF (1));
    fast SCHEME_OBJECT cdr = (ARG_REF (2));
    SIDE_EFFECT_IMPURIFY (pair, cdr);
    SET_PAIR_CDR (pair, cdr);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}
