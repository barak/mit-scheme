/* -*-C-*-

$Id: vector.c,v 9.40 2002/11/20 19:46:16 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* This file contains procedures for handling vectors. */

#include "scheme.h"
#include "prims.h"

#define ARG_VECTOR(argument_number)					\
  ((VECTOR_P (ARG_REF (argument_number)))				\
   ? (ARG_REF (argument_number))					\
   : ((error_wrong_type_arg (argument_number)), ((SCHEME_OBJECT) 0)))

#define ARG_RECORD(argument_number)					\
  ((RECORD_P (ARG_REF (argument_number)))				\
   ? (ARG_REF (argument_number))					\
   : ((error_wrong_type_arg (argument_number)), ((SCHEME_OBJECT) 0)))

#define ARG_VECTOR_INDEX(argument_number, vector)			\
  (arg_index_integer (argument_number, (VECTOR_LENGTH (vector))))

#define ARG_GC_VECTOR(argument_number)					\
  ((GC_VECTOR_P (ARG_REF (argument_number)))				\
   ? (ARG_REF (argument_number))					\
   : ((error_wrong_type_arg (argument_number)), ((SCHEME_OBJECT) 0)))

SCHEME_OBJECT
DEFUN (allocate_non_marked_vector, (type_code, length, gc_check_p),
       int type_code AND fast long length AND Boolean gc_check_p)
{
  fast SCHEME_OBJECT result;

  if (gc_check_p)
    Primitive_GC_If_Needed (length + 1);
  result = (MAKE_POINTER_OBJECT (type_code, Free));
  (*Free++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, length));
  Free += length;
  return (result);
}

SCHEME_OBJECT
DEFUN (allocate_marked_vector, (type_code, length, gc_check_p),
       int type_code AND fast long length AND Boolean gc_check_p)
{
  if (gc_check_p)
    Primitive_GC_If_Needed (length + 1);
  {
    fast SCHEME_OBJECT result = (MAKE_POINTER_OBJECT (type_code, Free));
    (*Free++) = (MAKE_OBJECT (TC_MANIFEST_VECTOR, length));
    Free += length;
    return (result);
  }
}

SCHEME_OBJECT
DEFUN (make_vector, (length, contents, gc_check_p),
       fast long length AND fast SCHEME_OBJECT contents AND Boolean gc_check_p)
{
  if (gc_check_p)
    Primitive_GC_If_Needed (length + 1);
  {
    fast SCHEME_OBJECT result = (MAKE_POINTER_OBJECT (TC_VECTOR, Free));
    (*Free++) = (MAKE_OBJECT (TC_MANIFEST_VECTOR, length));
    while ((length--) > 0)
      (*Free++) = contents;
    return (result);
  }
}

DEFINE_PRIMITIVE ("VECTOR-CONS", Prim_vector_cons, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (make_vector ((arg_nonnegative_integer (1)), (ARG_REF (2)), true));
}

DEFINE_PRIMITIVE ("VECTOR", Prim_vector, 0, LEXPR, 0)
{
  PRIMITIVE_HEADER (LEXPR);
  {
    SCHEME_OBJECT result =
      (allocate_marked_vector (TC_VECTOR, (LEXPR_N_ARGUMENTS ()), true));
    fast SCHEME_OBJECT * argument_scan = (ARG_LOC (1));
    fast SCHEME_OBJECT * argument_limit =
      (ARG_LOC ((LEXPR_N_ARGUMENTS ()) + 1));
    fast SCHEME_OBJECT * result_scan = (VECTOR_LOC (result, 0));
    while (argument_scan != argument_limit)
      (*result_scan++) = (STACK_LOCATIVE_POP (argument_scan));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("%RECORD", Prim_record, 0, LEXPR, 0)
{
  PRIMITIVE_HEADER (LEXPR);
  {
    long nargs = (LEXPR_N_ARGUMENTS ());
    if (nargs < 1)
      signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    {
      SCHEME_OBJECT result = (allocate_marked_vector (TC_RECORD, nargs, true));
      fast SCHEME_OBJECT * argument_scan = (ARG_LOC (1));
      fast SCHEME_OBJECT * argument_limit = (ARG_LOC (nargs + 1));
      fast SCHEME_OBJECT * result_scan = (VECTOR_LOC (result, 0));
      while (argument_scan != argument_limit)
	(*result_scan++) = (STACK_LOCATIVE_POP (argument_scan));
      PRIMITIVE_RETURN (result);
    }
  }
}

DEFINE_PRIMITIVE ("VECTOR?", Prim_vector_p, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (VECTOR_P (object)));
}

DEFINE_PRIMITIVE ("%RECORD?", Prim_record_p, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (RECORD_P (object)));
}

DEFINE_PRIMITIVE ("SYSTEM-VECTOR?", Prim_sys_vector, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (GC_VECTOR_P (object)));
}

#define VECTOR_LENGTH_PRIMITIVE(arg_type)				\
{									\
  fast SCHEME_OBJECT vector;						\
  PRIMITIVE_HEADER (1);							\
  TOUCH_IN_PRIMITIVE ((arg_type (1)), vector);				\
  PRIMITIVE_RETURN (long_to_integer (VECTOR_LENGTH (vector)));		\
}

DEFINE_PRIMITIVE ("VECTOR-LENGTH", Prim_vector_length, 1, 1, 0)
     VECTOR_LENGTH_PRIMITIVE (ARG_VECTOR)

DEFINE_PRIMITIVE ("%RECORD-LENGTH", Prim_record_length, 1, 1, 0)
     VECTOR_LENGTH_PRIMITIVE (ARG_RECORD)

DEFINE_PRIMITIVE ("SYSTEM-VECTOR-SIZE", Prim_sys_vec_size, 1, 1, 0)
     VECTOR_LENGTH_PRIMITIVE (ARG_GC_VECTOR)

#define VECTOR_REF_PRIMITIVE(arg_type)					\
{									\
  fast SCHEME_OBJECT vector;						\
  PRIMITIVE_HEADER (2);							\
  TOUCH_IN_PRIMITIVE ((arg_type (1)), vector);				\
  PRIMITIVE_RETURN							\
    (VECTOR_REF (vector, (ARG_VECTOR_INDEX (2, vector))));		\
}

DEFINE_PRIMITIVE ("VECTOR-REF", Prim_vector_ref, 2, 2, 0)
     VECTOR_REF_PRIMITIVE (ARG_VECTOR)

DEFINE_PRIMITIVE ("%RECORD-REF", Prim_record_ref, 2, 2, 0)
     VECTOR_REF_PRIMITIVE (ARG_RECORD)

DEFINE_PRIMITIVE ("SYSTEM-VECTOR-REF", Prim_sys_vector_ref, 2, 2, 0)
     VECTOR_REF_PRIMITIVE (ARG_GC_VECTOR)

#define VECTOR_SET_PRIMITIVE(arg_type)					\
{									\
  fast SCHEME_OBJECT vector;						\
  PRIMITIVE_HEADER (3);							\
  TOUCH_IN_PRIMITIVE ((arg_type (1)), vector);				\
  {									\
    fast SCHEME_OBJECT new_value = (ARG_REF (3));			\
    SIDE_EFFECT_IMPURIFY (vector, new_value);				\
    VECTOR_SET (vector, (ARG_VECTOR_INDEX (2, vector)), new_value);	\
  }									\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

DEFINE_PRIMITIVE ("VECTOR-SET!", Prim_vector_set, 3, 3, 0)
     VECTOR_SET_PRIMITIVE (ARG_VECTOR)

DEFINE_PRIMITIVE ("%RECORD-SET!", Prim_record_set, 3, 3, 0)
     VECTOR_SET_PRIMITIVE (ARG_RECORD)

DEFINE_PRIMITIVE ("SYSTEM-VECTOR-SET!", Prim_sys_vec_set, 3, 3, 0)
     VECTOR_SET_PRIMITIVE (ARG_GC_VECTOR)

#define SUBVECTOR_TO_LIST_PRIMITIVE(arg_type)				\
{									\
  fast SCHEME_OBJECT vector;						\
  fast long start;							\
  fast long end;							\
  PRIMITIVE_HEADER (3);							\
  TOUCH_IN_PRIMITIVE ((arg_type (1)), vector);				\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  if (end > ((long) (VECTOR_LENGTH (vector))))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
  PRIMITIVE_RETURN (subvector_to_list (vector, start, end));		\
}

static SCHEME_OBJECT
DEFUN (subvector_to_list, (vector, start, end),
       SCHEME_OBJECT vector AND long start AND long end)
{
  SCHEME_OBJECT result;
  fast SCHEME_OBJECT *scan;
  fast SCHEME_OBJECT *end_scan;
  fast SCHEME_OBJECT *pair_scan;
  if (start == end)
    return (EMPTY_LIST);
  Primitive_GC_If_Needed (2 * (end - start));
  result = (MAKE_POINTER_OBJECT (TC_LIST, Free));
  scan = (VECTOR_LOC (vector, start));
  end_scan = (VECTOR_LOC (vector, (end - 1)));
  pair_scan = Free;
  while (scan < end_scan)
    {
      Free += 2;
      (*pair_scan++) = (MEMORY_FETCH (*scan++));
      (*pair_scan++) = (MAKE_POINTER_OBJECT (TC_LIST, Free));
    }
  Free += 2;
  (*pair_scan++) = (MEMORY_FETCH (*scan));
  (*pair_scan) = EMPTY_LIST;
  return (result);
}

DEFINE_PRIMITIVE ("SUBVECTOR->LIST", Prim_subvector_to_list, 3, 3, 0)
     SUBVECTOR_TO_LIST_PRIMITIVE (ARG_VECTOR)

DEFINE_PRIMITIVE ("SYSTEM-SUBVECTOR-TO-LIST", Prim_sys_subvector_to_list, 3, 3, 0)
     SUBVECTOR_TO_LIST_PRIMITIVE (ARG_GC_VECTOR)

static SCHEME_OBJECT
DEFUN (list_to_vector, (result_type, argument_number),
       long result_type AND long argument_number)
{
  fast SCHEME_OBJECT list;
  fast long count;
  SCHEME_OBJECT *result;

  list = (ARG_REF (argument_number));
  TOUCH_IN_PRIMITIVE (list, list);
  count = 0;
  result = (Free++);
  while (PAIR_P (list))
    {
      Primitive_GC_If_Needed (0);
      count += 1;
      (*Free++) = (PAIR_CAR (list));
      TOUCH_IN_PRIMITIVE ((PAIR_CDR (list)), list);
    }
  if (list != EMPTY_LIST)
    error_wrong_type_arg (argument_number);
  (*result) = (MAKE_OBJECT (TC_MANIFEST_VECTOR, count));
  return (MAKE_POINTER_OBJECT (result_type, result));
}

DEFINE_PRIMITIVE ("LIST->VECTOR", Prim_list_to_vector, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (list_to_vector (TC_VECTOR, 1));
}

DEFINE_PRIMITIVE ("SYSTEM-LIST-TO-VECTOR", Prim_sys_list_to_vector, 2, 2, 0)
{
  long type_code;
  PRIMITIVE_HEADER (2);

  type_code = (arg_index_integer (1, (MAX_TYPE_CODE + 1)));
  if ((GC_Type_Code (type_code)) != GC_Vector)
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (list_to_vector (type_code, 2));
}

/* Primitive vector copy and fill */

#define SUBVECTOR_MOVE_PREFIX()						\
  SCHEME_OBJECT vector1, vector2;					\
  long start1, end1, start2, end2;					\
  fast long length;							\
  fast SCHEME_OBJECT *scan1, *scan2;					\
  PRIMITIVE_HEADER (5);							\
  TOUCH_IN_PRIMITIVE ((ARG_VECTOR (1)), vector1);			\
  start1 = (arg_nonnegative_integer (2));				\
  end1 = (arg_nonnegative_integer (3));					\
  TOUCH_IN_PRIMITIVE ((ARG_VECTOR (4)), vector2);			\
  start2 = (arg_nonnegative_integer (5));				\
  if (end1 > ((long) (VECTOR_LENGTH (vector1))))			\
    error_bad_range_arg (3);						\
  if (start1 > end1)							\
    error_bad_range_arg (2);						\
  length = (end1 - start1);						\
  end2 = (start2 + length);						\
  if (end2 > ((long) (VECTOR_LENGTH (vector2))))			\
    error_bad_range_arg (5);						\
  if (ADDRESS_PURE_P (OBJECT_ADDRESS (vector2)))			\
    signal_error_from_primitive (ERR_WRITE_INTO_PURE_SPACE)

DEFINE_PRIMITIVE ("SUBVECTOR-MOVE-RIGHT!", Prim_subvector_move_right, 5, 5, 0)
{
  SUBVECTOR_MOVE_PREFIX ();
  scan1 = (VECTOR_LOC (vector1, end1));
  scan2 = (VECTOR_LOC (vector2, end2));
  while ((length--) > 0)
    (*--scan2) = (*--scan1);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SUBVECTOR-MOVE-LEFT!", Prim_subvector_move_left, 5, 5, 0)
{
  SUBVECTOR_MOVE_PREFIX ();
  scan1 = (VECTOR_LOC (vector1, start1));
  scan2 = (VECTOR_LOC (vector2, start2));
  while ((length--) > 0)
    (*scan2++) = (*scan1++);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SUBVECTOR-FILL!", Prim_vector_fill, 4, 4, 0)
{
  SCHEME_OBJECT vector;
  long start, end;
  fast SCHEME_OBJECT fill_value;
  fast SCHEME_OBJECT *scan;
  fast long length;
  PRIMITIVE_HEADER (4);
  TOUCH_IN_PRIMITIVE ((ARG_VECTOR (1)), vector);
  start = (arg_nonnegative_integer (2));
  end = (arg_nonnegative_integer (3));
  fill_value = (ARG_REF (4));
  if (end > ((long) (VECTOR_LENGTH (vector))))
    error_bad_range_arg (3);
  if (start > end)
    error_bad_range_arg (2);
  length = (end - start);
  SIDE_EFFECT_IMPURIFY (vector, fill_value);
  scan = (VECTOR_LOC (vector, start));
  while ((length--) > 0)
    (*scan++) = fill_value;
  PRIMITIVE_RETURN (UNSPECIFIC);
}
