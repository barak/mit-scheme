/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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
  ((GC_TYPE_VECTOR (ARG_REF (argument_number)))				\
   ? (ARG_REF (argument_number))					\
   : ((error_wrong_type_arg (argument_number)), ((SCHEME_OBJECT) 0)))

SCHEME_OBJECT
allocate_vector (unsigned int type,
		 unsigned int manifest_type,
		 unsigned long length,
		 SCHEME_OBJECT ** fp)
{
  SCHEME_OBJECT result = (MAKE_POINTER_OBJECT (type, (*fp)));
  (*(*fp)++) = (MAKE_OBJECT (manifest_type, length));
  (*fp) += length;
  return (result);
}

SCHEME_OBJECT
allocate_non_marked_vector (unsigned int type,
			    unsigned long length,
			    bool gc_check_p)
{
  if (gc_check_p)
    Primitive_GC_If_Needed (1 + length);
  return (allocate_vector (type, TC_MANIFEST_NM_VECTOR, length, (&Free)));
}

SCHEME_OBJECT
allocate_marked_vector (unsigned int type,
			unsigned long length,
			bool gc_check_p)
{
  if (gc_check_p)
    Primitive_GC_If_Needed (1 + length);
  return (allocate_vector (type, TC_MANIFEST_VECTOR, length, (&Free)));
}

SCHEME_OBJECT
make_vector (unsigned long length, SCHEME_OBJECT contents, bool gc_check_p)
{
  if (gc_check_p)
    Primitive_GC_If_Needed (length + 1);
  {
    SCHEME_OBJECT result = (MAKE_POINTER_OBJECT (TC_VECTOR, Free));
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
      (allocate_marked_vector (TC_VECTOR, GET_LEXPR_ACTUALS, true));
    SCHEME_OBJECT * argument_scan = (ARG_LOC (1));
    SCHEME_OBJECT * argument_limit = (ARG_LOC (GET_LEXPR_ACTUALS + 1));
    SCHEME_OBJECT * result_scan = (VECTOR_LOC (result, 0));
    while (argument_scan != argument_limit)
      (*result_scan++) = (STACK_LOCATIVE_POP (argument_scan));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("%RECORD", Prim_record, 0, LEXPR, 0)
{
  PRIMITIVE_HEADER (LEXPR);
  {
    unsigned long nargs = GET_LEXPR_ACTUALS;
    if (nargs < 1)
      signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    {
      SCHEME_OBJECT result = (allocate_marked_vector (TC_RECORD, nargs, true));
      SCHEME_OBJECT * argument_scan = (ARG_LOC (1));
      SCHEME_OBJECT * argument_limit = (ARG_LOC (nargs + 1));
      SCHEME_OBJECT * result_scan = (VECTOR_LOC (result, 0));
      while (argument_scan != argument_limit)
	(*result_scan++) = (STACK_LOCATIVE_POP (argument_scan));
      PRIMITIVE_RETURN (result);
    }
  }
}

DEFINE_PRIMITIVE ("VECTOR?", Prim_vector_p, 1, 1, 0)
{
  SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  object = (ARG_REF (1));
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (VECTOR_P (object)));
}

DEFINE_PRIMITIVE ("%RECORD?", Prim_record_p, 1, 1, 0)
{
  SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  object = (ARG_REF (1));
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (RECORD_P (object)));
}

DEFINE_PRIMITIVE ("SYSTEM-VECTOR?", Prim_sys_vector, 1, 1, 0)
{
  SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  object = (ARG_REF (1));
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (GC_TYPE_VECTOR (object)));
}

#define VECTOR_LENGTH_PRIMITIVE(arg_type)				\
{									\
  PRIMITIVE_HEADER (1);							\
  PRIMITIVE_RETURN (long_to_integer (VECTOR_LENGTH (arg_type (1))));	\
}

DEFINE_PRIMITIVE ("VECTOR-LENGTH", Prim_vector_length, 1, 1, 0)
     VECTOR_LENGTH_PRIMITIVE (ARG_VECTOR)

DEFINE_PRIMITIVE ("%RECORD-LENGTH", Prim_record_length, 1, 1, 0)
     VECTOR_LENGTH_PRIMITIVE (ARG_RECORD)

DEFINE_PRIMITIVE ("SYSTEM-VECTOR-SIZE", Prim_sys_vec_size, 1, 1, 0)
     VECTOR_LENGTH_PRIMITIVE (ARG_GC_VECTOR)

#define VECTOR_REF_PRIMITIVE(arg_type)					\
{									\
  SCHEME_OBJECT vector;							\
  PRIMITIVE_HEADER (2);							\
  vector = (arg_type (1));						\
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
  SCHEME_OBJECT vector;							\
  PRIMITIVE_HEADER (3);							\
  vector = (arg_type (1));						\
  {									\
    SCHEME_OBJECT new_value = (ARG_REF (3));				\
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
  SCHEME_OBJECT vector;							\
  long start;								\
  long end;								\
  PRIMITIVE_HEADER (3);							\
  vector = (arg_type (1));						\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  if (end > ((long) (VECTOR_LENGTH (vector))))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
  PRIMITIVE_RETURN (subvector_to_list (vector, start, end));		\
}

static SCHEME_OBJECT
subvector_to_list (SCHEME_OBJECT vector, long start, long end)
{
  SCHEME_OBJECT result;
  SCHEME_OBJECT *scan;
  SCHEME_OBJECT *end_scan;
  SCHEME_OBJECT *pair_scan;
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
      (*pair_scan++) = (*scan++);
      (*pair_scan++) = (MAKE_POINTER_OBJECT (TC_LIST, Free));
    }
  Free += 2;
  (*pair_scan++) = (*scan);
  (*pair_scan) = EMPTY_LIST;
  return (result);
}

DEFINE_PRIMITIVE ("SUBVECTOR->LIST", Prim_subvector_to_list, 3, 3, 0)
     SUBVECTOR_TO_LIST_PRIMITIVE (ARG_VECTOR)

DEFINE_PRIMITIVE ("SYSTEM-SUBVECTOR-TO-LIST", Prim_sys_subvector_to_list, 3, 3, 0)
     SUBVECTOR_TO_LIST_PRIMITIVE (ARG_GC_VECTOR)

static SCHEME_OBJECT
list_to_vector (unsigned long result_type, long argument_number)
{
  SCHEME_OBJECT list;
  unsigned long count;
  SCHEME_OBJECT *result;

  list = (ARG_REF (argument_number));
  count = 0;
  result = (Free++);
  while (PAIR_P (list))
    {
      Primitive_GC_If_Needed (0);
      count += 1;
      (*Free++) = (PAIR_CAR (list));
      list = (PAIR_CDR (list));
    }
  if (!EMPTY_LIST_P (list))
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
  unsigned long type_code;
  PRIMITIVE_HEADER (2);

  type_code = (arg_ulong_index_integer (1, N_TYPE_CODES));
  if ((GC_TYPE_CODE (type_code)) != GC_VECTOR)
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (list_to_vector (type_code, 2));
}

/* Primitive vector copy and fill */

#define SUBVECTOR_MOVE_PREFIX()						\
  SCHEME_OBJECT vector1, vector2;					\
  long start1, end1, start2, end2;					\
  long length;								\
  SCHEME_OBJECT *scan1, *scan2;						\
  PRIMITIVE_HEADER (5);							\
  vector1 = (ARG_VECTOR (1));						\
  start1 = (arg_nonnegative_integer (2));				\
  end1 = (arg_nonnegative_integer (3));					\
  vector2 = (ARG_VECTOR (4));						\
  start2 = (arg_nonnegative_integer (5));				\
  if (end1 > ((long) (VECTOR_LENGTH (vector1))))			\
    error_bad_range_arg (3);						\
  if (start1 > end1)							\
    error_bad_range_arg (2);						\
  length = (end1 - start1);						\
  end2 = (start2 + length);						\
  if (end2 > ((long) (VECTOR_LENGTH (vector2))))			\
    error_bad_range_arg (5);

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
  SCHEME_OBJECT fill_value;
  SCHEME_OBJECT *scan;
  long length;
  PRIMITIVE_HEADER (4);
  vector = (ARG_VECTOR (1));
  start = (arg_nonnegative_integer (2));
  end = (arg_nonnegative_integer (3));
  fill_value = (ARG_REF (4));
  if (end > ((long) (VECTOR_LENGTH (vector))))
    error_bad_range_arg (3);
  if (start > end)
    error_bad_range_arg (2);
  length = (end - start);
  scan = (VECTOR_LOC (vector, start));
  while ((length--) > 0)
    (*scan++) = fill_value;
  PRIMITIVE_RETURN (UNSPECIFIC);
}
