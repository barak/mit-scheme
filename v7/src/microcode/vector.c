/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/vector.c,v 9.28 1987/12/16 19:38:05 cph Exp $ */

/* This file contains procedures for handling vectors and conversion
   back and forth to lists. */

#include "scheme.h"
#include "primitive.h"

#define ARG_VECTOR(argument_number)					\
((VECTOR_P (ARG_REF (argument_number)))					\
 ? (ARG_REF (argument_number))						\
 : ((Pointer) (error_wrong_type_arg (argument_number))))

/* Flush old definition -- we won't use it. */
#ifdef VECTOR_LENGTH
#undef VECTOR_LENGTH
#endif

#define VECTOR_LENGTH(vector)						\
(UNSIGNED_FIXNUM_VALUE (Fast_Vector_Ref ((vector), 0)))

#define ARG_VECTOR_INDEX(argument_number, vector)			\
(arg_index_integer (argument_number, (Vector_Length (vector))))

#define GC_VECTOR_P(object) ((GC_Type (object)) == GC_Vector)

#define ARG_GC_VECTOR(argument_number)					\
((GC_VECTOR_P (ARG_REF (argument_number)))				\
 ? (ARG_REF (argument_number))						\
 : ((Pointer) (error_wrong_type_arg (argument_number))))

/* VECTOR_TOUCH does nothing, this is copied from a previous version
   of this code.  Perhaps it should do a touch? -- CPH */
#define VECTOR_TOUCH(vector)
#define GC_VECTOR_TOUCH(vector) Touch_In_Primitive (vector, vector)

#define VECTOR_REF(vector, index) (Vector_Ref ((vector), ((index) + 1)))
#define VECTOR_LOC(vector, index) (Nth_Vector_Loc ((vector), ((index) + 1)))

Pointer
allocate_non_marked_vector (type_code, length, gc_check_p)
     int type_code;
     fast long length;
     Boolean gc_check_p;
{
  fast Pointer result;

  if (gc_check_p)
    Primitive_GC_If_Needed (length + 1);
  result = (Make_Pointer (type_code, Free));
  (*Free++) = (Make_Non_Pointer (TC_MANIFEST_NM_VECTOR, length));
  Free += length;
  return (result);
}

Pointer
allocate_marked_vector (type_code, length, gc_check_p)
     int type_code;
     fast long length;
     Boolean gc_check_p;
{
  fast Pointer result;

  if (gc_check_p)
    Primitive_GC_If_Needed (length + 1);
  result = (Make_Pointer (type_code, Free));
  (*Free++) = (Make_Non_Pointer (TC_MANIFEST_VECTOR, length));
  Free += length;
  return (result);
}

Pointer
make_vector (length, contents)
     fast long length;
     fast Pointer contents;
{
  fast Pointer result;

  Primitive_GC_If_Needed (length + 1);
  result = (Make_Pointer (TC_VECTOR, Free));
  (*Free++) = (Make_Non_Pointer (TC_MANIFEST_VECTOR, length));
  while ((length--) > 0)
    (*Free++) = contents;
  return (result);
}

DEFINE_PRIMITIVE ("VECTOR-CONS", Prim_Vector_Cons, 2)
{
  PRIMITIVE_HEADER (2);

  PRIMITIVE_RETURN
    (make_vector ((arg_nonnegative_integer (1)), (ARG_REF (2))));
}

DEFINE_PRIMITIVE ("SYSTEM-VECTOR?", Prim_Sys_Vector, 1)
{
  fast Pointer object;
  PRIMITIVE_HEADER (1);

  object = (ARG_REF (1));
  Touch_In_Primitive (object, object);
  PRIMITIVE_RETURN ((GC_VECTOR_P (object)) ? TRUTH : NIL);
}

#define VECTOR_LENGTH_PRIMITIVE(arg_type, arg_touch)			\
  fast Pointer vector;							\
  PRIMITIVE_HEADER (1);							\
									\
  vector = (arg_type (1));						\
  arg_touch (vector);							\
  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (VECTOR_LENGTH (vector)))

DEFINE_PRIMITIVE ("VECTOR-LENGTH", Prim_Vector_Size, 1)
{ VECTOR_LENGTH_PRIMITIVE (ARG_VECTOR, VECTOR_TOUCH); }

DEFINE_PRIMITIVE ("SYSTEM-VECTOR-SIZE", Prim_Sys_Vec_Size, 1)
{ VECTOR_LENGTH_PRIMITIVE (ARG_GC_VECTOR, GC_VECTOR_TOUCH); }

#define VECTOR_REF_PRIMITIVE(arg_type, arg_touch)			\
  fast Pointer vector;							\
  PRIMITIVE_HEADER (2);							\
									\
  vector = (arg_type (1));						\
  arg_touch (vector);							\
  PRIMITIVE_RETURN (VECTOR_REF (vector, (ARG_VECTOR_INDEX (2, vector))))

DEFINE_PRIMITIVE ("VECTOR-REF", Prim_Vector_Ref, 2)
{ VECTOR_REF_PRIMITIVE (ARG_VECTOR, VECTOR_TOUCH); }

DEFINE_PRIMITIVE ("SYSTEM-VECTOR-REF", Prim_Sys_Vector_Ref, 2)
{ VECTOR_REF_PRIMITIVE (ARG_GC_VECTOR, GC_VECTOR_TOUCH); }

#define VECTOR_SET_PRIMITIVE(arg_type, arg_touch)			\
  fast Pointer vector;							\
  fast Pointer new_value;						\
  fast Pointer *locative;						\
  PRIMITIVE_HEADER (3);							\
									\
  vector = (arg_type (1));						\
  arg_touch (vector);							\
  new_value = (ARG_REF (3));						\
  locative = (VECTOR_LOC (vector, (ARG_VECTOR_INDEX (2, vector))));	\
  Side_Effect_Impurify (vector, new_value);				\
  PRIMITIVE_RETURN (Swap_Pointers (locative, new_value))

DEFINE_PRIMITIVE ("VECTOR-SET!", Prim_Vector_Set, 3)
{ VECTOR_SET_PRIMITIVE (ARG_VECTOR, VECTOR_TOUCH); }

DEFINE_PRIMITIVE ("SYSTEM-VECTOR-SET!", Prim_Sys_Vec_Set, 3)
{ VECTOR_SET_PRIMITIVE (ARG_GC_VECTOR, GC_VECTOR_TOUCH); }

#define SUBVECTOR_TO_LIST_PRIMITIVE(arg_type, arg_touch)		\
  fast Pointer vector;							\
  fast long start;							\
  fast long end;							\
  PRIMITIVE_HEADER (3);							\
									\
  vector = (arg_type (1));						\
  arg_touch (vector);							\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  if (end > (VECTOR_LENGTH (vector)))					\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
  PRIMITIVE_RETURN (subvector_to_list (vector, start, end))

static Pointer
subvector_to_list (vector, start, end)
     Pointer vector;
     long start;
     long end;
{
  Pointer result;
  fast Pointer *scan;
  fast Pointer *end_scan;
  fast Pointer *pair_scan;

  if (start == end)
    return (NIL);
  Primitive_GC_If_Needed (2 * (end - start));
  result = (Make_Pointer (TC_LIST, Free));
  scan = (VECTOR_LOC (vector, start));
  end_scan = (VECTOR_LOC (vector, (end - 1)));
  pair_scan = Free;
  while (scan < end_scan)
    {
      Free += 2;
      (*pair_scan++) = (Fetch (*scan++));
      (*pair_scan++) = (Make_Pointer (TC_LIST, Free));
    }
  Free += 2;
  (*pair_scan++) = (Fetch (*scan));
  (*pair_scan) = NIL;
  return (result);
}

DEFINE_PRIMITIVE ("SUBVECTOR->LIST", Prim_Subvector_To_List, 3)
{ SUBVECTOR_TO_LIST_PRIMITIVE (ARG_VECTOR, VECTOR_TOUCH); }

DEFINE_PRIMITIVE ("SYSTEM-SUBVECTOR-TO-LIST", Prim_Sys_Subvector_To_List, 3)
{ SUBVECTOR_TO_LIST_PRIMITIVE (ARG_GC_VECTOR, GC_VECTOR_TOUCH); }

static Pointer
list_to_vector (result_type, argument_number)
     long argument_number;
     long result_type;
{
  fast Pointer list;
  fast long count;
  Pointer *result;

  list = (ARG_REF (argument_number));
  Touch_In_Primitive (list, list);
  count = 0;
  result = (Free++);
  while (PAIR_P (list))
    {
      Primitive_GC_If_Needed (0);
      count += 1;
      (*Free++) = (Vector_Ref (list, CONS_CAR));
      Touch_In_Primitive ((Vector_Ref (list, CONS_CDR)), list);
    }
  if (list != NIL)
    error_wrong_type_arg (argument_number);
  (*result) = (Make_Non_Pointer (TC_MANIFEST_VECTOR, count));
  return (Make_Pointer (result_type, result));
}

DEFINE_PRIMITIVE ("LIST->VECTOR", Prim_List_To_Vector, 1)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (list_to_vector (TC_VECTOR, 1));
}

DEFINE_PRIMITIVE ("SYSTEM-LIST-TO-VECTOR", Prim_Sys_List_To_Vector, 2)
{
  long type_code;
  PRIMITIVE_HEADER (2);

  type_code = (arg_index_integer (1, (MAX_TYPE_CODE + 1)));
  if ((GC_Type_Code (type_code)) != GC_Vector)
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (list_to_vector (type_code, 2));
}

/* Primitive vector copy and fill */

#define subvector_move_prefix()						\
  Pointer vector1, vector2;						\
  long start1, end1, start2, end2;					\
  fast long length;							\
  fast Pointer *scan1, *scan2;						\
  PRIMITIVE_HEADER (5);							\
									\
  vector1 = (ARG_VECTOR (1));						\
  VECTOR_TOUCH (vector1);						\
  start1 = (arg_nonnegative_integer (2));				\
  end1 = (arg_nonnegative_integer (3));					\
  vector2 = (ARG_VECTOR (2));						\
  VECTOR_TOUCH (vector2);						\
  start2 = (arg_nonnegative_integer (5));				\
									\
  if (end1 > (VECTOR_LENGTH (vector1)))					\
    error_bad_range_arg (3);						\
  if (start1 > end1)							\
    error_bad_range_arg (2);						\
  length = (end1 - start1);						\
									\
  end2 = (start2 + length);						\
  if (end2 > (VECTOR_LENGTH (vector2)))					\
    error_bad_range_arg (5);						\
									\
  if (Is_Pure (Get_Pointer (vector2)))					\
    Primitive_Error (ERR_WRITE_INTO_PURE_SPACE)

DEFINE_PRIMITIVE ("SUBVECTOR-MOVE-RIGHT!", Prim_subvector_move_right, 5)
{
  subvector_move_prefix ();

  scan1 = (VECTOR_LOC (vector1, end1));
  scan2 = (VECTOR_LOC (vector2, end2));
  while ((length--) > 0)
    (*--scan2) = (*--scan1);
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("SUBVECTOR-MOVE-LEFT!", Prim_subvector_move_left, 5)
{
  subvector_move_prefix ();

  scan1 = (VECTOR_LOC (vector1, start1));
  scan2 = (VECTOR_LOC (vector2, start2));
  while ((length--) > 0)
    (*scan2++) = (*scan1++);
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("SUBVECTOR-FILL!", Prim_vector_fill, 4)
{
  Pointer vector;
  long start, end;
  fast Pointer fill_value;
  fast Pointer *scan;
  fast long length;
  PRIMITIVE_HEADER (4);

  vector = (ARG_VECTOR (1));
  VECTOR_TOUCH (1);
  start = (arg_nonnegative_integer (2));
  end = (arg_nonnegative_integer (3));
  fill_value = (ARG_REF (4));

  if (end > (VECTOR_LENGTH (vector)))
    error_bad_range_arg (3);
  if (start > end)
    error_bad_range_arg (2);
  length = (end - start);

  Side_Effect_Impurify (vector, fill_value);
  scan = (VECTOR_LOC (vector, start));
  while ((length--) > 0)
    (*scan++) = fill_value;
  PRIMITIVE_RETURN (NIL);
}
