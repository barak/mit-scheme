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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/string.c,v 9.30 1988/03/31 21:23:18 jrm Rel $ */

/* String primitives. */

#include "scheme.h"
#include "primitive.h"
#include "char.h"
#include "string.h"

Pointer
allocate_string (nbytes)
     fast long nbytes;
{
  fast long count;
  fast Pointer result;

  /* Add 1 to nbytes to account for '\0' at end of string.
     Add 1 to count to account for string header words. */

  count = ((BYTES_TO_POINTERS (nbytes + 1)) + 1);
  result = (allocate_non_marked_vector (TC_CHARACTER_STRING, count, true));
  set_string_length (result, nbytes);
  return (result);
}

Pointer
memory_to_string (nbytes, data)
     fast long nbytes;
     fast char *data;
{
  Pointer result;
  fast char *scan_result;

  result = (allocate_string (nbytes));
  scan_result = (string_pointer (result, 0));
  while ((nbytes--) > 0)
    (*scan_result++) = (*data++);
  return (result);
}

/* Currently the strings used in symbols have type codes in the length
   field.  They should be changed to have just longwords there. */

DEFINE_PRIMITIVE ("STRING-ALLOCATE", Prim_String_Allocate, 1)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (allocate_string (arg_nonnegative_integer (1)));
}

DEFINE_PRIMITIVE ("STRING?", Prim_String_P, 1)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN ((STRING_P (ARG_REF (1))) ? TRUTH : NIL);
}

DEFINE_PRIMITIVE ("STRING-LENGTH", Prim_String_Length, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (string_length (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("STRING-MAXIMUM-LENGTH", Prim_String_Maximum_Length, 1)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (Make_Unsigned_Fixnum ((maximum_string_length (ARG_REF (1))) - 1));
}

DEFINE_PRIMITIVE ("SET-STRING-LENGTH!", Prim_Set_String_Length, 2)
{
  fast Pointer string;
  fast long length;
  fast long result;
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, STRING_P);
  string = (ARG_REF (1));
  length = (arg_nonnegative_integer (2));
  if (length > (maximum_string_length (string)))
    error_bad_range_arg (2);

  result = (string_length (string));
  set_string_length (string, length);
  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (result));
}

long
substring_length_min (start1, end1, start2, end2)
     long start1, end1, start2, end2;
{
  fast long length1, length2;

  length1 = (end1 - start1);
  length2 = (end2 - start2);
  return ((length1 < length2) ? length1 : length2);
}

#define STRING_REF_BODY(process_result)					\
  fast Pointer string;							\
  fast long index;							\
  PRIMITIVE_HEADER (2);							\
									\
  CHECK_ARG (1, STRING_P);						\
  string = (ARG_REF (1));						\
  index = (arg_index_integer (2, (string_length (string))));		\
									\
  PRIMITIVE_RETURN (process_result (string_ref (string, index)))

DEFINE_PRIMITIVE ("STRING-REF", Prim_String_Ref, 2)
{ STRING_REF_BODY (c_char_to_scheme_char); }

DEFINE_PRIMITIVE ("VECTOR-8B-REF", Prim_Vec_8b_Ref, 2)
{ STRING_REF_BODY (Make_Unsigned_Fixnum); }

#define STRING_SET_BODY(get_ascii, process_result)			\
  fast Pointer string;							\
  fast long index;							\
  long ascii;								\
  char *char_pointer;							\
  Pointer result;							\
  PRIMITIVE_HEADER (3);							\
									\
  CHECK_ARG (1, STRING_P);						\
  string = (ARG_REF (1));						\
  index = (arg_index_integer (2, (string_length (string))));		\
  ascii = (get_ascii (3));						\
									\
  char_pointer = (string_pointer (string, index));			\
  result = (char_to_long (*char_pointer));				\
  (*char_pointer) = ascii;						\
  PRIMITIVE_RETURN (process_result (result))

DEFINE_PRIMITIVE ("STRING-SET!", Prim_String_Set, 3)
{ STRING_SET_BODY (arg_ascii_char, c_char_to_scheme_char); }

DEFINE_PRIMITIVE ("VECTOR-8B-SET!", Prim_Vec_8b_Set, 3)
{ STRING_SET_BODY (arg_ascii_integer, MAKE_UNSIGNED_FIXNUM); }

#define SUBSTRING_MOVE_PREFIX()						\
  long start1, end1, start2, end2, length;				\
  fast char *scan1, *scan2;						\
  PRIMITIVE_HEADER (5);							\
									\
  CHECK_ARG (1, STRING_P);						\
  start1 = (arg_nonnegative_integer (2));				\
  end1 = (arg_nonnegative_integer (3));					\
  CHECK_ARG (4, STRING_P);						\
  start2 = (arg_nonnegative_integer (5));				\
									\
  if (end1 > (string_length (ARG_REF (1))))				\
    error_bad_range_arg (2);						\
  if (start1 > end1)							\
    error_bad_range_arg (1);						\
  length = (end1 - start1);						\
									\
  end2 = (start2 + length);						\
  if (end2 > (string_length (ARG_REF (4))))				\
    error_bad_range_arg (3)

DEFINE_PRIMITIVE ("SUBSTRING-MOVE-RIGHT!", Prim_Substring_Move_Right, 5)
{
  SUBSTRING_MOVE_PREFIX ();

  scan1 = (string_pointer ((ARG_REF (1)), end1));
  scan2 = (string_pointer ((ARG_REF (4)), end2));
  while ((length--) > 0)
    (*--scan2) = (*--scan1);
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("SUBSTRING-MOVE-LEFT!", Prim_Substring_Move_Left, 5)
{
  SUBSTRING_MOVE_PREFIX ();

  scan1 = (string_pointer ((ARG_REF (1)), start1));
  scan2 = (string_pointer ((ARG_REF (4)), start2));
  while ((length--) > 0)
    (*scan2++) = (*scan1++);
  PRIMITIVE_RETURN (NIL);
}

#define VECTOR_8B_SUBSTRING_PREFIX()					\
  long start, end, ascii;						\
  fast long length;							\
  fast char *scan;							\
  PRIMITIVE_HEADER (4);							\
									\
  CHECK_ARG (1, STRING_P);						\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  ascii = (arg_ascii_integer (4));					\
									\
  if (end > (string_length (ARG_REF (1))))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2)

DEFINE_PRIMITIVE ("VECTOR-8B-FILL!", Prim_Vec_8b_Fill, 4)
{
  VECTOR_8B_SUBSTRING_PREFIX ();

  length = (end - start);
  scan = (string_pointer ((ARG_REF (1)), start));
  while ((length--) > 0)
    (*scan++) = ascii;
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-NEXT-CHAR", Prim_Vec_8b_Find_Next_Char, 4)
{
  VECTOR_8B_SUBSTRING_PREFIX ();

  scan = (string_pointer ((ARG_REF (1)), start));
  while (start < end)
    {
      if ((char_to_long (*scan++)) == ascii)
	PRIMITIVE_RETURN (Make_Unsigned_Fixnum (start));
      start += 1;
    }
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-PREVIOUS-CHAR", Prim_Vec_8b_Find_Prev_Char, 4)
{
  VECTOR_8B_SUBSTRING_PREFIX ();

  scan = (string_pointer ((ARG_REF (1)), end));
  while ((end--) > start)
    if ((char_to_long (*--scan)) == ascii)
      PRIMITIVE_RETURN (Make_Unsigned_Fixnum (end));
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-NEXT-CHAR-CI", Prim_Vec_8b_Find_Next_Char_Ci, 4)
{
  char char1;
  VECTOR_8B_SUBSTRING_PREFIX ();

  scan = (string_pointer ((ARG_REF (1)), start));
  char1 = (char_upcase (ascii));
  while (start < end)
    {
      if ((char_upcase (*scan++)) == char1)
	PRIMITIVE_RETURN (Make_Unsigned_Fixnum( start));
      start += 1;
    }
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-PREVIOUS-CHAR-CI", Prim_Vec_8b_Find_Prev_Char_Ci, 4)
{
  char char1;
  VECTOR_8B_SUBSTRING_PREFIX ();

  scan = (string_pointer ((ARG_REF (1)), end));
  char1 = (char_upcase (ascii));
  while ((end--) > start)
    {
      if ((char_upcase (*--scan)) == char1)
	PRIMITIVE_RETURN (Make_Unsigned_Fixnum (end));
    }
  PRIMITIVE_RETURN (NIL);
}

#define SUBSTR_FIND_CHAR_IN_SET_PREFIX()				\
  long start, end, length;						\
  char *char_set, *scan;						\
  PRIMITIVE_HEADER (4);							\
									\
  CHECK_ARG (1, STRING_P);						\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  CHECK_ARG (4, STRING_P);						\
									\
  if (end > (string_length (ARG_REF (1))))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
  if ((string_length (ARG_REF (4))) != MAX_ASCII)			\
    error_bad_range_arg (4)

DEFINE_PRIMITIVE ("SUBSTRING-FIND-NEXT-CHAR-IN-SET", Prim_Find_Next_Char_In_Set, 4)
{
  SUBSTR_FIND_CHAR_IN_SET_PREFIX ();

  char_set = (Scheme_String_To_C_String (ARG_REF (4)));
  scan = (string_pointer ((ARG_REF (1)), start));
  while (start < end)
    {
      if (char_set[(char_to_long (*scan++))] != '\0')
	PRIMITIVE_RETURN (Make_Unsigned_Fixnum (start));
      start += 1;
    }
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET", Prim_Find_Prev_Char_In_Set, 4)
{
  SUBSTR_FIND_CHAR_IN_SET_PREFIX ();

  char_set = Scheme_String_To_C_String(ARG_REF (4));
  scan = (string_pointer ((ARG_REF (1)), end));
  while (end-- > start)
    if (char_set[(char_to_long (*--scan))] != '\0')
      PRIMITIVE_RETURN (Make_Unsigned_Fixnum (end));
  PRIMITIVE_RETURN (NIL);
}

#define SUBSTRING_COMPARE_PREFIX(index1, index2)		\
  long start1, end1, start2, end2;				\
  fast char *scan1, *scan2;					\
  PRIMITIVE_HEADER (6);						\
								\
  CHECK_ARG (1, STRING_P);					\
  start1 = (arg_nonnegative_integer (2));			\
  end1 = (arg_nonnegative_integer (3));				\
  CHECK_ARG (4, STRING_P);					\
  start2 = (arg_nonnegative_integer (5));			\
  end2 = (arg_nonnegative_integer (6));				\
								\
  if (end1 > (string_length (ARG_REF (1))))			\
    error_bad_range_arg (3);					\
  if (start1 > end1)						\
    error_bad_range_arg (2);					\
								\
  if (end2 > (string_length (ARG_REF (4))))			\
    error_bad_range_arg (6);					\
  if (start2 > end2)						\
    error_bad_range_arg (5);					\
								\
  scan1 = (string_pointer ((ARG_REF (1)), index1));		\
  scan2 = (string_pointer ((ARG_REF (4)), index2))

#define SUBSTRING_EQUAL_PREFIX()				\
  long length;							\
  SUBSTRING_COMPARE_PREFIX (start1, start2);			\
								\
  length = (end1 - start1);					\
  if (length != (end2 - start2))				\
    PRIMITIVE_RETURN (NIL);

DEFINE_PRIMITIVE ("SUBSTRING=?", Prim_Substring_Equal, 6)
{
  SUBSTRING_EQUAL_PREFIX ();

  while ((length--) > 0)
    if ((*scan1++) != (*scan2++))
      PRIMITIVE_RETURN (NIL);
  PRIMITIVE_RETURN (TRUTH);
}

DEFINE_PRIMITIVE ("SUBSTRING-CI=?", Prim_Substring_Ci_Equal, 6)
{
  SUBSTRING_EQUAL_PREFIX ();

  while ((length--) > 0)
    if ((char_upcase (*scan1++)) != (char_upcase (*scan2++)))
      PRIMITIVE_RETURN (NIL);
  PRIMITIVE_RETURN (TRUTH);
}

DEFINE_PRIMITIVE ("SUBSTRING<?", Prim_Substring_Less, 6)
{
  long length, length1, length2;
  SUBSTRING_COMPARE_PREFIX (start1, start2);

  length1 = (end1 - start1);
  length2 = (end2 - start2);
  length = ((length1 < length2) ? length1 : length2);

  while ((length--) > 0)
    if ((*scan1++) != (*scan2++))
      PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((scan1 [-1]) < (scan2 [-1])));
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (length1 < length2));
}

#define SUBSTRING_MODIFIER(char_map)					\
  Pointer string;							\
  long start, end;							\
  fast long length;							\
  fast char *scan, temp;						\
  PRIMITIVE_HEADER (3);							\
									\
  CHECK_ARG (1, STRING_P);						\
  string = (ARG_REF (1));						\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
									\
  if (end > (string_length (string)))					\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
									\
  length = (end - start);						\
  scan = (string_pointer (string, start));				\
  while ((length--) > 0)						\
    {									\
      temp = (*scan);							\
      (*scan++) = (char_map (temp));					\
    }									\
  PRIMITIVE_RETURN (NIL)

DEFINE_PRIMITIVE ("SUBSTRING-UPCASE!", Prim_Substring_Upcase, 3)
{ SUBSTRING_MODIFIER (char_upcase); }

DEFINE_PRIMITIVE ("SUBSTRING-DOWNCASE!", Prim_Substring_Downcase, 3)
{ SUBSTRING_MODIFIER (char_downcase); }

#define SUBSTRING_MATCH_PREFIX(index1, index2)			\
  long length, unmatched;					\
  SUBSTRING_COMPARE_PREFIX (index1, index2);			\
								\
  length = (substring_length_min (start1, end1, start2, end2));	\
  unmatched = length;

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-FORWARD", Prim_Match_Forward, 6)
{
  SUBSTRING_MATCH_PREFIX (start1, start2);

  while (unmatched-- > 0)
    if ((*scan1++) != (*scan2++))
      PRIMITIVE_RETURN (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (length));
}

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-FORWARD-CI", Prim_Match_Forward_Ci, 6)
{
  SUBSTRING_MATCH_PREFIX (start1, start2);

  while (unmatched-- > 0)
    if ((char_upcase (*scan1++)) != (char_upcase (*scan2++)))
      PRIMITIVE_RETURN (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (length));
}

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-BACKWARD", Prim_Match_Backward, 6)
{
  SUBSTRING_MATCH_PREFIX (end1, end2);

  while (unmatched-- > 0)
    if ((*--scan1) != (*--scan2))
      PRIMITIVE_RETURN (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (length));
}

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-BACKWARD-CI", Prim_Match_Backward_Ci, 6)
{
  SUBSTRING_MATCH_PREFIX (end1, end2);

  while (unmatched-- > 0)
    if ((char_upcase (*--scan1)) != (char_upcase (*--scan2)))
      PRIMITIVE_RETURN (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (length));
}
