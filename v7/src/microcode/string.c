/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/string.c,v 9.33 1989/09/20 23:11:55 cph Rel $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* String primitives. */

#include "scheme.h"
#include "prims.h"

SCHEME_OBJECT
allocate_string (nbytes)
     fast long nbytes;
{
  fast long count = (STRING_LENGTH_TO_GC_LENGTH (nbytes));
  fast SCHEME_OBJECT result =
    (allocate_non_marked_vector (TC_CHARACTER_STRING, count, true));
  SET_STRING_LENGTH (result, nbytes);
  return (result);
}

SCHEME_OBJECT
memory_to_string (nbytes, data)
     long nbytes;
     fast unsigned char * data;
{
  SCHEME_OBJECT result = (allocate_string (nbytes));
  fast unsigned char * scan_result = (STRING_LOC (result, 0));
  fast unsigned char * end_result = (scan_result + nbytes);
  while (scan_result < end_result)
    (*scan_result++) = (*data++);
  return (result);
}

SCHEME_OBJECT
char_pointer_to_string (char_pointer)
     unsigned char * char_pointer;
{
  unsigned char * scan = char_pointer;
  while ((*scan++) != '\0')
    ;
  return (memory_to_string (((scan - 1) - char_pointer), char_pointer));
}

/* Currently the strings used in symbols have type codes in the length
   field.  They should be changed to have just longwords there. */

DEFINE_PRIMITIVE ("STRING-ALLOCATE", Prim_string_allocate, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (allocate_string (arg_nonnegative_integer (1)));
}

DEFINE_PRIMITIVE ("STRING?", Prim_string_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (STRING_P (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("STRING-LENGTH", Prim_string_length, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (STRING_LENGTH (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("STRING-MAXIMUM-LENGTH", Prim_string_maximum_length, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM (MAXIMUM_STRING_LENGTH (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("SET-STRING-LENGTH!", Prim_set_string_length, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  {
    fast SCHEME_OBJECT string = (ARG_REF (1));
    SET_STRING_LENGTH
      (string,
       (arg_index_integer (2, ((MAXIMUM_STRING_LENGTH (string)) + 1))));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SET-STRING-MAXIMUM-LENGTH!", Prim_set_string_maximum_length, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  {
    fast SCHEME_OBJECT string = (ARG_REF (1));
    fast long length =
      (arg_index_integer (2, ((MAXIMUM_STRING_LENGTH (string)) + 1)));
    MEMORY_SET
      (string,
       STRING_HEADER,
       (MAKE_OBJECT
	(TC_MANIFEST_NM_VECTOR, ((BYTES_TO_WORDS (length + 1)) + 1))));
    SET_STRING_LENGTH (string, length);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define STRING_REF_BODY(process_result)					\
{									\
  PRIMITIVE_HEADER (2);							\
  CHECK_ARG (1, STRING_P);						\
  {									\
    fast SCHEME_OBJECT string = (ARG_REF (1));				\
    PRIMITIVE_RETURN							\
      (process_result							\
       (STRING_REF							\
	(string, (arg_index_integer (2, (STRING_LENGTH (string)))))));	\
  }									\
}

DEFINE_PRIMITIVE ("STRING-REF", Prim_string_ref, 2, 2, 0)
     STRING_REF_BODY (ASCII_TO_CHAR)

DEFINE_PRIMITIVE ("VECTOR-8B-REF", Prim_vec_8b_ref, 2, 2, 0)
     STRING_REF_BODY (LONG_TO_UNSIGNED_FIXNUM)

#define STRING_SET_BODY(get_ascii)					\
{									\
  PRIMITIVE_HEADER (3);							\
  CHECK_ARG (1, STRING_P);						\
  {									\
    fast SCHEME_OBJECT string = (ARG_REF (1));				\
    STRING_SET								\
      (string,								\
       (arg_index_integer (2, (STRING_LENGTH (string)))),		\
       (get_ascii (3)));						\
  }									\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

DEFINE_PRIMITIVE ("STRING-SET!", Prim_string_set, 3, 3, 0)
     STRING_SET_BODY (arg_ascii_char)

DEFINE_PRIMITIVE ("VECTOR-8B-SET!", Prim_vec_8b_set, 3, 3, 0)
     STRING_SET_BODY (arg_ascii_integer)

#define SUBSTRING_MOVE_PREFIX()						\
  long start1, end1, start2, end2, length;				\
  fast unsigned char *scan1, *scan2, *limit;				\
  PRIMITIVE_HEADER (5);							\
  CHECK_ARG (1, STRING_P);						\
  start1 = (arg_nonnegative_integer (2));				\
  end1 = (arg_nonnegative_integer (3));					\
  CHECK_ARG (4, STRING_P);						\
  start2 = (arg_nonnegative_integer (5));				\
  length = (end1 - start1);						\
  end2 = (start2 + length);						\
  if (end1 > (STRING_LENGTH (ARG_REF (1))))				\
    error_bad_range_arg (2);						\
  if (start1 > end1)							\
    error_bad_range_arg (1);						\
  if (end2 > (STRING_LENGTH (ARG_REF (4))))				\
    error_bad_range_arg (3)

DEFINE_PRIMITIVE ("SUBSTRING-MOVE-RIGHT!", Prim_substring_move_right, 5, 5, 0)
{
  SUBSTRING_MOVE_PREFIX ();
  scan1 = (STRING_LOC ((ARG_REF (1)), end1));
  scan2 = (STRING_LOC ((ARG_REF (4)), end2));
  limit = (scan1 - length);
  while (scan1 > limit)
    (*--scan2) = (*--scan1);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SUBSTRING-MOVE-LEFT!", Prim_substring_move_left, 5, 5, 0)
{
  SUBSTRING_MOVE_PREFIX ();
  scan1 = (STRING_LOC ((ARG_REF (1)), start1));
  scan2 = (STRING_LOC ((ARG_REF (4)), start2));
  limit = (scan1 + length);
  while (scan1 < limit)
    (*scan2++) = (*scan1++);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define SUBSTRING_MODIFIER(char_map)					\
{									\
  SCHEME_OBJECT string;							\
  long start, end;							\
  fast long length;							\
  fast unsigned char *scan, temp;					\
  PRIMITIVE_HEADER (3);							\
  CHECK_ARG (1, STRING_P);						\
  string = (ARG_REF (1));						\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  if (end > (STRING_LENGTH (string)))					\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
  length = (end - start);						\
  scan = (STRING_LOC (string, start));					\
  while ((length--) > 0)						\
    {									\
      temp = (*scan);							\
      (*scan++) = (char_map (temp));					\
    }									\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

DEFINE_PRIMITIVE ("SUBSTRING-UPCASE!", Prim_substring_upcase, 3, 3, 0)
     SUBSTRING_MODIFIER (char_upcase)

DEFINE_PRIMITIVE ("SUBSTRING-DOWNCASE!", Prim_substring_downcase, 3, 3, 0)
     SUBSTRING_MODIFIER (char_downcase)

#define VECTOR_8B_SUBSTRING_PREFIX()					\
  long start, end, ascii;						\
  fast unsigned char *string_start, *scan, *limit;			\
  PRIMITIVE_HEADER (4);							\
  CHECK_ARG (1, STRING_P);						\
  string_start = (STRING_LOC ((ARG_REF (1)), 0));			\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  ascii = (arg_ascii_integer (4));					\
  if (end > (STRING_LENGTH (ARG_REF (1))))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2)

#define VECTOR_8B_SUBSTRING_PREFIX_FORWARD()				\
  VECTOR_8B_SUBSTRING_PREFIX ();					\
  scan = (string_start + start);					\
  limit = (string_start + end);

#define VECTOR_8B_SUBSTRING_PREFIX_BACKWARD()				\
  VECTOR_8B_SUBSTRING_PREFIX ();					\
  scan = (string_start + end);						\
  limit = (string_start + start);

DEFINE_PRIMITIVE ("VECTOR-8B-FILL!", Prim_vec_8b_fill, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_FORWARD ();
  while (scan < limit)
    (*scan++) = ascii;
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-NEXT-CHAR", Prim_vec_8b_find_next_char, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_FORWARD ();
  while (scan < limit)
    if ((*scan++) == ascii)
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan - 1) - string_start));
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-PREVIOUS-CHAR", Prim_vec_8b_find_prev_char, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_BACKWARD ();
  while (scan > limit)
    if ((*--scan) == ascii)
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan - string_start));
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-NEXT-CHAR-CI", Prim_vec_8b_find_next_char_ci, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_FORWARD ();
  {
    fast unsigned char char1 = (char_upcase (ascii));
    while (scan < limit)
      if ((char_upcase (*scan++)) == char1)
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan - 1) - string_start));
  }
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-PREVIOUS-CHAR-CI", Prim_vec_8b_find_prev_char_ci, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_BACKWARD ();
  {
    fast unsigned char char1 = (char_upcase (ascii));
    while (scan > limit)
      if ((char_upcase (*--scan)) == char1)
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan - string_start));
  }
  PRIMITIVE_RETURN (SHARP_F);
}

#define SUBSTR_FIND_CHAR_IN_SET_PREFIX()				\
  long start, end;							\
  unsigned char *char_set, *string_start, *scan, *limit;		\
  PRIMITIVE_HEADER (4);							\
  CHECK_ARG (1, STRING_P);						\
  string_start = (STRING_LOC ((ARG_REF (1)), 0));			\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  CHECK_ARG (4, STRING_P);						\
  char_set = (STRING_LOC ((ARG_REF (4)), 0));				\
  if (end > (STRING_LENGTH (ARG_REF (1))))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
  if ((STRING_LENGTH (ARG_REF (4))) != MAX_ASCII)			\
    error_bad_range_arg (4)

DEFINE_PRIMITIVE ("SUBSTRING-FIND-NEXT-CHAR-IN-SET", Prim_find_next_char_in_set, 4, 4, 0)
{
  SUBSTR_FIND_CHAR_IN_SET_PREFIX ();
  scan = (string_start + start);
  limit = (string_start + end);
  while (scan < limit)
    if ((char_set [*scan++]) != '\0')
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan - 1) - string_start));
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET", Prim_find_prev_char_in_set, 4, 4, 0)
{
  SUBSTR_FIND_CHAR_IN_SET_PREFIX ();
  scan = (string_start + end);
  limit = (string_start + start);
  while (scan > limit)
    if ((char_set [*--scan]) != '\0')
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan - string_start));
  PRIMITIVE_RETURN (SHARP_F);
}

#define SUBSTRING_COMPARE_PREFIX()				\
  long start1, end1, start2, end2;				\
  unsigned char *string1_start, *string2_start;			\
  PRIMITIVE_HEADER (6);						\
  CHECK_ARG (1, STRING_P);					\
  string1_start = (STRING_LOC ((ARG_REF (1)), 0));		\
  start1 = (arg_nonnegative_integer (2));			\
  end1 = (arg_nonnegative_integer (3));				\
  CHECK_ARG (4, STRING_P);					\
  string2_start = (STRING_LOC ((ARG_REF (4)), 0));		\
  start2 = (arg_nonnegative_integer (5));			\
  end2 = (arg_nonnegative_integer (6));				\
  if (end1 > (STRING_LENGTH (ARG_REF (1))))			\
    error_bad_range_arg (3);					\
  if (start1 > end1)						\
    error_bad_range_arg (2);					\
  if (end2 > (STRING_LENGTH (ARG_REF (4))))			\
    error_bad_range_arg (6);					\
  if (start2 > end2)						\
    error_bad_range_arg (5)

#define SUBSTRING_EQUAL_PREFIX()				\
  fast unsigned char *scan1, *scan2, *limit;			\
  SUBSTRING_COMPARE_PREFIX ();					\
  if ((end1 - start1) != (end2 - start2))			\
    PRIMITIVE_RETURN (SHARP_F);					\
  scan1 = (string1_start + start1);				\
  limit = (string1_start + end1);				\
  scan2 = (string2_start + start2)

DEFINE_PRIMITIVE ("SUBSTRING=?", Prim_substring_equal, 6, 6, 0)
{
  SUBSTRING_EQUAL_PREFIX ();
  while (scan1 < limit)
    if ((*scan1++) != (*scan2++))
      PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("SUBSTRING-CI=?", Prim_substring_ci_equal, 6, 6, 0)
{
  SUBSTRING_EQUAL_PREFIX ();
  while (scan1 < limit)
    if ((char_upcase (*scan1++)) != (char_upcase (*scan2++)))
      PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("SUBSTRING<?", Prim_substring_less, 6, 6, 0)
{
  SUBSTRING_COMPARE_PREFIX ();
  {
    fast unsigned char * scan1 = (string1_start + start1);
    fast unsigned char * scan2 = (string2_start + start2);
    long length1 = (end1 - start1);
    long length2 = (end2 - start2);
    fast unsigned char * limit =
      (scan1 + ((length1 < length2) ? length1 : length2));
    while (scan1 < limit)
      if ((*scan1++) != (*scan2++))
	PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((scan1 [-1]) < (scan2 [-1])));
    PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (length1 < length2));
  }
}

static long
substring_length_min (start1, end1, start2, end2)
     long start1, end1, start2, end2;
{
  fast long length1 = (end1 - start1);
  fast long length2 = (end2 - start2);
  return ((length1 < length2) ? length1 : length2);
}

#define SUBSTRING_MATCH_PREFIX()					\
  fast unsigned char *scan1, *scan2, *limit;				\
  long length;								\
  unsigned char *scan1_start;						\
  SUBSTRING_COMPARE_PREFIX ();						\
  length = (substring_length_min (start1, end1, start2, end2))

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-FORWARD", Prim_match_forward, 6, 6, 0)
{
  SUBSTRING_MATCH_PREFIX ();
  scan1 = (string1_start + start1);
  scan2 = (string2_start + start2);
  limit = (scan1 + length);
  scan1_start = scan1;
  while (scan1 < limit)
    if ((*scan1++) != (*scan2++))
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan1 - 1) - scan1_start));
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (length));
}

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-FORWARD-CI", Prim_match_forward_ci, 6, 6, 0)
{
  SUBSTRING_MATCH_PREFIX ();
  scan1 = (string1_start + start1);
  scan2 = (string2_start + start2);
  limit = (scan1 + length);
  scan1_start = scan1;
  while (scan1 < limit)
    if ((char_upcase (*scan1++)) != (char_upcase (*scan2++)))
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan1 - 1) - scan1_start));
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (length));
}

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-BACKWARD", Prim_match_backward, 6, 6, 0)
{
  SUBSTRING_MATCH_PREFIX ();
  scan1 = (string1_start + end1);
  scan2 = (string2_start + end2);
  limit = (scan1 - length);
  scan1_start = scan1;
  while (scan1 > limit)
    if ((*--scan1) != (*--scan2))
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan1_start - (scan1 + 1)));
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (length));
}

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-BACKWARD-CI", Prim_match_backward_ci, 6, 6, 0)
{
  SUBSTRING_MATCH_PREFIX ();
  scan1 = (string1_start + end1);
  scan2 = (string2_start + end2);
  limit = (scan1 - length);
  scan1_start = scan1;
  while (scan1 > limit)
    if ((char_upcase (*--scan1)) != (char_upcase (*--scan2)))
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan1_start - (scan1 + 1)));
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (length));
}
