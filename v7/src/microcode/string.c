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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/string.c,v 9.25 1987/07/15 22:09:33 cph Rel $ */

/* String primitives. */

#include "scheme.h"
#include "primitive.h"
#include "character.h"
#include "stringprim.h"

/* Currently the strings used in symbols have type codes in the length
   field.  They should be changed to have just longwords there. */

Built_In_Primitive (Prim_String_Allocate, 1, "STRING-ALLOCATE", 0x13E)
{
  long length, count;
  Pointer result;
  Primitive_1_Arg ();

  length = (arg_nonnegative_integer (1));
  /* Add 1 to length to account for '\0' at end of string.
     Add 2 to count to account for string header words. */
  count =
    ((((length + 1) + ((sizeof (Pointer)) - 1))
      / (sizeof (Pointer)))
     + 2);
  Primitive_GC_If_Needed (count);
  result = Make_Pointer (TC_CHARACTER_STRING, Free);
  Free[STRING_HEADER] =
    (Make_Non_Pointer (TC_MANIFEST_NM_VECTOR, (count - 1)));
  Free[STRING_LENGTH] = ((long) length);
  *(string_pointer (result, length)) = '\0';
  Free += count;
  return (result);
}

Built_In_Primitive (Prim_String_P, 1, "STRING?", 0x138)
{
  Primitive_1_Arg ();

  return ((STRING_P (Arg1)) ? TRUTH : NIL);
}

Built_In_Primitive (Prim_String_Length, 1, "STRING-LENGTH", 0x139)
{
  Primitive_1_Arg ();

  CHECK_ARG (1, STRING_P);
  return (Make_Unsigned_Fixnum (string_length (Arg1)));
}

Built_In_Primitive (Prim_String_Maximum_Length, 1,
		    "STRING-MAXIMUM-LENGTH", 0x13F)
{
  Primitive_1_Arg ();

  CHECK_ARG (1, STRING_P);
  return (Make_Unsigned_Fixnum ((maximum_string_length (Arg1)) - 1));
}

Built_In_Primitive (Prim_Set_String_Length, 2, "SET-STRING-LENGTH!", 0x140)
{
  long length, result;
  Primitive_2_Args ();

  CHECK_ARG (1, STRING_P);
  length = (arg_nonnegative_integer (2));
  if (length > (maximum_string_length (Arg1)))
    error_bad_range_arg (2);

  result = (string_length (Arg1));
  set_string_length (Arg1, length);
  return (Make_Unsigned_Fixnum (result));
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

#define string_ref_body(process_result)				\
{								\
  long index;							\
  long result;							\
  Primitive_2_Args ();						\
								\
  CHECK_ARG (1, STRING_P);					\
  index = (arg_index_integer (2, (string_length (Arg1))));	\
								\
  return (process_result (string_ref (Arg1, index)));		\
}

Built_In_Primitive (Prim_String_Ref, 2, "STRING-REF", 0x13A)
  string_ref_body (c_char_to_scheme_char)

Built_In_Primitive (Prim_Vec_8b_Ref, 2, "VECTOR-8B-REF", 0xA5)
  string_ref_body (Make_Unsigned_Fixnum)

#define string_set_body(get_ascii, process_result)		\
{								\
  long index, ascii;						\
  char *char_pointer;						\
  Pointer result;						\
  Primitive_3_Args ();						\
								\
  CHECK_ARG (1, STRING_P);					\
  index = (arg_index_integer (2, (string_length (Arg1))));	\
  ascii = (get_ascii (3));					\
								\
  char_pointer = (string_pointer (Arg1, index));		\
  result = (char_to_long (*char_pointer));			\
  *char_pointer = ascii;					\
  return (process_result (result));				\
}

Built_In_Primitive (Prim_String_Set, 3, "STRING-SET!", 0x13B)
  string_set_body (arg_ascii_char, c_char_to_scheme_char)

Built_In_Primitive (Prim_Vec_8b_Set, 3, "VECTOR-8B-SET!", 0xA6)
  string_set_body (arg_ascii_integer, MAKE_UNSIGNED_FIXNUM)

#define substring_move_prefix()					\
  long start1, end1, start2, end2, length;			\
  fast char *scan1, *scan2;					\
  Primitive_5_Args ();						\
								\
  CHECK_ARG (1, STRING_P);					\
  start1 = (arg_nonnegative_integer (2));			\
  end1 = (arg_nonnegative_integer (3));				\
  CHECK_ARG (4, STRING_P);					\
  start2 = (arg_nonnegative_integer (5));			\
								\
  if (end1 > (string_length (Arg1)))				\
    error_bad_range_arg (2);					\
  if (start1 > end1)						\
    error_bad_range_arg (1);					\
  length = (end1 - start1);					\
								\
  end2 = (start2 + length);					\
  if (end2 > (string_length (Arg4)))				\
    error_bad_range_arg (3);

Built_In_Primitive (Prim_Substring_Move_Right, 5,
		    "SUBSTRING-MOVE-RIGHT!", 0x13C)
{
  substring_move_prefix()

  scan1 = (string_pointer (Arg1, end1));
  scan2 = (string_pointer (Arg4, end2));
  while (length-- > 0)
    *--scan2 = *--scan1;
  return (NIL);
}

Built_In_Primitive (Prim_Substring_Move_Left, 5,
		    "SUBSTRING-MOVE-LEFT!", 0x13D)
{
  substring_move_prefix()

  scan1 = (string_pointer (Arg1, start1));
  scan2 = (string_pointer (Arg4, start2));
  while (length-- > 0)
    *scan2++ = *scan1++;
  return (NIL);
}

#define vector_8b_substring_prefix()				\
  long start, end, ascii;					\
  long length;							\
  char *scan;							\
  Primitive_4_Args ();						\
								\
  CHECK_ARG (1, STRING_P);					\
  start = (arg_nonnegative_integer (2));			\
  end = (arg_nonnegative_integer (3));				\
  ascii = (arg_ascii_integer (4));				\
								\
  if (end > (string_length (Arg1)))				\
    error_bad_range_arg (3);					\
  if (start > end)						\
    error_bad_range_arg (2);

Built_In_Primitive (Prim_Vec_8b_Fill, 4, "VECTOR-8B-FILL!", 0x141)
{
  vector_8b_substring_prefix ();

  length = (end - start);
  scan = (string_pointer (Arg1, start));
  while (length-- > 0)
    *scan++ = ascii;
  return (NIL);
}

Built_In_Primitive (Prim_Vec_8b_Find_Next_Char, 4,
		    "VECTOR-8B-FIND-NEXT-CHAR", 0x142)
{
  vector_8b_substring_prefix ();

  scan = (string_pointer (Arg1, start));
  while (start < end)
    {
      if ((char_to_long (*scan++)) == ascii)
	return (Make_Unsigned_Fixnum (start));
      start += 1;
    }
  return (NIL);
}

Built_In_Primitive (Prim_Vec_8b_Find_Prev_Char, 4,
		    "VECTOR-8B-FIND-PREVIOUS-CHAR", 0x143)
{
  vector_8b_substring_prefix ();

  scan = (string_pointer (Arg1, end));
  while (end-- > start)
    if ((char_to_long (*--scan)) == ascii)
      return (Make_Unsigned_Fixnum (end));
  return (NIL);
}

Built_In_Primitive(Prim_Vec_8b_Find_Next_Char_Ci, 4,
		   "VECTOR-8B-FIND-NEXT-CHAR-CI", 0x144)
{
  char char1;
  vector_8b_substring_prefix ();

  scan = (string_pointer (Arg1, start));
  char1 = (char_upcase (ascii));
  while (start < end)
    {
      if ((char_upcase (*scan++)) == char1)
	return (Make_Unsigned_Fixnum( start));
      start += 1;
    }
  return (NIL);
}

Built_In_Primitive(Prim_Vec_8b_Find_Prev_Char_Ci, 4,
		   "VECTOR-8B-FIND-PREVIOUS-CHAR-CI", 0x145)
{
  char char1;
  vector_8b_substring_prefix ();

  scan = (string_pointer (Arg1, end));
  char1 = (char_upcase (ascii));
  while (end-- > start)
    {
      if ((char_upcase (*--scan)) == char1)
	return (Make_Unsigned_Fixnum (end));
    }
  return (NIL);
}

#define substr_find_char_in_set_prefix()			\
  long start, end, length;					\
  char *char_set, *scan;					\
  Primitive_4_Args ();						\
								\
  CHECK_ARG (1, STRING_P);					\
  start = (arg_nonnegative_integer (2));			\
  end = (arg_nonnegative_integer (3));				\
  CHECK_ARG (4, STRING_P);					\
								\
  if (end > (string_length (Arg1)))				\
    error_bad_range_arg (3);					\
  if (start > end)						\
    error_bad_range_arg (2);					\
  if ((string_length (Arg4)) != MAX_ASCII)			\
    error_bad_range_arg (4);

Built_In_Primitive(Prim_Find_Next_Char_In_Set, 4,
		   "SUBSTRING-FIND-NEXT-CHAR-IN-SET", 0x146)
{
  substr_find_char_in_set_prefix ();

  char_set = (Scheme_String_To_C_String (Arg4));
  scan = (string_pointer (Arg1, start));
  while (start < end)
    {
      if (char_set[(char_to_long (*scan++))] != '\0')
	return (Make_Unsigned_Fixnum (start));
      start += 1;
    }
  return (NIL);
}

Built_In_Primitive(Prim_Find_Prev_Char_In_Set, 4,
		   "SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET", 0x147)
{
  substr_find_char_in_set_prefix ();

  char_set = Scheme_String_To_C_String(Arg4);
  scan = (string_pointer (Arg1, end));
  while (end-- > start)
    if (char_set[(char_to_long (*--scan))] != '\0')
      return (Make_Unsigned_Fixnum (end));
  return (NIL);
}

#define substring_compare_prefix(index1, index2)		\
  long start1, end1, start2, end2;				\
  char *scan1, *scan2;						\
  Primitive_6_Args ();						\
								\
  CHECK_ARG (1, STRING_P);					\
  start1 = (arg_nonnegative_integer (2));			\
  end1 = (arg_nonnegative_integer (3));				\
  CHECK_ARG (4, STRING_P);					\
  start2 = (arg_nonnegative_integer (5));			\
  end2 = (arg_nonnegative_integer (6));				\
								\
  if (end1 > (string_length (Arg1)))				\
    error_bad_range_arg (3);					\
  if (start1 > end1)						\
    error_bad_range_arg (2);					\
								\
  if (end2 > (string_length (Arg4)))				\
    error_bad_range_arg (6);					\
  if (start2 > end2)						\
    error_bad_range_arg (5);					\
								\
  scan1 = (string_pointer (Arg1, index1));			\
  scan2 = (string_pointer (Arg4, index2));

#define substring_equal_prefix()				\
  long length;							\
  substring_compare_prefix (start1, start2);			\
								\
  length = (end1 - start1);					\
  if (length != (end2 - start2))				\
    return (NIL);

Built_In_Primitive(Prim_Substring_Equal, 6, "SUBSTRING=?", 0x148)
{
  substring_equal_prefix ();

  while (length-- > 0)
    if ((*scan1++) != (*scan2++))
      return (NIL);
  return (TRUTH);
}

Built_In_Primitive(Prim_Substring_Ci_Equal, 6, "SUBSTRING-CI=?", 0x149)
{
  substring_equal_prefix ();

  while (length-- > 0)
    if ((char_upcase (*scan1++)) != (char_upcase (*scan2++)))
      return (NIL);
  return (TRUTH);
}

Built_In_Primitive (Prim_Substring_Less, 6, "SUBSTRING<?", 0x14A)
{
  long length, length1, length2;
  substring_compare_prefix (start1, start2);

  length1 = (end1 - start1);
  length2 = (end2 - start2);
  length = ((length1 < length2) ? length1 : length2);

  while (length-- > 0)
    if ((*scan1++) != (*scan2++))
      return (((scan1[-1]) < (scan2[-1])) ? TRUTH : NIL);

  return ((length1 < length2) ? TRUTH : NIL);
}

#define substring_modification_prefix()				\
  long start, end;						\
  fast long length;						\
  fast char *scan, temp;					\
  Primitive_3_Args ();						\
								\
  CHECK_ARG (1, STRING_P);					\
  start = (arg_nonnegative_integer (2));			\
  end = (arg_nonnegative_integer (3));				\
								\
  if (end > (string_length (Arg1)))				\
    error_bad_range_arg (3);					\
  if (start > end)						\
    error_bad_range_arg (2);					\
								\
  length = (end - start);					\
  scan = (string_pointer (Arg1, start));

Built_In_Primitive(Prim_Substring_Upcase, 3, "SUBSTRING-UPCASE!", 0x14B)
{
  substring_modification_prefix ();

  while (length-- > 0)
  { temp = *scan;
    *scan++ = (char_upcase (temp));
  }
  return (NIL);
}

Built_In_Primitive(Prim_Substring_Downcase, 3, "SUBSTRING-DOWNCASE!", 0x14C)
{
  substring_modification_prefix ();

  while (length-- > 0)
  { temp = *scan;
    *scan++ = (char_downcase (temp));
  }
  return (NIL);
}

#define substring_match_prefix(index1, index2)			\
  long length, unmatched;					\
  substring_compare_prefix (index1, index2);			\
								\
  length = (substring_length_min (start1, end1, start2, end2));	\
  unmatched = length;

Built_In_Primitive (Prim_Match_Forward, 6,
		    "SUBSTRING-MATCH-FORWARD", 0x14D)
{
  substring_match_prefix (start1, start2);

  while (unmatched-- > 0)
    if ((*scan1++) != (*scan2++))
      return (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  return (Make_Unsigned_Fixnum (length));
}

Built_In_Primitive (Prim_Match_Forward_Ci, 6,
		   "SUBSTRING-MATCH-FORWARD-CI", 0x14F)
{
  substring_match_prefix (start1, start2);

  while (unmatched-- > 0)
    if ((char_upcase (*scan1++)) != (char_upcase (*scan2++)))
      return (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  return (Make_Unsigned_Fixnum (length));
}

Built_In_Primitive (Prim_Match_Backward, 6,
		   "SUBSTRING-MATCH-BACKWARD", 0x14E)
{
  substring_match_prefix (end1, end2);

  while (unmatched-- > 0)
    if ((*--scan1) != (*--scan2))
      return (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  return (Make_Unsigned_Fixnum (length));
}

Built_In_Primitive(Prim_Match_Backward_Ci, 6,
		   "SUBSTRING-MATCH-BACKWARD-CI", 0x150)
{
  substring_match_prefix (end1, end2);

  while (unmatched-- > 0)
    if ((char_upcase (*--scan1)) != (char_upcase (*--scan2)))
      return (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  return (Make_Unsigned_Fixnum (length));
}
