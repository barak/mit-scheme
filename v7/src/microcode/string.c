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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/string.c,v 5.3 1987/01/12 17:20:13 cph Exp $ */

/* String primitives. */

#include "scheme.h"
#include "primitive.h"
#include "character.h"
#include "stringprim.h"

/* Currently the strings used in symbols have type codes in the length
   field.  They should be changed to have just longwords there. */

Built_In_Primitive (Prim_String_Allocate, 1, "STRING-ALLOCATE")
{
  long length, count;
  Pointer result;
  Primitive_1_Arg ();

  length = (guarantee_nonnegative_integer_arg_1 (Arg1));
  /* Add 1 to length to account for '\0' at end of string.
     Add 2 to count to account for string header words. */
  count =
    ((((length + 1) + ((sizeof (Pointer)) - 1))
      / (sizeof (Pointer)))
     + 2);
  Primitive_GC_If_Needed (count);
  result = ((Pointer) Free);
  Free[STRING_HEADER] =
    (Make_Non_Pointer (TC_MANIFEST_NM_VECTOR, (count - 1)));
  Free[STRING_LENGTH] = ((long) length);
  *(string_pointer (result, length)) = '\0';
  Free += count;
  return (Make_Pointer (TC_CHARACTER_STRING, result));
}

Built_In_Primitive (Prim_String_P, 1, "STRING?")
{
  Primitive_1_Arg ();

  return ((string_p (Arg1)) ? TRUTH : NIL);
}

Built_In_Primitive (Prim_String_Length, 1, "STRING-LENGTH")
{
  Primitive_1_Arg ();

  guarantee_string_arg_1 ();
  return (Make_Unsigned_Fixnum (string_length (Arg1)));
}

Built_In_Primitive (Prim_String_Maximum_Length, 1, "STRING-MAXIMUM-LENGTH")
{
  Primitive_1_Arg ();

  guarantee_string_arg_1 ();
  return (Make_Unsigned_Fixnum ((maximum_string_length (Arg1)) - 1));
}

Built_In_Primitive (Prim_Set_String_Length, 2, "SET-STRING-LENGTH!")
{
  long length, result;
  Primitive_2_Args ();

  guarantee_string_arg_1 ();
  length = (guarantee_nonnegative_integer_arg_2 (Arg2));
  if (length > (maximum_string_length (Arg1)))
    error_bad_range_arg_2 ();

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
  guarantee_string_arg_1 ();					\
  index = (guarantee_index_arg_2 (Arg2, (string_length (Arg1)))); \
								\
  return (process_result (string_ref (Arg1, index)));		\
}

Built_In_Primitive (Prim_String_Ref, 2, "STRING-REF")
  string_ref_body (c_char_to_scheme_char)

Built_In_Primitive (Prim_Vector_8b_Ref, 2, "VECTOR-8B-REF")
  string_ref_body (Make_Unsigned_Fixnum)

#define string_set_body(get_ascii, process_result)		\
{								\
  long index, ascii;						\
  char *char_pointer;						\
  Pointer result;						\
  Primitive_3_Args ();						\
								\
  guarantee_string_arg_1 ();					\
  index = (guarantee_index_arg_2 (Arg2, (string_length (Arg1)))); \
  ascii = (get_ascii (Arg3));					\
								\
  char_pointer = (string_pointer (Arg1, index));		\
  result = (char_to_long (*char_pointer));			\
  *char_pointer = ascii;					\
  return (process_result (result));				\
}

Built_In_Primitive (Prim_String_Set, 3, "STRING-SET!")
  string_set_body (guarantee_ascii_character_arg_3, c_char_to_scheme_char)

Built_In_Primitive (Prim_Vector_8b_Set, 3, "VECTOR-8B-SET!")
  string_set_body (guarantee_ascii_integer_arg_3, Make_Unsigned_Fixnum)

#define substring_move_prefix()					\
  long start1, end1, start2, end2, length;			\
  fast char *scan1, *scan2;					\
  Primitive_5_Args ();						\
								\
  guarantee_string_arg_1 ();					\
  start1 = (guarantee_nonnegative_integer_arg_2 (Arg2));	\
  end1 = (guarantee_nonnegative_integer_arg_3 (Arg3));		\
  guarantee_string_arg_4 ();					\
  start2 = (guarantee_nonnegative_integer_arg_5 (Arg5));	\
								\
  if (end1 > (string_length (Arg1)))				\
    error_bad_range_arg_2 ();					\
  if (start1 > end1)						\
    error_bad_range_arg_1 ();					\
  length = (end1 - start1);					\
								\
  end2 = (start2 + length);					\
  if (end2 > (string_length (Arg4)))				\
    error_bad_range_arg_3 ();

Built_In_Primitive (Prim_Substring_Move_Right, 5, "SUBSTRING-MOVE-RIGHT!")
{
  substring_move_prefix()

  scan1 = (string_pointer (Arg1, end1));
  scan2 = (string_pointer (Arg4, end2));
  while (length-- > 0)
    *--scan2 = *--scan1;
  return (NIL);
}

Built_In_Primitive (Prim_Substring_Move_Left, 5, "SUBSTRING-MOVE-LEFT!")
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
  guarantee_string_arg_1 ();					\
  start = (guarantee_nonnegative_integer_arg_2 (Arg2));		\
  end = (guarantee_nonnegative_integer_arg_3 (Arg3));		\
  ascii = (guarantee_ascii_integer_arg_4 (Arg4));		\
								\
  if (end > (string_length (Arg1)))				\
    error_bad_range_arg_3 ();					\
  if (start > end)						\
    error_bad_range_arg_2 ();

Built_In_Primitive (Prim_Vector_8b_Fill, 4, "VECTOR-8B-FILL!")
{
  vector_8b_substring_prefix ();

  length = (end - start);
  scan = (string_pointer (Arg1, start));
  while (length-- > 0)
    *scan++ = ascii;
  return (NIL);
}

Built_In_Primitive (Prim_Vector_8b_Find_Next_Char, 4,
		    "VECTOR-8B-FIND-NEXT-CHAR")
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

Built_In_Primitive (Prim_Vector_8b_Find_Previous_Char, 4,
		    "VECTOR-8B-FIND-PREVIOUS-CHAR")
{
  vector_8b_substring_prefix ();

  scan = (string_pointer (Arg1, end));
  while (end-- > start)
    if ((char_to_long (*--scan)) == ascii)
      return (Make_Unsigned_Fixnum (end));
  return (NIL);
}

Built_In_Primitive(Prim_Vector_8b_Find_Next_Char_Ci, 4,
		   "VECTOR-8B-FIND-NEXT-CHAR-CI")
{
  char char1, char2;
  vector_8b_substring_prefix ();

  scan = (string_pointer (Arg1, start));
  char1 = Real_To_Upper (ascii);
  while (start < end)
    {
      char2 = (*scan++);
      if ((Real_To_Upper (char2)) == char1)
	return (Make_Unsigned_Fixnum( start));
      start += 1;
    }
  return (NIL);
}

Built_In_Primitive(Prim_Vector_8b_Find_Previous_Char_Ci, 4,
		   "VECTOR-8B-FIND-PREVIOUS-CHAR-CI")
{
  char char1, char2;
  vector_8b_substring_prefix ();

  scan = (string_pointer (Arg1, end));
  char1 = Real_To_Upper (ascii);
  while (end-- > start)
    {
      char2 = (*--scan);
      if ((Real_To_Upper (char2)) == char1)
	return (Make_Unsigned_Fixnum (end));
    }
  return (NIL);
}

#define substring_find_char_in_set_prefix()			\
  long start, end, length;					\
  char *char_set, *scan;					\
  Primitive_4_Args ();						\
								\
  guarantee_string_arg_1 ();					\
  start = (guarantee_nonnegative_integer_arg_2 (Arg2));		\
  end = (guarantee_nonnegative_integer_arg_3 (Arg3));		\
  guarantee_string_arg_4 ();					\
								\
  if (end > (string_length (Arg1)))				\
    error_bad_range_arg_3 ();					\
  if (start > end)						\
    error_bad_range_arg_2 ();					\
  if ((string_length (Arg4)) != MAX_ASCII)			\
    error_bad_range_arg_4 ();

Built_In_Primitive(Prim_Substring_Find_Next_Char_In_Set, 4,
		   "SUBSTRING-FIND-NEXT-CHAR-IN-SET")
{
  substring_find_char_in_set_prefix ();

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

Built_In_Primitive(Prim_Substring_Find_Previous_Char_In_Set, 4,
		   "SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET")
{
  substring_find_char_in_set_prefix ();

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
  guarantee_string_arg_1 ();					\
  start1 = (guarantee_nonnegative_integer_arg_2 (Arg2));	\
  end1 = (guarantee_nonnegative_integer_arg_3 (Arg3));		\
  guarantee_string_arg_4 ();					\
  start2 = (guarantee_nonnegative_integer_arg_5 (Arg5));	\
  end2 = (guarantee_nonnegative_integer_arg_6 (Arg6));		\
								\
  if (end1 > (string_length (Arg1)))				\
    error_bad_range_arg_3 ();					\
  if (start1 > end1)						\
    error_bad_range_arg_2 ();					\
								\
  if (end2 > (string_length (Arg4)))				\
    error_bad_range_arg_6 ();					\
  if (start2 > end2)						\
    error_bad_range_arg_5 ();					\
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

Built_In_Primitive (Prim_Substring_Equal, 6, "SUBSTRING=?")
{
  substring_equal_prefix ();

  while (length-- > 0)
    if ((*scan1++) != (*scan2++))
      return (NIL);
  return (TRUTH);
}

Built_In_Primitive(Prim_Substring_Ci_Equal, 6, "SUBSTRING-CI=?")
{
  substring_equal_prefix ();

  while (length-- > 0)
    if ((Real_To_Upper (*scan1++)) != (Real_To_Upper (*scan2++)))
      return (NIL);
  return (TRUTH);
}

Built_In_Primitive (Prim_Substring_Less, 6, "SUBSTRING<?")
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
  long start, end, length;					\
  char *scan;							\
  Primitive_3_Args ();						\
								\
  guarantee_string_arg_1 ();					\
  start = (guarantee_nonnegative_integer_arg_2 (Arg2));		\
  end = (guarantee_nonnegative_integer_arg_3 (Arg3));		\
								\
  if (end > (string_length (Arg1)))				\
    error_bad_range_arg_3 ();					\
  if (start > end)						\
    error_bad_range_arg_2 ();					\
								\
  length = (end - start);					\
  scan = (string_pointer (Arg1, start));

Built_In_Primitive(Prim_Substring_Upcase, 3, "SUBSTRING-UPCASE!")
{
  substring_modification_prefix ();

  while (length-- > 0)
    *scan++ = (Real_To_Upper (*scan));
  return (NIL);
}

Built_In_Primitive(Prim_Substring_Downcase, 3, "SUBSTRING-DOWNCASE!")
{
  substring_modification_prefix ();

  while (length-- > 0)
    *scan++ = (Real_To_Lower (*scan));
  return (NIL);
}

#define substring_match_prefix(index1, index2)			\
  long length, unmatched;					\
  substring_compare_prefix (index1, index2);			\
								\
  length = (substring_length_min (start1, end1, start2, end2));	\
  unmatched = length;

Built_In_Primitive (Prim_Substring_Match_Forward, 6, "SUBSTRING-MATCH-FORWARD")
{
  substring_match_prefix (start1, start2);

  while (unmatched-- > 0)
    if ((*scan1++) != (*scan2++))
      return (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  return (Make_Unsigned_Fixnum (length));
}

Built_In_Primitive (Prim_Substring_Match_Forward_Ci, 6,
		   "SUBSTRING-MATCH-FORWARD-CI")
{
  substring_match_prefix (start1, start2);

  while (unmatched-- > 0)
    if ((Real_To_Upper (*scan1++)) != (Real_To_Upper (*scan2++)))
      return (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  return (Make_Unsigned_Fixnum (length));
}

Built_In_Primitive (Prim_Substring_Match_Backward, 6,
		   "SUBSTRING-MATCH-BACKWARD")
{
  substring_match_prefix (end1, end2);

  while (unmatched-- > 0)
    if ((*--scan1) != (*--scan2))
      return (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  return (Make_Unsigned_Fixnum (length));
}

Built_In_Primitive(Prim_Substring_Match_Backward_Ci, 6,
		   "SUBSTRING-MATCH-BACKWARD-CI")
{
  substring_match_prefix (end1, end2);

  while (unmatched-- > 0)
    if ((Real_To_Upper (*--scan1)) != (Real_To_Upper (*--scan2)))
      return (Make_Unsigned_Fixnum (length - (unmatched + 1)));
  return (Make_Unsigned_Fixnum (length));
}
