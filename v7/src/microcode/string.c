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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/string.c,v 5.2 1987/01/11 13:18:20 cph Exp $

String primitives. */

#include "scheme.h"
#include "primitive.h"
#include "character.h"
#include "stringprim.h"

/* The first 6 primitives are in RRRS:
1.  STRING?
2.  STRING-LENGTH
3.  STRING-REF
4.  STRING-SET
5.  SUBSTRING-MOVE-RIGHT!
6.  SUBSTRING-MOVE-LEFT!
*/

Built_In_Primitive(Prim_String_P, 1, "STRING?")
{ Primitive_1_Args();
  if (Type_Code(Arg1) != (TC_CHARACTER_STRING)) return NIL;
  else return TRUTH;
}

Built_In_Primitive(Prim_String_Length, 1, "STRING-LENGTH")
{ Primitive_1_Args();
  Arg_1_Type(TC_CHARACTER_STRING);
  return Make_Unsigned_Fixnum(String_Length(Arg1));
}

Built_In_Primitive(Prim_String_Ref, 2, "STRING-REF")
{ long index;
  char *first;
  Primitive_2_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  MY_Range_Check(index, Arg2,
		 BEGINNING, String_Length(Arg1),
		 ERR_ARG_2_BAD_RANGE);

  first = (char *) String_Index(Arg1, index);
  return (c_char_to_scheme_char( *first));
}

Built_In_Primitive(Prim_String_Set, 3, "STRING-SET!")
{ long index, ascii;
  Pointer Result;
  char *first;
  Primitive_3_Args();

  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_CHARACTER);
  MY_Range_Check(index, Arg2,
		 BEGINNING, String_Length(Arg1),
		 ERR_ARG_2_BAD_RANGE);

  ascii = scheme_char_to_c_char( Arg3);
  if (ascii == NOT_ASCII) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  first = (char *) String_Index(Arg1, index);
  Result = c_char_to_scheme_char( *first);
  *first = ascii;
  return (Result);
}

Built_In_Primitive(Prim_Substring_Move_Right, 5, "SUBSTRING-MOVE-RIGHT!")
{ long diff, start, end, length;
  char *first, *second, *firststart;
  Primitive_5_Args();

  Arg_4_Type(TC_CHARACTER_STRING);
  Arg_5_Type(TC_FIXNUM);
  Check_Substring_Args();
  diff = end - start;
  Check_Substring_Index(Arg4, Arg5, SUM_ARG_AND_INTEGER(Arg5, diff),
			ERR_ARG_5_BAD_RANGE, ERR_ARG_3_BAD_RANGE,
			second, start, end, length);

  firststart = first + diff;
  second += diff;
  while (first < firststart) *--second = *--firststart;
  return (NIL);
}

Built_In_Primitive(Prim_Substring_Move_Left, 5, "SUBSTRING-MOVE-LEFT!")
{ long diff, start, end, length;
  char *first, *second, *firstend;

  Primitive_5_Args();
  Arg_4_Type(TC_CHARACTER_STRING);
  Arg_5_Type(TC_FIXNUM);
  Check_Substring_Args();
  diff = end - start;
  Check_Substring_Index(Arg4, Arg5, SUM_ARG_AND_INTEGER(Arg5, diff),
			ERR_ARG_5_BAD_RANGE, ERR_ARG_3_BAD_RANGE,
			second,
			start, end, length);

  firstend = first + diff;
  while (first < firstend) *second++ = *first++;
  return (NIL);
}

/* Eventually the strings used in symbols must be reformatted
   to be the same as this format.  Specifically, they can't have
   type codes in the length field. */

/* Some length primitives
1.  STRING-ALLOCATE               like calling make-string with no character
                                  obj
2.  STRING-MAXIMUM-LENGTH         returns the max length of a string
                                  which is = or > string-length
3.  SET-STRING-LENGTH!            changes string from string-length to a
                                  length < or = string-max-length.
*/

Built_In_Primitive(Prim_String_Allocate, 1, "STRING-ALLOCATE")
{ long length, count;
  Pointer result;

  Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);

  length = Get_Integer(Arg1);
  Allocate_String(result, length, count);
  return result;
}

Built_In_Primitive(Prim_String_Maximum_Length, 1, "STRING-MAXIMUM-LENGTH")
{ Primitive_1_Args();
  Arg_1_Type(TC_CHARACTER_STRING);

  return Make_Unsigned_Fixnum(Max_String_Length(Arg1) - 1);
}				/* -1 for null at end */

Built_In_Primitive(Prim_Set_String_Length, 2, "SET-STRING-LENGTH!")
{ long length;
  Pointer Result;

  Primitive_2_Args();
  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(length, Arg2,
	      BEGINNING, (Max_String_Length(Arg1)),
	      ERR_ARG_2_BAD_RANGE);

  Result = Make_Unsigned_Fixnum(String_Length(Arg1));
  Set_String_Length(Arg1, length);
  return Result;
}

Built_In_Primitive(Prim_Vector_8b_Ref, 2, "VECTOR-8B-REF")
{ long index;
  char *first;

  Primitive_2_Args();
  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  MY_Range_Check(index, Arg2,
		 BEGINNING, String_Length(Arg1),
		 ERR_ARG_2_BAD_RANGE);

  first = (char *) String_Index(Arg1, index);
  return Make_Unsigned_Fixnum(*first);
}

Built_In_Primitive(Prim_Vector_8b_Set, 3, "VECTOR-8B-SET!")
{ long index, ascii;
  Pointer Result;
  char *first;

  Primitive_3_Args();
  Arg_1_Type(TC_CHARACTER_STRING);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);
  MY_Range_Check(index, Arg2,
		 BEGINNING, String_Length(Arg1),
		 ERR_ARG_2_BAD_RANGE);
  MY_Range_Check(ascii, Arg3,
		 BEGINNING, MAX_ASCII,
		 ERR_ARG_3_BAD_RANGE);

  first = (char *) String_Index(Arg1, index);
  Result = Make_Unsigned_Fixnum(*first);
  *first = ascii;
  return Result;
}

Built_In_Primitive(Prim_Vector_8b_Fill, 4, "VECTOR-8B-FILL!")
{ long start, end, ascii, length;
  char *first, *firstend;

  Primitive_4_Args();
  Arg_4_Type(TC_FIXNUM);
  Check_Substring_Args();
  MY_Range_Check(ascii, Arg4,
		 BEGINNING, MAX_ASCII,
		 ERR_ARG_4_BAD_RANGE);

  firstend = first + end - start;
  while (first < firstend) *first++ = ascii;
  return NIL;
}

Built_In_Primitive(Prim_Vector_8b_Find_Next_Char, 4,
		   "VECTOR-8B-FIND-NEXT-CHAR")
{
  long start, end, ascii, length;
  char *first, *firstend;

  Primitive_4_Args();
  Arg_4_Type(TC_FIXNUM);
  Check_Substring_Args();
  MY_Range_Check(ascii, Arg4,
		 BEGINNING, MAX_ASCII,
		 ERR_ARG_4_BAD_RANGE);

  while (start < end)
    {
      if (*first++ == ascii)
	return (Make_Unsigned_Fixnum( start));
      start += 1;
    }
  return (NIL);
}

Built_In_Primitive(Prim_Vector_8b_Find_Previous_Char, 4,
		   "VECTOR-8B-FIND-PREVIOUS-CHAR")
{
  long start, end, ascii, length;
  char *first, *firststart;

  Primitive_4_Args();
  Arg_4_Type(TC_FIXNUM);
  Check_Substring_Args();
  MY_Range_Check(ascii, Arg4,
		 BEGINNING, MAX_ASCII,
		 ERR_ARG_4_BAD_RANGE);

  first = String_Index( Arg1, end);
  while (end > start)
    {
      end -= 1;
      if (*--first == ascii)
	return (Make_Unsigned_Fixnum( end));
    }
  return (NIL);
}

Built_In_Primitive(Prim_Vector_8b_Find_Next_Char_Ci, 4,
		   "VECTOR-8B-FIND-NEXT-CHAR-CI")
{
  long start, end, ascii, length;
  char *first, *firstend;

  Primitive_4_Args();
  Arg_4_Type(TC_FIXNUM);
  Check_Substring_Args();
  MY_Range_Check(ascii, Arg4,
		 BEGINNING, MAX_ASCII,
		 ERR_ARG_4_BAD_RANGE);

  ascii = Real_To_Upper( ascii);
  while (start < end)
    {
      if (Real_To_Upper( *first++) == ascii)
	return (Make_Unsigned_Fixnum( start));
      start += 1;
    }
  return (NIL);
}

Built_In_Primitive(Prim_Vector_8b_Find_Previous_Char_Ci, 4,
		   "VECTOR-8B-FIND-PREVIOUS-CHAR-CI")
{
  long start, end, ascii, length;
  char *first, *firststart;

  Primitive_4_Args();
  Arg_4_Type(TC_FIXNUM);
  Check_Substring_Args();
  MY_Range_Check(ascii, Arg4,
		 BEGINNING, MAX_ASCII,
		 ERR_ARG_4_BAD_RANGE);

  first = String_Index( Arg1, end);
  ascii = Real_To_Upper( ascii);
  while (end > start)
    {
      end -= 1;
      if (Real_To_Upper( *--first) == ascii)
	return (Make_Unsigned_Fixnum( end));
    }
  return (NIL);
}

/* Substring primitives:
1. SUBSTRING-FIND-NEXT-CHAR-IN-SET
2. SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET  Searches through the specified
                                        substring to find the next
					character in the given char set.
3. SUBSTRING=?
4. SUBSTRING-CI=?                       Comparisons of substrings, done
5. SUBSTRING<?                          like the dictionary.
6. SUBSTRING-UPCASE!                    Makes each member of the
7. SUBSTRING-DOWNCASE!                  substring the specified case.
8. SUBSTRING-MATCH-FORWARD
9. SUBSTRING-MATCH-BACKWARD             Returns number of characters which
                                        did match.
10. SUBSTRING-MATCH-FORWARD-CI
11. SUBSTRING-MATCH-BACKWARD-CI         Case insensitive of 8 & 9.
*/

Built_In_Primitive(Prim_Substring_Find_Next_Char_In_Set, 4,
		   "SUBSTRING-FIND-NEXT-CHAR-IN-SET")
{
  long length;
  fast char *first, *char_set;
  fast long start, end, c;
  Primitive_4_Args();

  Check_Substring_Args();
  Arg_4_Type(TC_CHARACTER_STRING);
  if (String_Length(Arg4) != MAX_ASCII)
    Primitive_Error(ERR_ARG_4_BAD_RANGE);
  char_set = Scheme_String_To_C_String(Arg4);

  while (start < end)
    {
      c = *first++;
      if (char_set[c] != '\0')
	return (Make_Unsigned_Fixnum( start));
      start += 1;
    }
  return (NIL);
}

Built_In_Primitive(Prim_Substring_Find_Previous_Char_In_Set, 4,
		   "SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET")
{
  long length;
  fast char *first, *char_set;
  fast long start, end, c;
  Primitive_4_Args();

  Check_Substring_Args();
  Arg_4_Type(TC_CHARACTER_STRING);
  if (String_Length(Arg4) != MAX_ASCII)
    Primitive_Error(ERR_ARG_4_BAD_RANGE);
  char_set = Scheme_String_To_C_String(Arg4);

  first = String_Index( Arg1, end);
  while (end > start)
    {
      end -= 1;
      c = *--first;
      if (char_set[c] != '\0')
	return (Make_Unsigned_Fixnum( end));
    }
  return (NIL);
}

Built_In_Primitive(Prim_Substring_Equal, 6, "SUBSTRING=?")
{ long start, start_2, end, end_2, j, length, length_2, diff;
  char *first, *second, *firstend;

  Primitive_6_Args();
  Check_Substring_Args();
  Check_Substring_Args_B();

  diff = end - start;
  firstend = diff + first;
  if (diff != end_2 - start_2) return (NIL);
  for (; first < firstend; first++, second++)
    if (*first != *second) return NIL;
  return TRUTH;
}

Built_In_Primitive(Prim_Substring_Ci_Equal, 6, "SUBSTRING-CI=?")
{ long start, start_2, end, end_2, j, length, length_2, diff;
  char *first, *second, *firstend;

  Primitive_6_Args();
  Check_Substring_Args();
  Check_Substring_Args_B();

  diff = end - start;
  firstend = first + diff;
  if (diff != end_2 - start_2) return (NIL);
  for (; first < firstend; first++, second++)
    if (Real_To_Upper(*first) != Real_To_Upper(*second))
      return NIL;
  return (TRUTH);
}

Built_In_Primitive(Prim_Substring_Less, 6, "SUBSTRING<?")
{ long start, start_2, end, end_2, j, length, length_2, diff, diff_2;
  long string_length;
  char *first, *second, *firstend;
  Pointer Equal_Answer;

  Primitive_6_Args();
  Check_Substring_Args();
  Check_Substring_Args_B();

  diff = end - start;
  diff_2 = end_2 - start_2;
  if (diff < diff_2)
  { string_length = diff;
    Equal_Answer = TRUTH;
  }
  else
  { string_length = diff_2;
    Equal_Answer = NIL;
  }
  firstend = first + string_length;
  for (; first < firstend; first++, second++)
    if (*first > *second) return NIL;
    else if (*first < *second) return TRUTH;
  return Equal_Answer;
}

Built_In_Primitive(Prim_Substring_Upcase, 3, "SUBSTRING-UPCASE!")
{ long start, end, length;
  char *first, *firstend;

  Primitive_3_Args();
  Check_Substring_Args();

  firstend = first + end - start;
  while (first < firstend) *first++ = Real_To_Upper(*first);
  return (NIL);
}

Built_In_Primitive(Prim_Substring_Downcase, 3, "SUBSTRING-DOWNCASE!")
{ long start, end, length;
  char *first, *firstend;

  Primitive_3_Args();
  Check_Substring_Args();

  firstend = first + end - start;
  while (first < firstend) *first++ = Real_To_Lower(*first);
  return (NIL);
}

Built_In_Primitive(Prim_Substring_Match_Forward, 6, "SUBSTRING-MATCH-FORWARD")
{ long start, start_2, end, end_2, length, length_2,
       diff, diff_2, count, firstend;
  char *first, *second;

  Primitive_6_Args();
  Check_Substring_Args();
  Check_Substring_Args_B();

  diff = end - start;
  diff_2 = end_2 - start_2;
  if (diff > diff_2) firstend = diff_2;
  else firstend = diff;
  for (count=0; count < firstend; first++, second++, count++)
    if (*first != *second) return Make_Unsigned_Fixnum(count);
  return Make_Unsigned_Fixnum(count);
}

Built_In_Primitive(Prim_Substring_Match_Forward_Ci, 6,
		   "SUBSTRING-MATCH-FORWARD-CI")
{ long start, start_2, end, end_2, length, length_2,
       diff, diff_2, firstend, count;
  char *first, *second;

  Primitive_6_Args();
  Check_Substring_Args();
  Check_Substring_Args_B();

  diff = end - start;
  diff_2 = end_2 - start_2;
  if (diff > diff_2) firstend = diff_2;
  else firstend = diff;
  for (count=0; count < firstend; first++, second++, count++)
    if (Real_To_Upper(*first) != Real_To_Upper(*second))
      return Make_Unsigned_Fixnum(count);
  return Make_Unsigned_Fixnum(count);
}

Built_In_Primitive(Prim_Substring_Match_Backward, 6,
		   "SUBSTRING-MATCH-BACKWARD")
{ long start, start_2, end, end_2, length, length_2,
       diff, diff_2, min_length, count;
  char *first, *second, *firststart;

  Primitive_6_Args();
  Check_Substring_Args();
  Check_Substring_Args_B();

  diff = end - start;
  diff_2 = end_2 - start_2;
  if (diff > diff_2) min_length = diff_2;
  else min_length = diff;
  first += diff - 1;
  second += diff_2 - 1;

  for (count = 0; count < min_length; first--, second--, count++)
    if (*first != *second)
      return Make_Unsigned_Fixnum(count);
  return Make_Unsigned_Fixnum(count);
}

Built_In_Primitive(Prim_Substring_Match_Backward_Ci, 6,
		   "SUBSTRING-MATCH-BACKWARD-CI")
{ long start, start_2, end, end_2, length, length_2,
       diff, diff_2, min_length, count;
  char *first, *second, *firststart;

  Primitive_6_Args();
  Check_Substring_Args();
  Check_Substring_Args_B();

  diff = end - start;
  diff_2 =  end_2 - start_2;
  if (diff > diff_2) min_length = diff_2;
  else min_length = diff;
  first += diff - 1;
  second += diff_2 - 1;

  for (count = 0; count < min_length; first--, second--, count++)
    if (Real_To_Upper(*first) != Real_To_Upper(*second))
      return Make_Unsigned_Fixnum(count);
  return Make_Unsigned_Fixnum(count);
}
