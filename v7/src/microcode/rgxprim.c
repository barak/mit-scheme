/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/rgxprim.c,v 1.5 1987/11/23 05:18:09 cph Exp $

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

/* Primitives for regular expression matching and search. */

#include "scheme.h"
#include "primitive.h"
#include "string.h"
#include "char.h"
#include "edwin.h"
#include "syntax.h"
#include "regex.h"

#define RE_CHAR_SET_P(object)						\
  ((STRING_P (object)) &&						\
   ((string_length (object)) == (MAX_ASCII / ASCII_LENGTH)))

#define CHAR_SET_P(argument)						\
  ((STRING_P (argument)) && ((string_length (argument)) == MAX_ASCII))

#define CHAR_TRANSLATION_P(argument)					\
  ((STRING_P (argument)) && ((string_length (argument)) == MAX_ASCII))

#define RE_REGISTERS_P(object)						\
  (((object) == NIL) ||							\
   ((VECTOR_P (object)) &&						\
    ((Vector_Length (object)) == (RE_NREGS + RE_NREGS))))

#define RE_MATCH_RESULTS(result, vector) do				\
{									\
  if ((result) >= 0)							\
    {									\
      if ((vector) != NIL)						\
	{								\
	  int i;							\
	  long index;							\
									\
	  for (i = 0; (i < RE_NREGS); i += 1)				\
	    {								\
	      index = ((registers . start) [i]);			\
	      User_Vector_Set						\
		(vector,						\
		 i,							\
		 ((index == -1)						\
		  ? NIL							\
		  : (C_Integer_To_Scheme_Integer (index))));		\
	      index = ((registers . end) [i]);				\
	      User_Vector_Set						\
		(vector,						\
		 (i + RE_NREGS),					\
		 ((index == -1)						\
		  ? NIL							\
		  : (C_Integer_To_Scheme_Integer (index))));		\
	    }								\
	}								\
      PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (result));		\
    }									\
  else if ((result) == (-1))						\
    PRIMITIVE_RETURN (NIL);						\
  else if ((result) == (-2))						\
    error_bad_range_arg (1);						\
  else									\
    error_external_return ();						\
} while (0)

DEFINE_PRIMITIVE ("RE-CHAR-SET-ADJOIN!", Prim_re_char_set_adjoin, 2)
{
  int ascii;
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, RE_CHAR_SET_P);
  ascii = (arg_ascii_char (2));
  (* (string_pointer ((ARG_REF (1)), (ascii / ASCII_LENGTH)))) |=
    (1 << (ascii % ASCII_LENGTH));
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("RE-COMPILE-FASTMAP", Prim_re_compile_fastmap, 4)
{
  fast Pointer pattern;
  fast int can_be_null;
  PRIMITIVE_HEADER (4);

  CHECK_ARG (1, STRING_P);
  pattern = (ARG_REF (1));
  CHECK_ARG (2, CHAR_TRANSLATION_P);
  CHECK_ARG (3, SYNTAX_TABLE_P);
  CHECK_ARG (4, CHAR_SET_P);

  can_be_null =
    (re_compile_fastmap
     ((string_pointer (pattern, 0)),
      (string_pointer (pattern, (string_length (pattern)))),
      (string_pointer ((ARG_REF (2)), 0)),
      (ARG_REF (3)),
      (string_pointer ((ARG_REF (4)), 0))));

  if (can_be_null >= 0)
    PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (can_be_null));
  else if (can_be_null == (-2))
    error_bad_range_arg (1);
  else
    error_external_return ();
}

/* (re-match-substring regexp translation syntax-table registers
		       string start end)

   Attempt to match REGEXP against the substring [STRING, START, END].
   Return the index of the end of the match (exclusive) if successful.
   Otherwise return false.  REGISTERS, if not false, is set to contain
   the appropriate indices for the match registers. */

#define RE_SUBSTRING_PRIMITIVE(procedure)				\
  fast Pointer regexp;							\
  long match_start, match_end, text_end;				\
  char *text;								\
  struct re_buffer buffer;						\
  struct re_registers registers;					\
  int result;								\
  PRIMITIVE_HEADER (7);							\
									\
  CHECK_ARG (1, STRING_P);						\
  regexp = (ARG_REF (1));						\
  CHECK_ARG (2, CHAR_TRANSLATION_P);					\
  CHECK_ARG (3, SYNTAX_TABLE_P);					\
  CHECK_ARG (4, RE_REGISTERS_P);					\
  CHECK_ARG (5, STRING_P);						\
  match_start = (arg_nonnegative_integer (6));				\
  match_end = (arg_nonnegative_integer (7));				\
  text = (string_pointer ((ARG_REF (5)), 0));				\
  text_end = (string_length (ARG_REF (5)));				\
									\
  if (match_end > text_end) error_bad_range_arg (7);			\
  if (match_start > match_end) error_bad_range_arg (6);			\
									\
  re_buffer_initialize							\
    ((& buffer), (string_pointer ((ARG_REF (2)), 0)), (ARG_REF (3)),	\
     text, 0, text_end, text_end, text_end);				\
									\
  result =								\
    (procedure ((string_pointer (regexp, 0)),				\
		(string_pointer (regexp, (string_length (regexp)))),	\
		(& buffer),						\
		(((ARG_REF (4)) == NIL) ? NULL : (& registers)),	\
		(& (text [match_start])),				\
		(& (text [match_end]))));				\
  RE_MATCH_RESULTS (result, (ARG_REF (4)))

DEFINE_PRIMITIVE ("RE-MATCH-SUBSTRING", Prim_re_match_substring, 7)
{ RE_SUBSTRING_PRIMITIVE (re_match); }

DEFINE_PRIMITIVE ("RE-SEARCH-SUBSTRING-FORWARD", Prim_re_search_substr_forward, 7)
{ RE_SUBSTRING_PRIMITIVE (re_search_forward); }

DEFINE_PRIMITIVE ("RE-SEARCH-SUBSTRING-BACKWARD", Prim_re_search_substr_backward, 7)
{ RE_SUBSTRING_PRIMITIVE (re_search_backward); }

#define RE_BUFFER_PRIMITIVE(procedure)					\
  fast Pointer regexp, group;						\
  long match_start, match_end, text_start, text_end, gap_start;		\
  char *text;								\
  struct re_buffer buffer;						\
  struct re_registers registers;					\
  int result;								\
  Primitive_7_Args ();							\
									\
  CHECK_ARG (1, STRING_P);						\
  regexp = (ARG_REF (1));						\
  CHECK_ARG (2, CHAR_TRANSLATION_P);					\
  CHECK_ARG (3, SYNTAX_TABLE_P);					\
  CHECK_ARG (4, RE_REGISTERS_P);					\
  CHECK_ARG (5, GROUP_P);						\
  group = (ARG_REF (5));						\
  match_start = (arg_nonnegative_integer (6));				\
  match_end = (arg_nonnegative_integer (7));				\
									\
  text = (string_pointer ((GROUP_TEXT (group)), 0));			\
  text_start = (MARK_POSITION (GROUP_START_MARK (group)));		\
  text_end = (MARK_POSITION (GROUP_END_MARK (group)));			\
  gap_start = (GROUP_GAP_START (group));				\
									\
  if (match_end > gap_start)						\
    {									\
      match_end += (GROUP_GAP_LENGTH (group));				\
      if (match_start >= gap_start)					\
	match_start += (GROUP_GAP_LENGTH (group));			\
    }									\
									\
  if (match_start > match_end) error_bad_range_arg (6);			\
  if (match_end > text_end) error_bad_range_arg (7);			\
  if (match_start < text_start) error_bad_range_arg (6);		\
									\
  re_buffer_initialize							\
    ((& buffer), (string_pointer ((ARG_REF (2)), 0)), (ARG_REF (3)),	\
     text, text_start, text_end, gap_start, (GROUP_GAP_END (group)));	\
									\
  result =								\
    (procedure ((string_pointer (regexp, 0)),				\
		(string_pointer (regexp, (string_length (regexp)))),	\
		(& buffer),						\
		(((ARG_REF (4)) == NIL) ? NULL : (& registers)),	\
		(& (text [match_start])),				\
		(& (text [match_end]))));				\
  RE_MATCH_RESULTS (result, (ARG_REF (4)))

DEFINE_PRIMITIVE ("RE-MATCH-BUFFER", Prim_re_match_buffer, 7)
{ RE_BUFFER_PRIMITIVE (re_match); }

DEFINE_PRIMITIVE ("RE-SEARCH-BUFFER-FORWARD", Prim_re_search_buffer_forward, 7)
{ RE_BUFFER_PRIMITIVE (re_search_forward); }

DEFINE_PRIMITIVE ("RE-SEARCH-BUFFER-BACKWARD", Prim_re_search_buffer_backward, 7)
{ RE_BUFFER_PRIMITIVE (re_search_backward); }
