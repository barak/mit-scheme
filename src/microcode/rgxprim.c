/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

/* Primitives for regular expression matching and search. */

#include "scheme.h"
#include "prims.h"
#include "edwin.h"
#include "syntax.h"
#include "regex.h"

extern int re_max_failures;

#define RE_CHAR_SET_P(object)						\
  ((STRING_P (object)) &&						\
   ((STRING_LENGTH (object)) == (MAX_ASCII / ASCII_LENGTH)))

#define CHAR_SET_P(argument)						\
  ((STRING_P (argument)) && ((STRING_LENGTH (argument)) == MAX_ASCII))

#define CHAR_TRANSLATION_P(argument)					\
  ((STRING_P (argument)) && ((STRING_LENGTH (argument)) == MAX_ASCII))

#define RE_REGISTERS_P(object)						\
  (((object) == SHARP_F) ||						\
   ((VECTOR_P (object)) &&						\
    ((VECTOR_LENGTH (object)) == (RE_NREGS + RE_NREGS))))

#define RE_MATCH_RESULTS(result, vector) do				\
{									\
  if ((result) >= 0)							\
    {									\
      if ((vector) != SHARP_F)						\
	{								\
	  int i;							\
	  long index;							\
									\
	  for (i = 0; (i < RE_NREGS); i += 1)				\
	    {								\
	      index = ((registers . start) [i]);			\
	      VECTOR_SET						\
		(vector,						\
		 i,							\
		 ((index == -1)						\
		  ? SHARP_F						\
		  : (long_to_integer (index))));			\
	      index = ((registers . end) [i]);				\
	      VECTOR_SET						\
		(vector,						\
		 (i + RE_NREGS),					\
		 ((index == -1)						\
		  ? SHARP_F						\
		  : (long_to_integer (index))));			\
	    }								\
	}								\
      PRIMITIVE_RETURN (long_to_integer (result));			\
    }									\
  else if (((result) == (-1)) || ((result) == (-4)))			\
    PRIMITIVE_RETURN (SHARP_F);						\
  else if ((result) == (-2))						\
    error_bad_range_arg (1);						\
  else									\
    error_external_return ();						\
  /*NOTREACHED*/							\
  return (0);								\
} while (0)

DEFINE_PRIMITIVE ("RE-CHAR-SET-ADJOIN!", Prim_re_char_set_adjoin, 2, 2, 0)
{
  int ascii;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, RE_CHAR_SET_P);
  ascii = (arg_ascii_integer (2));
  (* (STRING_LOC ((ARG_REF (1)), (ascii / ASCII_LENGTH)))) |=
    (1 << (ascii % ASCII_LENGTH));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("RE-COMPILE-FASTMAP", Prim_re_compile_fastmap, 4, 4, 0)
{
  SCHEME_OBJECT pattern;
  int can_be_null;
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, STRING_P);
  pattern = (ARG_REF (1));
  CHECK_ARG (2, CHAR_TRANSLATION_P);
  CHECK_ARG (3, SYNTAX_TABLE_P);
  CHECK_ARG (4, CHAR_SET_P);
  can_be_null =
    (re_compile_fastmap
     ((STRING_LOC (pattern, 0)),
      (STRING_LOC (pattern, (STRING_LENGTH (pattern)))),
      (STRING_LOC ((ARG_REF (2)), 0)),
      (ARG_REF (3)),
      (STRING_LOC ((ARG_REF (4)), 0))));
  if (can_be_null >= 0)
    PRIMITIVE_RETURN (long_to_integer (can_be_null));
  else if (can_be_null == (-2))
    error_bad_range_arg (1);
  else
    error_external_return ();
  /*NOTREACHED*/
  return (0);
}

/* (re-match-substring regexp translation syntax-table registers
		       string start end)

   Attempt to match REGEXP against the substring [STRING, START, END].
   Return the index of the end of the match (exclusive) if successful.
   Otherwise return false.  REGISTERS, if not false, is set to contain
   the appropriate indices for the match registers. */

#define RE_SUBSTRING_PRIMITIVE(procedure)				\
{									\
  SCHEME_OBJECT regexp;							\
  long match_start, match_end, text_end;				\
  unsigned char * text;							\
  struct re_buffer buffer;						\
  struct re_registers registers;					\
  int result;								\
  PRIMITIVE_HEADER (7);							\
  CHECK_ARG (1, STRING_P);						\
  regexp = (ARG_REF (1));						\
  CHECK_ARG (2, CHAR_TRANSLATION_P);					\
  CHECK_ARG (3, SYNTAX_TABLE_P);					\
  CHECK_ARG (4, RE_REGISTERS_P);					\
  CHECK_ARG (5, STRING_P);						\
  match_start = (arg_nonnegative_integer (6));				\
  match_end = (arg_nonnegative_integer (7));				\
  text = (STRING_LOC ((ARG_REF (5)), 0));				\
  text_end = (STRING_LENGTH (ARG_REF (5)));				\
  if (match_end > text_end) error_bad_range_arg (7);			\
  if (match_start > match_end) error_bad_range_arg (6);			\
  re_max_failures = 20000;						\
  re_buffer_initialize							\
    ((& buffer), (STRING_LOC ((ARG_REF (2)), 0)), (ARG_REF (3)),	\
     text, 0, text_end, text_end, text_end);				\
  result =								\
    (procedure ((STRING_LOC (regexp, 0)),				\
		(STRING_LOC (regexp, (STRING_LENGTH (regexp)))),	\
		(& buffer),						\
		(((ARG_REF (4)) == SHARP_F) ? NULL : (& registers)),	\
		(& (text [match_start])),				\
		(& (text [match_end]))));				\
  RE_MATCH_RESULTS (result, (ARG_REF (4)));				\
}

DEFINE_PRIMITIVE ("RE-MATCH-SUBSTRING", Prim_re_match_substring, 7, 7, 0)
     RE_SUBSTRING_PRIMITIVE (re_match)

DEFINE_PRIMITIVE ("RE-SEARCH-SUBSTRING-FORWARD", Prim_re_search_substr_forward, 7, 7, 0)
     RE_SUBSTRING_PRIMITIVE (re_search_forward)

DEFINE_PRIMITIVE ("RE-SEARCH-SUBSTRING-BACKWARD", Prim_re_search_substr_backward, 7, 7, 0)
     RE_SUBSTRING_PRIMITIVE (re_search_backward)

#define RE_BUFFER_PRIMITIVE(procedure)					\
{									\
  SCHEME_OBJECT regexp, group;						\
  long match_start, match_end, text_start, text_end, gap_start;		\
  unsigned char * text;							\
  struct re_buffer buffer;						\
  struct re_registers registers;					\
  int result;								\
  PRIMITIVE_HEADER (7);							\
  CHECK_ARG (1, STRING_P);						\
  regexp = (ARG_REF (1));						\
  CHECK_ARG (2, CHAR_TRANSLATION_P);					\
  CHECK_ARG (3, SYNTAX_TABLE_P);					\
  CHECK_ARG (4, RE_REGISTERS_P);					\
  CHECK_ARG (5, GROUP_P);						\
  group = (ARG_REF (5));						\
  match_start = (arg_nonnegative_integer (6));				\
  match_end = (arg_nonnegative_integer (7));				\
  text = (GROUP_TEXT (group, 0));					\
  text_start = (MARK_INDEX (GROUP_START_MARK (group)));			\
  text_end = (MARK_INDEX (GROUP_END_MARK (group)));			\
  gap_start = (GROUP_GAP_START (group));				\
  if (text_end > gap_start)						\
    text_end += (GROUP_GAP_LENGTH (group));				\
  if (match_end > gap_start)						\
    {									\
      match_end += (GROUP_GAP_LENGTH (group));				\
      if (match_start >= gap_start)					\
	match_start += (GROUP_GAP_LENGTH (group));			\
    }									\
  if (match_start > match_end) error_bad_range_arg (6);			\
  if (match_end > text_end) error_bad_range_arg (7);			\
  if (match_start < text_start) error_bad_range_arg (6);		\
  re_max_failures = 20000;						\
  re_buffer_initialize							\
    ((& buffer), (STRING_LOC ((ARG_REF (2)), 0)), (ARG_REF (3)),	\
     text, text_start, text_end, gap_start, (GROUP_GAP_END (group)));	\
  result =								\
    (procedure ((STRING_LOC (regexp, 0)),				\
		(STRING_LOC (regexp, (STRING_LENGTH (regexp)))),	\
		(& buffer),						\
		(((ARG_REF (4)) == SHARP_F) ? NULL : (& registers)),	\
		(& (text [match_start])),				\
		(& (text [match_end]))));				\
  RE_MATCH_RESULTS (result, (ARG_REF (4)));				\
}

DEFINE_PRIMITIVE ("RE-MATCH-BUFFER", Prim_re_match_buffer, 7, 7, 0)
     RE_BUFFER_PRIMITIVE (re_match)

DEFINE_PRIMITIVE ("RE-SEARCH-BUFFER-FORWARD", Prim_re_search_buffer_forward, 7, 7, 0)
     RE_BUFFER_PRIMITIVE (re_search_forward)

DEFINE_PRIMITIVE ("RE-SEARCH-BUFFER-BACKWARD", Prim_re_search_buffer_backward, 7, 7, 0)
     RE_BUFFER_PRIMITIVE (re_search_backward)
