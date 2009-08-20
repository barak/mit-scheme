/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* Primitives to support Edwin syntax tables, word and list parsing. */

/* NOTE: This program was created by translation from the syntax table
   code of GNU Emacs; it was translated from the original C to 68000
   assembly language (in 1986), and then translated back from 68000
   assembly language to C (in 1987).  */

#include "scheme.h"
#include "prims.h"
#include "edwin.h"
#include "syntax.h"

/* Syntax Codes */

/* Convert a letter which signifies a syntax code
   into the code it signifies. */

#define ILLEGAL ((unsigned char) syntaxcode_max)

unsigned char syntax_spec_code [0x80] =
  {
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,

    ((unsigned char) syntaxcode_whitespace),
    ILLEGAL,
    ((unsigned char) syntaxcode_string),
    ILLEGAL,
    ((unsigned char) syntaxcode_math),
    ILLEGAL,
    ILLEGAL,
    ((unsigned char) syntaxcode_quote),

    ((unsigned char) syntaxcode_open),
    ((unsigned char) syntaxcode_close),
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ((unsigned char) syntaxcode_whitespace),
    ((unsigned char) syntaxcode_punct),
    ((unsigned char) syntaxcode_charquote),

    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,

    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ((unsigned char) syntaxcode_comment),
    ILLEGAL,
    ((unsigned char) syntaxcode_endcomment),
    ILLEGAL,

    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,

    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ((unsigned char) syntaxcode_word),

    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ((unsigned char) syntaxcode_escape),
    ILLEGAL,
    ILLEGAL,
    ((unsigned char) syntaxcode_symbol),

    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,

    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ILLEGAL,
    ((unsigned char) syntaxcode_word),

    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL
  };

/* Indexed by syntax code, give the letter that describes it. */

unsigned char syntax_code_spec[13] =
  {
    ' ', '.', 'w', '_', '(', ')', '\'', '\"', '$', '\\', '/', '<', '>'
  };

#define MERGE_PREFIX_BIT(bit) do					\
{									\
  if ((result & bit) != 0)						\
    error_bad_range_arg (1);						\
  result |= bit;							\
} while (0)

#define MERGE_COMMENT(bit) MERGE_PREFIX_BIT ((bit) << 12)

DEFINE_PRIMITIVE ("STRING->SYNTAX-ENTRY", Prim_string_to_syntax_entry, 1, 1, 0)
{
  unsigned long length;
  unsigned long result;
  unsigned char * scan;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  length = (STRING_LENGTH (ARG_REF (1)));
  scan = (STRING_LOC ((ARG_REF (1)), 0));

  if (length > 0)
    {
      unsigned long c = (*scan++);
      length -= 1;
      if (c >= 0200) error_bad_range_arg (1);
      result = (syntax_spec_code[c]);
      if (result == ILLEGAL) error_bad_range_arg (1);
    }
  else
    result = ((unsigned long) syntaxcode_whitespace);

  if (length > 0)
    {
      unsigned long c = (*scan++);
      length -= 1;
      if (c != ' ') result |= (c << 4);
    }

  while ((length--) > 0)
    switch (*scan++)
      {
      case '1': MERGE_COMMENT (COMSTART_FIRST_B); break;
      case '2': MERGE_COMMENT (COMSTART_SECOND_B); break;
      case '3': MERGE_COMMENT (COMEND_FIRST_B); break;
      case '4': MERGE_COMMENT (COMEND_SECOND_B); break;
      case '5': MERGE_COMMENT (COMSTART_FIRST_A); break;
      case '6': MERGE_COMMENT (COMSTART_SECOND_A); break;
      case '7': MERGE_COMMENT (COMEND_FIRST_A); break;
      case '8': MERGE_COMMENT (COMEND_SECOND_A); break;
      case 'b':
	switch (SYNTAX_ENTRY_CODE (result))
	  {
	  case syntaxcode_comment: MERGE_COMMENT (COMSTART_FIRST_B); break;
	  case syntaxcode_endcomment: MERGE_COMMENT (COMEND_FIRST_B); break;
	  default: break;
	  }
	break;
      case 'p': MERGE_PREFIX_BIT (1 << 20); break;
      case ' ': break;
      default: error_bad_range_arg (1);
      }
  if (((SYNTAX_ENTRY_CODE (result)) == syntaxcode_comment)
      && (! ((SYNTAX_ENTRY_COMMENT_BITS (result)) & COMSTART_FIRST)))
    MERGE_COMMENT (COMSTART_FIRST_A);
  if (((SYNTAX_ENTRY_CODE (result)) == syntaxcode_endcomment)
      && (! ((SYNTAX_ENTRY_COMMENT_BITS (result)) & COMEND_FIRST)))
    MERGE_COMMENT (COMEND_FIRST_A);
  PRIMITIVE_RETURN (ULONG_TO_FIXNUM (result));
}

DEFINE_PRIMITIVE ("CHAR->SYNTAX-CODE", Prim_char_to_syntax_code, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, SYNTAX_TABLE_P);
  PRIMITIVE_RETURN
    (ASCII_TO_CHAR
     (syntax_code_spec
      [((int)
	(SYNTAX_ENTRY_CODE
	 (SYNTAX_TABLE_REF ((ARG_REF (1)), (arg_ascii_char (2))))))]));
}

/* Parser Initialization */

#define NORMAL_INITIALIZATION_COMMON(arity)				\
  SCHEME_OBJECT syntax_table;						\
  SCHEME_OBJECT group;							\
  unsigned char * start;						\
  unsigned char * first_char, * end;					\
  long gap_length;							\
  PRIMITIVE_HEADER (arity);						\
  CHECK_ARG (1, SYNTAX_TABLE_P);					\
  syntax_table = (ARG_REF (1));						\
  CHECK_ARG (2, GROUP_P);						\
  group = (ARG_REF (2));						\
  first_char = (GROUP_TEXT (group, 0));					\
  start = (first_char + (arg_nonnegative_integer (3)));			\
  end = (first_char + (arg_nonnegative_integer (4)));			\
  gap_start = (first_char + (GROUP_GAP_START (group)));			\
  gap_length = (GROUP_GAP_LENGTH (group));				\
  gap_end = (first_char + (GROUP_GAP_END (group)))

#define NORMAL_INITIALIZATION_FORWARD(arity)				\
  unsigned char * gap_start;						\
  unsigned char * gap_end;						\
  NORMAL_INITIALIZATION_COMMON (arity);					\
  if (start >= gap_start)						\
    start += gap_length;						\
  if (end >= gap_start)							\
    end += gap_length

#define NORMAL_INITIALIZATION_BACKWARD(arity)				\
  unsigned char * gap_start;						\
  unsigned char * gap_end;						\
  NORMAL_INITIALIZATION_COMMON (arity);					\
  if (start > gap_start)						\
    start += gap_length;						\
  if (end > gap_start)							\
    end += gap_length

#define SCAN_LIST_INITIALIZATION(initialization)			\
  long depth, min_depth;						\
  bool sexp_flag, ignore_comments, math_exit;				\
  int c;								\
  initialization (7);							\
  depth = (arg_integer (5));						\
  min_depth = ((depth >= 0) ? 0 : depth);				\
  sexp_flag = (BOOLEAN_ARG (6));					\
  ignore_comments = (BOOLEAN_ARG (7));					\
  math_exit = false

/* Parse Scanning */

#define PEEK_RIGHT(scan) (SYNTAX_TABLE_REF (syntax_table, (*scan)))
#define PEEK_LEFT(scan) (SYNTAX_TABLE_REF (syntax_table, (scan[-1])))

#define MOVE_RIGHT(scan) do						\
{									\
  if ((++scan) == gap_start)						\
    scan = gap_end;							\
} while (0)

#define MOVE_LEFT(scan) do						\
{									\
  if ((--scan) == gap_end)						\
    scan = gap_start;							\
} while (0)

#define READ_RIGHT(scan, target) do					\
{									\
  target = (SYNTAX_TABLE_REF (syntax_table, (*scan++)));		\
  if (scan == gap_start)						\
    scan = gap_end;							\
} while (0)

#define READ_LEFT(scan, target) do					\
{									\
  target = (SYNTAX_TABLE_REF (syntax_table, (*--scan)));		\
  if (scan == gap_end)							\
    scan = gap_start;							\
} while (0)

#define RIGHT_END_P(scan) (scan >= end)
#define LEFT_END_P(scan) (scan <= end)

#define LOSE_IF(expression) do						\
{									\
  if (expression)							\
    PRIMITIVE_RETURN (SHARP_F);						\
} while (0)

#define LOSE_IF_RIGHT_END(scan) LOSE_IF (RIGHT_END_P (scan))
#define LOSE_IF_LEFT_END(scan) LOSE_IF (LEFT_END_P (scan))

#define SCAN_TO_INDEX(scan)						\
  ((((scan) > gap_start) ? ((scan) - gap_length) : (scan)) - first_char)

#define INDEX_TO_SCAN(index)						\
  ((((index) + first_char) > gap_start)					\
   ? (((index) + first_char) + gap_length)				\
   : ((index) + first_char))

#define WIN_IF(expression) do						\
{									\
  if (expression)							\
    PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (SCAN_TO_INDEX (start)));	\
} while (0)

#define WIN_IF_RIGHT_END(scan) WIN_IF (RIGHT_END_P (scan))
#define WIN_IF_LEFT_END(scan) WIN_IF (LEFT_END_P (scan))

#define RIGHT_QUOTED_P_INTERNAL(scan, quoted) do			\
{									\
  quoted = false;							\
  while (true)								\
    {									\
      long sentry;							\
      if (LEFT_END_P (scan))						\
	break;								\
      READ_LEFT (scan, sentry);						\
      if (! (SYNTAX_ENTRY_QUOTE (sentry)))				\
	break;								\
      quoted = (! quoted);						\
    }									\
} while (0)

#define RIGHT_QUOTED_P(scan_init, quoted) do				\
{									\
  unsigned char * scan = (scan_init);					\
  RIGHT_QUOTED_P_INTERNAL (scan, quoted);				\
} while (0)

#define LEFT_QUOTED_P(scan_init, quoted) do				\
{									\
  unsigned char * scan = (scan_init);					\
  MOVE_LEFT (scan);							\
  RIGHT_QUOTED_P_INTERNAL (scan, quoted);				\
} while (0)

/* Quote Parsers */

DEFINE_PRIMITIVE ("QUOTED-CHAR?", Prim_quoted_char_p, 4, 4, 0)
{
  bool quoted;
  NORMAL_INITIALIZATION_BACKWARD (4);

  RIGHT_QUOTED_P (start, quoted);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (quoted));
}

/* This is used in conjunction with `scan-list-backward' to find the
   beginning of an s-expression. */

DEFINE_PRIMITIVE ("SCAN-BACKWARD-PREFIX-CHARS", Prim_scan_backward_prefix_chars, 4, 4, 0)
{
  bool quoted;
  NORMAL_INITIALIZATION_BACKWARD (4);

  while (true)
    {
      WIN_IF_LEFT_END (start);
      LEFT_QUOTED_P (start, quoted);
      WIN_IF (quoted);
      {
	long sentry = (PEEK_LEFT (start));
	WIN_IF (! (((SYNTAX_ENTRY_CODE (sentry)) == syntaxcode_quote)
		   || (SYNTAX_ENTRY_PREFIX (sentry))));
      }
      MOVE_LEFT (start);
    }
}

DEFINE_PRIMITIVE ("SCAN-FORWARD-PREFIX-CHARS", Prim_scan_forward_prefix_chars, 4, 4, 0)
{
  bool quoted;
  NORMAL_INITIALIZATION_FORWARD (4);

  while (true)
    {
      WIN_IF_RIGHT_END (start);
      RIGHT_QUOTED_P (start, quoted);
      WIN_IF (quoted);
      {
	long sentry = (PEEK_RIGHT (start));
	WIN_IF (! (((SYNTAX_ENTRY_CODE (sentry)) == syntaxcode_quote)
		   || (SYNTAX_ENTRY_PREFIX (sentry))));
      }
      MOVE_RIGHT (start);
    }
}

/* Word Parsers */

DEFINE_PRIMITIVE ("SCAN-FORWARD-TO-WORD", Prim_scan_forward_to_word, 4, 4, 0)
{
  NORMAL_INITIALIZATION_FORWARD (4);

  while (true)
    {
      LOSE_IF_RIGHT_END (start);
      WIN_IF ((SYNTAX_ENTRY_CODE (PEEK_RIGHT (start))) == syntaxcode_word);
      MOVE_RIGHT (start);
    }
}

DEFINE_PRIMITIVE ("SCAN-WORD-FORWARD", Prim_scan_word_forward, 4, 4, 0)
{
  NORMAL_INITIALIZATION_FORWARD (4);

  while (true)
    {
      long sentry;
      LOSE_IF_RIGHT_END (start);
      READ_RIGHT (start, sentry);
      if ((SYNTAX_ENTRY_CODE (sentry)) == syntaxcode_word)
	break;
    }
  while (true)
    {
      WIN_IF_RIGHT_END (start);
      WIN_IF ((SYNTAX_ENTRY_CODE (PEEK_RIGHT (start))) != syntaxcode_word);
      MOVE_RIGHT (start);
    }
}

DEFINE_PRIMITIVE ("SCAN-WORD-BACKWARD", Prim_scan_word_backward, 4, 4, 0)
{
  NORMAL_INITIALIZATION_BACKWARD (4);

  while (true)
    {
      long sentry;
      LOSE_IF_LEFT_END (start);
      READ_LEFT (start, sentry);
      if ((SYNTAX_ENTRY_CODE (sentry)) == syntaxcode_word)
	break;
    }
  while (true)
    {
      WIN_IF_LEFT_END (start);
      WIN_IF ((SYNTAX_ENTRY_CODE (PEEK_LEFT (start))) != syntaxcode_word);
      MOVE_LEFT (start);
    }
}

/* S-Expression Parsers */

DEFINE_PRIMITIVE ("SCAN-LIST-FORWARD", Prim_scan_list_forward, 7, 7, 0)
{
  SCAN_LIST_INITIALIZATION (NORMAL_INITIALIZATION_FORWARD);

  while (true)
    {
      long sentry;
      LOSE_IF_RIGHT_END (start);
      c = (*start);
      READ_RIGHT (start, sentry);

      {
	unsigned int style = 0;
	if (((SYNTAX_ENTRY_CODE (sentry))) == syntaxcode_comment)
	  style = (SYNTAX_ENTRY_COMMENT_STYLE (sentry, COMSTART_FIRST));
	else if (! (RIGHT_END_P (start)))
	  {
	    style
	      = ((SYNTAX_ENTRY_COMMENT_STYLE (sentry, COMSTART_FIRST))
		 & (SYNTAX_ENTRY_COMMENT_STYLE ((PEEK_RIGHT (start)),
						COMSTART_SECOND)));
	    if (style != 0)
	      MOVE_RIGHT (start);
	  }
	if (style != 0)
	  {
	    LOSE_IF_RIGHT_END (start);
	    while (true)
	      {
		READ_RIGHT (start, sentry);
		if ((SYNTAX_ENTRY_COMMENT_BITS (sentry))
		    & COMEND_FIRST
		    & style)
		  {
		    if (((SYNTAX_ENTRY_CODE (sentry)))
			== syntaxcode_endcomment)
		      break;
		    LOSE_IF_RIGHT_END (start);
		    if ((SYNTAX_ENTRY_COMMENT_BITS (PEEK_RIGHT (start)))
			& COMEND_SECOND
			& style)
		      {
			MOVE_RIGHT (start);
			break;
		      }
		  }
	      }
	    continue;
	  }
      }
      if (SYNTAX_ENTRY_PREFIX (sentry))
	continue;

      switch (SYNTAX_ENTRY_CODE (sentry))
	{
	case syntaxcode_escape:
	case syntaxcode_charquote:
	  LOSE_IF_RIGHT_END (start);
	  MOVE_RIGHT (start);

	case syntaxcode_word:
	case syntaxcode_symbol:
	  if ((depth != 0) || (! sexp_flag))
	    break;
	  while (true)
	    {
	      WIN_IF_RIGHT_END (start);
	      switch (SYNTAX_ENTRY_CODE (PEEK_RIGHT (start)))
		{
		case syntaxcode_escape:
		case syntaxcode_charquote:
		  MOVE_RIGHT (start);
		  LOSE_IF_RIGHT_END (start);

		case syntaxcode_word:
		case syntaxcode_symbol:
		  MOVE_RIGHT (start);
		  break;

		default:
		  WIN_IF (true);
		}
	    }

	case syntaxcode_math:
	  if (! sexp_flag)
	    break;
	  if ((! (RIGHT_END_P (start))) && (c == *start))
	    MOVE_RIGHT (start);
	  if (math_exit)
	    {
	      WIN_IF ((--depth) == 0);
	      LOSE_IF (depth < min_depth);
	      math_exit = false;
	    }
	  else
	    {
	      WIN_IF ((++depth) == 0);
	      math_exit = true;
	    }
	  break;

	case syntaxcode_open:
	  WIN_IF ((++depth) == 0);
	  break;

	case syntaxcode_close:
	  WIN_IF ((--depth) == 0);
	  LOSE_IF (depth < min_depth);
	  break;

	case syntaxcode_string:
	  while (true)
	    {
	      LOSE_IF_RIGHT_END (start);
	      if (c == *start)
		break;
	      READ_RIGHT (start, sentry);
	      if (SYNTAX_ENTRY_QUOTE (sentry))
		{
		  LOSE_IF_RIGHT_END (start);
		  MOVE_RIGHT (start);
		}
	    }
	  MOVE_RIGHT (start);
	  WIN_IF ((depth == 0) && sexp_flag);
	  break;

	default:
	  break;
	}
    }
}

DEFINE_PRIMITIVE ("SCAN-LIST-BACKWARD", Prim_scan_list_backward, 7, 7, 0)
{
  bool quoted;
  SCAN_LIST_INITIALIZATION (NORMAL_INITIALIZATION_BACKWARD);

  while (true)
    {
      long sentry;
      LOSE_IF_LEFT_END (start);
      LEFT_QUOTED_P (start, quoted);
      if (quoted)
	{
	  MOVE_LEFT (start);
	  /* existence of this character is guaranteed by LEFT_QUOTED_P. */
	  READ_LEFT (start, sentry);
	  goto word_entry;
	}
      c = (start[-1]);
      READ_LEFT (start, sentry);

      {
	unsigned int style = 0;
	if (((SYNTAX_ENTRY_CODE (sentry))) == syntaxcode_endcomment)
	  {
	    if (ignore_comments)
	      style = (SYNTAX_ENTRY_COMMENT_STYLE (sentry, COMEND_SECOND));
	  }
	else if (! (LEFT_END_P (start)))
	  {
	    LEFT_QUOTED_P (start, quoted);
	    if (!quoted)
	      {
		style
		  = ((SYNTAX_ENTRY_COMMENT_STYLE (sentry, COMEND_SECOND))
		     & (SYNTAX_ENTRY_COMMENT_STYLE ((PEEK_LEFT (start)),
						    COMEND_FIRST)));
		if (style != 0)
		  MOVE_LEFT (start);
	      }
	  }
	if (style != 0)
	  {
	    LOSE_IF_LEFT_END (start);
	    while (true)
	      {
		READ_LEFT (start, sentry);
		if ((((SYNTAX_ENTRY_CODE (sentry))) == syntaxcode_comment)
		    && ((SYNTAX_ENTRY_COMMENT_BITS (sentry))
			& COMSTART_FIRST
			& style))
		  break;
		LOSE_IF_LEFT_END (start);
		if (((SYNTAX_ENTRY_COMMENT_BITS (sentry))
		     & COMSTART_SECOND
		     & style)
		    && ((SYNTAX_ENTRY_COMMENT_BITS (PEEK_LEFT (start)))
			& COMSTART_FIRST
			& style))
		  {
		    MOVE_LEFT (start);
		    break;
		  }
	      }
	    continue;
	  }
      }

      switch (SYNTAX_ENTRY_CODE (sentry))
	{
	case syntaxcode_word:
	case syntaxcode_symbol:
	word_entry:
	  if ((depth != 0) || (! sexp_flag))
	    break;
	  while (true)
	    {
	      WIN_IF_LEFT_END (start);
	      LEFT_QUOTED_P (start, quoted);
	      if (quoted)
		MOVE_LEFT (start);
	      else
		{
		  sentry = (PEEK_LEFT (start));
		  WIN_IF (((SYNTAX_ENTRY_CODE (sentry)) != syntaxcode_word) &&
			  ((SYNTAX_ENTRY_CODE (sentry)) != syntaxcode_symbol));
		}
	      MOVE_LEFT (start);
	    }

	case syntaxcode_math:
	  if (! sexp_flag)
	    break;
	  if ((! (LEFT_END_P (start))) && (c == start[-1]))
	    MOVE_LEFT (start);
	  if (math_exit)
	    {
	      WIN_IF ((--depth) == 0);
	      LOSE_IF (depth < min_depth);
	      math_exit = false;
	    }
	  else
	    {
	      WIN_IF ((++depth) == 0);
	      math_exit = true;
	    }
	  break;

	case syntaxcode_close:
	  WIN_IF ((++depth) == 0);
	  break;

	case syntaxcode_open:
	  WIN_IF ((--depth) == 0);
	  LOSE_IF (depth < min_depth);
	  break;

	case syntaxcode_string:
	  while (true)
	    {
	      LOSE_IF_LEFT_END (start);
	      LEFT_QUOTED_P (start, quoted);
	      if ((! quoted) && (c == start[-1]))
		break;
	      MOVE_LEFT (start);
	    }
	  MOVE_LEFT (start);
	  WIN_IF ((depth == 0) && sexp_flag);
	  break;

	default:
	  break;
	}
    }
}

/* Partial S-Expression Parser */

#define LEVEL_ARRAY_LENGTH 100
struct levelstruct { unsigned char * last, * previous; };

#define DONE_IF(expression) do						\
{									\
  if (expression)							\
    goto done;								\
} while (0)

#define DONE_IF_RIGHT_END(scan) DONE_IF (RIGHT_END_P (scan))

#define SEXP_START() do							\
{									\
  if (stop_before) goto stop;						\
  (level -> last) = start;						\
} while (0)

#define SSF_STATE_LENGTH 9
#define SSF_STATE_DEPTH 0
#define SSF_STATE_IN_STRING_P 1
#define SSF_STATE_COMMENT_STATE 2
#define SSF_STATE_QUOTED_P 3
#define SSF_STATE_START_OF_SEXP 4
#define SSF_STATE_LAST_SEXP 5
#define SSF_STATE_CONTAINING_SEXP 6
#define SSF_STATE_LOCATION 7
#define SSF_STATE_COMMENT_START 8

DEFINE_PRIMITIVE ("SCAN-SEXPS-FORWARD", Prim_scan_sexps_forward, 7, 7, 0)
{
  long target_depth;
  bool stop_before;
  SCHEME_OBJECT state_argument;
  long depth = 0;
  long in_string = -1;		/* -1 or delimiter character */
  /* Values of in_comment:
     0 = not in comment
     1 = in comment
     2 = found first start of comment
     3 = found first end of comment */
  unsigned int in_comment = 0;
  unsigned int comment_style = COMMENT_STYLE_A;
  unsigned char * comment_start = 0;
  bool quoted = false;
  struct levelstruct level_start[LEVEL_ARRAY_LENGTH];
  struct levelstruct *level;
  struct levelstruct *level_end;
  int c = 0;
  long sentry = 0;
  SCHEME_OBJECT result;
  NORMAL_INITIALIZATION_FORWARD (7);

  target_depth = (arg_integer (5));
  stop_before = (BOOLEAN_ARG (6));
  state_argument = (ARG_REF (7));

  level = level_start;
  level_end = (level_start + LEVEL_ARRAY_LENGTH);
  (level -> previous) = NULL;

  /* Initialize the state variables from the state argument. */

  if (state_argument == SHARP_F)
    {
      depth = 0;
      in_string = -1;
      in_comment = 0;
      quoted = false;
    }
  else if ((VECTOR_P (state_argument))
	   && (VECTOR_LENGTH (state_argument)) == SSF_STATE_LENGTH)
    {
      SCHEME_OBJECT temp;

      temp = (VECTOR_REF (state_argument, SSF_STATE_DEPTH));
      if (FIXNUM_P (temp))
	depth = (FIXNUM_TO_LONG (temp));
      else
	error_bad_range_arg (7);

      temp = (VECTOR_REF (state_argument, SSF_STATE_IN_STRING_P));
      if (temp == SHARP_F)
	in_string = -1;
      else if ((UNSIGNED_FIXNUM_P (temp)) &&
	       ((UNSIGNED_FIXNUM_TO_LONG (temp)) < MAX_ASCII))
	in_string = (UNSIGNED_FIXNUM_TO_LONG (temp));
      else
	error_bad_range_arg (7);

      temp = (VECTOR_REF (state_argument, SSF_STATE_COMMENT_STATE));
      if (temp == SHARP_F)
	in_comment = 0;
      else if (temp == (LONG_TO_UNSIGNED_FIXNUM (1)))
	{
	  in_comment = 1;
	  comment_style = COMMENT_STYLE_A;
	}
      else if (temp == (LONG_TO_UNSIGNED_FIXNUM (2)))
	{
	  in_comment = 2;
	  comment_style = COMMENT_STYLE_A;
	}
      else if (temp == (LONG_TO_UNSIGNED_FIXNUM (3)))
	{
	  in_comment = 3;
	  comment_style = COMMENT_STYLE_A;
	}
      else if (temp == (LONG_TO_UNSIGNED_FIXNUM (4)))
	{
	  in_comment = 2;
	  comment_style = (COMMENT_STYLE_A | COMMENT_STYLE_B);
	}
      else if (temp == (LONG_TO_UNSIGNED_FIXNUM (5)))
	{
	  in_comment = 1;
	  comment_style = COMMENT_STYLE_B;
	}
      else if (temp == (LONG_TO_UNSIGNED_FIXNUM (6)))
	{
	  in_comment = 2;
	  comment_style = COMMENT_STYLE_B;
	}
      else if (temp == (LONG_TO_UNSIGNED_FIXNUM (7)))
	{
	  in_comment = 3;
	  comment_style = COMMENT_STYLE_B;
	}
      else
	error_bad_range_arg (7);

      quoted = ((VECTOR_REF (state_argument, SSF_STATE_QUOTED_P)) != SHARP_F);

      if (in_comment != 0)
	{
	  temp = (VECTOR_REF (state_argument, SSF_STATE_COMMENT_START));
	  if (MARK_P (temp))
	    comment_start = (INDEX_TO_SCAN (MARK_INDEX (temp)));
	  else if (UNSIGNED_FIXNUM_P (temp))
	    comment_start = (INDEX_TO_SCAN (UNSIGNED_FIXNUM_TO_LONG (temp)));
	  else
	    error_bad_range_arg (7);
	}
      if ((in_comment != 0) && ((in_string != -1) || (quoted != false)))
	error_bad_range_arg (7);
    }
  else
    error_bad_range_arg (7);

  /* Make sure there is enough room for the result before we start. */

  Primitive_GC_If_Needed (SSF_STATE_LENGTH);

  /* Enter main loop at place appropiate for initial state. */

  switch (in_comment)
    {
    case 1: goto in_comment_1;
    case 2: goto in_comment_2;
    case 3: goto in_comment_3;
    }
  if (quoted)
    {
      quoted = false;
      if (in_string != -1)
	goto start_quoted_in_string;
      else
	goto start_quoted;
    }
  if (in_string != -1)
    goto start_in_string;

  while (true)
    {
      DONE_IF_RIGHT_END (start);
      c = (*start);
      comment_start = start;
      READ_RIGHT (start, sentry);
      comment_style = (SYNTAX_ENTRY_COMMENT_STYLE (sentry, COMSTART_FIRST));
      if (((SYNTAX_ENTRY_CODE (sentry))) == syntaxcode_comment)
	goto in_comment_1;
      if (comment_style == 0)
	goto not_in_comment;

    in_comment_2:
      in_comment = 2;
      DONE_IF_RIGHT_END (start);
      comment_style
	&= (SYNTAX_ENTRY_COMMENT_STYLE ((PEEK_RIGHT (start)),
					COMSTART_SECOND));
      if (comment_style == 0)
	goto not_in_comment;
      MOVE_RIGHT (start);

    in_comment_1:
      while (true)
	{
	  in_comment = 1;
	  DONE_IF_RIGHT_END (start);
	  READ_RIGHT (start, sentry);
	  if ((SYNTAX_ENTRY_COMMENT_BITS (sentry))
	      & COMEND_FIRST
	      & comment_style)
	    {
	      if (((SYNTAX_ENTRY_CODE (sentry))) == syntaxcode_endcomment)
		break;
	    in_comment_3:
	      in_comment = 3;
	      DONE_IF_RIGHT_END (start);
	      if ((SYNTAX_ENTRY_COMMENT_BITS (PEEK_RIGHT (start)))
		  & COMEND_SECOND
		  & comment_style)
		{
		  MOVE_RIGHT (start);
		  break;
		}
	    }
	}
      in_comment = 0;
      continue;

    not_in_comment:
      in_comment = 0;
      if (SYNTAX_ENTRY_PREFIX (sentry))
	continue;

      switch (SYNTAX_ENTRY_CODE (sentry))
	{
	case syntaxcode_escape:
	case syntaxcode_charquote:
	  SEXP_START ();
	start_quoted:
	  if (RIGHT_END_P (start))
	    {
	      quoted = true;
	      DONE_IF (true);
	    }
	  MOVE_RIGHT (start);
	  goto start_atom;

	case syntaxcode_word:
	case syntaxcode_symbol:
	  SEXP_START ();
	start_atom:
	  while (! (RIGHT_END_P (start)))
	    {
	      switch (SYNTAX_ENTRY_CODE (PEEK_RIGHT (start)))
		{
		case syntaxcode_escape:
		case syntaxcode_charquote:
		  MOVE_RIGHT (start);
		  if (RIGHT_END_P (start))
		    {
		      quoted = true;
		      DONE_IF (true);
		    }

		case syntaxcode_word:
		case syntaxcode_symbol:
		  MOVE_RIGHT (start);
		  break;

		default:
		  goto end_atom;
		}
	    }
	end_atom:
	  (level -> previous) = (level -> last);
	  break;

	case syntaxcode_open:
	  SEXP_START ();
	  depth += 1;
	  level += 1;
	  if (level == level_end)
	    error_bad_range_arg (5); /* random error */
	  (level -> last) = NULL;
	  (level -> previous) = NULL;
	  DONE_IF ((--target_depth) == 0);
	  break;

	case syntaxcode_close:
	  depth -= 1;
	  if (level != level_start)
	    level -= 1;
	  (level -> previous) = (level -> last);
	  DONE_IF ((++target_depth) == 0);
	  break;

	case syntaxcode_string:
	  SEXP_START ();
	  in_string = (c);
	start_in_string:
	  while (true)
	    {
	      DONE_IF_RIGHT_END (start);
	      if (in_string == (*start))
		break;
	      READ_RIGHT (start, sentry);
	      if (SYNTAX_ENTRY_QUOTE (sentry))
		{
		start_quoted_in_string:
		  if (RIGHT_END_P (start))
		    {
		      quoted = true;
		      DONE_IF (true);
		    }
		  MOVE_RIGHT (start);
		}
	    }
	  in_string = -1;
	  (level -> previous) = (level -> last);
	  MOVE_RIGHT (start);
	  break;

	default:
	  break;
	}
    }
  /* NOTREACHED */

 stop:
  /* Back up to point at character that starts sexp. */
  if (start == gap_end)
    start = gap_start;
  start -= 1;

 done:
  result = (allocate_marked_vector (TC_VECTOR, SSF_STATE_LENGTH, true));
  VECTOR_SET (result, SSF_STATE_DEPTH, (LONG_TO_FIXNUM (depth)));
  VECTOR_SET
    (result, SSF_STATE_IN_STRING_P,
     ((in_string == -1)
      ? SHARP_F
      : (LONG_TO_UNSIGNED_FIXNUM (in_string))));
  VECTOR_SET
    (result, SSF_STATE_COMMENT_STATE,
     ((in_comment == 0)
      ? SHARP_F
      : (LONG_TO_UNSIGNED_FIXNUM
	 (((in_comment == 2)
	   && (comment_style == (COMMENT_STYLE_A | COMMENT_STYLE_B)))
	  ? 4
	  : (comment_style == COMMENT_STYLE_A)
	  ? in_comment
	  : (in_comment + 4)))));
  VECTOR_SET (result, SSF_STATE_QUOTED_P, (BOOLEAN_TO_OBJECT (quoted)));
  VECTOR_SET
    (result, SSF_STATE_START_OF_SEXP,
     (((level -> last) == NULL)
      ? SHARP_F
      : (LONG_TO_UNSIGNED_FIXNUM ((SCAN_TO_INDEX (level -> last)) - 1))));
  VECTOR_SET
    (result, SSF_STATE_LAST_SEXP,
     (((level -> previous) == NULL)
      ? SHARP_F
      : (LONG_TO_UNSIGNED_FIXNUM ((SCAN_TO_INDEX (level -> previous)) - 1))));
  VECTOR_SET
    (result, SSF_STATE_CONTAINING_SEXP,
     (((level == level_start) || (((level - 1) -> last) == NULL))
      ? SHARP_F
      : (LONG_TO_UNSIGNED_FIXNUM
	 ((SCAN_TO_INDEX ((level - 1) -> last)) - 1))));
  VECTOR_SET
    (result, SSF_STATE_LOCATION,
     (LONG_TO_UNSIGNED_FIXNUM (SCAN_TO_INDEX (start))));
  VECTOR_SET
    (result, SSF_STATE_COMMENT_START,
     ((in_comment == 0)
      ? SHARP_F
      : (LONG_TO_UNSIGNED_FIXNUM (SCAN_TO_INDEX (comment_start)))));
  PRIMITIVE_RETURN (result);
}
