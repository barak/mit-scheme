/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/syntax.c,v 1.8 1987/12/01 16:34:04 jrm Rel $

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

/* Primitives to support Edwin syntax tables, word and list parsing.
   Translated from GNU Emacs. */

#include "scheme.h"
#include "primitive.h"
#include "char.h"
#include "string.h"
#include "edwin.h"
#include "syntax.h"

/* Syntax Codes */

/* Convert a letter which signifies a syntax code
   into the code it signifies. */

#define ILLEGAL ((char) syntaxcode_max)

char syntax_spec_code[0200] =
  {
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,

    ((char) syntaxcode_whitespace), ILLEGAL, ((char) syntaxcode_string),
        ILLEGAL, ((char) syntaxcode_math), ILLEGAL, ILLEGAL,
        ((char) syntaxcode_quote),
    ((char) syntaxcode_open), ((char) syntaxcode_close), ILLEGAL, ILLEGAL,
        ILLEGAL, ((char) syntaxcode_whitespace), ((char) syntaxcode_punct),
        ((char) syntaxcode_charquote),
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ((char) syntaxcode_comment),
        ILLEGAL, ((char) syntaxcode_endcomment), ILLEGAL,

    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
        ((char) syntaxcode_word),
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ((char) syntaxcode_escape), ILLEGAL,
        ILLEGAL, ((char) syntaxcode_symbol),

    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL,
        ((char) syntaxcode_word),
    ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL, ILLEGAL
  };

/* Indexed by syntax code, give the letter that describes it. */

char syntax_code_spec[13] =
  {
    ' ', '.', 'w', '_', '(', ')', '\'', '\"', '$', '\\', '/', '<', '>'
  };

DEFINE_PRIMITIVE ("STRING->SYNTAX-ENTRY", Prim_String_To_Syntax_Entry, 1)
{
  long length, c, result;
  char *scan;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  length = (string_length (ARG_REF (1)));
  if (length > 6) error_bad_range_arg (1);
  scan = (string_pointer ((ARG_REF (1)), 0));

  if ((length--) > 0)
    {
      c = (char_to_long (*scan++));
      if (c >= 0200) error_bad_range_arg (1);
      result = (char_to_long (syntax_spec_code[c]));
      if (result == ILLEGAL) error_bad_range_arg (1);
    }
  else
    result = ((long) syntaxcode_whitespace);

  if ((length--) > 0)
    {
      c = (char_to_long (*scan++));
      if (c != ' ') result |= (c << 8);
    }

  while ((length--) > 0)
    switch (*scan++)
      {
      case '1': result |= (1 << 16); break;
      case '2': result |= (1 << 17); break;
      case '3': result |= (1 << 18); break;
      case '4': result |= (1 << 19); break;
      case ' ': break;
      default: error_bad_range_arg (1);
      }

  PRIMITIVE_RETURN (Make_Unsigned_Fixnum (result));
}

DEFINE_PRIMITIVE ("CHAR->SYNTAX-CODE", Prim_Char_To_Syntax_Code, 2)
{
  Primitive_2_Args ();

  CHECK_ARG (1, SYNTAX_TABLE_P);
  PRIMITIVE_RETURN
    (c_char_to_scheme_char
     ((char)
      (SYNTAX_ENTRY_CODE
       (SYNTAX_TABLE_REF ((ARG_REF (1)), (arg_ascii_char (2)))))));
}

/* Parser Initialization */

#define NORMAL_INITIALIZATION_COMMON(arity)				\
  fast Pointer syntax_table;						\
  fast Pointer group;							\
  fast char *start;							\
  char *first_char, *end;						\
  long sentry;								\
  long gap_length;							\
  PRIMITIVE_HEADER (arity);						\
									\
  CHECK_ARG (1, SYNTAX_TABLE_P);					\
  syntax_table = (ARG_REF (1));						\
  CHECK_ARG (2, GROUP_P);						\
  group = (ARG_REF (2));						\
  first_char = (string_pointer ((GROUP_TEXT (group)), 0));		\
  start = (first_char + (arg_nonnegative_integer (3)));			\
  end = (first_char + (arg_nonnegative_integer (4)));			\
  gap_start = (first_char + (GROUP_GAP_START (group)));			\
  gap_length = (GROUP_GAP_LENGTH (group));				\
  gap_end = (first_char + (GROUP_GAP_END (group)))

#define NORMAL_INITIALIZATION_FORWARD(arity)				\
  char *gap_start;							\
  fast char *gap_end;							\
  NORMAL_INITIALIZATION_COMMON (arity);					\
  if (start >= gap_start)						\
    start += gap_length;						\
  if (end >= gap_start)							\
    end += gap_length

#define NORMAL_INITIALIZATION_BACKWARD(arity)				\
  fast char *gap_start;							\
  char *gap_end;							\
  Boolean quoted;							\
  NORMAL_INITIALIZATION_COMMON (arity);					\
  if (start > gap_start)						\
    start += gap_length;						\
  if (end > gap_start)							\
    end += gap_length

#define SCAN_LIST_INITIALIZATION(initialization)			\
  long depth, min_depth;						\
  Boolean sexp_flag, ignore_comments, math_exit;			\
  char c;								\
  initialization (7);							\
  CHECK_ARG (5, FIXNUM_P);						\
  FIXNUM_VALUE ((ARG_REF (5)), depth);					\
  min_depth = ((depth >= 0) ? 0 : depth);				\
  sexp_flag = ((ARG_REF (6)) != NIL);					\
  ignore_comments = ((ARG_REF (7)) != NIL);				\
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
    PRIMITIVE_RETURN (NIL);						\
} while (0)

#define LOSE_IF_RIGHT_END(scan) LOSE_IF (RIGHT_END_P (scan))
#define LOSE_IF_LEFT_END(scan) LOSE_IF (LEFT_END_P (scan))

#define SCAN_TO_INDEX(scan)						\
  ((((scan) > gap_start) ? ((scan) - gap_length) : (scan)) - first_char)

#define WIN_IF(expression) do						\
{									\
  if (expression)							\
    PRIMITIVE_RETURN (Make_Unsigned_Fixnum (SCAN_TO_INDEX (start)));	\
} while (0)

#define WIN_IF_RIGHT_END(scan) WIN_IF (RIGHT_END_P (scan))
#define WIN_IF_LEFT_END(scan) WIN_IF (LEFT_END_P (scan))

#define RIGHT_QUOTED_P_INTERNAL(scan, quoted) do			\
{									\
  long sentry;								\
									\
  quoted = false;							\
  while (true)								\
    {									\
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
  char *scan;								\
									\
  scan = (scan_init);							\
  RIGHT_QUOTED_P_INTERNAL (scan, quoted);				\
} while (0)

#define LEFT_QUOTED_P(scan_init, quoted) do				\
{									\
  char *scan;								\
									\
  scan = (scan_init);							\
  MOVE_LEFT (scan);							\
  RIGHT_QUOTED_P_INTERNAL (scan, quoted);				\
} while (0)

/* Quote Parsers */

DEFINE_PRIMITIVE ("QUOTED-CHAR?", Prim_Quoted_Char_P, 4)
{
  NORMAL_INITIALIZATION_BACKWARD (4);

  RIGHT_QUOTED_P (start, quoted);
  PRIMITIVE_RETURN (quoted ? TRUTH : NIL);
}

/* This is used in conjunction with `scan-list-backward' to find the
   beginning of an s-expression. */

DEFINE_PRIMITIVE ("SCAN-BACKWARD-PREFIX-CHARS", Prim_Scan_Backward_Prefix_Chars, 4)
{
  NORMAL_INITIALIZATION_BACKWARD (4);

  while (true)
    {
      WIN_IF_LEFT_END (start);
      LEFT_QUOTED_P (start, quoted);
      WIN_IF (quoted ||
	      ((SYNTAX_ENTRY_CODE (PEEK_LEFT (start))) != syntaxcode_quote));
      MOVE_LEFT (start);
    }
}

/* Word Parsers */

DEFINE_PRIMITIVE ("SCAN-FORWARD-TO-WORD", Prim_Scan_Forward_To_Word, 4)
{
  NORMAL_INITIALIZATION_FORWARD (4);

  while (true)
    {
      LOSE_IF_RIGHT_END (start);
      WIN_IF ((SYNTAX_ENTRY_CODE (PEEK_RIGHT (start))) == syntaxcode_word);
      MOVE_RIGHT (start);
    }
}

DEFINE_PRIMITIVE ("SCAN-WORD-FORWARD", Prim_Scan_Word_Forward, 4)
{
  NORMAL_INITIALIZATION_FORWARD (4);

  while (true)
    {
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

DEFINE_PRIMITIVE ("SCAN-WORD-BACKWARD", Prim_Scan_Word_Backward, 4)
{
  NORMAL_INITIALIZATION_BACKWARD (4);

  while (true)
    {
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

DEFINE_PRIMITIVE ("SCAN-LIST-FORWARD", Prim_Scan_List_Forward, 7)
{
  SCAN_LIST_INITIALIZATION (NORMAL_INITIALIZATION_FORWARD);

  while (true)
    {
      LOSE_IF_RIGHT_END (start);
      c = (*start);
      READ_RIGHT(start, sentry);

      if ((! (RIGHT_END_P (start))) &&
	  (SYNTAX_ENTRY_COMSTART_FIRST (sentry)) &&
	  (SYNTAX_ENTRY_COMSTART_SECOND (PEEK_RIGHT (start))))
	{
	  MOVE_RIGHT (start);
	  LOSE_IF_RIGHT_END (start);
	  while (true)
	    {
	      READ_RIGHT (start, sentry);
	      LOSE_IF_RIGHT_END (start);
	      if ((SYNTAX_ENTRY_COMEND_FIRST (sentry)) &&
		  (SYNTAX_ENTRY_COMEND_SECOND (PEEK_RIGHT (start))))
		{
		  MOVE_RIGHT (start);
		  break;
		}
	    }
	  break;
	}

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

	case syntaxcode_comment:
	  if (! ignore_comments)
	    break;
	  while (true)
	    {
	      LOSE_IF_RIGHT_END (start);
	      if ((SYNTAX_ENTRY_CODE (PEEK_RIGHT (start))) ==
		  syntaxcode_endcomment)
		break;
	      MOVE_RIGHT (start);
	    }
	  break;

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
	  WIN_IF ((depth == 0) || sexp_flag);
	  break;

	default:
	  break;
	}
    }
}

DEFINE_PRIMITIVE ("SCAN-LIST-BACKWARD", Prim_Scan_List_Backward, 7)
{
  SCAN_LIST_INITIALIZATION (NORMAL_INITIALIZATION_BACKWARD);

  while (true)
    {
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
      if ((! (LEFT_END_P (start))) &&
	  (SYNTAX_ENTRY_COMEND_SECOND (sentry)) &&
	  (SYNTAX_ENTRY_COMEND_FIRST (PEEK_LEFT (start))))
	{
	  LEFT_QUOTED_P (start, quoted);
	  if (! quoted)
	    {
	      MOVE_LEFT (start);
	      LOSE_IF_LEFT_END (start);
	      while (true)
		{
		  READ_LEFT (start, sentry);
		  LOSE_IF_LEFT_END (start);
		  if ((SYNTAX_ENTRY_COMSTART_SECOND (sentry)) &&
		      (SYNTAX_ENTRY_COMSTART_SECOND (PEEK_LEFT (start))))
		    {
		      MOVE_LEFT (start);
		      break;
		    }
		}
	      break;
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

	case syntaxcode_endcomment:
	  if (! ignore_comments)
	    break;
	  while (true)
	    {
	      LOSE_IF_LEFT_END (start);
	      if ((SYNTAX_ENTRY_CODE (PEEK_LEFT (start))) ==
		  syntaxcode_comment)
		break;
	      MOVE_LEFT (start);
	    }
	  break;

	default:
	  break;
	}
    }
}

/* Partial S-Expression Parser */

#define LEVEL_ARRAY_LENGTH 100
struct levelstruct { char *last, *previous; };

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

DEFINE_PRIMITIVE ("SCAN-SEXPS-FORWARD", Prim_Scan_Sexps_Forward, 7)
{
  long target_depth;
  Boolean stop_before;
  Pointer state_argument;
  long depth;
  long in_string;		/* -1 or delimiter character */
  long in_comment;		/* 0, 1, or 2 */
  Boolean quoted;
  struct levelstruct level_start[LEVEL_ARRAY_LENGTH];
  struct levelstruct *level;
  struct levelstruct *level_end;
  char c;
  Pointer result;
  NORMAL_INITIALIZATION_FORWARD (7);

  CHECK_ARG (5, FIXNUM_P);
  FIXNUM_VALUE ((ARG_REF (5)), target_depth);
  stop_before = ((ARG_REF (6)) != NIL);
  state_argument = (ARG_REF (7));

  level = level_start;
  level_end = (level_start + LEVEL_ARRAY_LENGTH);
  (level -> previous) = NULL;

  /* Initialize the state variables from the state argument. */

  if (state_argument == NIL)
    {
      depth = 0;
      in_string = -1;
      in_comment = 0;
      quoted = false;
    }
  else if (((pointer_type (state_argument)) == TC_VECTOR) &&
	   (Vector_Length (state_argument)) == 7)
    {
      Pointer temp;

      temp = (User_Vector_Ref (state_argument, 0));
      if (FIXNUM_P (temp))
	{
	  Sign_Extend (temp, depth);
	}
      else
	error_bad_range_arg (7);

      temp = (User_Vector_Ref (state_argument, 1));
      if (temp == NIL)
	in_string = -1;
      else if ((FIXNUM_P (temp)) && ((pointer_datum (temp)) < MAX_ASCII))
	in_string = (pointer_datum (temp));
      else
	error_bad_range_arg (7);

      temp = (User_Vector_Ref (state_argument, 2));
      if (temp == NIL)
	in_comment = 0;
      else if (temp == (Make_Unsigned_Fixnum (1)))
	in_comment = 1;
      else if (temp == (Make_Unsigned_Fixnum (2)))
	in_comment = 2;
      else
	error_bad_range_arg (7);

      quoted = ((User_Vector_Ref (state_argument, 3)) != NIL);

      if ((in_comment != 0) && ((in_string != -1) || (quoted != false)))
	error_bad_range_arg (7);

    }
  else
    error_bad_range_arg (7);

  /* Make sure there is enough room for the result before we start. */

  Primitive_GC_If_Needed (8);

  /* Enter main loop at place appropiate for initial state. */

  if (in_comment == 1)
    goto start_in_comment;
  if (in_comment == 2)
    goto start_in_comment2;
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
      READ_RIGHT (start, sentry);
      if ((! (RIGHT_END_P (start))) &&
	  (SYNTAX_ENTRY_COMSTART_FIRST (sentry)) &&
	  (SYNTAX_ENTRY_COMSTART_FIRST (PEEK_RIGHT (start))))
	{
	  MOVE_RIGHT (start);
	  in_comment = 2;
	start_in_comment2:
	  while (true)
	    {
	      DONE_IF_RIGHT_END (start);
	      READ_RIGHT (start, sentry);
	      if (SYNTAX_ENTRY_COMEND_FIRST (sentry))
		{
		  /* Actually, terminating here is a special case.  There
		     should be a third value of in_comment to handle it. */
		  DONE_IF_RIGHT_END (start);
		  if (SYNTAX_ENTRY_COMEND_SECOND (PEEK_RIGHT (start)))
		    {
		      MOVE_RIGHT (start);
		      break;
		    }
		}
	    }
	  in_comment = 0;
	}
      else

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

	  case syntaxcode_comment:
	    in_comment = 1;
	  start_in_comment:
	    while (true)
	      {
		DONE_IF_RIGHT_END (start);
		READ_RIGHT (start, sentry);
		if ((SYNTAX_ENTRY_CODE (sentry)) == syntaxcode_endcomment)
		  break;
	      }
	    in_comment = 0;
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
	    in_string = (char_to_long (c));
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
		  }
	      }
	    in_string = -1;
	    (level -> previous) = (level -> last);
	    MOVE_RIGHT (start);
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
  result = (allocate_marked_vector (TC_VECTOR, 7, true));
  (User_Vector_Set(result, 0, (Make_Signed_Fixnum (depth))));
  (User_Vector_Set(result, 1, ((in_string == -1)
			       ? NIL
			       : (Make_Unsigned_Fixnum (in_string)))));
  (User_Vector_Set(result, 2, ((in_comment == 0)
			       ? NIL
			       : (Make_Unsigned_Fixnum (in_comment)))));
  (User_Vector_Set(result, 3, ((quoted == false) ? NIL : TRUTH)));
  (User_Vector_Set(result, 4, (((level -> previous) == NULL)
			       ? NIL
			       : (Make_Unsigned_Fixnum 
				  ((SCAN_TO_INDEX (level -> previous))
				   - 1)))));
  (User_Vector_Set(result, 5, (((level == level_start)
				|| ((level -> previous) == NULL))
			       ? NIL
			       : (Make_Unsigned_Fixnum 
				  ((SCAN_TO_INDEX ((level - 1) -> last))
				   - 1)))));
  (User_Vector_Set(result, 6, (Make_Unsigned_Fixnum (SCAN_TO_INDEX (start)))));
  PRIMITIVE_RETURN (result);
}
