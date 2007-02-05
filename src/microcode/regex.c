/* -*-C-*-

$Id: regex.c,v 1.24 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* Regular expression matching and search. */

/* NOTE: This program was created by translation from the regular
expression code of GNU Emacs; it was translated from the original C to
68000 assembly language (in 1986), and then translated back from 68000
assembly language to C (in 1987).  Users should be aware that the GNU
GENERAL PUBLIC LICENSE may apply to this code.  A copy of that license
should have been included along with this file. */

#include "scheme.h"
#include "syntax.h"
#include "regex.h"

#ifdef STDC_HEADERS
#  include <stdlib.h>
#else
   extern char * malloc ();
   extern char * realloc ();
   extern void free ();
#endif

#if defined(__IRIX__) || defined(_AIX)
#define SIGN_EXTEND_CHAR(x) ((((int) (x)) >= 0x80)			\
			     ? (((int) (x)) - 0x100)			\
			     : ((int) (x)))
#endif

#ifndef SIGN_EXTEND_CHAR
#define SIGN_EXTEND_CHAR(x) (x)
#endif /* not SIGN_EXTEND_CHAR */

#ifndef SWITCH_ENUM
#define SWITCH_ENUM(enum_type, expression)				\
  switch ((enum enum_type) (expression))
#endif /* not SWITCH_ENUM */

#ifndef RE_NFAILURES
#define RE_NFAILURES 512
#endif

#define FOR_INDEX_RANGE(index, start, end)				\
  for (index = (start); (index < (end)); index += 1)

#define FOR_INDEX_BELOW(index, limit)					\
  FOR_INDEX_RANGE (index, 0, (limit))

#define FOR_ALL_ASCII(index)						\
  FOR_INDEX_BELOW (index, MAX_ASCII)

#define FOR_ALL_ASCII_SUCH_THAT(index, expression)			\
  FOR_ALL_ASCII (index)							\
    if (expression)

#define TRANSLATE_CHAR(ascii)						\
  ((translation == NULL) ? (ascii) : (translation [(ascii)]))

#define WORD_CONSTITUENT_P(ascii)					\
  (SYNTAX_CONSTITUENT_P (syntaxcode_word, (ascii)))

#define SYNTAX_CONSTITUENT_P(code, ascii)				\
  ((SYNTAX_ENTRY_CODE (SYNTAX_TABLE_REF (syntax_table, (ascii)))) == (code))

#define CHAR_SET_MEMBER_P(length, char_set, ascii)			\
  (((ascii) < ((length) * ASCII_LENGTH)) &&				\
   (CHAR_SET_MEMBER_P_INTERNAL (char_set, ascii)))

#define CHAR_SET_MEMBER_P_INTERNAL(char_set, ascii)			\
  ((((char_set) [((ascii) / ASCII_LENGTH)]) &				\
    (1 << ((ascii) % ASCII_LENGTH)))					\
   != 0)

#define READ_PATTERN_CHAR(target) do					\
{									\
  if (pattern_pc >= pattern_end)					\
    BAD_PATTERN ();							\
  (target) = (*pattern_pc++);						\
} while (0)

#define READ_PATTERN_OFFSET(target) do					\
{									\
  SIGNED char _fetched;							\
  if ((pattern_pc + 1) >= pattern_end)					\
    BAD_PATTERN ();							\
  (target) = (*pattern_pc++);						\
  _fetched = (* ((SIGNED char *) (pattern_pc++)));			\
  (target) += ((SIGN_EXTEND_CHAR (_fetched)) << ASCII_LENGTH);		\
  if (((pattern_pc + (target)) < pattern_start) ||			\
      ((pattern_pc + (target)) > pattern_end))				\
    BAD_PATTERN ();							\
} while (0)

#define READ_PATTERN_LENGTH(target) do					\
{									\
  int _len;								\
									\
  if (pattern_pc >= pattern_end)					\
    BAD_PATTERN ();							\
  _len = ((int) (*pattern_pc++));					\
  if ((pattern_pc + _len) > pattern_end)				\
    BAD_PATTERN ();							\
  (target) = _len;							\
} while (0)

#define READ_PATTERN_REGISTER(target) do				\
{									\
  if ((pattern_pc >= pattern_end) ||					\
      (((target) = (*pattern_pc++)) >= RE_NREGS))			\
    BAD_PATTERN ();							\
} while (0)

#define READ_PATTERN_SYNTAXCODE(target) do				\
{									\
  if ((pattern_pc >= pattern_end) ||					\
      (((int) ((target) = ((enum syntaxcode) (*pattern_pc++))))		\
       >= ((int) syntaxcode_max)))					\
    BAD_PATTERN ();							\
} while (0)

#define BAD_PATTERN() RE_RETURN (-2)

#define PUSH_FAILURE_POINT(pattern_pc, match_pc) do			\
{									\
  if (stack_pointer == stack_end)					\
    {									\
      long stack_length;						\
      unsigned char **stack_temporary;					\
									\
      stack_length = ((stack_end - stack_start) * 2);			\
      if (stack_length > (re_max_failures * 2))				\
	RE_RETURN (-4);							\
      stack_temporary =							\
	((unsigned char **)						\
	 (realloc							\
	  (stack_start, (stack_length * (sizeof (unsigned char *))))));	\
      if (stack_temporary == NULL)					\
	RE_RETURN (-3);							\
      stack_end = (& (stack_temporary [stack_length]));			\
      stack_pointer =							\
	(& (stack_temporary [(stack_pointer - stack_start)]));		\
      stack_start = stack_temporary;					\
    }									\
  (*stack_pointer++) = (pattern_pc);					\
  (*stack_pointer++) = (match_pc);					\
} while (0)

#define RE_RETURN(value)						\
{									\
  return_value = (value);						\
  goto return_point;							\
}

void
DEFUN (re_buffer_initialize,
       (buffer, translation, syntax_table, text,
	text_start_index, text_end_index,
	gap_start_index, gap_end_index),
       struct re_buffer * buffer
       AND unsigned char * translation
       AND SYNTAX_TABLE_TYPE syntax_table
       AND unsigned char * text
       AND unsigned long text_start_index
       AND unsigned long text_end_index
       AND unsigned long gap_start_index
       AND unsigned long gap_end_index)
{
  unsigned char *text_start, *text_end, *gap_start, *gap_end;

  /* Assumes that
     ((text_start_index <= gap_start_index) &&
      (gap_start_index <= gap_end_index) &&
      (gap_end_index <= text_end_index)) */

  text_start = (text + text_start_index);
  text_end = (text + text_end_index);
  gap_start = (text + gap_start_index);
  gap_end = (text + gap_end_index);

  (buffer -> translation) = translation;
  (buffer -> syntax_table) = syntax_table;
  (buffer -> text) = text;
  (buffer -> text_start) = ((text_start == gap_start) ? gap_end : text_start);
  (buffer -> text_end) = ((text_end == gap_end) ? gap_start : text_end);
  (buffer -> gap_start) = gap_start;
  (buffer -> gap_end) = gap_end;
  return;
}

/* Given a compiled pattern between `pattern_start' and `pattern_end',
   generate a character set which is true of all characters which can
   be the first character of a match.

   See the documentation of `struct re_buffer' for a description of
   `translation' and `syntax_table'.

   `fastmap' is the resulting character set.  It is a character array
   whose elements are either `FASTMAP_FALSE' or `FASTMAP_TRUE'.

   Return values:
   0 => pattern cannot match the null string.
   1 => pattern can match the null string.
   2 => pattern can match the null string, but only at end of match
     text or to left of a character in `fastmap'.
   -2 => the pattern is improperly formed.
   else => undefined. */

#define FASTMAP_FALSE '\0'
#define FASTMAP_TRUE '\1'

int
DEFUN (re_compile_fastmap,
       (pattern_start, pattern_end, translation, syntax_table, fastmap),
       unsigned char * pattern_start
       AND fast unsigned char * pattern_end
       AND unsigned char * translation
       AND SYNTAX_TABLE_TYPE syntax_table
       AND fast unsigned char * fastmap)
{
  fast unsigned char *pattern_pc;
  unsigned char *stack_start[RE_NFAILURES];
  unsigned char **stack_pointer;
  int return_value;

  pattern_pc = pattern_start;
  return_value = 0;
  stack_pointer = stack_start;

  {
    fast int i;

    FOR_ALL_ASCII (i)
      (fastmap [i]) = FASTMAP_FALSE;
  }

 loop:
  if (pattern_pc >= pattern_end)
    RE_RETURN (1);

  SWITCH_ENUM (regexpcode, (*pattern_pc++))
    {
    case regexpcode_unused:
    case regexpcode_line_start:
    case regexpcode_buffer_start:
    case regexpcode_buffer_end:
    case regexpcode_word_start:
    case regexpcode_word_end:
    case regexpcode_word_bound:
    case regexpcode_not_word_bound:
      goto loop;

    case regexpcode_line_end:
      {
	(fastmap [(TRANSLATE_CHAR ('\n'))]) = FASTMAP_TRUE;
	if (return_value == 0)
	  return_value = 2;
	goto next;
      }

    case regexpcode_exact_1:
      {
	fast int ascii;

	READ_PATTERN_CHAR (ascii);
	(fastmap [(TRANSLATE_CHAR (ascii))]) = FASTMAP_TRUE;
	goto next;
      }

    case regexpcode_exact_n:
      {
	fast int length;

	READ_PATTERN_LENGTH (length);
	if (length == 0)
	  goto loop;
	(fastmap [(TRANSLATE_CHAR (pattern_pc [0]))]) = FASTMAP_TRUE;
	goto next;
      }

    case regexpcode_any_char:
      {
	fast int ascii;

	FOR_ALL_ASCII_SUCH_THAT (ascii, (ascii != '\n'))
	  (fastmap [(TRANSLATE_CHAR (ascii))]) = FASTMAP_TRUE;
	if (return_value != 0)
	  goto return_point;
	goto next;
      }

    case regexpcode_char_set:
      {
	fast int length;
	fast int ascii;

	READ_PATTERN_LENGTH (length);
	length = (length * ASCII_LENGTH);
	FOR_INDEX_BELOW (ascii, length)
	  if (CHAR_SET_MEMBER_P_INTERNAL (pattern_pc, ascii))
	    (fastmap [(TRANSLATE_CHAR (ascii))]) = FASTMAP_TRUE;
	goto next;
      }

    case regexpcode_not_char_set:
      {
	fast int length;
	fast int ascii;

	READ_PATTERN_LENGTH (length);
	length = (length * ASCII_LENGTH);
	FOR_INDEX_BELOW (ascii, length)
	  if (! (CHAR_SET_MEMBER_P_INTERNAL (pattern_pc, ascii)))
	    (fastmap [(TRANSLATE_CHAR (ascii))]) = FASTMAP_TRUE;
	FOR_INDEX_RANGE (ascii, length, MAX_ASCII)
	  (fastmap [(TRANSLATE_CHAR (ascii))]) = FASTMAP_TRUE;
	goto next;
      }

    case regexpcode_word_char:
      {
	fast int ascii;

	FOR_ALL_ASCII_SUCH_THAT (ascii, (WORD_CONSTITUENT_P (ascii)))
	  (fastmap [(TRANSLATE_CHAR (ascii))]) = FASTMAP_TRUE;
	goto next;
      }

    case regexpcode_not_word_char:
      {
	fast int ascii;

	FOR_ALL_ASCII_SUCH_THAT (ascii, (! (WORD_CONSTITUENT_P (ascii))))
	  (fastmap [(TRANSLATE_CHAR (ascii))]) = FASTMAP_TRUE;
	goto next;
      }

    case regexpcode_syntax_spec:
      {
	fast enum syntaxcode code;
	fast int ascii;

	READ_PATTERN_SYNTAXCODE (code);
	FOR_ALL_ASCII_SUCH_THAT (ascii, (SYNTAX_CONSTITUENT_P (code, ascii)))
	  (fastmap [(TRANSLATE_CHAR (ascii))]) = FASTMAP_TRUE;
	goto next;
      }

    case regexpcode_not_syntax_spec:
      {
	fast enum syntaxcode code;
	fast int ascii;

	READ_PATTERN_SYNTAXCODE (code);
	FOR_ALL_ASCII_SUCH_THAT (ascii,
				 (! (SYNTAX_CONSTITUENT_P (code, ascii))))
	  (fastmap [(TRANSLATE_CHAR (ascii))]) = FASTMAP_TRUE;
	goto next;
      }

    case regexpcode_start_memory:
    case regexpcode_stop_memory:
      {
	fast int register_number;

	READ_PATTERN_REGISTER (register_number);
	goto loop;
      }

    case regexpcode_duplicate:
      {
	fast int register_number;
	fast int ascii;

	READ_PATTERN_REGISTER (register_number);
	FOR_ALL_ASCII (ascii)
	  (fastmap [ascii]) = FASTMAP_TRUE;
	RE_RETURN (1);
      }

    case regexpcode_jump:
    case regexpcode_finalize_jump:
    case regexpcode_maybe_finalize_jump:
    case regexpcode_dummy_failure_jump:
      {
	fast int offset;

	return_value = 1;
	READ_PATTERN_OFFSET (offset);
	pattern_pc += offset;
	if (offset > 0)
	  goto loop;

	/* Jump backward reached implies we just went through the
	   body of a loop and matched nothing.  Opcode jumped to
	   should be an on_failure_jump.  Just treat it like an
	   ordinary jump.  For a * loop, it has pushed its failure
	   point already; if so, discard that as redundant. */
	if (pattern_pc >= pattern_end)
	  BAD_PATTERN ();
	if (((enum regexpcode) (pattern_pc [0])) !=
	    regexpcode_on_failure_jump)
	  goto loop;
	READ_PATTERN_OFFSET (offset);
	pattern_pc += offset;
	if ((stack_pointer != stack_start) &&
	    ((stack_pointer [-1]) == pattern_pc))
	  stack_pointer -= 1;
	goto loop;
      }

    case regexpcode_on_failure_jump:
      {
	fast int offset;

	READ_PATTERN_OFFSET (offset);
	(*stack_pointer++) = (pattern_pc + offset);
	goto loop;
      }

    default:
      BAD_PATTERN ();
    }

 next:
  if (stack_pointer != stack_start)
    {
      pattern_pc = (*--stack_pointer);
      goto loop;
    }

 return_point:
  return (return_value);
}

/* Match the compiled pattern described by `pattern_start' and
   `pattern_end' against the characters in `buffer' between
   `match_start' and `match_end'.

   `registers', if not NULL, will be filled with the start and end
   indices of the match registers if the match succeeds.

   It is assumed that the following is true:

   (! ((gap_start < gap_end) &&
       (match_start < match_end) &&
       ((match_start == gap_start) || (match_end == gap_end))))

   Return values:

   non-negative => the end index (exclusive) of the match.
   -1 => no match.
   -2 => the pattern is badly formed.
   -3 => memory allocation error.
   -4 => match stack overflow.
   other => undefined. */

#define RE_MATCH_FAILED (-1)

/* This macro is used by search procedures to decide when a match at a
   particular place has failed.  If true, the search continues by
   advancing to the next match point.  */
#define RE_MATCH_FAILURE_RESULT(result)					\
  (((result) == RE_MATCH_FAILED) || ((result) == (-4)))

#define ADDRESS_TO_INDEX(address)					\
  ((((address) > gap_start) ? ((address) - gap_length) : (address))	\
   - (buffer -> text))

#define READ_MATCH_CHAR(target) do					\
{									\
  if (match_pc >= match_end)						\
    goto re_match_fail;							\
  (target) = (TRANSLATE_CHAR (*match_pc++));				\
  if (match_pc == gap_start)						\
    match_pc = gap_end;							\
} while (0)

static Boolean
DEFUN (beq_translate, (scan1, scan2, length, translation),
       unsigned char * scan1 AND
       unsigned char * scan2 AND
       long length AND
       unsigned char * translation)
{
  while ((length--) > 0)
    if ((TRANSLATE_CHAR (*scan1++)) != (TRANSLATE_CHAR (*scan2++)))
      return (false);
  return (true);
}

int re_max_failures = 1000;

int
DEFUN (re_match,
       (pattern_start, pattern_end, buffer, registers, match_start, match_end),
       unsigned char * pattern_start
       AND unsigned char * pattern_end
       AND struct re_buffer * buffer
       AND struct re_registers * registers
       AND unsigned char * match_start
       AND unsigned char * match_end)
{
  fast unsigned char *pattern_pc, *match_pc;
  unsigned char *gap_start, *gap_end;
  unsigned char *translation;
  SYNTAX_TABLE_TYPE syntax_table;
  long gap_length;
  int return_value;

  /* Failure point stack.  Each place that can handle a failure
     further down the line pushes a failure point on this stack.  It
     consists of two char *'s.  The first one pushed is where to
     resume scanning the pattern; the second pushed is where to resume
     scanning the match text.  If the latter is NULL, the failure
     point is a "dummy".  If a failure happens and the innermost
     failure point is dormant, it discards that failure point and
     tries the next one. */

  unsigned char **stack_start, **stack_end, **stack_pointer;

  /* Information on the "contents" of registers.  These are pointers
     into the match text; they record just what was matched (on this
     attempt) by some part of the pattern.  The start_memory command
     stores the start of a register's contents and the stop_memory
     command stores the end.

     At that point, (register_start [regnum]) points to the first
     character in the register, and (register_end [regnum]) points to
     the first character beyond the end of the register. */

  unsigned char *register_start[RE_NREGS];
  unsigned char *register_end[RE_NREGS];

  pattern_pc = pattern_start;
  match_pc = match_start;
  gap_start = (buffer -> gap_start);
  gap_end = (buffer -> gap_end);
  gap_length = (gap_end - gap_start);
  translation = (buffer -> translation);
  syntax_table = (buffer -> syntax_table);

  stack_start =
    ((unsigned char **) (malloc ((2 * RE_NFAILURES) * (sizeof (char *)))));
  if (stack_start == NULL)
    RE_RETURN (-3);

  stack_end = (& (stack_start [(2 * RE_NFAILURES)]));
  stack_pointer = stack_start;

  {
    fast int i;

    FOR_INDEX_BELOW (i, RE_NREGS)
      {
	(register_start [i]) = NULL;
	(register_end [i]) = NULL;
      }
  }

 re_match_loop:
  if (pattern_pc >= pattern_end)
    {
      /* Reaching here indicates that match was successful. */
      if (registers != NULL)
	{
	  fast int i;

	  (register_start [0]) = match_start;
	  (register_end [0]) = match_pc;
	  FOR_INDEX_BELOW (i, RE_NREGS)
	    {
	      ((registers -> start) [i]) =
		(((register_start [i]) == NULL)
		 ? -1
		 : (ADDRESS_TO_INDEX (register_start [i])));
	      ((registers -> end) [i]) =
		(((register_end [i]) == NULL)
		 ? -1
		 : (ADDRESS_TO_INDEX (register_end [i])));
	    }
	}
      RE_RETURN (ADDRESS_TO_INDEX (match_pc));
    }

  SWITCH_ENUM (regexpcode, (*pattern_pc++))
    {
    case regexpcode_unused:
      goto re_match_loop;

    case regexpcode_exact_1:
      {
	fast int ascii;
	fast int ascii_p;

	READ_MATCH_CHAR (ascii);
	READ_PATTERN_CHAR (ascii_p);
	if (ascii == ascii_p)
	  goto re_match_loop;
	goto re_match_fail;
      }

    case regexpcode_exact_n:
      {
	fast int length;
	fast int ascii;

	READ_PATTERN_LENGTH (length);
	while ((length--) > 0)
	  {
	    READ_MATCH_CHAR (ascii);
	    if (ascii != (*pattern_pc++))
	      goto re_match_fail;
	  }
	goto re_match_loop;
      }

    case regexpcode_any_char:
      {
	fast int ascii;

	READ_MATCH_CHAR (ascii);
	if (ascii == '\n')
	  goto re_match_fail;
	goto re_match_loop;
      }

#define RE_MATCH_CHAR_SET(winning_label, losing_label)			\
      {									\
	fast int ascii;							\
	fast int length;						\
									\
	READ_MATCH_CHAR (ascii);					\
	READ_PATTERN_LENGTH (length);					\
	if (CHAR_SET_MEMBER_P (length, pattern_pc, ascii))		\
	  {								\
	    pattern_pc += length;					\
	    goto winning_label;						\
	  }								\
	else								\
	  {								\
	    pattern_pc += length;					\
	    goto losing_label;						\
	  }								\
      }

    case regexpcode_char_set:
      RE_MATCH_CHAR_SET (re_match_loop, re_match_fail);

    case regexpcode_not_char_set:
      RE_MATCH_CHAR_SET (re_match_fail, re_match_loop);

#undef RE_MATCH_CHAR_SET

    /* \( is represented by a start_memory, \) by a stop_memory.  Both
       of those commands contain a "register number" argument.  The
       text matched within the \( and \) is recorded under that
       number.  Then, \<digit> turns into a `duplicate' command which
       is followed by the numeric value of <digit> as the register
       number. */

    case regexpcode_start_memory:
      {
	fast int register_number;

	READ_PATTERN_REGISTER (register_number);
	(register_start [register_number]) = match_pc;
	goto re_match_loop;
      }

    case regexpcode_stop_memory:
      {
	fast int register_number;

	READ_PATTERN_REGISTER (register_number);
	(register_end [register_number]) =
	  ((match_pc == gap_end) ? gap_start : match_pc);
	goto re_match_loop;
      }

    case regexpcode_duplicate:
      {
	fast int register_number;
	unsigned char *start, *end, *new_end;
	long length;

	READ_PATTERN_REGISTER (register_number);
	start = (register_start [register_number]);
	end = (register_end [register_number]);
	length = (end - start);
	if (length <= 0)
	  goto re_match_loop;
	new_end = (match_pc + length);
	if (new_end > match_end)
	  goto re_match_fail;
	if ((match_pc <= gap_start) && (new_end > gap_start))
	  {
	    long length1, length2;

	    new_end += gap_length;
	    if (new_end > match_end)
	      goto re_match_fail;
	    length1 = (gap_start - match_pc);
	    length2 = (length - length1);
	    if (!
		((beq_translate (match_pc, start, length1, translation)) &&
		 (beq_translate (gap_end, (start + length1), length2,
				 translation))))
	      goto re_match_fail;
	  }
	else if ((start <= gap_start) && (end > gap_start))
	  {
	    long length1, length2;

	    length1 = (gap_start - start);
	    length2 = (end - gap_end);
	    if (!
		((beq_translate (match_pc, start, length1, translation)) &&
		 (beq_translate ((match_pc + length1), gap_end, length2,
				 translation))))
	      goto re_match_fail;
	  }
	else
	  {
	    if (! (beq_translate (match_pc, start, length, translation)))
	      goto re_match_fail;
	  }
	match_pc = ((new_end == gap_start) ? gap_end : new_end);
	goto re_match_loop;
      }

    case regexpcode_buffer_start:
      {
	if ((ADDRESS_TO_INDEX (match_pc))
	    == (ADDRESS_TO_INDEX (buffer -> text_start)))
	  goto re_match_loop;
	goto re_match_fail;
      }

    case regexpcode_buffer_end:
      {
	if ((ADDRESS_TO_INDEX (match_pc))
	    == (ADDRESS_TO_INDEX (buffer -> text_end)))
	  goto re_match_loop;
	goto re_match_fail;
      }

    case regexpcode_line_start:
      {
	if ((ADDRESS_TO_INDEX (match_pc))
	    == (ADDRESS_TO_INDEX (buffer -> text_start)))
	  goto re_match_loop;
	if ((TRANSLATE_CHAR
	     (((match_pc == gap_end) ? gap_start : match_pc) [-1]))
	    == '\n')
	  goto re_match_loop;
	goto re_match_fail;
      }

    case regexpcode_line_end:
      {
	if (((ADDRESS_TO_INDEX (match_pc))
	     == (ADDRESS_TO_INDEX (buffer -> text_end)))
	    || ((TRANSLATE_CHAR (match_pc [0])) == '\n'))
	  goto re_match_loop;
	goto re_match_fail;
      }

#define RE_MATCH_WORD_BOUND(word_bound_p)				\
  if ((match_pc == gap_end)						\
      ? (word_bound_p							\
	 (((gap_start != (buffer -> text_start))			\
	   && (WORD_CONSTITUENT_P (TRANSLATE_CHAR (gap_start[-1])))),	\
	  ((gap_end != (buffer -> text_end))				\
	   && (WORD_CONSTITUENT_P (TRANSLATE_CHAR (gap_end[0]))))))	\
      : (word_bound_p							\
	 (((match_pc != (buffer -> text_start))				\
	   && (WORD_CONSTITUENT_P (TRANSLATE_CHAR (match_pc[-1])))),	\
	  ((match_pc != (buffer -> text_end))				\
	   && (WORD_CONSTITUENT_P (TRANSLATE_CHAR (match_pc[0])))))))	\
      goto re_match_loop;						\
    goto re_match_fail

    case regexpcode_word_bound:
#define WORD_BOUND_P(left_p, right_p) ((left_p) != (right_p))
      RE_MATCH_WORD_BOUND (WORD_BOUND_P);
#undef WORD_BOUND_P

    case regexpcode_not_word_bound:
#define NOT_WORD_BOUND_P(left_p, right_p) ((left_p) == (right_p))
      RE_MATCH_WORD_BOUND (NOT_WORD_BOUND_P);
#undef NOT_WORD_BOUND_P

    case regexpcode_word_start:
#define WORD_START_P(left_p, right_p) ((! (left_p)) && (right_p))
      RE_MATCH_WORD_BOUND (WORD_START_P);
#undef WORD_START_P

    case regexpcode_word_end:
#define WORD_END_P(left_p, right_p) ((left_p) && (! (right_p)))
      RE_MATCH_WORD_BOUND (WORD_END_P);
#undef WORD_END_P

#undef RE_MATCH_WORD_BOUND

    case regexpcode_syntax_spec:
      {
	fast int ascii;
	fast enum syntaxcode code;

	READ_MATCH_CHAR (ascii);
	READ_PATTERN_SYNTAXCODE (code);
	if (SYNTAX_CONSTITUENT_P (code, ascii))
	  goto re_match_loop;
	goto re_match_fail;
      }

    case regexpcode_not_syntax_spec:
      {
	fast int ascii;
	fast enum syntaxcode code;

	READ_MATCH_CHAR (ascii);
	READ_PATTERN_SYNTAXCODE (code);
	if (! (SYNTAX_CONSTITUENT_P (code, ascii)))
	  goto re_match_loop;
	goto re_match_fail;
      }

    case regexpcode_word_char:
      {
	fast int ascii;

	READ_MATCH_CHAR (ascii);
	if (WORD_CONSTITUENT_P (ascii))
	  goto re_match_loop;
	goto re_match_fail;
      }

    case regexpcode_not_word_char:
      {
	fast int ascii;

	READ_MATCH_CHAR (ascii);
	if (! (WORD_CONSTITUENT_P (ascii)))
	  goto re_match_loop;
	goto re_match_fail;
      }

    /* "or" constructs ("|") are handled by starting each alternative
       with an on_failure_jump that points to the start of the next
       alternative.  Each alternative except the last ends with a jump
       to the joining point.  (Actually, each jump except for the last
       one really jumps to the following jump, because tensioning the
       jumps is a hassle.)

       The start of a stupid repeat has an on_failure_jump that points
       past the end of the repeat text.  This makes a failure point so
       that, on failure to match a repetition, matching restarts past
       as many repetitions have been found with no way to fail and
       look for another one.

       A smart repeat is similar but loops back to the on_failure_jump
       so that each repetition makes another failure point. */

    case regexpcode_on_failure_jump:
      {
	fast long offset;

	READ_PATTERN_OFFSET (offset);
	PUSH_FAILURE_POINT ((pattern_pc + offset), match_pc);
	goto re_match_loop;
      }

    /* The end of a smart repeat has a maybe_finalize_jump back.
       Change it either to a finalize_jump or an ordinary jump. */

    case regexpcode_maybe_finalize_jump:
      {
	fast long offset;
	fast long ascii;

	READ_PATTERN_OFFSET (offset);
	if (pattern_pc == pattern_end)
	  goto finalize_jump;

	/* Compare what follows with the beginning of the repeat.
	   If we can establish that there is nothing that they
	   would both match, we can change to `finalize_jump'. */

	SWITCH_ENUM (regexpcode, (pattern_pc [0]))
	  {
	  case regexpcode_exact_1:
	    ascii = (pattern_pc [1]);
	    break;

	  case regexpcode_exact_n:
	    ascii = (pattern_pc [2]);
	    break;

	  case regexpcode_line_end:
	    ascii = ('\n');
	    break;

	  default:
	    goto dont_finalize_jump;
	  }

	/* (pattern_pc [(offset - 3)]) is an `on_failure_jump'.
	   Examine what follows that. */
	SWITCH_ENUM (regexpcode, (pattern_pc [offset]))
	  {
	  case regexpcode_exact_1:
	    {
	      if (ascii != (pattern_pc [(offset + 1)]))
		goto finalize_jump;
	      goto dont_finalize_jump;
	    }

	  case regexpcode_exact_n:
	    {
	      if (ascii != (pattern_pc [(offset + 2)]))
		goto finalize_jump;
	      goto dont_finalize_jump;
	    }

	  case regexpcode_char_set:
	    {
	      if (CHAR_SET_MEMBER_P ((pattern_pc [(offset + 1)]),
				     (& (pattern_pc [(offset + 2)])),
				     ascii))
		goto dont_finalize_jump;
	      goto finalize_jump;
	    }

	  case regexpcode_not_char_set:
	    {
	      if (CHAR_SET_MEMBER_P ((pattern_pc [(offset + 1)]),
				     (& (pattern_pc [(offset + 2)])),
				     ascii))
		goto finalize_jump;
	      goto dont_finalize_jump;
	    }

	  default:
	    goto dont_finalize_jump;
	  }

      finalize_jump:
	pattern_pc -= 2;
	(pattern_pc [-1]) = ((unsigned char) regexpcode_finalize_jump);
	goto re_match_finalize_jump;

      dont_finalize_jump:
	pattern_pc -= 2;
	(pattern_pc [-1]) = ((unsigned char) regexpcode_jump);
	goto re_match_jump;
      }

    case regexpcode_finalize_jump:
    re_match_finalize_jump:
      {
	stack_pointer -= 2;
	goto re_match_jump;
      }

    case regexpcode_jump:
    re_match_jump:
      {
	fast long offset;

	READ_PATTERN_OFFSET (offset);
	pattern_pc += offset;
	goto re_match_loop;
      }

    case regexpcode_dummy_failure_jump:
      {
	PUSH_FAILURE_POINT (NULL, NULL);
	goto re_match_jump;
      }

    default:
      {
	BAD_PATTERN ();
      }
    }

 re_match_fail:
  if (stack_pointer == stack_start)
    RE_RETURN (RE_MATCH_FAILED);
  match_pc = (*--stack_pointer);
  pattern_pc = (*--stack_pointer);
  if (pattern_pc != NULL)
    goto re_match_loop;
  goto re_match_fail;

 return_point:
  if (stack_start != NULL)
    free (stack_start);
  return (return_value);
}

#define DEFINE_RE_SEARCH(name)						\
int									\
DEFUN (name,								\
       (pattern_start, pattern_end, buffer, registers,			\
	match_start, match_end),					\
       unsigned char * pattern_start					\
       AND unsigned char * pattern_end					\
       AND struct re_buffer * buffer					\
       AND struct re_registers * registers				\
       AND unsigned char * match_start					\
       AND unsigned char * match_end)

#define INITIALIZE_RE_SEARCH(pc, limit, gap_limit)			\
  int can_be_null;							\
  unsigned char *translation;						\
  int match_result;							\
									\
  fast unsigned char *match_pc;						\
  fast unsigned char *match_limit;					\
  fast unsigned char *gap_limit;					\
  fast unsigned char *fastmap;						\
  unsigned char fastmap_array[MAX_ASCII];				\
									\
  fastmap = &fastmap_array[0];						\
  translation = (buffer -> translation);				\
  can_be_null =								\
    (re_compile_fastmap							\
     (pattern_start, pattern_end, translation,				\
      (buffer -> syntax_table), fastmap));				\
  if (can_be_null < 0)							\
    return (can_be_null);						\
									\
  match_pc = (pc);							\
  match_limit = (limit);						\
  gap_limit = (buffer -> gap_limit)

#define RE_SEARCH_TEST(start)						\
  (re_match								\
   (pattern_start, pattern_end, buffer, registers, (start), match_end))

#define RE_SEARCH_FORWARD_FAST(limit) do				\
{									\
  while (true)								\
    {									\
      if (match_pc >= (limit))						\
	break;								\
									\
      if ((fastmap [(TRANSLATE_CHAR (*match_pc++))]) == FASTMAP_FALSE)	\
	continue;							\
									\
      match_result = (RE_SEARCH_TEST (match_pc - 1));			\
      if (RE_MATCH_FAILURE_RESULT (match_result))			\
	continue;							\
									\
      return (match_result);						\
    }									\
} while (0)

DEFINE_RE_SEARCH (re_search_forward)
{
  INITIALIZE_RE_SEARCH (match_start, match_end, gap_start);

  if (can_be_null != 1)
    {
      if ((match_pc < gap_start) && (gap_start < match_limit))
	RE_SEARCH_FORWARD_FAST (gap_start);
      if (match_pc == gap_start)
	match_pc = (buffer -> gap_end);
      RE_SEARCH_FORWARD_FAST (match_limit);
      return
	((can_be_null == 0)
	 ? RE_MATCH_FAILED
	 : (RE_SEARCH_TEST (match_limit)));
    }
  else
    {
      while (true)
	{
	  match_result = (RE_SEARCH_TEST (match_pc));
	  if (! (RE_MATCH_FAILURE_RESULT (match_result)))
	    return (match_result);
	  match_pc += 1;
	  if (match_pc == gap_start)
	    match_pc = (buffer -> gap_end);
	  if (match_pc > match_limit)
	    return (match_result);
	}
    }
}

#define RE_SEARCH_BACKWARD_FAST(limit) do				\
{									\
  while (true)								\
    {									\
      if (match_pc <= (limit))						\
	break;								\
									\
      if ((fastmap [(TRANSLATE_CHAR (*--match_pc))]) == FASTMAP_FALSE)	\
	continue;							\
									\
      match_result = (RE_SEARCH_TEST (match_pc));			\
      if (RE_MATCH_FAILURE_RESULT (match_result))			\
	continue;							\
									\
      RE_SEARCH_BACKWARD_RETURN (match_pc);				\
    }									\
} while (0)

#define RE_SEARCH_BACKWARD_RETURN(start)				\
  return								\
    ((match_result < 0)							\
     ? match_result							\
     : ((((start) > (buffer -> gap_start))				\
	 ? ((start) - (gap_end - (buffer -> gap_start)))		\
	 : (start))							\
	- (buffer -> text)))

DEFINE_RE_SEARCH (re_search_backward)
{
  INITIALIZE_RE_SEARCH (match_end, match_start, gap_end);

  if (can_be_null != 1)
    {
      if ((match_pc > gap_end) && (gap_end > match_limit))
	RE_SEARCH_BACKWARD_FAST (gap_end);
      if (match_pc == gap_end)
	match_pc = (buffer -> gap_start);
      RE_SEARCH_BACKWARD_FAST (match_limit);
      if (can_be_null == 0)
	return (RE_MATCH_FAILED);
      match_result = (RE_SEARCH_TEST (match_limit));
      RE_SEARCH_BACKWARD_RETURN (match_limit);
    }
  else
    {
      while (true)
	{
	  match_result = (RE_SEARCH_TEST (match_pc));
	  if (! (RE_MATCH_FAILURE_RESULT (match_result)))
	    RE_SEARCH_BACKWARD_RETURN (match_pc);
	  if (match_pc == gap_end)
	    match_pc = (buffer -> gap_start);
	  match_pc -= 1;
	  if (match_pc < match_limit)
	    RE_SEARCH_BACKWARD_RETURN (match_pc);
	}
    }
}
