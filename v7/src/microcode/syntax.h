/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/syntax.h,v 1.3 1988/08/15 20:56:14 cph Exp $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* Definitions for Edwin syntax tables. */

/* CODE is the syntax code for the character. */

#define SYNTAX_ENTRY_CODE(entry) ((enum syntaxcode) ((entry) & 0xFF))

/* MATCH is a matching delimiter, if the character is a delimiter type.
   For example, if the character is '(', then MATCH is usually ')'. */

#define SYNTAX_ENTRY_MATCH(entry) (((entry) >> 8) & 0xFF)

/* Bits indicating whether this character is part of a two-character
   comment delimiter sequence. */

#define SYNTAX_ENTRY_COMSTART_FIRST(entry) (((entry) >> 16) & 1)
#define SYNTAX_ENTRY_COMSTART_SECOND(entry) (((entry) >> 17) & 1)
#define SYNTAX_ENTRY_COMEND_FIRST(entry) (((entry) >> 18) & 1)
#define SYNTAX_ENTRY_COMEND_SECOND(entry) (((entry) >> 19) & 1)

enum syntaxcode			/* The possible syntax codes. */
  {
    syntaxcode_whitespace,	/* whitespace char */
    syntaxcode_punct,		/* random punctuation char */
    syntaxcode_word,		/* word constituent */
    syntaxcode_symbol,		/* symbol constituent other than word */
    syntaxcode_open,		/* beginning delimiter */
    syntaxcode_close,		/* ending delimiter */
    syntaxcode_quote,		/* prefix char like Lisp ' */
    syntaxcode_string,		/* string-grouping char like Lisp " */
    syntaxcode_math,		/* delimiters like $ in Tex. */
    syntaxcode_escape,		/* char that begins a C-style escape */
    syntaxcode_charquote,	/* char that quotes the following char */
    syntaxcode_comment,		/* a comment-starting char */
    syntaxcode_endcomment,	/* a comment-ending char */
    syntaxcode_max		/* Upper bound on codes that are meaningful */
  };

#define SYNTAX_ENTRY_QUOTE(entry)					\
  (((SYNTAX_ENTRY_CODE (entry)) == syntaxcode_escape) ||		\
   ((SYNTAX_ENTRY_CODE (entry)) == syntaxcode_charquote))

/* This array, indexed by a character, contains the syntax code which that
   character signifies (as a char).  For example,
   ((enum syntaxcode) syntax_spec_code['w']) is syntaxcode_word. */

extern char syntax_spec_code[0200];

#define SYNTAX_TABLE_P(argument)					\
  (((pointer_type (argument)) == TC_VECTOR) &&				\
   ((Vector_Length (argument)) == 0x100))

#define SYNTAX_TABLE_TYPE Pointer

#define SYNTAX_TABLE_REF(table, index)					\
  (User_Vector_Ref ((table), ((index) & 0xFF)))
