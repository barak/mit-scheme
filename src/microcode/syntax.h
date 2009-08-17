/* -*-C-*-

$Id: 64afc7dc71daf426a51b0d53fb7d8b40cbf95b21 $

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

/* Definitions for Edwin syntax tables. */

/* NOTE: This program was created by translation from the syntax table
   code of GNU Emacs; it was translated from the original C to 68000
   assembly language (in 1986), and then translated back from 68000
   assembly language to C (in 1987).  */

/* CODE is the syntax code for the character. */
#define SYNTAX_ENTRY_CODE(entry) ((enum syntaxcode) ((entry) & 0xF))

/* MATCH is a matching delimiter, if the character is a delimiter type.
   For example, if the character is '(', then MATCH is usually ')'. */
#define SYNTAX_ENTRY_MATCH(entry) (((entry) >> 4) & 0xFF)

/* Bits indicating whether this character is part of a two-character
   comment delimiter sequence. */
#define SYNTAX_ENTRY_COMMENT_BITS(entry) (((entry) >> 12) & 0xFF)

#define COMSTART_FIRST_A	0x80
#define COMSTART_FIRST_B	0x40
#define COMSTART_SECOND_A	0x20
#define COMSTART_SECOND_B	0x10
#define COMEND_FIRST_A		0x08
#define COMEND_FIRST_B		0x04
#define COMEND_SECOND_A		0x02
#define COMEND_SECOND_B		0x01

#define COMMENT_STYLE_A		0xAA
#define COMMENT_STYLE_B		0x55
#define COMSTART_FIRST		0xC0
#define COMSTART_SECOND		0x30
#define COMEND_FIRST		0x0C
#define COMEND_SECOND		0x03

#define SYNTAX_ENTRY_COMMENT_STYLE(sentry, m)				\
  ((((SYNTAX_ENTRY_COMMENT_BITS (sentry)) & (m) & COMMENT_STYLE_A)	\
    ? COMMENT_STYLE_A							\
    : 0)								\
   | (((SYNTAX_ENTRY_COMMENT_BITS (sentry)) & (m) & COMMENT_STYLE_B)	\
      ? COMMENT_STYLE_B							\
      : 0))

/* PREFIX says to skip over this character if it precedes an s-expression.  */
#define SYNTAX_ENTRY_PREFIX(entry) (((entry) >> 20) & 1)

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
  (((SYNTAX_ENTRY_CODE (entry)) == syntaxcode_escape)			\
   || ((SYNTAX_ENTRY_CODE (entry)) == syntaxcode_charquote))

/* This array, indexed by a character, contains the syntax code which that
   character signifies (as a char).  For example,
   ((enum syntaxcode) syntax_spec_code['w']) is syntaxcode_word. */
extern unsigned char syntax_spec_code [0x80];

#define SYNTAX_TABLE_P(argument)					\
  ((VECTOR_P (argument)) && ((VECTOR_LENGTH (argument)) == 0x100))

#define SYNTAX_TABLE_TYPE SCHEME_OBJECT

#define SYNTAX_TABLE_REF(table, index)					\
  (VECTOR_REF ((table), ((index) & 0xFF)))
