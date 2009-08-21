/* -*-C-*-

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

/* NOTE: This program was created by translation from the regular
   expression code of GNU Emacs; it was translated from the original C
   to 68000 assembly language (in 1986), and then translated back from
   68000 assembly language to C (in 1987).  */

/* Structure to represent a buffer of text to match against.
   This contains the information that an editor buffer would have
   to supply for the matching process to be executed.

   `translation' is an array of MAX_ASCII characters which is used to
   map each character before matching.  Both the pattern and the match
   text are mapped.  This is normally used to implement case
   insensitive searches.

   `syntax_table' describes the syntax of the match text.  See the
   syntax table primitives for more information.

   `text' points to the beginning of the match text.  It is used only
   for translating match text pointers into indices.

   `text_start' and `text_end' delimit the match text.  They define
   the buffer-start and buffer-end for those matching commands that
   refer to them.  Also, all matching must take place within these
   limits.

   `gap_start' and `gap_end' delimit a gap in the match text.  Editor
   buffers normally have such a gap.  For applications without a gap,
   it is recommended that these be set to the same value as
   `text_end'.

   Both `text_start' and `gap_start' are inclusive indices, while
   `text_end' and `gap_end' are exclusive.

   The following conditions must be true:

   (text <= text_start)
   (text_start <= text_end)
   (gap_start <= gap_end)
   (! ((text_start < text_end) &&
       (gap_start < gap_end) &&
       ((text_start == gap_start) || (text_end == gap_end))))

   */

struct re_buffer
  {
    unsigned char *translation;
    SYNTAX_TABLE_TYPE syntax_table;
    unsigned char *text;
    unsigned char *text_start;
    unsigned char *text_end;
    unsigned char *gap_start;
    unsigned char *gap_end;
  };

/* Structure to store "register" contents data in.

   Pass the address of such a structure as an argument to re_match,
   etc., if you want this information back.

   start[i] and end[i] record the string matched by \( ... \) grouping
   i, for i from 1 to RE_NREGS - 1.

   start[0] and end[0] record the entire string matched. */

#define RE_NREGS 10

struct re_registers
  {
    long start[RE_NREGS];
    long end[RE_NREGS];
  };

/* These are the command codes that appear in compiled regular
   expressions, one per byte.  Some command codes are followed by
   argument bytes.  A command code can specify any interpretation
   whatever for its arguments.  Zero-bytes may appear in the compiled
   regular expression. */

enum regexpcode
  {
    regexpcode_unused,
    regexpcode_exact_1,		/* Followed by 1 literal byte */

    /* Followed by one byte giving n, and then by n literal bytes. */
    regexpcode_exact_n,

    regexpcode_line_start,	/* Fails unless at beginning of line */
    regexpcode_line_end,	/* Fails unless at end of line */

    /* Followed by two bytes giving relative address to jump to. */
    regexpcode_jump,

    /* Followed by two bytes giving relative address of place to
       resume at in case of failure. */
    regexpcode_on_failure_jump,

    /* Throw away latest failure point and then jump to address. */
    regexpcode_finalize_jump,

    /* Like jump but finalize if safe to do so.  This is used to jump
       back to the beginning of a repeat.  If the command that follows
       this jump is clearly incompatible with the one at the beginning
       of the repeat, such that we can be sure that there is no use
       backtracking out of repetitions already completed, then we
       finalize. */
    regexpcode_maybe_finalize_jump,

    /* jump, and push a dummy failure point.  This failure point will
       be thrown away if an attempt is made to use it for a failure.
       A + construct makes this before the first repeat. */
    regexpcode_dummy_failure_jump,

    regexpcode_any_char,	/* Matches any one character */

    /* Matches any one char belonging to specified set.  First
       following byte is # bitmap bytes.  Then come bytes for a
       bit-map saying which chars are in.  Bits in each byte are
       ordered low-bit-first.  A character is in the set if its bit is
       1.  A character too large to have a bit in the map is
       automatically not in the set. */
    regexpcode_char_set,

    /* Similar but match any character that is NOT one of those
       specified. */
    regexpcode_not_char_set,

    /* Starts remembering the text that is matched and stores it in a
       memory register.  Followed by one byte containing the register
       number.  Register numbers must be in the range 0 through
       (RE_NREGS - 1) inclusive.  */
    regexpcode_start_memory,

    /* Stops remembering the text that is matched and stores it in a
       memory register.  Followed by one byte containing the register
       number.  Register numbers must be in the range 0 through
       (RE_NREGS - 1) inclusive.  */
    regexpcode_stop_memory,

    /* Match a duplicate of something remembered.  Followed by one
       byte containing the index of the memory register. */
    regexpcode_duplicate,

    regexpcode_buffer_start,	/* Succeeds if at beginning of buffer */
    regexpcode_buffer_end,	/* Succeeds if at end of buffer */
    regexpcode_word_char,	/* Matches any word-constituent character */

    /* Matches any char that is not a word-constituent. */
    regexpcode_not_word_char,

    regexpcode_word_start,	/* Succeeds if at word beginning */
    regexpcode_word_end,	/* Succeeds if at word end */
    regexpcode_word_bound,	/* Succeeds if at a word boundary */
    regexpcode_not_word_bound,	/* Succeeds if not at a word boundary */

    /* Matches any character whose syntax is specified.  Followed by a
       byte which contains a syntax code, Sword or such like. */
    regexpcode_syntax_spec,

    /* Matches any character whose syntax differs from the specified. */
    regexpcode_not_syntax_spec
  };

extern void
  re_buffer_initialize (struct re_buffer *, unsigned char *, SYNTAX_TABLE_TYPE,
	  unsigned char *, unsigned long, unsigned long,
	  unsigned long, unsigned long);

extern int
  re_compile_fastmap (unsigned char *, unsigned char *, unsigned char *,
	  SYNTAX_TABLE_TYPE, unsigned char *);

extern int
  re_match (unsigned char *, unsigned char *, struct re_buffer *,
	  struct re_registers *, unsigned char *, unsigned char *);

extern int
  re_search_forward (unsigned char *, unsigned char *, struct re_buffer *,
	  struct re_registers *, unsigned char *, unsigned char *);

extern int
  re_search_backward (unsigned char *, unsigned char *, struct re_buffer *,
	  struct re_registers *, unsigned char *, unsigned char *);
