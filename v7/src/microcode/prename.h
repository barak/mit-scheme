/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prename.h,v 1.3 1989/09/20 23:10:32 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

/* Definitions of aliases for primitives. */

static struct primitive_alias aliases [] =
  {
    { "NULL?", "NOT" },
    { "FALSE?", "NOT" },
    { "PRIMITIVE-TYPE", "OBJECT-TYPE" },
    { "PRIMITIVE-GC-TYPE", "PRIMITIVE-OBJECT-GC-TYPE" },
    { "PRIMITIVE-TYPE?", "OBJECT-TYPE?" },
    { "PRIMITIVE-DATUM", "PRIMITIVE-OBJECT-DATUM" },
    { "PRIMITIVE-SET-TYPE", "OBJECT-SET-TYPE" },
    { "&MAKE-OBJECT", "PRIMITIVE-OBJECT-SET-TYPE" },
    { "SYSTEM-MEMORY-REF", "PRIMITIVE-OBJECT-REF" },
    { "SYSTEM-MEMORY-SET!", "PRIMITIVE-OBJECT-SET!" },
    { "OBJECT-NEW-TYPE", "OBJECT-SET-TYPE" },
    { "PRIMITIVE-OBJECT-NEW-TYPE", "PRIMITIVE-OBJECT-SET-TYPE" },
    { "SINE-FLONUM", "FLONUM-SIN" },
    { "COSINE-FLONUM", "FLONUM-COS" },
    { "ATAN-FLONUM", "FLONUM-ATAN" },
    { "EXP-FLONUM", "FLONUM-EXP" },
    { "LN-FLONUM", "FLONUM-LOG" },
    { "SQRT-FLONUM", "FLONUM-SQRT" },
    { "PLUS-FLONUM", "FLONUM-ADD" },
    { "MINUS-FLONUM", "FLONUM-SUBTRACT" },
    { "MULTIPLY-FLONUM", "FLONUM-MULTIPLY" },
    { "DIVIDE-FLONUM", "FLONUM-DIVIDE" },
    { "ZERO-FLONUM?", "FLONUM-ZERO?" },
    { "POSITIVE-FLONUM?", "FLONUM-POSITIVE?" },
    { "NEGATIVE-FLONUM?", "FLONUM-NEGATIVE?" },
    { "EQUAL-FLONUM?", "FLONUM-EQUAL?" },
    { "LESS-THAN-FLONUM?", "FLONUM-LESS?" },
    { "GREATER-THAN-FLONUM?", "FLONUM-GREATER?" },
    { "TRUNCATE-FLONUM", "FLONUM-TRUNCATE->EXACT" }
  };

#define N_ALIASES 29
