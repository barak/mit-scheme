/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prename.h,v 1.4 1990/06/20 17:41:41 cph Rel $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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
    { "PRIMITIVE-TYPE?", "OBJECT-TYPE?" },
    { "&MAKE-OBJECT", "PRIMITIVE-OBJECT-SET-TYPE" },
    { "SYSTEM-MEMORY-REF", "PRIMITIVE-OBJECT-REF" },
    { "PRIMITIVE-OBJECT-NEW-TYPE", "PRIMITIVE-OBJECT-SET-TYPE" },
    { "FILE-CLOSE-CHANNEL", "CHANNEL-CLOSE" },
    { "PHOTO-OPEN", "TRANSCRIPT-ON" },
    { "PHOTO-CLOSE", "TRANSCRIPT-OFF" },
    { "GET-NEXT-INTERRUPT-CHARACTER", "TTY-NEXT-INTERRUPT-CHAR" },
    { "CHECK-AND-CLEAN-UP-INPUT-CHANNEL", "TTY-CLEAN-INTERRUPTS" },
    { "REMOVE-FILE", "FILE-REMOVE" },
    { "RENAME-FILE", "FILE-RENAME" },
    { "COPY-FILE", "FILE-COPY" },
    { "MAKE-DIRECTORY", "DIRECTORY-MAKE" },
    { "OPEN-DIRECTORY", "DIRECTORY-OPEN" },
    { "SCREEN-X-SIZE", "TTY-X-SIZE" },
    { "SCREEN-Y-SIZE", "TTY-Y-SIZE" },
    { "FILE-SYMLINK?", "FILE-SOFT-LINK?" }
  };

#define N_ALIASES 20
