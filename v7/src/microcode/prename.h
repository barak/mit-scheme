/* -*-C-*-

$Id: prename.h,v 1.8 1993/08/03 08:29:57 gjr Exp $

Copyright (c) 1988-1993 Massachusetts Institute of Technology

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

#ifndef SCM_PRENAME_H
#define SCM_PRENAME_H

struct primitive_alias_s
{
  char * alias;
  char * name;
};

static struct primitive_alias_s primitive_aliases [] =
{
  { "FALSE?", "NOT" },
  { "PRIMITIVE-TYPE", "OBJECT-TYPE" },
  { "PRIMITIVE-TYPE?", "OBJECT-TYPE?" },
  { "&MAKE-OBJECT", "PRIMITIVE-OBJECT-SET-TYPE" },
  { "SYSTEM-MEMORY-REF", "PRIMITIVE-OBJECT-REF" },
  { "PRIMITIVE-OBJECT-NEW-TYPE", "PRIMITIVE-OBJECT-SET-TYPE" },
  { "FILE-CLOSE-CHANNEL", "CHANNEL-CLOSE" },
  { "GET-NEXT-INTERRUPT-CHARACTER", "TTY-NEXT-INTERRUPT-CHAR" },
  { "REMOVE-FILE", "FILE-REMOVE" },
  { "RENAME-FILE", "FILE-RENAME" },
  { "COPY-FILE", "FILE-COPY" },
  { "MAKE-DIRECTORY", "DIRECTORY-MAKE" },
  { "SCREEN-X-SIZE", "TTY-X-SIZE" },
  { "SCREEN-Y-SIZE", "TTY-Y-SIZE" },
  { "FILE-SYMLINK?", "FILE-SOFT-LINK?" },
  { "X-GRAPHICS-SET-CLASS-HINT", "X-WINDOW-SET-CLASS-HINT" },
  { "CURRENT-FILE-TIME", "ENCODED-TIME" }
};

#define N_PRIMITIVE_ALIASES						\
  ((sizeof (primitive_aliases)) / (sizeof (struct primitive_alias_s)))

#endif /* SCM_PRENAME_H */
