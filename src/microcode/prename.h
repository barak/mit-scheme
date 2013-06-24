/* -*-C-*-

$Id: prename.h,v 1.9 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

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
