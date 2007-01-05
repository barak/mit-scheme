/* -*-C-*-

$Id: dmpwrld.c,v 9.44 2007/01/05 15:33:06 cph Exp $

Copyright (c) 1987-2000, 2002 Massachusetts Institute of Technology

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

/* This file contains a primitive to dump an executable version of Scheme.
   It uses unexec.c from GNU Emacs.
   Look at unexec.c for more information. */

#include "scheme.h"
#include "prims.h"

#ifndef __unix__
#include "Error: dumpworld.c does not work on non-unix machines."
#endif

#include "ux.h"
#include "osfs.h"
#include <sys/file.h>

/* Compatibility definitions for GNU Emacs's unexec.c.
   Taken from the various m-*.h and s-*.h files for GNU Emacs.
*/

#define CANNOT_UNEXEC

#if defined (vax)
#undef CANNOT_UNEXEC
#endif

#if defined (hp9000s300) || defined (__hp9000s300)
#undef CANNOT_UNEXEC
#define ADJUST_EXEC_HEADER   						\
  hdr.a_magic = ((ohdr.a_magic.file_type == OLDMAGIC.file_type) ?	\
		 NEWMAGIC : ohdr.a_magic);
#endif

#if defined (hp9000s800) || defined (__hp9000s800)
#undef CANNOT_UNEXEC
#endif

#if defined (sun3)
#undef CANNOT_UNEXEC
#define SEGMENT_MASK		(SEGSIZ - 1)
#define A_TEXT_OFFSET(HDR)	sizeof (HDR)
#define TEXT_START		(PAGSIZ + (sizeof(struct exec)))
#endif

/* I haven't tried any below this point. */

#if defined (umax)
#undef CANNOT_UNEXEC
#define HAVE_GETPAGESIZE
#define COFF
#define UMAX
#define SECTION_ALIGNMENT	pagemask
#define SEGMENT_MASK		(64 * 1024 - 1)
#endif

#if defined (celerity)
#undef CANNOT_UNEXEC
#endif

#if defined (sun2)
#undef CANNOT_UNEXEC
#define SEGMENT_MASK		(SEGSIZ - 1)
#endif

#if defined (pyr)
#undef CANNOT_UNEXEC
#define SEGMENT_MASK (2048-1)	/* ZMAGIC format */
				/* man a.out for info */
#endif

#ifdef CANNOT_UNEXEC
#include "Error: dmpwrld.c only works on a few machines."
#endif

#ifndef TEXT_START
#define TEXT_START	0
#endif

#ifndef SEGMENT_MASK
#define DATA_START	(&etext)
#else
#define DATA_START	\
(((((unsigned) &etext) - 1) & ~SEGMENT_MASK) + (SEGMENT_MASK + 1))
#endif

#if defined (__HPUX__)
#define USG
#define HPUX
#endif

/* More compatibility definitions for unexec. */

extern int end, etext, edata;

char
*start_of_text()
{
  return ((char *) TEXT_START);
}

char
*start_of_data()
{
  return ((char *) DATA_START);
}

#if defined (USG) || defined (NO_BZERO)

#define bzero(b,len)	(memset((b), 0, (len)))

#else

extern void bzero();

#endif

#define static

#if defined (hp9000s800) || defined (__hp9000s800)
#include "unexhp9k800.c"
#else
#include "unexec.c"
#endif

#undef static

void
DEFUN (unix_find_pathname, (program_name, target),
       CONST char * program_name AND char * target)
{
  int length;
  char
    * path,
    * next;
  extern char *
    EXFUN (index, (char * path AND char srchr));
  extern void
    EXFUN (strcpy, (char * target AND CONST char * source));

  /* Attempt first in the connected directory */

  if (((program_name[0]) == '/')
      || (OS_file_access (program_name, X_OK))
      || ((path = ((char *) (getenv ("PATH")))) == ((char *) NULL)))
  {
    strcpy (target, program_name);
    return;
  }
  for (next = (index (path, ':'));
       path != ((char *)  NULL);
       path = (next + 1),
       next = (index (path, ':')))
  {
    length = ((next == ((char *) NULL))
	      ? (strlen (path))
	      : (next-path));
    strncpy (target, path, length);
    target[length] = '/';
    target[length + 1] = '\0';
    strcpy ((target + (length + 1)), program_name);
    if (OS_file_access (target, X_OK))
    {
      return;
    }
  }
  strcpy (target, program_name);
  return;
}

/* The primitive visible from Scheme. */

extern Boolean scheme_dumped_p;

DEFINE_PRIMITIVE ("DUMP-WORLD", Prim_dump_world, 1, 1, 0)
{
  int result;
  SCHEME_OBJECT arg;
  Boolean saved_dumped_p;
  char
    * fname,
    path_buffer[FILE_NAME_LENGTH];
  PRIMITIVE_HEADER (1);

  PRIMITIVE_CANONICALIZE_CONTEXT();

  arg = (ARG_REF (1));
  fname = (STRING_ARG (1));

  /* Set up for restore */

  saved_dumped_p = scheme_dumped_p;

  scheme_dumped_p = true;
  val_register = SHARP_T;
  POP_PRIMITIVE_FRAME (1);

  /* Dump! */

  unix_find_pathname (scheme_program_name, path_buffer);
  result = (unexec (fname,
		    path_buffer,
		    ((unsigned) 0),		/* default */
		    ((unsigned) 0),		/* default */
		    ((unsigned) start_of_text())));

  /* Restore State */

  val_register = SHARP_F;
  scheme_dumped_p = saved_dumped_p;

  /* IO: Restoring cached input for this job. */

  if (result != 0)
  {
    STACK_PUSH (arg);
    error_external_return ();
  }

  PRIMITIVE_ABORT (PRIM_POP_RETURN);
  /*NOTREACHED*/
}
