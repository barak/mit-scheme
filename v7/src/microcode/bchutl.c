/* -*-C-*-

$Id: bchutl.c,v 1.9 2000/01/18 05:06:14 cph Exp $

Copyright (c) 1991-1999 Massachusetts Institute of Technology

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

#include "oscond.h"
#include "ansidecl.h"
#include <stdio.h>

#include <errno.h>
#ifndef EINTR
#define EINTR 1999
#endif

#ifndef DOS386
#ifndef WINNT
#ifndef _OS2
#ifndef _NEXTOS
#include <unistd.h>
#endif
#endif
#endif
#endif

extern char * EXFUN (error_name, (int));
extern int EXFUN (retrying_file_operation,
		  (int (*)(int, char *, unsigned int),
		   int, char *, long, long, char *, char *, long *,
		   int (*)(char *, char *)));

#ifdef WINNT

#define lseek _lseek

char *
DEFUN (error_name, (code), int code)
{
  static char buf[512];

  sprintf (&buf[0], "%d, unknown error", code);
  return (&buf[0]);
}

#else /* not WINNT */
#ifdef _OS2

#if defined(__IBMC__) || defined(__WATCOMC__) || defined(__EMX__)
#include <io.h>
#endif

char *
DEFUN (error_name, (code), int code)
{
  static char buf [512];
  sprintf ((&buf[0]), "%d, unknown error", code);
  return (&buf[0]);
}

#else /* not _OS2 */

char *
DEFUN (error_name, (code), int code)
{
  static char buf[512];

  if ((code >= 0) && (code <= sys_nerr))
    sprintf (&buf[0], "%d, %s", code, sys_errlist[code]);
  else
    sprintf (&buf[0], "%d, unknown error", code);
  return (&buf[0]);
}

#endif /* not _OS2 */
#endif /* not WINNT */

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

int
DEFUN (retrying_file_operation,
       (operation, fid, ptr, position, nbytes, name, noise, curpos, abort_p),
       int EXFUN ((*operation), (int, char *, unsigned int))
       AND int fid AND char * ptr AND long position AND long nbytes
       AND char * name AND char * noise AND long * curpos
       AND int EXFUN ((*abort_p), (char *, char *)))
{
  char * membuf = ptr;
  long
    bytes_to_transfer = nbytes,
    bytes_transferred;

  if (*curpos != position)
    while ((lseek (fid, position, SEEK_SET)) == -1)
      if ((errno != EINTR) && ((*abort_p) ("lseek", noise)))
	goto fail;

  while ((bytes_to_transfer > 0)
	 && ((bytes_transferred =
	      ((*operation) (fid, membuf, ((unsigned int) bytes_to_transfer))))
	     != bytes_to_transfer))
    if (bytes_transferred == -1)
    {
      if ((errno != EINTR) && ((*abort_p) (name, noise)))
	goto fail;

      while ((lseek (fid, (position + (nbytes - bytes_to_transfer)), SEEK_SET))
	     == -1)
	if ((errno != EINTR) && ((*abort_p) ("lseek", noise)))
	  goto fail;
    }
    else
    {
      bytes_to_transfer -= bytes_transferred;
      membuf += bytes_transferred;
    }
  *curpos = (position + nbytes);
  return (nbytes);

fail:
  *curpos = -1;
  return (-1);
}

