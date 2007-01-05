/* -*-C-*-

$Id: bchutl.c,v 1.16 2007/01/05 15:33:06 cph Exp $

Copyright (c) 1991-2000, 2002 Massachusetts Institute of Technology

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

#include "config.h"
#include <stdio.h>

#include <errno.h>
#ifndef EINTR
#  define EINTR 1999
#endif

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#ifdef STDC_HEADERS
#  include <string.h>
#endif

#ifdef HAVE_STRERROR

char *
DEFUN (error_name, (code), int code)
{
  static char buf [512];
  sprintf (buf, "%d, %s", code, (strerror (code)));
  return (buf);
}

#else /* not HAVE_STRERROR */
#ifdef __WIN32__

#define lseek _lseek

char *
DEFUN (error_name, (code), int code)
{
  static char buf [512];
  sprintf (buf, "%d, unknown error", code);
  return (buf);
}

#else /* not __WIN32__ */
#ifdef __OS2__

#if defined(__IBMC__) || defined(__WATCOMC__) || defined(__EMX__)
# include <io.h>
#endif

char *
DEFUN (error_name, (code), int code)
{
  static char buf [512];
  sprintf (buf, "%d, unknown error", code);
  return (buf);
}

#else /* not __OS2__ */

char *
DEFUN (error_name, (code), int code)
{
  static char buf [512];
  if ((code >= 0) && (code <= sys_nerr))
    sprintf (buf, "%d, %s", code, sys_errlist[code]);
  else
    sprintf (buf, "%d, unknown error", code);
  return (buf);
}

#endif /* not __OS2__ */
#endif /* not __WIN32__ */
#endif /* not HAVE_STRERROR */

#ifndef SEEK_SET
#  define SEEK_SET 0
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

