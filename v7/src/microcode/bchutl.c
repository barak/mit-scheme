/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchutl.c,v 1.1 1991/10/29 22:35:01 jinx Exp $

Copyright (c) 1991 Massachusetts Institute of Technology

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

#include <errno.h>
#include <unistd.h>
#include "ansidecl.h"

extern char * EXFUN (error_name, (int));
extern int EXFUN (retrying_file_operation,
		  (int (*)(int, char *, unsigned int),
		   int, char *, long, long, char *, char *, long *,
		   int (*)(char *, char *)));

char *
DEFUN (error_name, (code), int code)
{
  extern int sys_nerr;
  extern char *sys_errlist[];
  static char buf[512];

  if ((code >= 0) && (code <= sys_nerr))
    sprintf (&buf[0], "%d, %s", code, sys_errlist[code]);
  else
    sprintf (&buf[0], "%d, unknown error", code);
  return (&buf[0]);
}

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

