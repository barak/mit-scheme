/* -*-C-*-

$Id: ntsys.c,v 1.4 1993/09/03 18:03:03 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

#include <stdio.h>
#include "nt.h"
#include "ntsys.h"

int
nt_console_write (void * vbuffer, size_t nsize)
{
  unsigned char * buffer = vbuffer;
  int i;

  for (i = 0; i < nsize; i++)
    putchar (buffer[i]);

  return (nsize);
}

void
nt_get_version (version_t * version_number)
{
  DWORD ver;
  ver = (GetVersion ());
  version_number->platform = ((unsigned char)
			      ((((unsigned long) ver) & 0x80000000UL) >> 31));
  version_number->major = (LOBYTE (LOWORD (ver)));
  version_number->minor = (HIBYTE (LOWORD (ver)));
  return;
}

BOOL
nt_pathname_as_filename (char * name, char * buffer)
{ /* Returns whether directory encountered is top level */
  unsigned int end_index = ((strlen (name)) - 1);

  /* The runtime system comes down with a name that has a back slash
     at the end.  This will choke DOS.
   */
  strcpy (buffer, name);
  if ((end_index >= 0) && (buffer[end_index] == '\\'))
  { /* Name is indeed a directory */
    if (end_index == 0) /* if only one char, name is top */
      return (TRUE);
    else
    {
      if (buffer[end_index-1] == ':') /* Preceded by drive letter, top */
	return (TRUE);
      else
      {
	buffer[end_index] = '\0';
	return (FALSE);
      }
    }
  }
  return (FALSE);
}
