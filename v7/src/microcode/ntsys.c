/* -*-C-*-

$Id: ntsys.c,v 1.10 2002/11/20 19:46:10 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

#include <stdio.h>
#include "nt.h"
#include "ntsys.h"

int
nt_console_write (void * vbuffer, size_t nsize)
{
  unsigned char * buffer = vbuffer;
  int i;

  for (i = 0; i < ((int) nsize); i++)
    putchar (buffer[i]);

  return (nsize);
}

BOOL
nt_pathname_as_filename (const char * name, char * buffer)
{ /* Returns whether directory encountered is top level */
  int end_index = ((strlen (name)) - 1);

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
