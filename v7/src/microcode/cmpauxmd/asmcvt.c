/* -*-C-*-

$Id: asmcvt.c,v 1.2 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1995, 1999 Massachusetts Institute of Technology

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

/* Program to preprocess assembly files for Intel assembler.  */

#include <stdio.h>

void
main (unsigned int argc, const char ** argv)
{
  if ((argc > 1) && ((strcmp ((argv[1]), "pre")) == 0))
    {
      /* Convert '#' to ';' and eliminate formfeeds.  */
      printf("changecom(`;')\n");
      while (1)
	{
	  int c = (getchar ());
	  switch (c)
	    {
	    case EOF: exit (0);
	    case '#': putchar (';'); break;
	    case '\f': break;
	    default: putchar (c); break;
	    }
	}
    }
  else
    {
      /* Delete blank lines.  */
      enum state { line_start, line_middle };
      enum state s = line_start;
      while (1)
	{
	  int c = (getchar ());
	  if (c == EOF)
	    exit (0);
	  if (c == '\n')
	    {
	      if (s == line_middle)
		{
		  putchar (c);
		  s = line_start;
		}
	    }
	  else
	    {
	      putchar (c);
	      s = line_middle;
	    }
	}
    }
}
