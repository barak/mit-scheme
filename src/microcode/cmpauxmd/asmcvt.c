/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
