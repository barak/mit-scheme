/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

/* Utility to generate nonces for liarc.  */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static void usage (const char *);

int
main (int argc, const char ** argv)
{
  unsigned int n_bytes;
  unsigned int i;

  if (argc != 2)
    usage (argv[0]);
  {
    int na = (atoi (argv[1]));
    if (! ((na > 0) && (na < 0x1000)))
      usage (argv[0]);
    n_bytes = na;
  }

  srand (time (0));
  for (i = 0; (i < n_bytes); i += 1)
    printf ("%02x",
	    ((unsigned int)
	     ((((double) (rand ())) * 255.)
	      / ((double) RAND_MAX))));
  printf ("\n");
  return (0);
}

static void
usage (const char * program)
{
  fprintf (stderr, "usage: %s N-BYTES\n", program);
  fflush (stderr);
  exit (1);
}
