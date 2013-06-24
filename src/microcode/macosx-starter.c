/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* This is a "starter" program to be placed in a MacOS X application
   bundle.  It calls the real executable in the "Resources/" directory
   of the bundle, passing appropriate arguments to it.  */

#include <unistd.h>
#include <CoreServices/CoreServices.h>

int
main (int argc, const char ** argv)
{
  CFBundleRef bundle;
  CFURLRef url;
  UInt8 buffer [4096];
  char * bp;
  pid_t pid;

  bundle = (CFBundleGetMainBundle());
  if (bundle == 0)
    return (1);

  url = (CFBundleCopyResourceURL (bundle,
				  (CFSTR ("mit-scheme")),
				  (CFSTR ("")),
				  0));
  if (url == 0)
    return (2);

  if (!CFURLGetFileSystemRepresentation (url, true, buffer, (sizeof (buffer))))
    return (3);

  bp = ((char *) buffer);
  pid = (vfork ());
  if (pid == 0)
    {
      execl (bp, bp, "--macosx-application", "--edit", ((char *) 0));
      _exit (1);
    }
  return ((pid > 0) ? 0 : 3);
}
