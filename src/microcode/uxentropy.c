/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

#include "prims.h"
#include "ux.h"

#include <unistd.h>

#define PATH_URANDOM "/dev/urandom"

void
OS_get_entropy (uint8_t buf [32])
{
  size_t nbytes = 32;
  int fd;
  STD_FD_SYSTEM_CALL (syscall_open, fd, (UX_open (PATH_URANDOM, O_RDONLY)));
  ssize_t nread;
  while ((nread = (UX_read (fd, buf, nbytes))) != 0)
    {
      if (nread == -1)
	{
	  UX_prim_check_errno (syscall_read);
	  continue;
	}
      if (((size_t) nread) >= nbytes)
	return;
      nbytes -= ((size_t) nread);
      buf += ((size_t) nread);
    }
  /* Premature EOF makes no sense on /dev/urandom.  */
  error_external_return ();
}
