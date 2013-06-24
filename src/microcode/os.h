/* -*-C-*-

$Id: os.h,v 1.12 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

#ifndef SCM_OS_H
#define SCM_OS_H

#include "config.h"

typedef unsigned int Tchannel;

extern PTR EXFUN (OS_malloc_init, (unsigned int));
extern PTR EXFUN (OS_malloc, (unsigned int));
extern PTR EXFUN (OS_realloc, (PTR, unsigned int));
extern void EXFUN (OS_free, (PTR));

#define FASTCOPY(from, to, n)						\
{									\
  const char * FASTCOPY_scan_src = (from);				\
  const char * FASTCOPY_end_src = (FASTCOPY_scan_src + (n));		\
  char * FASTCOPY_scan_dst = (to);					\
  while (FASTCOPY_scan_src < FASTCOPY_end_src)				\
    (*FASTCOPY_scan_dst++) = (*FASTCOPY_scan_src++);			\
}

#endif /* SCM_OS_H */
