/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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
#include "outf.h"
#include "dstack.h"
#include "os.h"

Tptrvec
ptrvec_allocate (Tptrvec_length length)
{
  Tptrvec ptrvec = (OS_malloc (sizeof (struct struct_ptrvec)));
  (ptrvec -> length) = length;
  (ptrvec -> elements)
    = ((length > 0) ? (OS_malloc (length * (sizeof (void *)))) : 0);
  return (ptrvec);
}

void
ptrvec_deallocate (Tptrvec ptrvec)
{
  if ((ptrvec -> length) > 0)
    OS_free (ptrvec -> elements);
  OS_free (ptrvec);
}

void
ptrvec_set_length (Tptrvec ptrvec,
       Tptrvec_length length)
{
  (ptrvec -> length) = length;
  (ptrvec -> elements)
    = ((length > 0)
       ? (OS_realloc ((ptrvec -> elements), (length * (sizeof (void *)))))
       : 0);
}

Tptrvec
ptrvec_copy (Tptrvec ptrvec)
{
  Tptrvec_length length = (PTRVEC_LENGTH (ptrvec));
  Tptrvec result = (ptrvec_allocate (length));
  void ** scan_source = (PTRVEC_START (ptrvec));
  void ** end_source = (scan_source + length);
  void ** scan_result = (PTRVEC_START (result));
  while (scan_source < end_source)
    (*scan_result++) = (*scan_source++);
  return (result);
}

void
ptrvec_adjoin (Tptrvec ptrvec, void * element)
{
  Tptrvec_length length = (PTRVEC_LENGTH (ptrvec));
  ptrvec_set_length (ptrvec, (length + 1));
  (PTRVEC_REF (ptrvec, length)) = element;
}

int
ptrvec_memq (Tptrvec ptrvec, void * element)
{
  void ** scan = (PTRVEC_START (ptrvec));
  void ** end = (scan + (PTRVEC_LENGTH (ptrvec)));
  while (scan < end)
    if (element == (*scan++))
      return (1);
  return (0);
}

void
ptrvec_move_left (Tptrvec source,
		  Tptrvec_index source_start,
		  Tptrvec_index source_end,
		  Tptrvec target,
		  Tptrvec_index target_start)
{
  void ** scan_source = (PTRVEC_LOC (source, source_start));
  void ** end_source = (PTRVEC_LOC (source, source_end));
  void ** scan_target = (PTRVEC_LOC (target, target_start));
  while (scan_source < end_source)
    (*scan_target++) = (*scan_source++);
}

void
ptrvec_move_right (Tptrvec source,
		   Tptrvec_index source_start,
		   Tptrvec_index source_end,
		   Tptrvec target,
		   Tptrvec_index target_start)
{
  void ** end_source = (PTRVEC_LOC (source, source_start));
  void ** scan_source = (PTRVEC_LOC (source, source_end));
  void ** scan_target
    = (PTRVEC_LOC (target, (target_start + (source_end - source_start))));
  while (scan_source > end_source)
    (*--scan_target) = (*--scan_source);
}
