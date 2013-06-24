/* -*-C-*-

$Id: ptrvec.c,v 1.8 2007/01/05 21:19:25 cph Exp $

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

#include "outf.h"
#include "dstack.h"

#if defined(__linux__) || defined(__APPLE__) || defined(__netbsd__)
#else
extern PTR EXFUN (malloc, (unsigned int length));
extern PTR EXFUN (realloc, (PTR ptr, unsigned int length));
#endif

static PTR
DEFUN (xmalloc, (length), unsigned int length)
{
  PTR result = (malloc (length));
  if (result == 0)
    {
      outf_fatal ("malloc: memory allocation failed\n");
      outf_flush_fatal ();
      abort ();
    }
  return (result);
}

static PTR
DEFUN (xrealloc, (ptr, length), PTR ptr AND unsigned int length)
{
  PTR result = (realloc (ptr, length));
  if (result == 0)
    {
      outf_fatal ("realloc: memory allocation failed\n");
      outf_flush_fatal ();
      abort ();
    }
  return (result);
}

Tptrvec
DEFUN (ptrvec_allocate, (length), Tptrvec_length length)
{
  Tptrvec ptrvec = (xmalloc (sizeof (struct struct_ptrvec)));
  (ptrvec -> length) = length;
  (ptrvec -> elements) =
    ((length > 0) ? (xmalloc (length * (sizeof (PTR)))) : 0);
  return (ptrvec);
}

void
DEFUN (ptrvec_deallocate, (ptrvec), Tptrvec ptrvec)
{
  if ((ptrvec -> length) > 0)
    free (ptrvec -> elements);
  free (ptrvec);
}

void
DEFUN (ptrvec_set_length, (ptrvec, length),
       Tptrvec ptrvec AND
       Tptrvec_length length)
{
  (ptrvec -> length) = length;
  (ptrvec -> elements) =
    ((length > 0)
     ? (xrealloc ((ptrvec -> elements), (length * (sizeof (PTR)))))
     : 0);
}

Tptrvec
DEFUN (ptrvec_copy, (ptrvec), Tptrvec ptrvec)
{
  Tptrvec_length length = (PTRVEC_LENGTH (ptrvec));
  Tptrvec result = (ptrvec_allocate (length));
  PTR * scan_source = (PTRVEC_START (ptrvec));
  PTR * end_source = (scan_source + length);
  PTR * scan_result = (PTRVEC_START (result));
  while (scan_source < end_source)
    (*scan_result++) = (*scan_source++);
  return (result);
}

void
DEFUN (ptrvec_adjoin, (ptrvec, element), Tptrvec ptrvec AND PTR element)
{
  Tptrvec_length length = (PTRVEC_LENGTH (ptrvec));
  ptrvec_set_length (ptrvec, (length + 1));
  (PTRVEC_REF (ptrvec, length)) = element;
}

int
DEFUN (ptrvec_memq, (ptrvec, element), Tptrvec ptrvec AND PTR element)
{
  PTR * scan = (PTRVEC_START (ptrvec));
  PTR * end = (scan + (PTRVEC_LENGTH (ptrvec)));
  while (scan < end)
    if (element == (*scan++))
      return (1);
  return (0);
}

void
DEFUN (ptrvec_move_left,
       (source, source_start, source_end, target, target_start),
       Tptrvec source AND
       Tptrvec_index source_start AND
       Tptrvec_index source_end AND
       Tptrvec target AND
       Tptrvec_index target_start)
{
  PTR * scan_source = (PTRVEC_LOC (source, source_start));
  PTR * end_source = (PTRVEC_LOC (source, source_end));
  PTR * scan_target = (PTRVEC_LOC (target, target_start));
  while (scan_source < end_source)
    (*scan_target++) = (*scan_source++);
}

void
DEFUN (ptrvec_move_right,
       (source, source_start, source_end, target, target_start),
       Tptrvec source AND
       Tptrvec_index source_start AND
       Tptrvec_index source_end AND
       Tptrvec target AND
       Tptrvec_index target_start)
{
  PTR * end_source = (PTRVEC_LOC (source, source_start));
  PTR * scan_source = (PTRVEC_LOC (source, source_end));
  PTR * scan_target =
    (PTRVEC_LOC (target, (target_start + (source_end - source_start))));
  while (scan_source > end_source)
    (*--scan_target) = (*--scan_source);
}
