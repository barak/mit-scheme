/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

/* Bytevector primitives */

#include "scheme.h"
#include "prims.h"

uint8_t *
arg_bytevector (int n, unsigned long * len_r)
{
  CHECK_ARG (n, BYTEVECTOR_P);
  (*len_r) = (BYTEVECTOR_LENGTH (ARG_REF (n)));
  return (BYTEVECTOR_POINTER (ARG_REF (n)));
}

static uint8_t
arg_byte (int n)
{
  CHECK_ARG (n, FIXNUM_P);
  SCHEME_OBJECT argument = (ARG_REF (n));
  if (!FIXNUM_TO_ULONG_P (argument))
    error_bad_range_arg (n);
  unsigned long value = (FIXNUM_TO_ULONG (argument));
  if (value >= 0x100)
    error_bad_range_arg (n);
  return (uint8_t) value;
}

SCHEME_OBJECT
allocate_bytevector (unsigned long nbytes)
{
  SCHEME_OBJECT result
    = (allocate_non_marked_vector
       (TC_BYTEVECTOR,
        ((BYTES_TO_WORDS (nbytes)) + BYTEVECTOR_LENGTH_SIZE),
        true));
  SET_BYTEVECTOR_LENGTH (result, nbytes);
  return (result);
}

SCHEME_OBJECT
memory_to_bytevector (unsigned long n_bytes, const void * vp)
{
  SCHEME_OBJECT result = (allocate_bytevector (n_bytes));
  memcpy ((BYTEVECTOR_POINTER (result)), vp, n_bytes);
  return (result);
}

DEFINE_PRIMITIVE ("allocate-bytevector", Prim_allocate_bytevector, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (allocate_bytevector (arg_ulong_integer (1)));
}

DEFINE_PRIMITIVE ("bytevector?", Prim_bytevector_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (BYTEVECTOR_P (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("bytevector-length", Prim_bytevector_length, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, BYTEVECTOR_P);
  PRIMITIVE_RETURN (ulong_to_integer (BYTEVECTOR_LENGTH (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("bytevector-u8-ref", Prim_bytevector_u8_ref, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  unsigned long length;
  uint8_t * v = (arg_bytevector (1, (&length)));
  unsigned long index = (arg_ulong_index_integer (2, length));
  PRIMITIVE_RETURN (ulong_to_integer (v[index]));
}

DEFINE_PRIMITIVE ("bytevector-u8-set!", Prim_bytevector_u8_set, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  unsigned long length;
  uint8_t * v = (arg_bytevector (1, (&length)));
  unsigned long index = (arg_ulong_index_integer (2, length));
  (v[index]) = (arg_byte (3));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("bytevector-fill!", Prim_bytevector_fill, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  unsigned long length;
  uint8_t * v = (arg_bytevector (1, (&length)));
  uint8_t value = (arg_byte (2));
  unsigned long end = (arg_ulong_index_integer (4, (length + 1)));
  unsigned long start = (arg_ulong_index_integer (3, (end + 1)));
  memset ((v + start), value, (end - start));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("bytevector-copy", Prim_bytevector_copy, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  unsigned long length;
  uint8_t * v = (arg_bytevector (1, (&length)));
  unsigned long end = (arg_ulong_index_integer (3, (length + 1)));
  unsigned long start = (arg_ulong_index_integer (2, (end + 1)));
  PRIMITIVE_RETURN (memory_to_bytevector ((end - start), (v + start)));
}

DEFINE_PRIMITIVE ("bytevector-copy!", Prim_bytevector_copyx, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  unsigned long to_length;
  uint8_t * to_v = (arg_bytevector (1, (&to_length)));
  unsigned long to_start = (arg_ulong_index_integer (2, (to_length + 1)));
  unsigned long from_length;
  uint8_t * from_v = (arg_bytevector (3, (&from_length)));
  unsigned long from_end = (arg_ulong_index_integer (5, (from_length + 1)));
  unsigned long from_start = (arg_ulong_index_integer (4, (from_end + 1)));
  unsigned long length = (from_end - from_start);
  unsigned long to_end = (to_start + length);
  if (to_end > to_length)
    error_bad_range_arg (5);
  memmove ((to_v + to_start), (from_v + from_start), length);
  PRIMITIVE_RETURN (ulong_to_integer (to_end));
}
