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

#ifndef SCM_BITS_H_INCLUDED
#define SCM_BITS_H_INCLUDED

#include "config.h"

#define DEFINE_BIT_COUNT(NAME, TYPE)					\
static inline unsigned int						\
NAME (TYPE x)								\
{									\
  /* #b01010101... */							\
  static const uintmax_t two_bit_mask = ((~ ((TYPE) 0)) / (1 + (1 << 1))); \
									\
  /* #b00110011... */							\
  static const uintmax_t four_bit_mask = ((~ ((TYPE) 0)) / (1 + (1 << 2))); \
									\
  /* #b00001111... */							\
  static const uintmax_t eight_bit_mask = ((~ ((TYPE) 0)) / (1 + (1 << 4))); \
									\
  /* Assumption: The number of bits in a uintmax_t fits in eight bits	\
     (unsigned); that is, the number of bits is less than 256.  */	\
									\
  /* This is a bit mask covering the total number of bits we need.  */	\
  static const uintmax_t final_mask					\
    = (((CHAR_BIT * (sizeof (TYPE))) << 1) - 1);			\
									\
  int i;								\
									\
  /* Compute a two-bit population count for each two-bit group in x.	\
     #b00 -> #b00, #b01 -> #b01, #b10 -> #b01, #b11 -> #b10 */		\
  x -= ((x >> 1) & two_bit_mask);					\
									\
  /* For successive pairs of two-bit groups, add them up into a		\
     four-bit bit count for both two-bit groups.  Each four-bit group	\
     is now either #b0000, #b0001, #b0010, #b0011, or #b0100.  Thus	\
     there is always a high-order zero bit, which is safe for storing	\
     carries of addition.  */						\
  x = (((x >> 2) & four_bit_mask) + (x & four_bit_mask));		\
									\
  /* Add up the four-bit groups, yielding eight-bit groups whose lower	\
     half is the sum of two four-bit groups and whose upper half is	\
     garbage; mask off the garbage.  */					\
  x = (((x >> 4) + x) & eight_bit_mask);				\
									\
  /* Add everything up in larger and larger chunks until done.  */	\
  for (i = 8; i < (CHAR_BIT * (sizeof (TYPE))); i <<= 1)		\
    x += (x >> i);							\
									\
  return (x & final_mask);						\
}

DEFINE_BIT_COUNT (uintmax_bit_count, uintmax_t)
DEFINE_BIT_COUNT (ulong_bit_count, unsigned long)

#define DEFINE_LENGTH_IN_BITS(NAME, TYPE, BIT_COUNT)			\
static inline unsigned int						\
NAME (TYPE x)								\
{									\
  /* Round up to a power of two minus one; i.e., set all bits in x	\
     below and including its most significant set bit.  */		\
  int i, limit = (CHAR_BIT * (sizeof (TYPE)));				\
  /* Unrolling this loop substantially improves performance.  The `for'	\
     at the end is for completeness; a good compiler should realize	\
     that it is dead code on just about any system.  */			\
  if (1 < limit) x |= (x >> 1);						\
  if (2 < limit) x |= (x >> 2);						\
  if (4 < limit) x |= (x >> 4);						\
  if (8 < limit) x |= (x >> 8);						\
  if (0x10 < limit) x |= (x >> 0x10);					\
  if (0x20 < limit) x |= (x >> 0x20);					\
  for (i = 0x40; i < limit; i <<= 1)					\
    x |= (x >> i);							\
  return (BIT_COUNT (x));						\
}

DEFINE_LENGTH_IN_BITS (uintmax_length_in_bits, uintmax_t, uintmax_bit_count)
DEFINE_LENGTH_IN_BITS (ulong_length_in_bits, unsigned long, ulong_bit_count)

#define DEFINE_FIRST_SET_BIT(NAME, TYPE, LENGTH_IN_BITS)	\
static inline int						\
NAME (TYPE x)							\
{								\
  if (x == 0) return (-1);					\
  return (LENGTH_IN_BITS ((x ^ (x - 1)) >> 1));			\
}

DEFINE_FIRST_SET_BIT (uintmax_first_set_bit, uintmax_t, uintmax_length_in_bits)
DEFINE_FIRST_SET_BIT (ulong_first_set_bit, unsigned long, ulong_length_in_bits)

#endif /* !defined(SCM_BITS_H_INCLUDED) */
