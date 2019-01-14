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

#include "keccak.h"
#include "prims.h"
#include "scheme.h"

static inline void
scheme_le64enc (void * buf, uint64_t v)
{
  uint8_t * p = buf;

  (p[0]) = ((v >>  0) & 0xff);
  (p[1]) = ((v >>  8) & 0xff);
  (p[2]) = ((v >> 16) & 0xff);
  (p[3]) = ((v >> 24) & 0xff);
  (p[4]) = ((v >> 32) & 0xff);
  (p[5]) = ((v >> 40) & 0xff);
  (p[6]) = ((v >> 48) & 0xff);
  (p[7]) = ((v >> 56) & 0xff);
}

static inline uint64_t
scheme_le64dec(const void * buf)
{
  const uint8_t * p = buf;
  uint64_t v = 0;

  v |= (((uint64_t) (p[0])) <<  0);
  v |= (((uint64_t) (p[1])) <<  8);
  v |= (((uint64_t) (p[2])) << 16);
  v |= (((uint64_t) (p[3])) << 24);
  v |= (((uint64_t) (p[4])) << 32);
  v |= (((uint64_t) (p[5])) << 40);
  v |= (((uint64_t) (p[6])) << 48);
  v |= (((uint64_t) (p[7])) << 56);

  return v;
}

DEFINE_PRIMITIVE ("BYTEVECTOR-KECCAK-F1600", Prim_bytevector_keccak_f1600, 1, 1,
  "(STATE)\n\
Perform the Keccak-f[1600] permutation on a byte vector.")
{
  PRIMITIVE_HEADER (1);
  {
    uint64_t S[25];
    unsigned i;
    unsigned long n;
    uint8_t * state;

    state = (arg_bytevector (1, (&n)));
    if (n != 200)
      error_bad_range_arg (1);

    for (i = 0; (i < 25); i++)
      S[i] = (scheme_le64dec (&state[8*i]));
    keccakf1600 (S);
    for (i = 0; (i < 25); i++)
      scheme_le64enc ((&state[8*i]), (S[i]));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}
