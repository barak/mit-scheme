/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

/* Adapters for the Blowfish encryption library. */
#include "blowfish-shim.h"

#include <stdlib.h>

void
BF_set_key (BF_KEY * B, int nbytes, const unsigned char * key)
{
  blowfish_init (B, key, nbytes);
}

void
BF_ecb_encrypt (const unsigned char * in, unsigned char * out, BF_KEY * B,
		int dir)
{
  switch (dir) {
  case BF_ENCRYPT:
    blowfish_encrypt (B, in, out);
    break;
  case BF_DECRYPT:
    blowfish_decrypt (B, in, out);
    break;
  default:
    abort ();
  }
}

void
BF_cbc_encrypt (const unsigned char * in, unsigned char * out, long nbytes,
		BF_KEY * B, unsigned char * iv, int dir)
{
  switch (dir) {
  case BF_ENCRYPT:
    blowfish_encrypt_cbc (B, iv, in, out, nbytes);
    break;
  case BF_DECRYPT:
    blowfish_decrypt_cbc (B, iv, in, out, nbytes);
    break;
  default:
    abort ();
  }
}

int
do_BF_cfb64_encrypt (const unsigned char *in,
		     long istart,
		     unsigned char *out,
		     long ostart,
		     long length,
		     const BF_KEY *schedule,
		     unsigned char *ivec,
		     int num,
		     int dir)
{
  switch (dir) {
  case BF_ENCRYPT:
    return (blowfish_encrypt_cfb64 (schedule, ivec, num, in + istart,
				    out + ostart, length));
  case BF_DECRYPT:
    return (blowfish_decrypt_cfb64 (schedule, ivec, num, in + istart,
				    out + ostart, length));
  default:
    abort ();
  }
}

extern int
do_BF_ofb64_encrypt (const unsigned char *in,
		     long istart,
		     unsigned char *out,
		     long ostart,
		     long length,
		     const BF_KEY *schedule,
		     unsigned char *ivec,
		     int num)
{
  return (blowfish_encrypt_ofb64 (schedule, ivec, num, in + istart,
				  out + ostart, length));
}
