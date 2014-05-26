/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

/* Adapters for the mhash crypto-hash library. */

#include "mhash-shim.h"

extern void
do_mhash (MHASH thread, const char *string, int start, int end)
{
  mhash (thread, string + start, end - start);
}

extern void
do_mhash_end (MHASH context, char *string, size_t size)
{
  void * digest = mhash_end (context);
  memcpy (string, digest, size);
  free (digest);
}

extern void
do_mhash_hmac_end (MHASH context, char *string, size_t size)
{
  void * digest = mhash_hmac_end (context);
  memcpy (string, digest, size);
  free (digest);
}

extern int
do_mhash_keygen (keygenid algorithm,
		 hashid hashid1, hashid hashid2,
		 int count,
		 void *salt, int salt_size,
		 char *keyword, int keysize,
		 unsigned char *password, int passwordlen)
{
  KEYGEN keygen;

  keygen.hash_algorithm[0] = hashid1;
  keygen.hash_algorithm[1] = hashid2;
  keygen.count = count;
  keygen.salt = salt;
  keygen.salt_size = salt_size;

  return (mhash_keygen_ext (algorithm, keygen,
			    keyword, keysize,
			    password, passwordlen));
}
