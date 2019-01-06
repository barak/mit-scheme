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

/* Interface to MD5 library */

#include "scheme.h"
#include "prims.h"

#include "md5.h"

DEFINE_PRIMITIVE ("MD5", Prim_md5, 1, 1,
  "(BYTEVECTOR)\n\
Generate an MD5 digest of bytevector.\n\
The digest is returned as a 16-byte bytevector.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, BYTEVECTOR_P);
  {
    SCHEME_OBJECT bytevector = (ARG_REF (1));
    SCHEME_OBJECT result = (allocate_bytevector (16));
    struct md5 context;
    md5_init (&context);
    md5_update ((&context),
		(BYTEVECTOR_POINTER (bytevector)),
		(BYTEVECTOR_LENGTH (bytevector)));
    md5_final ((&context), (BYTEVECTOR_POINTER (result)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("MD5-INIT", Prim_md5_init, 0, 0,
  "()\n\
Create and return an MD5 digest context.")
{
  PRIMITIVE_HEADER (0);
  {
    SCHEME_OBJECT context = (allocate_bytevector (sizeof (struct md5)));
    md5_init ((struct md5 *) (BYTEVECTOR_POINTER (context)));
    PRIMITIVE_RETURN (context);
  }
}

static struct md5 *
md5_context_arg (int arg)
{
  CHECK_ARG (arg, BYTEVECTOR_P);
  if ((BYTEVECTOR_LENGTH (ARG_REF (arg))) != (sizeof (struct md5)))
    error_bad_range_arg (arg);
  return ((struct md5 *) (BYTEVECTOR_POINTER (ARG_REF (arg))));
}

DEFINE_PRIMITIVE ("MD5-UPDATE", Prim_md5_update, 4, 4,
  "(CONTEXT BYTEVECTOR START END)\n\
Update CONTEXT with the contents of the subbytevector (BYTEVECTOR,START,END).")
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, BYTEVECTOR_P);
  {
    SCHEME_OBJECT bytevector = (ARG_REF (2));
    unsigned long end
      = (arg_ulong_index_integer (4, ((BYTEVECTOR_LENGTH (bytevector)) + 1)));
    unsigned long start = (arg_ulong_index_integer (3, (end + 1)));
    md5_update ((md5_context_arg (1)),
		(BYTEVECTOR_LOC (bytevector, start)),
		(end - start));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("MD5-FINAL", Prim_md5_final, 1, 1,
  "(CONTEXT)\n\
Finalize CONTEXT and return the digest as a 16-byte bytevector.")
{
  PRIMITIVE_HEADER (1);
  {
    struct md5 * context = (md5_context_arg (1));
    SCHEME_OBJECT result = (allocate_bytevector (MD5_HASHLEN));
    md5_final (context, (BYTEVECTOR_POINTER (result)));
    PRIMITIVE_RETURN (result);
  }
}
