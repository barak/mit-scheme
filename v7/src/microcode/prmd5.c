/* -*-C-*-

$Id: prmd5.c,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1997, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Interface to MD5 library */

#include "scheme.h"
#include "prims.h"
#include <md5.h>

DEFINE_PRIMITIVE ("MD5", Prim_md5, 1, 1,
  "(STRING)\n\
Generate an MD5 digest of string.\n\
The digest is returned as a 16-byte string.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    SCHEME_OBJECT result = (allocate_string (16));
    unsigned char * scan_result = (STRING_LOC (result, 0));
    MD5_CTX context;
    unsigned char * scan_digest = (context . digest);
    unsigned char * end_digest = (scan_digest + 16);
    MD5Init (&context);
    MD5Update ((&context), (STRING_LOC (string, 0)), (STRING_LENGTH (string)));
    MD5Final (&context);
    while (scan_digest < end_digest)
      (*scan_result++) = (*scan_digest++);
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("MD5-INIT", Prim_md5_init, 0, 0,
  "()\n\
Create and return an MD5 digest context.")
{
  PRIMITIVE_HEADER (0);
  {
    SCHEME_OBJECT context = (allocate_string (sizeof (MD5_CTX)));
    MD5Init ((MD5_CTX *) (STRING_LOC (context, 0)));
    PRIMITIVE_RETURN (context);
  }
}

static MD5_CTX *
DEFUN (md5_context_arg, (arg), int arg)
{
  CHECK_ARG (arg, STRING_P);
  if ((STRING_LENGTH (ARG_REF (arg))) != (sizeof (MD5_CTX)))
    error_bad_range_arg (arg);
  return ((MD5_CTX *) (STRING_LOC ((ARG_REF (arg)), 0)));
}

DEFINE_PRIMITIVE ("MD5-UPDATE", Prim_md5_update, 4, 4,
  "(CONTEXT STRING START END)\n\
Update CONTEXT with the contents of the substring (STRING,START,END).")
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (2));
    unsigned long l = (STRING_LENGTH (string));
    unsigned long start = (arg_ulong_index_integer (3, l));
    unsigned long end = (arg_integer_in_range (4, start, (l + 1)));
    MD5Update ((md5_context_arg (1)),
	       (STRING_LOC (string, start)),
	       (end - start));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("MD5-FINAL", Prim_md5_final, 1, 1,
  "(CONTEXT)\n\
Finalize CONTEXT and return the digest as a 16-byte string.")
{
  PRIMITIVE_HEADER (1);
  {
    MD5_CTX * context = (md5_context_arg (1));
    MD5Final (context);
    {
      SCHEME_OBJECT result = (allocate_string (16));
      unsigned char * scan_result = (STRING_LOC (result, 0));
      unsigned char * scan_digest = (context -> digest);
      unsigned char * end_digest = (scan_digest + 16);
      while (scan_digest < end_digest)
	(*scan_result++) = (*scan_digest++);
      PRIMITIVE_RETURN (result);
    }
  }
}
