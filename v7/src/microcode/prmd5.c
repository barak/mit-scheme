/* -*-C-*-

$Id: prmd5.c,v 1.9 2003/02/14 18:28:23 cph Exp $

Copyright (c) 1997-2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* Interface to MD5 library */

#include "scheme.h"
#include "prims.h"

#if defined(HAVE_LIBCRYPTO) && defined(HAVE_OPENSSL_MD5_H)
#  include <openssl/md5.h>
#else
#  ifdef HAVE_MD5_H
#    include <md5.h>
#  endif
#endif

#ifdef HAVE_LIBCRYPTO
#  define MD5_INIT MD5_Init
#  define MD5_UPDATE MD5_Update
#  define MD5_FINAL MD5_Final
#else
#  define MD5_INIT MD5Init
#  define MD5_UPDATE MD5Update
#  define MD5_FINAL MD5Final
#  define MD5_DIGEST_LENGTH 16
#endif

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
#ifdef HAVE_LIBCRYPTO
    unsigned char digest [MD5_DIGEST_LENGTH];
#endif
    unsigned char * scan_digest;
    unsigned char * end_digest;

    MD5_INIT (&context);
    MD5_UPDATE ((&context), (STRING_LOC (string, 0)), (STRING_LENGTH (string)));
#ifdef HAVE_LIBCRYPTO
    MD5_FINAL (digest, (&context));
    scan_digest = digest;
#else
    MD5_FINAL (&context);
    scan_digest = (context . digest);
#endif
    end_digest = (scan_digest + MD5_DIGEST_LENGTH);
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
    MD5_INIT ((MD5_CTX *) (STRING_LOC (context, 0)));
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
    MD5_UPDATE ((md5_context_arg (1)),
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
#ifdef HAVE_LIBCRYPTO
    unsigned char digest [MD5_DIGEST_LENGTH];
    MD5_FINAL (digest, context);
#else
    MD5_FINAL (context);
#endif
    {
      SCHEME_OBJECT result = (allocate_string (MD5_DIGEST_LENGTH));
      unsigned char * scan_result = (STRING_LOC (result, 0));
#ifdef HAVE_LIBCRYPTO
      unsigned char * scan_digest = digest;
#else
      unsigned char * scan_digest = (context -> digest);
#endif
      unsigned char * end_digest = (scan_digest + MD5_DIGEST_LENGTH);
      while (scan_digest < end_digest)
	(*scan_result++) = (*scan_digest++);
      PRIMITIVE_RETURN (result);
    }
  }
}

#ifdef COMPILE_AS_MODULE

char *
DEFUN_VOID (dload_initialize_file)
{
  declare_primitive
    ("MD5", Prim_md5, 1, 1,
     "(STRING)\n\
Generate an MD5 digest of string.\n\
The digest is returned as a 16-byte string.");

  declare_primitive
    ("MD5-INIT", Prim_md5_init, 0, 0,
     "()\n\
Create and return an MD5 digest context.");

  declare_primitive
    ("MD5-UPDATE", Prim_md5_update, 4, 4,
     "(CONTEXT STRING START END)\n\
Update CONTEXT with the contents of the substring (STRING,START,END).");

  declare_primitive
    ("MD5-FINAL", Prim_md5_final, 1, 1,
     "(CONTEXT)\n\
Finalize CONTEXT and return the digest as a 16-byte string.");
  return "#prmd5";
}

#endif /* COMPILE_AS_MODULE */
