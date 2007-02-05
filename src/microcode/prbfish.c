/* -*-C-*-

$Id: prbfish.c,v 1.16 2007/01/05 21:19:25 cph Exp $

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

/* Interface to Blowfish library */

#include "scheme.h"
#include "prims.h"

#if defined(HAVE_LIBCRYPTO) && defined(HAVE_OPENSSL_BLOWFISH_H)
#  include <openssl/blowfish.h>
#else
#  ifdef HAVE_BLOWFISH_H
#    include <blowfish.h>
#  endif
#endif

/* This interface uses the Blowfish library from SSLeay.  */

DEFINE_PRIMITIVE ("BLOWFISH-SET-KEY", Prim_blowfish_set_key, 1, 1,
  "(STRING)\n\
Generate a Blowfish key from STRING.\n\
STRING must be 72 bytes or less in length.\n\
For text-string keys, use MD5 on the text, and pass the digest here.")
{
  SCHEME_OBJECT string;
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  string = (ARG_REF (1));
  if ((STRING_LENGTH (string)) > 72)
    error_bad_range_arg (1);
  result = (allocate_string (sizeof (BF_KEY)));
  BF_set_key (((BF_KEY *) (STRING_LOC (result, 0))),
	      (STRING_LENGTH (string)),
	      (STRING_LOC (string, 0)));
  PRIMITIVE_RETURN (result);
}

static BF_KEY *
DEFUN (key_arg, (arg), unsigned int arg)
{
  CHECK_ARG (arg, STRING_P);
  if ((STRING_LENGTH (ARG_REF (arg))) != (sizeof (BF_KEY)))
    error_bad_range_arg (arg);
  return ((BF_KEY *) (STRING_LOC ((ARG_REF (arg)), 0)));
}

static unsigned char *
DEFUN (init_vector_arg, (arg), unsigned int arg)
{
  CHECK_ARG (arg, STRING_P);
  if ((STRING_LENGTH (ARG_REF (arg))) != 8)
    error_bad_range_arg (arg);
  return (STRING_LOC ((ARG_REF (arg)), 0));
}

DEFINE_PRIMITIVE ("BLOWFISH-ECB", Prim_blowfish_ecb, 4, 4,
  "(INPUT OUTPUT KEY-VECTOR ENCRYPT?)\n\
Apply Blowfish in Electronic Code Book mode.\n\
INPUT is an 8-byte string.\n\
OUTPUT is an 8-byte string.\n\
KEY is a Blowfish key.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).")
{
  SCHEME_OBJECT input_text;
  SCHEME_OBJECT output_text;
  PRIMITIVE_HEADER (4);

  CHECK_ARG (1, STRING_P);
  input_text = (ARG_REF (1));
  if ((STRING_LENGTH (input_text)) != 8)
    error_bad_range_arg (1);
  CHECK_ARG (2, STRING_P);
  output_text = (ARG_REF (2));
  if ((STRING_LENGTH (output_text)) != 8)
    error_bad_range_arg (2);
  BF_ecb_encrypt ((STRING_LOC (input_text, 0)),
		  (STRING_LOC (output_text, 0)),
		  (key_arg (3)),
		  ((BOOLEAN_ARG (4)) ? BF_ENCRYPT : BF_DECRYPT));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("BLOWFISH-CBC-V2", Prim_blowfish_cbc, 5, 5,
  "(INPUT OUTPUT KEY INIT-VECTOR ENCRYPT?)\n\
Apply Blowfish in Cipher Block Chaining mode.\n\
INPUT is a string whose length is a multiple of 8 bytes.\n\
OUTPUT is a string whose length is the same as INPUT.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte string; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).")
{
  SCHEME_OBJECT input_text;
  SCHEME_OBJECT output_text;
  PRIMITIVE_HEADER (5);

  CHECK_ARG (1, STRING_P);
  input_text = (ARG_REF (1));
  if (((STRING_LENGTH (input_text)) % 8) != 0)
    error_bad_range_arg (1);
  CHECK_ARG (2, STRING_P);
  output_text = (ARG_REF (2));
  if ((output_text == input_text)
      || ((STRING_LENGTH (output_text)) != (STRING_LENGTH (input_text))))
    error_bad_range_arg (2);
  BF_cbc_encrypt ((STRING_LOC (input_text, 0)),
		  (STRING_LOC (output_text, 0)),
		  (STRING_LENGTH (input_text)),
		  (key_arg (3)),
		  (init_vector_arg (4)),
		  ((BOOLEAN_ARG (5)) ? BF_ENCRYPT : BF_DECRYPT));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("BLOWFISH-CFB64-SUBSTRING-V2", Prim_blowfish_cfb64_substring, 9, 9,
  "(INPUT ISTART IEND OUTPUT OSTART KEY INIT-VECTOR NUM ENCRYPT?)\n\
Apply Blowfish in Cipher Feed-Back mode.\n\
(INPUT,ISTART,IEND) is an arbitrary substring.\n\
OUTPUT is a string as large as the input substring.\n\
OSTART says where to start writing to the output string.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte string; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
  The initial value must be unique for each message/key pair.\n\
NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the\n\
  number of bytes that have previously been processed in this stream.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).\n\
Returned value is the new value of NUM.")
{
  SCHEME_OBJECT input_text;
  unsigned long istart;
  unsigned long iend;
  unsigned long ilen;
  SCHEME_OBJECT output_text;
  unsigned long ostart;
  int num;
  PRIMITIVE_HEADER (9);

  CHECK_ARG (1, STRING_P);
  input_text = (ARG_REF (1));
  {
    unsigned long l = (STRING_LENGTH (input_text));
    istart = (arg_ulong_index_integer (2, l));
    iend = (arg_integer_in_range (3, istart, (l + 1)));
  }
  ilen = (iend - istart);
  CHECK_ARG (4, STRING_P);
  output_text = (ARG_REF (4));
  ostart = (arg_ulong_index_integer (5, (STRING_LENGTH (output_text))));
  if ((output_text == input_text)
      && (ostart < iend)
      && (istart < (ostart + ilen)))
    error_bad_range_arg (4);
  num = (arg_index_integer (8, 8));
  BF_cfb64_encrypt ((STRING_LOC (input_text, istart)),
		    (STRING_LOC (output_text, ostart)),
		    ilen,
		    (key_arg (6)),
		    (init_vector_arg (7)),
		    (&num),
		    ((BOOLEAN_ARG (9)) ? BF_ENCRYPT : BF_DECRYPT));
  PRIMITIVE_RETURN (long_to_integer (num));
}

DEFINE_PRIMITIVE ("BLOWFISH-OFB64-SUBSTRING", Prim_blowfish_ofb64_substring, 8, 8,
  "(INPUT ISTART IEND OUTPUT OSTART KEY INIT-VECTOR NUM)\n\
Apply Blowfish in Output Feed-Back mode.\n\
(INPUT,ISTART,IEND) is an arbitrary substring.\n\
OUTPUT is a string as large as the input substring.\n\
OSTART says where to start writing to the output string.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte string; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
  The initial value must be unique for each message/key pair.\n\
NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the\n\
  number of bytes that have previously been processed in this stream.\n\
Returned value is the new value of NUM.")
{
  SCHEME_OBJECT input_text;
  unsigned long istart;
  unsigned long iend;
  unsigned long ilen;
  SCHEME_OBJECT output_text;
  unsigned long ostart;
  int num;
  PRIMITIVE_HEADER (8);

  CHECK_ARG (1, STRING_P);
  input_text = (ARG_REF (1));
  {
    unsigned long l = (STRING_LENGTH (input_text));
    istart = (arg_ulong_index_integer (2, l));
    iend = (arg_integer_in_range (3, istart, (l + 1)));
  }
  ilen = (iend - istart);
  CHECK_ARG (4, STRING_P);
  output_text = (ARG_REF (4));
  ostart = (arg_ulong_index_integer (5, (STRING_LENGTH (output_text))));
  if ((output_text == input_text)
      && (ostart < iend)
      && (istart < (ostart + ilen)))
    error_bad_range_arg (4);
  num = (arg_index_integer (8, 8));
  BF_ofb64_encrypt ((STRING_LOC (input_text, istart)),
		    (STRING_LOC (output_text, ostart)),
		    ilen,
		    (key_arg (6)),
		    (init_vector_arg (7)),
		    (&num));
  PRIMITIVE_RETURN (long_to_integer (num));
}

#ifdef COMPILE_AS_MODULE

char *
DEFUN_VOID (dload_initialize_file)
{
  declare_primitive
    ("BLOWFISH-SET-KEY", Prim_blowfish_set_key, 1, 1,
     "(STRING)\n\
Generate a Blowfish key from STRING.\n\
STRING must be 72 bytes or less in length.\n\
For text-string keys, use MD5 on the text, and pass the digest here.");
  declare_primitive
    ("BLOWFISH-ECB", Prim_blowfish_ecb, 4, 4,
     "(INPUT OUTPUT KEY-VECTOR ENCRYPT?)\n\
Apply Blowfish in Electronic Code Book mode.\n\
INPUT is an 8-byte string.\n\
OUTPUT is an 8-byte string.\n\
KEY is a Blowfish key.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).");
  declare_primitive
    ("BLOWFISH-CBC-V2", Prim_blowfish_cbc, 5, 5,
     "(INPUT OUTPUT KEY INIT-VECTOR ENCRYPT?)\n\
Apply Blowfish in Cipher Block Chaining mode.\n\
INPUT is a string whose length is a multiple of 8 bytes.\n\
OUTPUT is a string whose length is the same as INPUT.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte string; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).");
  declare_primitive
    ("BLOWFISH-CFB64-SUBSTRING-V2", Prim_blowfish_cfb64_substring, 9, 9,
     "(INPUT ISTART IEND OUTPUT OSTART KEY INIT-VECTOR NUM ENCRYPT?)\n\
Apply Blowfish in Cipher Feed-Back mode.\n\
\(INPUT,ISTART,IEND) is an arbitrary substring.\n\
OUTPUT is a string as large as the input substring.\n\
OSTART says where to start writing to the output string.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte string; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
  The initial value must be unique for each message/key pair.\n\
NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the\n\
  number of bytes that have previously been processed in this stream.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).\n\
Returned value is the new value of NUM.");
  declare_primitive
    ("BLOWFISH-OFB64-SUBSTRING", Prim_blowfish_ofb64_substring, 8, 8,
     "(INPUT ISTART IEND OUTPUT OSTART KEY INIT-VECTOR NUM)\n\
Apply Blowfish in Output Feed-Back mode.\n\
(INPUT,ISTART,IEND) is an arbitrary substring.\n\
OUTPUT is a string as large as the input substring.\n\
OSTART says where to start writing to the output string.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte string; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
  The initial value must be unique for each message/key pair.\n\
NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the\n\
  number of bytes that have previously been processed in this stream.\n\
Returned value is the new value of NUM.");
  return "#prbfish";
}

#endif /* COMPILE_AS_MODULE */
