/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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
  "(BYTEVECTOR)\n\
Generate a Blowfish key from BYTEVECTOR.\n\
BYTEVECTOR must be 72 bytes or less in length.\n\
For text-string keys, use MD5 on the text, and pass the digest here.")
{
  PRIMITIVE_HEADER (1);
  unsigned long length;
  uint8_t * bytes = (arg_bytevector (1, (&length)));
  if (length > 72)
    error_bad_range_arg (1);
  SCHEME_OBJECT result = (allocate_bytevector (sizeof (BF_KEY)));
  BF_set_key (((BF_KEY *) (BYTEVECTOR_POINTER (result))), length, bytes);
  PRIMITIVE_RETURN (result);
}

static uint8_t *
arg_bytevector_fixlen (unsigned int arg, unsigned long required)
{
  unsigned long length;
  uint8_t * bytes = (arg_bytevector (arg, (&length)));
  if (length != required)
    error_bad_range_arg (arg);
  return bytes;
}

static BF_KEY *
key_arg (unsigned int arg)
{
  return ((BF_KEY *) (arg_bytevector_fixlen (arg, (sizeof (BF_KEY)))));
}

static uint8_t *
init_vector_arg (unsigned int arg)
{
  return (arg_bytevector_fixlen (arg, 8));
}

DEFINE_PRIMITIVE ("BLOWFISH-ECB", Prim_blowfish_ecb, 4, 4,
  "(INPUT OUTPUT KEY-VECTOR ENCRYPT?)\n\
Apply Blowfish in Electronic Code Book mode.\n\
INPUT is an 8-byte bytevector.\n\
OUTPUT is an 8-byte bytevector.\n\
KEY is a Blowfish key.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).")
{
  PRIMITIVE_HEADER (4);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("BLOWFISH-CBC-V2", Prim_blowfish_cbc, 5, 5,
  "(INPUT OUTPUT KEY INIT-VECTOR ENCRYPT?)\n\
Apply Blowfish in Cipher Block Chaining mode.\n\
INPUT is a bytevector whose length is a multiple of 8 bytes.\n\
OUTPUT is a bytevector whose length is the same as INPUT.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte bytevector; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).")
{
  PRIMITIVE_HEADER (5);
  unsigned long input_length;
  uint8_t * input = (arg_bytevector (1, (&input_length)));
  if ((input_length % 8) != 0)
    error_bad_range_arg (1);
  uint8_t * output = (arg_bytevector_fixlen (2, input_length));
  if (output == input)
    error_bad_range_arg (2);
  BF_cbc_encrypt (input,
		  output,
		  input_length,
		  (key_arg (3)),
		  (init_vector_arg (4)),
		  ((BOOLEAN_ARG (5)) ? BF_ENCRYPT : BF_DECRYPT));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("BLOWFISH-CFB64-SUBSTRING-V2", Prim_blowfish_cfb64_substring, 9, 9,
  "(INPUT ISTART IEND OUTPUT OSTART KEY INIT-VECTOR NUM ENCRYPT?)\n\
Apply Blowfish in Cipher Feed-Back mode.\n\
(INPUT,ISTART,IEND) is an arbitrary bytevector range.\n\
OUTPUT is a bytevector.\n\
OSTART says where to start writing in OUTPUT.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte bytevector; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
  The initial value must be unique for each message/key pair.\n\
NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the\n\
  number of bytes that have previously been processed in this stream.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).\n\
Returned value is the new value of NUM.")
{
  PRIMITIVE_HEADER (9);

  unsigned long input_length;
  uint8_t * input = (arg_bytevector (1, (&input_length)));
  unsigned long iend = (arg_ulong_index_integer (3, (input_length + 1)));
  unsigned long istart = (arg_ulong_index_integer (2, (iend + 1)));
  unsigned long ilen = (iend - istart);

  unsigned long output_length;
  uint8_t * output = (arg_bytevector (4, (&output_length)));
  unsigned long ostart = (arg_ulong_index_integer (5, (output_length + 1)));
  unsigned long oend = (ostart + ilen);
  if (oend > output_length)
    error_bad_range_arg (5);
  /* Don't allow overlaps of input and output ranges in same bytevector. */
  if ((output == input) && (ostart < iend) && (istart < oend))
    error_bad_range_arg (4);

  int num = (arg_index_integer (8, 8));
  BF_cfb64_encrypt
    (input, output, ilen, (key_arg (6)), (init_vector_arg (7)), (&num),
     ((BOOLEAN_ARG (9)) ? BF_ENCRYPT : BF_DECRYPT));
  PRIMITIVE_RETURN (long_to_integer (num));
}

DEFINE_PRIMITIVE ("BLOWFISH-OFB64-SUBSTRING", Prim_blowfish_ofb64_substring, 8, 8,
  "(INPUT ISTART IEND OUTPUT OSTART KEY INIT-VECTOR NUM)\n\
Apply Blowfish in Output Feed-Back mode.\n\
(INPUT,ISTART,IEND) is an arbitrary bytevector range.\n\
OUTPUT is a bytevector.\n\
OSTART says where to start writing in OUTPUT.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte bytevector; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
  The initial value must be unique for each message/key pair.\n\
NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the\n\
  number of bytes that have previously been processed in this stream.\n\
Returned value is the new value of NUM.")
{
  PRIMITIVE_HEADER (8);

  unsigned long input_length;
  uint8_t * input = (arg_bytevector (1, (&input_length)));
  unsigned long iend = (arg_ulong_index_integer (3, (input_length + 1)));
  unsigned long istart = (arg_ulong_index_integer (2, (iend + 1)));
  unsigned long ilen = (iend - istart);

  unsigned long output_length;
  uint8_t * output = (arg_bytevector (4, (&output_length)));
  unsigned long ostart = (arg_ulong_index_integer (5, (output_length + 1)));
  unsigned long oend = (ostart + ilen);
  if (oend > output_length)
    error_bad_range_arg (5);
  /* Don't allow overlaps of input and output ranges in same bytevector. */
  if ((output == input) && (ostart < iend) && (istart < oend))
    error_bad_range_arg (4);

  int num = (arg_index_integer (8, 8));
  BF_ofb64_encrypt
    (input, output, ilen, (key_arg (6)), (init_vector_arg (7)), (&num));
  PRIMITIVE_RETURN (long_to_integer (num));
}

#ifdef COMPILE_AS_MODULE

const char *
dload_initialize_file (void)
{
  declare_primitive
    ("BLOWFISH-SET-KEY", Prim_blowfish_set_key, 1, 1,
     "(BYTEVECTOR)\n\
Generate a Blowfish key from BYTEVECTOR.\n\
BYTEVECTOR must be 72 bytes or less in length.\n\
For text-string keys, use MD5 on the text, and pass the digest here.");
  declare_primitive
    ("BLOWFISH-ECB", Prim_blowfish_ecb, 4, 4,
     "(INPUT OUTPUT KEY-VECTOR ENCRYPT?)\n\
Apply Blowfish in Electronic Code Book mode.\n\
INPUT is an 8-byte bytevector.\n\
OUTPUT is an 8-byte bytevector.\n\
KEY is a Blowfish key.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).");
  declare_primitive
    ("BLOWFISH-CBC-V2", Prim_blowfish_cbc, 5, 5,
     "(INPUT OUTPUT KEY INIT-VECTOR ENCRYPT?)\n\
Apply Blowfish in Cipher Block Chaining mode.\n\
INPUT is a bytevector whose length is a multiple of 8 bytes.\n\
OUTPUT is a bytevector whose length is the same as INPUT.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte bytevector; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).");
  declare_primitive
    ("BLOWFISH-CFB64-SUBSTRING-V2", Prim_blowfish_cfb64_substring, 9, 9,
     "(INPUT ISTART IEND OUTPUT OSTART KEY INIT-VECTOR NUM ENCRYPT?)\n\
Apply Blowfish in Cipher Feed-Back mode.\n\
(INPUT,ISTART,IEND) is an arbitrary bytevector range.\n\
OUTPUT is a bytevector.\n\
OSTART says where to start writing in OUTPUT.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte bytevector; it is modified after each call.\n\
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
(INPUT,ISTART,IEND) is an arbitrary bytevector range.\n\
OUTPUT is a bytevector.\n\
OSTART says where to start writing in OUTPUT.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte bytevector; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
  The initial value must be unique for each message/key pair.\n\
NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the\n\
  number of bytes that have previously been processed in this stream.\n\
Returned value is the new value of NUM.");
  return "#prbfish";
}

#endif /* COMPILE_AS_MODULE */
