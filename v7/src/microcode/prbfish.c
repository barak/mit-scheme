/* -*-C-*-

$Id: prbfish.c,v 1.4 1999/01/02 06:11:34 cph Exp $

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

/* Interface to Blowfish library */

#include "scheme.h"
#include "prims.h"
#include <blowfish.h>

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

DEFINE_PRIMITIVE ("BLOWFISH-CBC", Prim_blowfish_cbc, 4, 4,
  "(INPUT KEY INIT-VECTOR ENCRYPT?)\n\
Apply Blowfish in Cipher Block Chaining mode.\n\
INPUT is a string whose length is a multiple of 8 bytes.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte string; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).\n\
Returned value is a string of the same length as INPUT.")
{
  SCHEME_OBJECT input_text;
  BF_KEY * key;
  unsigned char * init_vector;
  SCHEME_OBJECT output_text;
  PRIMITIVE_HEADER (4);

  CHECK_ARG (1, STRING_P);
  input_text = (ARG_REF (1));
  if (((STRING_LENGTH (input_text)) % 8) != 0)
    error_bad_range_arg (1);
  key = (key_arg (2));
  init_vector = (init_vector_arg (3));
  output_text = (allocate_string (STRING_LENGTH (input_text)));
  BF_cbc_encrypt ((STRING_LOC (input_text, 0)),
		  (STRING_LOC (output_text, 0)),
		  (STRING_LENGTH (input_text)),
		  key,
		  init_vector,
		  (BOOLEAN_ARG (4)));
  PRIMITIVE_RETURN (output_text);
}

DEFINE_PRIMITIVE ("BLOWFISH-CFB64", Prim_blowfish_cfb64, 5, 5,
  "(INPUT KEY INIT-VECTOR NUM ENCRYPT?)\n\
Apply Blowfish in Cipher FeedBack mode.\n\
INPUT is an arbitrary string.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte string; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the\n\
  number of bytes that have previously been processed in this stream.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).\n\
Returned value is a string of the same length as INPUT.")
{
  SCHEME_OBJECT input_text;
  BF_KEY * key;
  unsigned char * init_vector;
  int num;
  SCHEME_OBJECT output_text;
  PRIMITIVE_HEADER (5);

  CHECK_ARG (1, STRING_P);
  input_text = (ARG_REF (1));
  key = (key_arg (2));
  init_vector = (init_vector_arg (3));
  num = (arg_index_integer (4, 8));
  output_text = (allocate_string (STRING_LENGTH (input_text)));
  BF_cfb64_encrypt ((STRING_LOC (input_text, 0)),
		    (STRING_LOC (output_text, 0)),
		    (STRING_LENGTH (input_text)),
		    key,
		    init_vector,
		    (&num),
		    (BOOLEAN_ARG (5)));
  PRIMITIVE_RETURN (output_text);
}

DEFINE_PRIMITIVE ("BLOWFISH-CFB64-SUBSTRING", Prim_blowfish_cfb64_substring, 7, 7,
  "(INPUT START END KEY INIT-VECTOR NUM ENCRYPT?)\n\
Apply Blowfish in Cipher FeedBack mode.\n\
(INPUT,START,END) is an arbitrary substring.\n\
KEY is a Blowfish key.\n\
INIT-VECTOR is an 8-byte string; it is modified after each call.\n\
  The value from any call may be passed in to a later call.\n\
NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the\n\
  number of bytes that have previously been processed in this stream.\n\
ENCRYPT? says whether to encrypt (#T) or decrypt (#F).\n\
Returned value is a string of the same length as INPUT.")
{
  SCHEME_OBJECT input_text;
  unsigned long l;
  unsigned long start;
  unsigned long end;
  BF_KEY * key;
  unsigned char * init_vector;
  int num;
  SCHEME_OBJECT output_text;
  PRIMITIVE_HEADER (7);

  CHECK_ARG (1, STRING_P);
  input_text = (ARG_REF (1));
  l = (STRING_LENGTH (input_text));
  start = (arg_ulong_index_integer (2, l));
  end = (arg_integer_in_range (3, start, (l + 1)));
  key = (key_arg (4));
  init_vector = (init_vector_arg (5));
  num = (arg_index_integer (6, 8));
  output_text = (allocate_string (end - start));
  BF_cfb64_encrypt ((STRING_LOC (input_text, start)),
		    (STRING_LOC (output_text, 0)),
		    (end - start),
		    key,
		    init_vector,
		    (&num),
		    (BOOLEAN_ARG (7)));
  PRIMITIVE_RETURN (output_text);
}
