/* -*-C-*-

$Id: prbfish.c,v 1.1 1997/06/09 07:43:36 cph Exp $

Copyright (c) 1997 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* Interface to Blowfish library */

#include "scheme.h"
#include "prims.h"
#include <blowfish.h>

/* This interface uses the Blowfish library from SSLeay.  */

DEFINE_PRIMITIVE ("BLOWFISH-SET-KEY", Prim_blowfish_set_key, 1, 1,
  "(STRING)
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
  "(INPUT KEY INIT-VECTOR NUM ENCRYPT?)
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
