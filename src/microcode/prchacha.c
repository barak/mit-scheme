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

#include "chacha.h"
#include "prims.h"

static void
do_chacha_core(void (*core)(uint8_t *, const uint8_t *, const uint8_t *,
			    const uint8_t *))
{
    uint8_t * output;
    unsigned long offset;
    const uint8_t * input;
    const uint8_t * key;
    const uint8_t * constant;
    unsigned long noutput, ninput, nkey, nconstant;

    output = (arg_bytevector (1, (&noutput)));
    if (noutput < 64)
      error_bad_range_arg (1);

    offset = (arg_ulong_index_integer (2, noutput - 63));

    input = (arg_bytevector (3, (&ninput)));
    if (ninput != 16)
      error_bad_range_arg (3);

    key = (arg_bytevector (4, (&nkey)));
    if (nkey != 32)
      error_bad_range_arg (4);

    constant = (arg_bytevector (5, (&nconstant)));
    if (nconstant != 16)
      error_bad_range_arg (5);

    (*core)(output, input, key, constant);
}

DEFINE_PRIMITIVE ("CHACHA8-CORE", Prim_chacha8_core, 5, 5,
  "(OUTPUT OFFSET INPUT KEY CONSTANT)\n\
Compute the ChaCha8 core hash function:\n\
OUTPUT[OFFSET, OFFSET+1, ..., OFFSET+63] := ChaCha8(INPUT, KEY, CONST).")
{
  PRIMITIVE_HEADER (1);
  do_chacha_core(&chacha8_core);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("CHACHA12-CORE", Prim_chacha12_core, 5, 5,
  "(OUTPUT OFFSET INPUT KEY CONSTANT)\n\
Compute the ChaCha12 core hash function:\n\
OUTPUT[OFFSET, OFFSET+1, ..., OFFSET+63] := ChaCha12(INPUT, KEY, CONST).")
{
  PRIMITIVE_HEADER (1);
  do_chacha_core(&chacha12_core);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("CHACHA20-CORE", Prim_chacha20_core, 5, 5,
  "(OUTPUT OFFSET INPUT KEY CONSTANT)\n\
Compute the ChaCha20 core hash function:\n\
OUTPUT[OFFSET, OFFSET+1, ..., OFFSET+63] := ChaCha20(INPUT, KEY, CONST).")
{
  PRIMITIVE_HEADER (1);
  do_chacha_core(&chacha20_core);
  PRIMITIVE_RETURN (UNSPECIFIC);
}
