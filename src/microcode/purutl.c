/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

/* Pure/constant space utilities. */

#include "scheme.h"
#include "prims.h"

DEFINE_PRIMITIVE ("PRIMITIVE-IMPURIFY", Prim_impurify, 1, 1,
		  "(OBJECT)\n\
Remove OBJECT from pure space, allowing it to be modified.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ARG_REF (1));
}

DEFINE_PRIMITIVE ("CONSTANT?", Prim_constant_p, 1, 1,
		  "(OBJECT)\n\
Returns #T iff OBJECT is in constant space.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (object_in_constant_space_p (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("PURE?", Prim_pure_p, 1, 1,
		  "(OBJECT)\n\
Returns #T iff OBJECT is in constant space and is 'pure'.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (SHARP_F);
}

bool
object_in_constant_space_p (SCHEME_OBJECT object)
{
  SCHEME_OBJECT * address = (get_object_address (object));
  return ((address != 0) && (ADDRESS_IN_CONSTANT_P (address)));
}

SCHEME_OBJECT *
copy_to_constant_space (SCHEME_OBJECT * source, unsigned long n_words)
{
  SCHEME_OBJECT * result;
  SCHEME_OBJECT * limit;

  if (n_words > (constant_end - constant_alloc_next))
    {
      outf_fatal ("\nInsufficient constant space!\n");
      Microcode_Termination (TERM_NO_SPACE);
    }
  result = constant_alloc_next;
  limit = (constant_alloc_next + n_words);
  while (constant_alloc_next < limit)
    (*constant_alloc_next++) = (*source++);
  return (result);
}
