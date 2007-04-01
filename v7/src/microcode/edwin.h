/* -*-C-*-

$Id: edwin.h,v 1.14 2007/04/01 17:33:07 riastradh Exp $

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

/* Definitions for Edwin data structures.
   This MUST match the definitions in the Edwin source code. */

#define GROUP_P VECTOR_P
#define GROUP_TEXT(group) (VECTOR_REF ((group), 1))

#define GROUP_TEXT_LOC(group, offset)					\
  (((unsigned char *) (integer_to_ulong (GROUP_TEXT (group)))) + (offset))

#define GROUP_GAP_START(group)						\
  (UNSIGNED_FIXNUM_TO_LONG (VECTOR_REF ((group), 2)))

#define GROUP_GAP_LENGTH(group)						\
  (UNSIGNED_FIXNUM_TO_LONG (VECTOR_REF ((group), 3)))

#define GROUP_GAP_END(group)						\
  (UNSIGNED_FIXNUM_TO_LONG (VECTOR_REF ((group), 4)))

#define GROUP_START_MARK(group) (VECTOR_REF ((group), 6))
#define GROUP_END_MARK(group) (VECTOR_REF ((group), 7))
#define GROUP_MODIFIED_P(group) (VECTOR_REF ((group), 16))

#define MARK_P RECORD_P
#define MARK_GROUP(mark) (VECTOR_REF ((mark), 1))
#define MARK_INDEX(mark) (UNSIGNED_FIXNUM_TO_LONG (VECTOR_REF ((mark), 2)))
#define MARK_LEFT_INSERTING(mark) ((VECTOR_REF ((mark), 3)) != SHARP_F)
