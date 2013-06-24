/* -*-C-*-

$Id: edwin.h,v 1.9 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* Definitions for Edwin data structures.
   This MUST match the definitions in the Edwin source code. */

#define GROUP_P VECTOR_P
#define GROUP_TEXT(group) (VECTOR_REF ((group), 1))

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
