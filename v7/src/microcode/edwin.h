/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/edwin.h,v 1.5 1991/04/02 19:45:17 cph Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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

#define MARK_GROUP(mark) (VECTOR_REF ((mark), 1))
#define MARK_INDEX(mark) (UNSIGNED_FIXNUM_TO_LONG (VECTOR_REF ((mark), 2)))
#define MARK_LEFT_INSERTING(mark) ((VECTOR_REF ((mark), 3)) != SHARP_F)
