/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/winder.h,v 9.24 1989/09/20 23:13:09 cph Rel $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* Header file for dynamic winder. */

#define STATE_SPACE_P(object)						\
  ((VECTOR_P (object)) &&						\
   ((VECTOR_LENGTH (object)) == STATE_SPACE_LENGTH) &&			\
   ((MEMORY_REF ((object), STATE_SPACE_TAG)) ==				\
    (Get_Fixed_Obj_Slot (State_Space_Tag))))

#define STATE_SPACE_TAG			1
#define STATE_SPACE_NEAREST_POINT	2
#define STATE_SPACE_LENGTH		2

#define STATE_POINT_P(object)						\
  ((VECTOR_P (object)) &&						\
   ((VECTOR_LENGTH (object)) == STATE_POINT_LENGTH) &&			\
   ((MEMORY_REF ((object), STATE_POINT_TAG)) ==				\
    (Get_Fixed_Obj_Slot (State_Point_Tag))))

#define STATE_POINT_TAG			1
#define STATE_POINT_BEFORE_THUNK	2
#define STATE_POINT_AFTER_THUNK		3
#define STATE_POINT_NEARER_POINT	4
#define STATE_POINT_DISTANCE_TO_ROOT	5
#define STATE_POINT_LENGTH		5


#ifdef butterfly

#define guarantee_state_point()						\
{									\
  if (Current_State_Point == SHARP_F)					\
    Current_State_Point = (Get_Fixed_Obj_Slot (State_Space_Root));	\
}

#else

#define guarantee_state_point()

#endif
