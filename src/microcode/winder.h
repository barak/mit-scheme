/* -*-C-*-

$Id: winder.h,v 9.26 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1987, 1988, 1989, 1999 Massachusetts Institute of Technology

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
