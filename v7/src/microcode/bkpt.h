/* -*-C-*-

$Id: bkpt.h,v 9.34 2002/07/02 20:48:59 cph Exp $

Copyright (c) 1987-1999, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
*/

/* This file contains breakpoint utilities.
   Disabled when not debugging the interpreter.
   It "shadows" definitions in default.h */

#ifdef ENABLE_DEBUGGING_FLAGS

struct sp_record
{
  SCHEME_OBJECT * sp;
  struct sp_record * next;
};

typedef struct sp_record * sp_record_list;

#define debug_maxslots 100

#define Eval_Ucode_Hook()						\
{									\
  (local_circle [local_slotno++]) = exp_register;			\
  if (local_slotno >= debug_maxslots)					\
    local_slotno = 0;							\
  if (local_nslots < debug_maxslots)					\
    local_nslots += 1;							\
}

#define Pop_Return_Ucode_Hook()						\
{									\
  if (SP_List != 0)							\
  {									\
    Pop_Return_Break_Point ();						\
  }									\
}

/* Not implemented yet */

#define Apply_Ucode_Hook()

#endif /* ENABLE_DEBUGGING_FLAGS */
