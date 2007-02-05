/* -*-C-*-

$Id: bkpt.h,v 9.38 2007/01/05 21:19:25 cph Exp $

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
