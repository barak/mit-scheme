/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* This file contains breakpoint utility definitions.  */

#ifdef ENABLE_DEBUGGING_TOOLS

struct sp_record
{
  SCHEME_OBJECT * sp;
  struct sp_record * next;
};

typedef struct sp_record * sp_record_list;
extern sp_record_list SP_List;

#define DEBUG_MAXSLOTS 100

#define EVAL_UCODE_HOOK() do						\
{									\
  (local_circle [local_slotno++]) = GET_EXP;				\
  if (local_slotno >= DEBUG_MAXSLOTS)					\
    local_slotno = 0;							\
  if (local_nslots < DEBUG_MAXSLOTS)					\
    local_nslots += 1;							\
} while (0)

#define POP_RETURN_UCODE_HOOK() do					\
{									\
  if (SP_List != 0)							\
    Pop_Return_Break_Point ();						\
} while (0)

#endif /* ENABLE_DEBUGGING_TOOLS */
