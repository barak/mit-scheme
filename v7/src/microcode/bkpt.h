/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/bkpt.h,v 9.22 1987/04/03 00:08:07 jinx Exp $
 *
 * This file contains breakpoint utilities.
 * Disabled when not debugging the interpreter.
 * It "shadows" definitions in default.h
 *
 */

#ifdef ENABLE_DEBUGGING_TOOLS

struct sp_record
{ Pointer *sp;
  struct sp_record *next;
};
typedef struct sp_record *sp_record_list;

#define sp_nil ((sp_record_list) NULL)
#define debug_maxslots 64

#define Eval_Ucode_Hook()						\
{									\
  local_circle[local_slotno++] = Fetch_Expression();			\
  if (local_slotno >= debug_maxslots) local_slotno = 0;			\
  if (local_nslots < debug_maxslots) local_nslots++;			\
}

#define Pop_Return_Ucode_Hook()						\
{									\
  if (SP_List != sp_nil)						\
  { Export_Registers();							\
    Pop_Return_Break_Point();						\
    Import_Registers();							\
  }									\
}

/* Not implemented yet */

#define Apply_Ucode_Hook()

/* For performance metering we note the time spent handling each
 * primitive.  This MIGHT help us figure out where all the time
 * goes.  It should make the time zone kludge obselete someday.
 */

#if false
/* This code disabled by SAS 6/24/86 */
struct
{ int nprims;
  int primtime[1];
} perfinfo_data;

void Clear_Perfinfo_Data()
{ int i;
  perfinfo_data.nprims = MAX_PRIMITIVE_NUMBER+1;
  for (i=0; i <= MAX_PRIMITIVE_NUMBER; i++) perfinfo_data.primtime[i]=0;
}

#define Metering_Apply_Primitive(Loc, N)				\
{									\
  long Start_Time = Sys_Clock();					\
									\
  Loc = Apply_Primitive(N)						\
  perfinfo_data.primtime[N] += Sys_Clock() - Start_Time;		\
  Set_Time_Zone(Zone_Working);						\
}
#endif
#endif /* ifdef ENABLE_DEBUGGING_TOOLS */

