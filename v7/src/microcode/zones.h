/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/zones.h,v 9.24 1990/06/20 17:42:58 cph Rel $
 *
 * Metering stuff.
 * We break all times into time zones suitable for external analysis.
 * Primitives may be included for accessing this information if desired
 * by supplying additional files.
 */

#ifdef METERING
extern long New_Time, Old_Time, Time_Meters[], Current_Zone;

#ifdef ENABLE_DEBUGGING_TOOLS
#define Set_Time_Zone(Zone)						\
{									\
  New_Time = (OS_process_clock ());					\
  Time_Meters[Current_Zone] += New_Time-Old_Time;			\
  Old_Time = New_Time;							\
  Current_Zone = Zone;							\
}
#else
#define Set_Time_Zone(Zone) Current_Zone = Zone;
#endif

#define Save_Time_Zone(Zone)	Saved_Zone = Current_Zone; Set_Time_Zone(Zone);
#define Restore_Time_Zone()	Set_Time_Zone(Saved_Zone);
#else
#define Set_Time_Zone(Zone)
#define Save_Time_Zone(Zone)
#define Restore_Time_Zone()
#endif

#define Zone_Working 0
#define Zone_GetWork 1
#define Zone_TTY_IO 2
#define Zone_Disk_IO 3
#define Zone_Purify 4
#define Zone_GCLoop 5
#define Zone_Global_Int 6
#define Zone_Store_Lock 7
#define Zone_Math 8
#define Zone_GCIdle 9
#define Zone_Lookup 10
#define Zone_Scheduler 11

#define Max_Meters 20
