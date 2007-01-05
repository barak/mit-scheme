/* -*-C-*-

Copyright (c) 1987, 1988, 1999 Massachusetts Institute of Technology

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

/* $Id: zones.h,v 9.29 2007/01/05 15:33:08 cph Exp $
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
