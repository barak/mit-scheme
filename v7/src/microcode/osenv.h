/* -*-C-*-

$Id: osenv.h,v 1.16 2008/01/30 20:02:17 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

#ifndef SCM_OSENV_H
#define SCM_OSENV_H

#include "os.h"

struct time_structure
{
  unsigned int year;
  unsigned int month;
  unsigned int day;
  unsigned int hour;
  unsigned int minute;
  unsigned int second;
  unsigned int day_of_week;
  int daylight_savings_time;
  int time_zone;
};

extern time_t OS_encoded_time (void);
extern void OS_decode_time (time_t, struct time_structure *);
extern void OS_decode_utc (time_t, struct time_structure *);
extern time_t OS_encode_time (struct time_structure *);
extern double OS_process_clock (void);
extern double OS_real_time_clock (void);
extern void OS_process_timer_set (clock_t, clock_t);
extern void OS_process_timer_clear (void);
extern void OS_profile_timer_set (clock_t, clock_t);
extern void OS_profile_timer_clear (void);
extern void OS_real_timer_set (clock_t, clock_t);
extern void OS_real_timer_clear (void);
extern const char * OS_working_dir_pathname (void);
extern void OS_set_working_dir_pathname (const char *);

#endif /* SCM_OSENV_H */
