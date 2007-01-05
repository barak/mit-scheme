/* -*-C-*-

$Id: ntenv.c,v 1.23 2007/01/05 21:19:25 cph Exp $

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

#include "scheme.h"
#include "nt.h"
#include "osenv.h"
#include "ntscreen.h"
#include <stdlib.h>
#include <string.h>

extern unsigned long file_time_to_unix_time (FILETIME *);
extern void unix_time_to_file_time (unsigned long, FILETIME *);

static unsigned long
system_time_to_unix_time (SYSTEMTIME * st)
{
  FILETIME ft;
  (void) SystemTimeToFileTime (st, (&ft));
  return (file_time_to_unix_time (&ft));
}

#if 0
static void
unix_time_to_system_time (unsigned long ut, SYSTEMTIME * st)
{
  FILETIME ft;
  unix_time_to_file_time (ut, (&ft));
  (void) FileTimeToSystemTime ((&ft), st);
}
#endif

time_t
DEFUN_VOID (OS_encoded_time)
{
  SYSTEMTIME t;
  GetSystemTime (&t);
  return (system_time_to_unix_time (&t));
}

void
OS_decode_time (time_t t, struct time_structure * buffer)
{
  struct tm * ts;
  STD_PTR_UNIX_CALL (ts, localtime, (&t));
  (buffer -> year) = ((ts -> tm_year) + 1900);
  (buffer -> month) = ((ts -> tm_mon) + 1);
  (buffer -> day) = (ts -> tm_mday);
  (buffer -> hour) = (ts -> tm_hour);
  (buffer -> minute) = (ts -> tm_min);
  (buffer -> second) = (ts -> tm_sec);
  (buffer -> daylight_savings_time) = (ts -> tm_isdst);
  /* I'm assuming that `timezone' is implemented by the C library;
     this might need conditionalization.  -- cph */
  (buffer -> time_zone) = timezone;
  {
    /* In localtime() encoding, 0 is Sunday; in ours, it's Monday. */
    int wday = (ts -> tm_wday);
    (buffer -> day_of_week) = ((wday == 0) ? 6 : (wday - 1));
  }
}

void
OS_decode_utc (time_t t, struct time_structure * buffer)
{
  struct tm * ts;
  STD_PTR_UNIX_CALL (ts, gmtime, (&t));
  (buffer -> year) = ((ts -> tm_year) + 1900);
  (buffer -> month) = ((ts -> tm_mon) + 1);
  (buffer -> day) = (ts -> tm_mday);
  (buffer -> hour) = (ts -> tm_hour);
  (buffer -> minute) = (ts -> tm_min);
  (buffer -> second) = (ts -> tm_sec);
  (buffer -> daylight_savings_time) = (ts -> tm_isdst);
  (buffer -> time_zone) = 0;
  {
    /* In gmtime() encoding, 0 is Sunday; in ours, it's Monday. */
    int wday = (ts -> tm_wday);
    (buffer -> day_of_week) = ((wday == 0) ? 6 : (wday - 1));
  }
}

#if 0
/* This nice implementation can't be used because it only works under
   Windows NT.  */
void
OS_decode_time (time_t t, struct time_structure * buffer)
{
  SYSTEMTIME st;
  SYSTEMTIME lst;
  TIME_ZONE_INFORMATION tzi;
  unix_time_to_system_time (t, (&st));
  switch (GetTimeZoneInformation (&tzi))
    {
    case TIME_ZONE_ID_STANDARD:
      (buffer -> daylight_savings_time) = 0;
      (buffer -> time_zone) = (tzi . Bias);
      break;
    case TIME_ZONE_ID_DAYLIGHT:
      (buffer -> daylight_savings_time) = 1;
      (buffer -> time_zone) = (tzi . Bias);
      break;
    default:
      (buffer -> daylight_savings_time) = -1;
      (buffer -> time_zone) = 0;
      break;
    }
  if (((buffer -> daylight_savings_time) == 0) && ((buffer -> time_zone) == 0))
    lst = st;
  else
    (void) SystemTimeToTzSpecificLocalTime ((&tzi), (&st), (&lst));
  (buffer -> year) = (lst . wYear);
  (buffer -> month) = (lst . wMonth);
  (buffer -> day) = (lst . wDay);
  (buffer -> hour) = (lst . wHour);
  (buffer -> minute) = (lst . wMinute);
  (buffer -> second) = (lst . wSecond);
  {
    /* In SYSTEMTIME encoding, 0 is Sunday; in ours, it's Monday. */
    int wday = (lst . wDayOfWeek);
    (buffer -> day_of_week) = ((wday == 0) ? 6 : (wday - 1));
  }
}
#endif

time_t
OS_encode_time (struct time_structure * buffer)
{
  time_t t;
  struct tm ts_s, * ts;
  ts = &ts_s;
  (ts -> tm_year) = ((buffer -> year) - 1900);
  (ts -> tm_mon) = ((buffer -> month) - 1);
  (ts -> tm_mday) = (buffer -> day);
  (ts -> tm_hour) = (buffer -> hour);
  (ts -> tm_min) = (buffer -> minute);
  (ts -> tm_sec) = (buffer -> second);
  (ts -> tm_isdst) = (buffer -> daylight_savings_time);
  STD_UINT_UNIX_CALL (t, mktime, (ts));
  /* mktime assumes its argument is local time, and converts it to
     UTC; if the specified time zone is different, adjust the result.  */
  if (((buffer -> time_zone) != INT_MAX)
      && ((buffer -> time_zone) != timezone))
    t = ((t - timezone) + (buffer -> time_zone));
  return (t);
}

#define FILETIME_TO_MSECS(ft) \
  (((4294967296.0 * (double)  ft.dwHighDateTime) + ft.dwLowDateTime)*100e-6)

double
DEFUN_VOID (OS_process_clock)
{
  /* This must not signal an error in normal use. */
  /* Return answer in milliseconds, was in 1/100th seconds */

  FILETIME  creation_time, exit_time, kernel_time, user_time;
  if(GetProcessTimes(GetCurrentProcess(),
		     &creation_time, &exit_time, &kernel_time, &user_time)) {
    return  FILETIME_TO_MSECS(user_time) + FILETIME_TO_MSECS(kernel_time);
  } else {
    return ((((double) (clock ())) * 1000.0) / ((double) CLOCKS_PER_SEC));
  }
}

double
DEFUN_VOID (OS_real_time_clock)
{
  return ((((double) (clock ())) * 1000.0) / ((double) CLOCKS_PER_SEC));
}

/* The timers are all the same.
   This just provides three distinct timers.
 */

#define TIMER_ID_REAL		(TIMER_ID_BASE + 2)
#define TIMER_ID_PROFILE	(TIMER_ID_BASE + 1)
#define TIMER_ID_PROCESS	(TIMER_ID_BASE + 0)

#ifdef USE_WM_TIMER

#define TIMER_ID_BASE		0x100

enum timer_next
{
  timer_next_none,
  timer_next_normal,
  timer_next_disable,
  timer_next_set_period
};

struct timer_state_s
{
  int local_id;
  int global_id;
  clock_t period;
  enum timer_next next;
};

struct timer_state_s scheme_timers[3] =
{
  { TIMER_ID_PROCESS, 0, 0, timer_next_none, },
  { TIMER_ID_PROFILE, 0, 0, timer_next_none, },
  { TIMER_ID_REAL,    0, 0, timer_next_none, },
};

extern HANDLE master_tty_window;

static void
DEFUN (clear_timer, (timer_id), int timer_id)
{
  struct timer_state_s * timer = &scheme_timers[timer_id - TIMER_ID_BASE];
  if (timer->global_id != 0)
    KillTimer (master_tty_window, timer->global_id);
  timer->global_id = 0;
  timer->next = timer_next_none;
  return;
}

extern VOID /* CALLBACK */ EXFUN (TimerProc, (HWND, UINT, UINT, DWORD));

#define THE_TIMER_PROC ((TIMERPROC) NULL) /* TimerProc */

VOID /* CALLBACK */
DEFUN (TimerProc, (hwnd, umsg, timer_id, dwtime),
       HWND hwnd AND UINT umsg AND UINT timer_id AND DWORD dwtime)
{
  if (hwnd == master_tty_window)
  {
    struct timer_state_s * timer;

    REQUEST_INTERRUPT (INT_Timer);
    timer = &scheme_timers[timer_id - TIMER_ID_BASE];
    switch (timer->next)
    {
    case timer_next_set_period:
      // clear_timer (timer_id);
      timer->global_id = (SetTimer (master_tty_window,
				    timer->local_id,
				    timer->period,
				    THE_TIMER_PROC));
      timer->next = timer_next_normal;
      break;
      
    case timer_next_normal:
      break;

    case timer_next_none:
    case timer_next_disable:
    default:      
      clear_timer (timer_id);
      break;
    }
  }
  return;
}

static void
DEFUN (set_timer, (timer_id, first, interval),
       int timer_id AND clock_t first AND clock_t interval)
{
  struct timer_state_s * timer = &scheme_timers[timer_id - TIMER_ID_BASE];
  if (timer->global_id != 0)
  {
    KillTimer (master_tty_window, timer->global_id);
    timer->global_id = 0;
  }
  
  timer->period = interval;
  if ((first == 0) || (interval == first))
    timer->next = timer_next_normal;
  else if (interval == 0)
    timer->next = timer_next_disable;
  else
    timer->next = timer_next_set_period;
  timer->global_id = (SetTimer (master_tty_window,
				timer->local_id,
				((first == 0) ? interval : first),
				THE_TIMER_PROC));
  if (timer->global_id == 0)
  {
    timer->next = timer_next_none;
    NT_error_api_call ((GetLastError ()), apicall_SetTimer);
  }
  return;
}

#else /* not USE_WM_TIMER */

#define TIMER_ID_BASE		0

struct timer_state_s
{
  int counter;
  int reload;
};

struct timer_state_s scheme_timers[3] = { { 0, 0, }, { 0, 0, }, { 0, 0, } };

extern void EXFUN (low_level_timer_tick, (void));

void
DEFUN_VOID (low_level_timer_tick)
{
  int i;
  int number_signalled = 0;

  for (i = 0; i < 3; i++)
    if (scheme_timers[i].counter != 0)
    {
      scheme_timers[i].counter -= 1;
      if (scheme_timers[i].counter == 0)
      {
	scheme_timers[i].counter = scheme_timers[i].reload;
	number_signalled += 1;
      }
    }

  if (number_signalled != 0)
    REQUEST_INTERRUPT (INT_Timer);
  return;
}

static void
DEFUN (set_timer, (timer_id, first, interval),
       int timer_id AND clock_t first AND clock_t interval)
{
  struct timer_state_s * timer = &scheme_timers[timer_id];

  /* Round up. */ 
  timer->counter = ((first + 49) / 50);
  timer->reload = ((interval + 49) / 50);
  return;
}

static void
DEFUN (clear_timer, (timer_id), int timer_id)
{
  struct timer_state_s * timer = &scheme_timers[timer_id];

  timer->counter = 0;
  timer->reload = 0;
  return;
}

#endif /* USE_WM_TIMER */

void
DEFUN (OS_process_timer_set, (first, interval),
       clock_t first AND clock_t interval)
{
  set_timer (TIMER_ID_PROCESS, first, interval);
  return;
}

void
DEFUN_VOID (OS_process_timer_clear)
{
  clear_timer (TIMER_ID_PROCESS);
  return;
}

void
DEFUN (OS_profile_timer_set, (first, interval),
       clock_t first AND clock_t interval)
{
  set_timer (TIMER_ID_PROFILE, first, interval);
  return;
}

void
DEFUN_VOID (OS_profile_timer_clear)
{
  clear_timer (TIMER_ID_PROFILE);
  return;
}

void
DEFUN (OS_real_timer_set, (first, interval),
       clock_t first AND clock_t interval)
{
  set_timer (TIMER_ID_REAL, first, interval);
  return;
}

void
DEFUN_VOID (OS_real_timer_clear)
{
  clear_timer (TIMER_ID_REAL);
  return;
}

static size_t current_dir_path_size = 0;
static char * current_dir_path = 0;

CONST char *
DEFUN_VOID (OS_working_dir_pathname)
{
  if (current_dir_path) {
    return (current_dir_path);
  }
  if (current_dir_path_size == 0)
    {
      current_dir_path = (OS_malloc (1024));
      current_dir_path_size = 1024;
    }
  while (1)
    {
      if ((getcwd (current_dir_path, current_dir_path_size)) != 0)
      {
	strlwr (current_dir_path);
	return (current_dir_path);
      }
#ifdef ERANGE
      if (errno != ERANGE)
	NT_error_unix_call (errno, syscall_getcwd);
#endif
      current_dir_path_size *= 2;
      {
	char * new_current_dir_path
	  = (OS_realloc (current_dir_path, current_dir_path_size));
	current_dir_path = new_current_dir_path;
      }
    }
}

void
DEFUN (OS_set_working_dir_pathname, (name), CONST char * name)
{
  size_t name_size = (strlen (name));
  CONST char * filename = name;

  STD_BOOL_API_CALL (SetCurrentDirectory, (filename));

  while (1)
  {
    if (name_size < current_dir_path_size)
    {
      strcpy(current_dir_path, name);
      return;
    }
    current_dir_path_size *= 2;
    {
      char * new_current_dir_path
	= (OS_realloc (current_dir_path, current_dir_path_size));
      current_dir_path = new_current_dir_path;
    }
  }
}
