/* -*-C-*-

$Id: ntenv.c,v 1.13 1996/04/23 20:40:11 cph Exp $

Copyright (c) 1992-96 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "nt.h"
#include "osenv.h"
#include "ntscreen.h"
#include <stdlib.h>
#include <string.h>

time_t
DEFUN_VOID (OS_encoded_time)
{
  time_t t;
  STD_UINT_SYSTEM_CALL (syscall_time, t, (NT_time (0)));
  return (t);
}

void
DEFUN (OS_decode_time, (t, buffer), time_t t AND struct time_structure * buffer)
{
  struct tm * ts;
  STD_PTR_SYSTEM_CALL (syscall_localtime, ts, (NT_localtime (&t)));
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

time_t
DEFUN (OS_encode_time ,(buffer), struct time_structure * buffer)
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
  STD_UINT_SYSTEM_CALL (syscall_mktime, t, (NT_mktime (ts)));
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
    error_system_call ((GetLastError ()), syscall_setitimer);
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
      current_dir_path = (NT_malloc (1024));
      if (current_dir_path == 0)
	error_system_call (ENOMEM, syscall_malloc);
      current_dir_path_size = 1024;
    }
  while (1)
    {
      if ((NT_getcwd (current_dir_path, current_dir_path_size)) != 0)
      {
	strlwr (current_dir_path);
	return (current_dir_path);
      }
#ifdef ERANGE
      if (errno != ERANGE)
	error_system_call (errno, syscall_getcwd);
#endif
      current_dir_path_size *= 2;
      {
	char * new_current_dir_path =
	  (NT_realloc (current_dir_path, current_dir_path_size));
	if (new_current_dir_path == 0)
	  /* ANSI C requires `path' to be unchanged -- we may have to
	     discard it for systems that don't behave thus. */
	  error_system_call (ENOMEM, syscall_realloc);
	current_dir_path = new_current_dir_path;
      }
    }
}

void
DEFUN (OS_set_working_dir_pathname, (name), char * name)
{
  size_t name_size = (strlen (name));
  char * filename = name;

  STD_BOOL_SYSTEM_CALL (syscall_chdir, (SetCurrentDirectory (filename)));

  while (1)
  {
    if (name_size < current_dir_path_size)
    {
      strcpy(current_dir_path, name);
      return;
    }
    current_dir_path_size *= 2;
    {
      char * new_current_dir_path =
	(NT_realloc (current_dir_path, current_dir_path_size));
      if (new_current_dir_path == 0)
	error_system_call (ENOMEM, syscall_realloc);
      current_dir_path = new_current_dir_path;
    }
  }
}
