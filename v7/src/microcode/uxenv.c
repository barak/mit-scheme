/* -*-C-*-

$Id: uxenv.c,v 1.22 2002/11/20 19:46:15 cph Exp $

Copyright (c) 1990-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

#include "ux.h"
#include "osenv.h"
#include "config.h"		/* For TRUE/FALSE & true/false */

time_t
DEFUN_VOID (OS_encoded_time)
{
  time_t t;
  STD_UINT_SYSTEM_CALL (syscall_time, t, (UX_time (0)));
  return (t);
}

void
DEFUN (OS_decode_time, (t, buffer), time_t t AND struct time_structure * buffer)
{
  struct tm * ts;
  STD_PTR_SYSTEM_CALL (syscall_localtime, ts, (UX_localtime (&t)));
  (buffer -> year) = ((ts -> tm_year) + 1900);
  (buffer -> month) = ((ts -> tm_mon) + 1);
  (buffer -> day) = (ts -> tm_mday);
  (buffer -> hour) = (ts -> tm_hour);
  (buffer -> minute) = (ts -> tm_min);
  (buffer -> second) = (ts -> tm_sec);
  (buffer -> daylight_savings_time) = (ts -> tm_isdst);
#ifdef HAVE_TM_GMTOFF
  /* tm_gmtoff is in minutes east of UTC; we need minutes west.  */
  (buffer -> time_zone) = (- (ts -> TM_GMTOFF));
  if ((ts -> tm_isdst) > 0)
    (buffer -> time_zone) += 3600;
#else
#ifdef HAVE_TIMEZONE
  (buffer -> time_zone) = TIMEZONE;
#else
  (buffer -> time_zone) = INT_MAX;
#endif
#endif
  {
    /* In localtime() encoding, 0 is Sunday; in ours, it's Monday. */
    int wday = (ts -> tm_wday);
    (buffer -> day_of_week) = ((wday == 0) ? 6 : (wday - 1));
  }
}

void
DEFUN (OS_decode_utc, (t, buffer), time_t t AND struct time_structure * buffer)
{
  struct tm * ts;
  STD_PTR_SYSTEM_CALL (syscall_gmtime, ts, (UX_gmtime (&t)));
  (buffer -> year) = ((ts -> tm_year) + 1900);
  (buffer -> month) = ((ts -> tm_mon) + 1);
  (buffer -> day) = (ts -> tm_mday);
  (buffer -> hour) = (ts -> tm_hour);
  (buffer -> minute) = (ts -> tm_min);
  (buffer -> second) = (ts -> tm_sec);
  (buffer -> daylight_savings_time) = 0;
  (buffer -> time_zone) = 0;
  {
    /* In gmtime() encoding, 0 is Sunday; in ours, it's Monday. */
    int wday = (ts -> tm_wday);
    (buffer -> day_of_week) = ((wday == 0) ? 6 : (wday - 1));
  }
}

time_t
DEFUN (OS_encode_time, (buffer), struct time_structure * buffer)
{
#ifdef HAVE_MKTIME
  time_t t = 0;
  struct tm ts;
  (ts . tm_year) = ((buffer -> year) - 1900);
  (ts . tm_mon) = ((buffer -> month) - 1);
  (ts . tm_mday) = (buffer -> day);
  (ts . tm_hour) = (buffer -> hour);
  (ts . tm_min) = (buffer -> minute);
  (ts . tm_sec) = (buffer -> second);
  (ts . tm_isdst) = (buffer -> daylight_savings_time);
  STD_UINT_SYSTEM_CALL (syscall_mktime, t, (UX_mktime (&ts)));

  /* mktime assumes its argument is local time, and converts it to
     UTC; if the specified time zone is different, adjust the result.  */
#ifdef HAVE_TM_GMTOFF
  {
    if ((buffer -> time_zone) != INT_MAX)
      {
	long assumed_zone = (- (ts . TM_GMTOFF));
	if ((ts . tm_isdst) > 0)
	  assumed_zone += 3600;
	if ((buffer -> time_zone) != assumed_zone)
	  t = ((t - assumed_zone) + (buffer -> time_zone));
      }
  }
#else /* not HAVE_TM_GMTOFF */
#ifdef HAVE_TIMEZONE
  if (((buffer -> time_zone) != INT_MAX)
      && ((buffer -> time_zone) != TIMEZONE))
    t = ((t - TIMEZONE) + (buffer -> time_zone));
#endif /* HAVE_TIMEZONE */
#endif /* not HAVE_TM_GMTOFF */

  return (t);

#else /* not HAVE_MKTIME */
  error_system_call (ENOSYS, syscall_mktime);
  return (0);
#endif /* not HAVE_MKTIME */
}

static void
DEFUN_VOID (initialize_timezone)
{
#ifdef __CYGWIN__
  tzset ();
#endif  
}

#ifdef HAVE_TIMES

static clock_t initial_process_clock;

#ifdef __linux__
/* Linux seems to record the time in an unusual way.
   Time that Scheme programs spend computing do not seem to be recorded
   as "user" time, but as "system" time.  So return the sum of both times.  */
#define PROCESS_TIME(buffer) (((buffer) . tms_utime) + ((buffer) . tms_stime))
#else
#define PROCESS_TIME(buffer) ((buffer) . tms_utime)
#endif

static void
DEFUN_VOID (initialize_process_clock)
{
  struct tms buffer;
  UX_times (&buffer);
  initial_process_clock = (PROCESS_TIME (buffer));
}

double
DEFUN_VOID (OS_process_clock)
{
  double ct = ((double) (UX_SC_CLK_TCK ()));
  struct tms buffer;
  /* Was STD_VOID_SYSTEM_CALL, but at least one version of Ultrix
     returns negative numbers other than -1 when there are no errors.  */
  while ((UX_times (&buffer)) == (-1))
    if (errno != EINTR)
      error_system_call (errno, syscall_times);
  return
    (((((double) ((PROCESS_TIME (buffer)) - initial_process_clock)) * 2000.0)
      + ct)
     / (2.0 * ct));
}

#else /* not HAVE_TIMES */

static void
DEFUN_VOID (initialize_process_clock)
{
}

double
DEFUN_VOID (OS_process_clock)
{
  /* This must not signal an error in normal use. */
  return (0.0);
}

#endif /* HAVE_TIMES */

#ifdef HAVE_GETTIMEOFDAY

static struct timeval initial_rtc;

static void
DEFUN_VOID (initialize_real_time_clock)
{
  struct timezone tz;
  UX_gettimeofday ((&initial_rtc), (&tz));
}

double
DEFUN_VOID (OS_real_time_clock)
{
  struct timeval rtc;
  struct timezone tz;
  STD_VOID_SYSTEM_CALL
    (syscall_gettimeofday, (UX_gettimeofday ((&rtc), (&tz))));
  return
    ((((double) ((rtc . tv_sec) - (initial_rtc . tv_sec))) * 1000.0) +
     ((((double) ((rtc . tv_usec) - (initial_rtc . tv_usec))) + 500.0)
      / 1000.0));
}

#else /* not HAVE_GETTIMEOFDAY */
#ifdef HAVE_TIMES

static clock_t initial_rtc;

static void
DEFUN_VOID (initialize_real_time_clock)
{
  struct tms buffer;
  initial_rtc = (UX_times (&buffer));
}

double
DEFUN_VOID (OS_real_time_clock)
{
  double ct = ((double) (UX_SC_CLK_TCK ()));
  struct tms buffer;
  clock_t t;
  /* Was STD_UINT_SYSTEM_CALL, but at least one version of Ultrix
     returns negative numbers other than -1 when there are no errors.  */
  while ((t = (UX_times (&buffer))) == (-1))
    if (errno != EINTR)
      error_system_call (errno, syscall_times);
  return (((((double) (t - initial_rtc)) * 2000.0) + ct) / (2.0 * ct));
}

#else /* not HAVE_TIMES */

static time_t initial_rtc;

static void
DEFUN_VOID (initialize_real_time_clock)
{
  initial_rtc = (time (0));
}

double
DEFUN_VOID (OS_real_time_clock)
{
  time_t t;
  STD_UINT_SYSTEM_CALL (syscall_time, t, (UX_time (0)));
  return (((double) (t - initial_rtc)) * 1000.0);
}

#endif /* HAVE_TIMES */
#endif /* HAVE_GETTIMEOFDAY */

#ifdef HAVE_SETITIMER

static void
DEFUN (set_timer, (which, first, interval),
       int which AND
       clock_t first AND
       clock_t interval)
{
  struct itimerval value;
  struct itimerval ovalue;
  (value . it_value . tv_sec) = (first / 1000);
  (value . it_value . tv_usec) = ((first % 1000) * 1000);
  (value . it_interval . tv_sec) = (interval / 1000);
  (value . it_interval . tv_usec) = ((interval % 1000) * 1000);
  STD_VOID_SYSTEM_CALL
    (syscall_setitimer, (UX_setitimer (which, (&value), (&ovalue))));
}

void
DEFUN (OS_process_timer_set, (first, interval),
       clock_t first AND
       clock_t interval)
{
  set_timer (ITIMER_VIRTUAL, first, interval);
}

void
DEFUN_VOID (OS_process_timer_clear)
{
  set_timer (ITIMER_VIRTUAL, 0, 0);
}

void
DEFUN (OS_profile_timer_set, (first, interval),
       clock_t first AND
       clock_t interval)
{
  set_timer (ITIMER_PROF, first, interval);
}

void
DEFUN_VOID (OS_profile_timer_clear)
{
  set_timer (ITIMER_PROF, 0, 0);
}

void
DEFUN (OS_real_timer_set, (first, interval),
       clock_t first AND
       clock_t interval)
{
  set_timer (ITIMER_REAL, first, interval);
}

void
DEFUN_VOID (OS_real_timer_clear)
{
  set_timer (ITIMER_REAL, 0, 0);
}

#else /* not HAVE_SETITIMER */

static unsigned int alarm_interval;

void
DEFUN_VOID (reschedule_alarm)
{
  UX_alarm (alarm_interval);
}

void
DEFUN (OS_process_timer_set, (first, interval),
       clock_t first AND
       clock_t interval)
{
  error_unimplemented_primitive ();
}

void
DEFUN_VOID (OS_process_timer_clear)
{
  return;
}

void
DEFUN (OS_profile_timer_set, (first, interval),
       clock_t first AND
       clock_t interval)
{
  error_unimplemented_primitive ();
}

void
DEFUN_VOID (OS_profile_timer_clear)
{
  return;
}

void
DEFUN (OS_real_timer_set, (first, interval),
       clock_t first AND
       clock_t interval)
{
  alarm_interval = ((interval + 999) / 1000);
  UX_alarm ((first + 999) / 1000);
}

void
DEFUN_VOID (OS_real_timer_clear)
{
  alarm_interval = 0;
  UX_alarm (0);
}

#endif /* HAVE_SETITIMER */

void
DEFUN_VOID (UX_initialize_environment)
{
  initialize_timezone ();
  initialize_process_clock ();
  initialize_real_time_clock ();
#ifndef HAVE_SETITIMER
  alarm_interval = 0;
#endif
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
      current_dir_path = (UX_malloc (1024));
      if (current_dir_path == 0)
	error_system_call (ENOMEM, syscall_malloc);
      current_dir_path_size = 1024;
    }
  while (1)
    {
      if ((UX_getcwd (current_dir_path, current_dir_path_size)) != 0)
	return (current_dir_path);
      if (errno != ERANGE)
	error_system_call (errno, syscall_getcwd);
      current_dir_path_size *= 2;
      {
	char * new_current_dir_path =
	  (UX_realloc (current_dir_path, current_dir_path_size));
	if (new_current_dir_path == 0)
	  /* ANSI C requires `path' to be unchanged -- we may have to
	     discard it for systems that don't behave thus. */
	  error_system_call (ENOMEM, syscall_realloc);
	current_dir_path = new_current_dir_path;
      }
    }
}

void
DEFUN (OS_set_working_dir_pathname, (name), CONST char * name)
{
  size_t name_size = strlen (name);
  STD_VOID_SYSTEM_CALL (syscall_chdir, (UX_chdir (name)));
  while (1) {
    if (name_size < current_dir_path_size) {
      strcpy(current_dir_path, name);
      return;
    } 
    current_dir_path_size *= 2;
    {
      char * new_current_dir_path =
	(UX_realloc (current_dir_path, current_dir_path_size));
      if (new_current_dir_path == 0)
	error_system_call (ENOMEM, syscall_realloc);
      current_dir_path = new_current_dir_path;
    }
  }
}

CONST char *
DEFUN_VOID (OS_current_user_name)
{
  {
    CONST char * result = (UX_getlogin ());
    if ((result != 0) && (*result != '\0'))
      return (result);
  }
  {
    struct passwd * entry = (UX_getpwuid (UX_geteuid ()));
    if (entry != 0)
      return (entry -> pw_name);
  }
  error_external_return ();
  return (0);
}

CONST char *
DEFUN_VOID (OS_current_user_home_directory)
{
  {
    char * user_name = (UX_getlogin ());
    if (user_name != 0)
      {
	struct passwd * entry = (UX_getpwnam (user_name));
	if (entry != 0)
	  return (entry -> pw_dir);
      }
  }
  {
    struct passwd * entry = (UX_getpwuid (UX_getuid ()));
    if (entry != 0)
      return (entry -> pw_dir);
  }
  error_external_return ();
  return (0);
}
