/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxenv.c,v 1.7 1992/05/06 14:03:36 jinx Exp $

Copyright (c) 1990-1992 Massachusetts Institute of Technology

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

#include "ux.h"
#include "osenv.h"

void
DEFUN (OS_current_time, (buffer), struct time_structure * buffer)
{
  time_t t;
  struct tm * ts;
  STD_UINT_SYSTEM_CALL (syscall_time, t, (UX_time (0)));
  STD_PTR_SYSTEM_CALL (syscall_localtime, ts, (UX_localtime (&t)));
  (buffer -> year) = ((ts -> tm_year) + 1900);
  (buffer -> month) = ((ts -> tm_mon) + 1);
  (buffer -> day) = (ts -> tm_mday);
  (buffer -> hour) = (ts -> tm_hour);
  (buffer -> minute) = (ts -> tm_min);
  (buffer -> second) = (ts -> tm_sec);
  {
    /* In localtime() encoding, 0 is Sunday; in ours, it's Monday. */
    int wday = (ts -> tm_wday);
    (buffer -> day_of_week) = ((wday == 0) ? 6 : (wday - 1));
  }
}

#ifdef HAVE_TIMES

static clock_t initial_process_clock;

static void
DEFUN_VOID (initialize_process_clock)
{
  struct tms buffer;
  UX_times (&buffer);
  initial_process_clock = (buffer . tms_utime);
}

clock_t
DEFUN_VOID (OS_process_clock)
{
  clock_t ct = (UX_SC_CLK_TCK ());
  struct tms buffer;
  /* Was STD_VOID_SYSTEM_CALL, but at least one version of Ultrix
     returns negative numbers other than -1 when there are no errors.  */
  while ((UX_times (&buffer)) == (-1))
    if (errno != EINTR)
      error_system_call (errno, syscall_times);
  return
    (((((buffer . tms_utime) - initial_process_clock) * 2000) + ct) /
     (2 * ct));
}

#else /* not HAVE_TIMES */

static void
DEFUN_VOID (initialize_process_clock)
{
}

clock_t
DEFUN_VOID (OS_process_clock)
{
  /* This must not signal an error in normal use. */
  return (0);
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

clock_t
DEFUN_VOID (OS_real_time_clock)
{
  struct timeval rtc;
  struct timezone tz;
  STD_VOID_SYSTEM_CALL
    (syscall_gettimeofday, (UX_gettimeofday ((&rtc), (&tz))));
  return
    ((((rtc . tv_sec) - (initial_rtc . tv_sec)) * 1000) +
     ((((rtc . tv_usec) - (initial_rtc . tv_usec)) + 500) / 1000));
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

clock_t
DEFUN_VOID (OS_real_time_clock)
{
  clock_t ct = (UX_SC_CLK_TCK ());
  struct tms buffer;
  clock_t t;
  /* Was STD_UINT_SYSTEM_CALL, but at least one version of Ultrix
     returns negative numbers other than -1 when there are no errors.  */
  while ((t = (UX_times (&buffer))) == (-1))
    if (errno != EINTR)
      error_system_call (errno, syscall_times);
  return ((((t - initial_rtc) * 2000) + ct) / (2 * ct));
}

#else /* not HAVE_TIMES */

static time_t initial_rtc;

static void
DEFUN_VOID (initialize_real_time_clock)
{
  initial_rtc = (time (0));
}

clock_t
DEFUN_VOID (OS_real_time_clock)
{
  time_t t;
  STD_UINT_SYSTEM_CALL (syscall_time, t, (UX_time (0)));
  return ((t - initial_rtc) * 1000);
}

#endif /* HAVE_TIMES */
#endif /* HAVE_GETTIMEOFDAY */

#ifdef HAVE_ITIMER

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

#else /* not HAVE_ITIMER */

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

#endif /* HAVE_ITIMER */

void
DEFUN_VOID (UX_initialize_environment)
{
  initialize_process_clock ();
  initialize_real_time_clock ();
#ifndef HAVE_ITIMER
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
DEFUN (OS_get_environment_variable, (name), CONST char * name)
{
  return (UX_getenv (name));
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
    struct passwd * entry = (UX_getpwuid (UX_geteuid ()));
    if (entry != 0)
      return (entry -> pw_dir);
  }
  error_external_return ();
  return (0);
}
