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

#include "scheme.h"
#include "os2.h"
#include "osenv.h"
#include <time.h>
#include <sys\types.h>

#ifdef __IBMC__
#  include <sys\timeb.h>
#  define NC_TIMEZONE _timezone
#  define NC_DAYLIGHT _daylight
#  if (__IBMC__ >= 360)
#    define NC_FTIME ftime
#  else
#    define NC_FTIME _ftime
#  endif
#endif

#if defined(__WATCOMC__) || defined(__EMX__)
#  include <sys\timeb.h>
#  define NC_TIMEZONE timezone
#  define NC_DAYLIGHT daylight
#  define NC_FTIME ftime
#endif

#ifdef __GCC2__
#  include <errno.h>
#  include <sys/times.h>
#endif

static void initialize_real_time_clock (void);
static double get_real_time_clock (void);

static void initialize_timer (void);
static void timer_thread (void *);
static void handle_timer_event (msg_t *);

void
OS2_initialize_environment (void)
{
  initialize_real_time_clock ();
  initialize_timer ();
}

time_t
OS_encoded_time (void)
{
  time_t t = (time (0));
  if (t < 0)
    OS2_error_system_call (errno, syscall_time);
  return (t);
}

void
OS_decode_time (time_t t, struct time_structure * buffer)
{
  struct tm * ts = (localtime (&t));
  if (ts == 0)
    OS2_error_system_call (errno, syscall_localtime);
  (buffer -> year) = ((ts -> tm_year) + 1900);
  (buffer -> month) = ((ts -> tm_mon) + 1);
  (buffer -> day) = (ts -> tm_mday);
  (buffer -> hour) = (ts -> tm_hour);
  (buffer -> minute) = (ts -> tm_min);
  (buffer -> second) = (ts -> tm_sec);
  (buffer -> daylight_savings_time) = (ts -> tm_isdst);
#ifdef NC_TIMEZONE
  (buffer -> time_zone) = NC_TIMEZONE;
#else
  (buffer -> time_zone) = INT_MAX;
#endif
  {
    /* In localtime() encoding, 0 is Sunday; in ours, it's Monday. */
    int wday = (ts -> tm_wday);
    (buffer -> day_of_week) = ((wday == 0) ? 6 : (wday - 1));
  }
}

void
OS_decode_utc (time_t t, struct time_structure * buffer)
{
  struct tm * ts = (gmtime (&t));
  if (ts == 0)
    OS2_error_system_call (errno, syscall_gmtime);
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

time_t
OS_encode_time (struct time_structure * buffer)
{
  struct tm ts;
  (ts . tm_year) = ((buffer -> year) - 1900);
  (ts . tm_mon) = ((buffer -> month) - 1);
  (ts . tm_mday) = (buffer -> day);
  (ts . tm_hour) = (buffer -> hour);
  (ts . tm_min) = (buffer -> minute);
  (ts . tm_sec) = (buffer -> second);
  (ts . tm_isdst) = (buffer -> daylight_savings_time);
  {
    time_t t = (mktime (&ts));
    if (t < 0)
      OS2_error_system_call (errno, syscall_mktime);
#ifdef NC_TIMEZONE
    /* mktime assumes its argument is local time, and converts it to
       UTC; if the specified time zone is different, adjust the result.  */
    if (((buffer -> time_zone) != INT_MAX)
	&& ((buffer -> time_zone) != NC_TIMEZONE))
      t = ((t - NC_TIMEZONE) + (buffer -> time_zone));
#endif
    return (t);
  }
}

long
OS2_timezone (void)
{
#ifdef NC_TIMEZONE
  return (NC_TIMEZONE);
#else
  return (0);
#endif
}

int
OS2_daylight_savings_p (void)
{
#ifdef NC_DAYLIGHT
  return (NC_DAYLIGHT);
#else
  return (-1);
#endif
}

static double initial_rtc;

static void
initialize_real_time_clock (void)
{
  initial_rtc = (get_real_time_clock ());
}

double
OS_real_time_clock (void)
{
  return ((get_real_time_clock ()) - initial_rtc);
}

static double
get_real_time_clock (void)
{
#ifdef NC_FTIME
  struct timeb rtc;
  NC_FTIME (&rtc);
  return ((((double) (rtc . time)) * 1000.0) + ((double) (rtc . millitm)));
#endif
#ifdef __GCC2__
  struct tms rtc;
  times (&rtc);
  return (((double) (rtc . tms_utime)) * (1000.0 / ((double) CLK_TCK)));
#endif
}

double
OS_process_clock (void)
{
  /* This must not signal an error in normal use. */
  return (OS_real_time_clock ());
}

static HEV timer_event;
static int timer_handle_valid;
static HTIMER timer_handle;
TID OS2_timer_tid;

static void
initialize_timer (void)
{
  timer_event = (OS2_create_event_semaphore (0, 1));
  timer_handle_valid = 0;
  OS2_timer_tid = (OS2_beginthread (timer_thread, 0, 0));
}

static void
timer_thread (void * arg)
{
  EXCEPTIONREGISTRATIONRECORD registration;
  (void) OS2_thread_initialize ((&registration), QID_NONE);
  while (1)
    {
      ULONG count = (OS2_reset_event_semaphore (timer_event));
      while (count > 0)
	{
	  OS2_send_message (OS2_interrupt_qid,
			    (OS2_create_message (mt_timer_event)));
	  count -= 1;
	}
      (void) OS2_wait_event_semaphore (timer_event, 1);
    }
}

void
OS_real_timer_set (clock_t first, clock_t interval)
{
  /* **** No support for (first != interval), but runtime system never
     does that anyway.  */
  OS_real_timer_clear ();
  if (interval != 0)
    {
      STD_API_CALL (dos_start_timer, (interval,
				      ((HSEM) timer_event),
				      (&timer_handle)));
      timer_handle_valid = 1;
    }
  else if (first != 0)
    {
      STD_API_CALL (dos_async_timer, (first,
				      ((HSEM) timer_event),
				      (&timer_handle)));
      timer_handle_valid = 1;
    }
}

void
OS_real_timer_clear (void)
{
  if (timer_handle_valid)
    {
      STD_API_CALL (dos_stop_timer, (timer_handle));
      timer_handle_valid = 0;
    }
  (void) OS2_reset_event_semaphore (timer_event);
}

void
OS_process_timer_set (clock_t first, clock_t interval)
{
  OS2_error_unimplemented_primitive ();
}

void
OS_process_timer_clear (void)
{
}

void
OS_profile_timer_set (clock_t first, clock_t interval)
{
  OS2_error_unimplemented_primitive ();
}

void
OS_profile_timer_clear (void)
{
}

static size_t current_dir_path_size = 0;
static char * current_dir_path = 0;

const char *
OS_working_dir_pathname (void)
{
  ULONG drive_number;
  {
    ULONG drive_map;
    STD_API_CALL (dos_query_current_disk, ((&drive_number), (&drive_map)));
  }
  if ((current_dir_path_size == 0) || (current_dir_path == 0))
    {
      current_dir_path_size = 1024;
      current_dir_path = (OS_malloc (current_dir_path_size));
    }
  while (1)
    {
      ULONG size = (current_dir_path_size - 3);
      {
	APIRET rc =
	  (dos_query_current_dir
	   (drive_number, (current_dir_path + 3), (&size)));
	if (rc == NO_ERROR)
	  break;
	if (rc != ERROR_BUFFER_OVERFLOW)
	  OS2_error_system_call (rc, syscall_dos_query_current_dir);
      }
      do
	current_dir_path_size *= 2;
      while ((current_dir_path_size - 3) < size);
      OS_free (current_dir_path);
      current_dir_path = (OS_malloc (current_dir_path_size));
    }
  (current_dir_path[0]) = ('a' + drive_number - 1);
  (current_dir_path[1]) = ':';
  (current_dir_path[2]) = '\\';
  return (current_dir_path);
}

void
OS_set_working_dir_pathname (const char * name)
{
  extern char * OS2_remove_trailing_backslash (const char *);
  unsigned int length;
  name = (OS2_remove_trailing_backslash (name));
  length = (strlen (name));
  if ((length >= 2) && ((name[1]) == ':'))
    {
      STD_API_CALL
	(dos_set_default_disk,
	 ((name[0]) - ((islower (name[0])) ? 'a' : 'A') + 1));
      name += 2;
      length -= 2;
    }
  STD_API_CALL (dos_set_current_dir, ((length == 0) ? "\\" : ((char *) name)));
}
