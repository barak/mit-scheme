/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
#include "prims.h"
#include "ux.h"
#include "osenv.h"

/* Seconds since the UTC Epoch */

/* This is a disaster, thanks to the mind-boggling brain damage of
   POSIX.  At any time t, the POSIX time P(t) is the number of SI
   seconds, S(t), since the UTC epoch (1972-01-01T00:00:00Z), plus
   63072000 (= 2*365*86400, to adjust for the Unix epoch, which would
   be `1970-01-01T00:00:00Z' if modern UTC hadn't begun only in 1972),
   *minus* the number L(t) of those seconds that were leap seconds in
   UTC.  That is, P(t) = S(t) + 63072000 - L(t).

   Problem (in the sense of problem set): Find S given P.
   Problem (in the sense of disaster): POSIX doesn't tell us S or L.

   So what do we do?  Fortunately, many popular Unix systems set their
   clocks using the Network Time Protocol with the NTP Project's ntpd.
   In order to support this, they provide a couple of extra-POSIX
   routines, ntp_gettime and ntp_adjtime, giving the TAI - UTC offset.

   What if we don't have ntp_gettime or ntp_adjtime, or if the kernel
   doesn't know the TAI - UTC offset?  We're screwed.  We could guess
   that there have been 24 leap seconds, which is true at the time of
   writing (2010-12-09).  But this is wrong if the clock is set to TAI,
   and this causes the Scheme clock to misbehave if the kernel assumes
   the TAI - UTC offset to be zero and then increments it when an NTP
   server tells it of a new leap second: at this point Scheme would
   switch from assuming a TAI - UTC offset of 24 seconds to assuming a
   TAI - UTC offset of 1 second, and rewind the clock by 23 seconds --
   it would behave even worse than a POSIX clock.

   So instead, if the kernel reports a TAI - UTC offset of under ten,
   we take that to be the number of leap seconds, so that at least when
   it is incremented and the system clock is rewound by a second, we
   can show a smoothly advancing clock.  If the kernel reports a TAI -
   UTC offset of at least ten, we subtract ten from it to compute the
   number of leap seconds (because 1972-01-01T00:00:00Z is 1972-01-01
   at 00:00:10 in TAI).

   Why a threshold of ten?  Since modern UTC began, the TAI - UTC
   offset has never been under ten.  So if you represent times during
   the entire existence of modern UTC so far, this heuristic will
   work.  */

/* Summary of scenarios:

   (a) If your system lacks ntp_gettime/ntp_adjtime, and its clock is
       set to POSIX time, then you lose exactly as badly as any POSIX
       program does.  (The clock is wrong by 24 at the time of writing,
       and it misbehaves during a leap second.)

   (b) If your system lacks ntp_gettime/ntp_adjtime, and its clock is
       set to TAI, then you win.  (The clock is correct and behaves
       well.)

   (c) If your system has ntp_gettime/ntp_adjtime, its clock is set to
       POSIX time, and its TAI - UTC offset is initialized to 0 but is
       incremented at the same time the POSIX clock is rewound, then

       . you will have time stamps that are off by 24 seconds at the
         time of writing, but

       . your clock will behave well,

       at least for the next net of nine positive leap seconds during
       continuous operation of your system, which should cover a good
       decade or so.

   (d) If your system has ntp_gettime/ntp_adjtime, and its clock is set
       to POSIX time with the correct TAI - UTC offset, then you win,
       at least for the next net of fourteen negative leap seconds, but
       there never has been a negative leap second and probably never
       will be.

   (e) If your system has ntp_gettime/ntp_adjtime, and its clock is set
       to TAI with a constant TAI - UTC offset of 0, then you win.

   Scenario (c) is the case for most modern Unix systems that I know.
   Scenario (d) is easily configured for most such Unix systems.
   Scenarios (b) and (e) are the case for any system administered by
   users of djbware (see <http://cr.yp.to/proto/utctai.html>).

   What about time zones?  I will think about them some other time.  */

static intmax_t utc_epoch_minus_unix_epoch = 63072000L;

static long
guess_n_leap_seconds (long tai)
{
  return ((tai < 10) ? tai : (tai - 10));
}

static intmax_t
guess_time_from_posix (intmax_t posix, long tai)
{
  /* This is to be used only for querying the current clock -- it
     assumes tai is the current TAI - UTC offset.  To find the (best
     approximation of) the seconds since an epoch from POSIX time
     requires consulting a leap second table.  */
  /* FIXME: There is a minor danger of arithmetic overflow here -- but
     only on broken operating systems with wildly bogus values for
     POSIX time and the TAI - UTC offset, or near 2038 on archaic
     systems with no 64-bit integer type.  */
  return (posix - utc_epoch_minus_unix_epoch + (guess_n_leap_seconds (tai)));
}

/* The following routines may lose information -- namely, the
   information that the system's clock is bogus.  But if this is so,
   you'll probably notice the fact anyway, and this paranoia prevents
   bad values from making Scheme crash.  These are macros because the
   signedness of the types defined in struct timeval and struct
   timespec is pretty random.  */

#define SANITIZE_NSEC(NSEC)                                             \
  (((NSEC) < 0)                                 ? 0                     \
   : (((uintmax_t) (NSEC)) < 1000000000UL)      ? ((uint32_t) (NSEC))   \
   :                                              999999999UL)

#define SANITIZE_USEC(USEC)                                             \
  (((USEC) < 0)                         ? 0                             \
   : (((uintmax_t) (USEC)) < 1000000UL) ? (1000UL * ((uint32_t) (USEC))) \
   :                                      999999UL)

#if defined(HAVE_BSD_NTP)

void
OS_nanotime_since_utc_epoch (struct scheme_nanotime *t)
{
  struct ntptimeval ntv;
  STD_VOID_SYSTEM_CALL (syscall_ntp_gettime, (UX_ntp_gettime (&ntv)));
  (t->seconds)
    = (guess_time_from_posix (((intmax_t) (ntv.time.tv_sec)), (ntv.tai)));
  (t->nanoseconds) = (SANITIZE_NSEC (ntv.time.tv_nsec));
}

#elif defined(HAVE_LINUX_NTP)

void
OS_nanotime_since_utc_epoch (struct scheme_nanotime *t)
{
  static const struct timex zero_tx;
  struct timex tx = zero_tx;
  /* This doesn't actually adjust the time, because we have set
     tx.modes to zero, meaning no modifications.  It does, however,
     return some useful information in tx, which Linux's ntp_gettime
     failed to return until recent versions.  */
  STD_VOID_SYSTEM_CALL (syscall_ntp_adjtime, (UX_ntp_adjtime (&tx)));
  (t->seconds)
    = (guess_time_from_posix (((intmax_t) (tx.time.tv_sec)), (tx.tai)));
  (t->nanoseconds) = (SANITIZE_USEC (tx.time.tv_usec));
}

#elif defined(HAVE_CLOCK_GETTIME)

void
OS_nanotime_since_utc_epoch (struct scheme_nanotime t)
{
  struct timespec ts;
  STD_VOID_SYSTEM_CALL
    (syscall_clock_gettime, (UX_clock_gettime (CLOCK_REALTIME, (&ts))));
  (t->seconds) = (guess_time_from_posix (((intmax_t) (ts.tv_sec)), 0));
  (t->nanoseconds) = (SANITIZE_NSEC (ts.tv_nsec));
}

#elif defined(HAVE_GETTIMEOFDAY)

void
OS_nanotime_since_utc_epoch (struct scheme_nanotime *t)
{
  struct timeval tv;
  STD_VOID_SYSTEM_CALL (syscall_gettimeofday, (UX_gettimeofday ((&tv), 0)));
  (t->seconds) = (guess_time_from_posix (((intmax_t) (tv.tv_sec)), 0));
  (t->nanoseconds) = (SANITIZE_USEC (tv.tv_usec));
}

#else  /* You are a sad, strange little Unix.  */

void
OS_nanotime_since_utc_epoch (struct scheme_nanotime *t)
{
  intmax_t posix_time;
  STD_UINT_SYSTEM_CALL (syscall_time, posix_time, (UX_time (0)));
  (t->seconds) = (guess_time_from_posix (posix_time, 0));
  (t->nanoseconds) = 0;
}

#endif

time_t
OS_encoded_time (void)
{
  time_t t;
  STD_UINT_SYSTEM_CALL (syscall_time, t, (UX_time (0)));
  return (t);
}

void
OS_decode_time (time_t t, struct time_structure * buffer)
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
OS_decode_utc (time_t t, struct time_structure * buffer)
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
OS_encode_time (struct time_structure * buffer)
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
initialize_timezone (void)
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
initialize_process_clock (void)
{
  struct tms buffer;
  while ((UX_times (&buffer)) == (-1))
    if (errno != EINTR)
      {
	initial_process_clock = 0;
	return;
      }
  initial_process_clock = (PROCESS_TIME (buffer));
}

double
OS_process_clock (void)
{
  double ct = ((double) (UX_SC_CLK_TCK ()));
  struct tms buffer;
  clock_t t;
  /* Was STD_VOID_SYSTEM_CALL, but at least one version of Ultrix
     returns negative numbers other than -1 when there are no errors.
     Furthermore, this should not signal an error for any reason. */
  while ((UX_times (&buffer)) == (-1))
    if (errno == EINTR)
      deliver_pending_interrupts ();
    else
      {
	t = initial_process_clock;
	goto finish;
      }
  t = (PROCESS_TIME (buffer));
 finish:
  return
    (((((double) (t - initial_process_clock)) * 2000.0)
      + ct)
     / (2.0 * ct));
}

#else /* not HAVE_TIMES */

static void
initialize_process_clock (void)
{
}

double
OS_process_clock (void)
{
  /* This must not signal an error in normal use. */
  return (0.0);
}

#endif /* HAVE_TIMES */

#ifdef HAVE_GETTIMEOFDAY

static struct timeval initial_rtc;

static void
initialize_real_time_clock (void)
{
  struct timezone tz;
  while ((UX_gettimeofday ((&initial_rtc), (&tz))) == (-1))
    if (errno != EINTR)
      {
	(initial_rtc . tv_sec) = 0;
	(initial_rtc . tv_usec) = 0;
	break;
      }
}

double
OS_real_time_clock (void)
{
  struct timeval rtc;
  struct timezone tz;
  while ((UX_gettimeofday ((&rtc), (&tz))) == (-1))
    if (errno == EINTR)
      deliver_pending_interrupts ();
    else
      {
	rtc = initial_rtc;
	break;
      }
  return
    ((((double) ((rtc . tv_sec) - (initial_rtc . tv_sec))) * 1000.0) +
     ((((double) ((rtc . tv_usec) - (initial_rtc . tv_usec))) + 500.0)
      / 1000.0));
}

#else /* not HAVE_GETTIMEOFDAY */
#ifdef HAVE_TIMES

static clock_t initial_rtc;

static void
initialize_real_time_clock (void)
{
  struct tms buffer;
  while ((initial_rtc = (UX_times (&buffer))) == (-1))
    if (errno != EINTR)
      {
	initial_rtc = 0;
	break;
      }
}

double
OS_real_time_clock (void)
{
  double ct = ((double) (UX_SC_CLK_TCK ()));
  struct tms buffer;
  clock_t t;
  /* Was STD_UINT_SYSTEM_CALL, but at least one version of Ultrix
     returns negative numbers other than -1 when there are no errors.
     Furthermore, this should not signal an error for any reason. */
  while ((t = (UX_times (&buffer))) == (-1))
    if (errno == EINTR)
      deliver_pending_interrupts ();
    else
      {
	t = initial_rtc;
	break;
      }
  return (((((double) (t - initial_rtc)) * 2000.0) + ct) / (2.0 * ct));
}

#else /* not HAVE_TIMES */

static time_t initial_rtc;

static void
initialize_real_time_clock (void)
{
  while ((initial_rtc = (UX_time (0))) == (-1))
    if (errno != EINTR)
      {
	initial_rtc = 0;
	break;
      }
}

double
OS_real_time_clock (void)
{
  time_t t;
  while ((t = (UX_time (0))) == (-1))
    if (errno == EINTR)
      deliver_pending_interrupts ();
    else
      {
	t = initial_rtc;
	break;
      }
  return (((double) (t - initial_rtc)) * 1000.0);
}

#endif /* HAVE_TIMES */
#endif /* HAVE_GETTIMEOFDAY */

#ifdef HAVE_SETITIMER

static void
set_timer (int which,
       clock_t first,
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
OS_process_timer_set (clock_t first,
       clock_t interval)
{
  set_timer (ITIMER_VIRTUAL, first, interval);
}

void
OS_process_timer_clear (void)
{
  set_timer (ITIMER_VIRTUAL, 0, 0);
}

void
OS_profile_timer_set (clock_t first,
       clock_t interval)
{
  set_timer (ITIMER_PROF, first, interval);
}

void
OS_profile_timer_clear (void)
{
  set_timer (ITIMER_PROF, 0, 0);
}

void
OS_real_timer_set (clock_t first,
       clock_t interval)
{
  set_timer (ITIMER_REAL, first, interval);
}

void
OS_real_timer_clear (void)
{
  set_timer (ITIMER_REAL, 0, 0);
}

#else /* not HAVE_SETITIMER */

static unsigned int alarm_interval;

void
reschedule_alarm (void)
{
  UX_alarm (alarm_interval);
}

void
OS_process_timer_set (clock_t first,
       clock_t interval)
{
  error_unimplemented_primitive ();
}

void
OS_process_timer_clear (void)
{
  return;
}

void
OS_profile_timer_set (clock_t first,
       clock_t interval)
{
  error_unimplemented_primitive ();
}

void
OS_profile_timer_clear (void)
{
  return;
}

void
OS_real_timer_set (clock_t first,
       clock_t interval)
{
  alarm_interval = ((interval + 999) / 1000);
  UX_alarm ((first + 999) / 1000);
}

void
OS_real_timer_clear (void)
{
  alarm_interval = 0;
  UX_alarm (0);
}

#endif /* HAVE_SETITIMER */

void
UX_initialize_environment (void)
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

const char *
OS_working_dir_pathname (void)
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
OS_set_working_dir_pathname (const char * name)
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

const char *
OS_current_user_name (void)
{
  {
    const char * result = (UX_getlogin ());
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

const char *
OS_current_user_home_directory (void)
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
