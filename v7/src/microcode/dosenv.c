/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosenv.c,v 1.1 1992/05/05 06:55:13 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

#include "msdos.h"
#include "osenv.h"
#include <stdlib.h>

void
DEFUN (OS_current_time, (buffer), struct time_structure * buffer)
{
  time_t t;
  struct tm * ts;
  STD_UINT_SYSTEM_CALL (syscall_time, t, (DOS_time (0)));
  STD_PTR_SYSTEM_CALL (syscall_localtime, ts, (DOS_localtime (&t)));
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

clock_t
DEFUN_VOID (OS_process_clock)
{
  /* This must not signal an error in normal use. */
  /* Return answer in milliseconds, was in 1/100th seconds */
  return (clock()*((clock_t) (1000/CLOCKS_PER_SEC)));
}



clock_t
DEFUN_VOID (OS_real_time_clock)
{
  return (clock()*((clock_t) (1000/CLOCKS_PER_SEC)));
}



/* Timer adjustments */
#define PC_TIMER_TICKS_PER_SECOND	(18.2)
/* This should work out to about 55 */
#define PC_MILLISECONDS_PER_TIMER_TICK  \
  ((long) ((1000.0/PC_TIMER_TICKS_PER_SECOND)+0.5))

static unsigned long
DEFUN (ms_to_ticks, (clocks), clock_t clocks)
{ ldiv_t ticks;
  unsigned long result;

  ticks = ldiv((long) clocks, PC_MILLISECONDS_PER_TIMER_TICK);

  result = ((ticks.rem >= (PC_MILLISECONDS_PER_TIMER_TICK/2)) ?
   	    (ticks.quot + 1) : (ticks.quot));
  return (result == 0) ? 1 : result;  
}
  
void
DEFUN (OS_process_timer_set, (first, interval),
       clock_t first AND
       clock_t interval)
{ extern volatile unsigned long scm_itimer_counter, scm_itimer_reload;
  /* Convert granularity to 1/18.2 seconds */

  scm_itimer_counter = ms_to_ticks(first);
  scm_itimer_reload  = ms_to_ticks(interval);
  
  return;  
}

void
DEFUN_VOID (OS_process_timer_clear)
{
  scm_itimer_reload = scm_itimer_counter = 0;
  return;
}

void
DEFUN (OS_real_timer_set, (first, interval),
       clock_t first AND
       clock_t interval)
{
  OS_process_timer_set (first, interval);
}

void
DEFUN_VOID (OS_real_timer_clear)
{
  OS_process_timer_clear();
  return;
}

void
DEFUN_VOID (DOS_initialize_environment)
{
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
      current_dir_path = (DOS_malloc (1024));
      if (current_dir_path == 0)
	error_system_call (ENOMEM, syscall_malloc);
      current_dir_path_size = 1024;
    }
  while (1)
    {
      if ((DOS_getcwd (current_dir_path, current_dir_path_size)) != 0)
      { strlwr(current_dir_path);
	return (current_dir_path);
      }
#ifdef ERANGE
      if (errno != ERANGE)
	error_system_call (errno, syscall_getcwd);
#endif      
      current_dir_path_size *= 2;
      {
	char * new_current_dir_path =
	  (DOS_realloc (current_dir_path, current_dir_path_size));
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
{ char filename[128], drive[3];
  int drive_number;
  size_t name_size = strlen (name);
  
  drive_number = dos_split_filename(name, drive, filename);
  dos_set_default_drive(drive_number);
  STD_VOID_SYSTEM_CALL (syscall_chdir, (DOS_chdir (filename)));

  while (1) {
    if (name_size < current_dir_path_size) {
      strcpy(current_dir_path, name);
      return;
    } 
    current_dir_path_size *= 2;
    {
      char * new_current_dir_path =
	(DOS_realloc (current_dir_path, current_dir_path_size));
      if (new_current_dir_path == 0)
	error_system_call (ENOMEM, syscall_realloc);
      current_dir_path = new_current_dir_path;
    }
  }
}

CONST char *
DEFUN (OS_get_environment_variable, (name), CONST char * name)
{
  return (DOS_getenv (name));
}

CONST char *
DEFUN_VOID (OS_current_user_name)
{
  return ("dos");
}

CONST char *
DEFUN_VOID (OS_current_user_home_directory)
{
  return ("c:\\");
}
