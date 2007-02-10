/* -*-C-*-

$Id: bchmmg.c,v 9.108 2007/02/10 19:17:38 riastradh Exp $

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

/* Memory management top level.  Garbage collection to disk. */

#include "scheme.h"
#include "prims.h"
#include "memmag.h"
#include "option.h"
#include "osenv.h"
#include "osfs.h"

#ifdef __unix__
#  include "ux.h"
#  define SUB_DIRECTORY_DELIMITER '/'
/* This makes for surprising behavior: */
/* #  define UNLINK_BEFORE_CLOSE */
#endif

#ifdef __WIN32__
#  include "nt.h"
#  define SUB_DIRECTORY_DELIMITER '\\'
#endif

#ifdef __OS2__
#  include "os2.h"
#  define SUB_DIRECTORY_DELIMITER '\\'
#  if defined(__IBMC__) || defined(__WATCOMC__) || defined(__EMX__)
#    include <io.h>
#    include <sys\stat.h>
#  endif
#  ifndef F_OK
#    define F_OK 0
#    define X_OK 1
#    define W_OK 2
#    define R_OK 4
#  endif
#endif

#include "bchgcc.h"
#include "bchdrn.h"

#ifndef SEEK_SET
#  define SEEK_SET 0
#endif

#ifdef USE_SYSV_SHARED_MEMORY
#  define RECORD_GC_STATISTICS
#endif
#define MILLISEC * 1000

#define FLOOR(value,quant)	((quant) * ((value) / (quant)))
#define CEILING(value,quant)	(FLOOR (((value) + ((quant) - 1)), (quant)))

/* Memory management top level.  Garbage collection to disk.

   The algorithm is basically the same as for the 2 space collector,
   except that new space is on the disk, and there are two windows to
   it (the scan and free buffers).  The two windows are physically the
   same whent they correspond to the same section of the address space.
   There may be additional windows used to overlap I/O.

   For information on the 2 space collector, read the comments in the
   replaced files.

   The memory management code is spread over the following files:
   - bchgcc.h: shared header file for bchscheme.
   - bchmmg.c: top level, initialization and I/O.	Replaces memmag.c
   - bchgcl.c: main garbage collector loop.		Replaces gcloop.c
   - bchpur.c: constant/pure space hacking.		Replaces purify.c
   - bchdmp.c: object & world image dumping.		Replaces fasdump.c
   - bchdrn.h: header file for bchmmg.c and the bchdrn.c.
   - bchdrn.c: stand-alone program used as an overlapped I/O drone.
   - bchutl.c: utilities common to bchmmg.c and bchdrn.c.

   Problems with this implementation right now:
   - It only works on Unix (or systems which support Unix I/O calls).
   - Dumpworld does not work because the file is not closed at dump time or
     reopened at restart time.
   - Command-line specified gc files are only locked on versions of Unix
     that have lockf(2).  If your system does not have lockf, two
     processes can try to share the file and get very confused.

oo
   ------------------------------------------
   |        GC Buffer Space                 | (not always contiguous)
   |                                        |
   ------------------------------------------ <- fixed boundary (currently)
   |          Heap Space                    |
   |                                        |
   ------------------------------------------ <- boundary moved by purify
   |     Constant + Pure Space    /\        |
   |                              ||        |
   ------------------------------------------ <- fixed boundary (currently)
   |         Control Stack        ||        |
   |                              \/        |
   ------------------------------------------ <- fixed boundary (currently)
0

   Each area has a pointer to its starting address and a pointer to
   the next free cell (for the stack, it is a pointer to the last cell
   in use).  The GC buffer space contains two (or more) buffers used
   during the garbage collection process.  One is the scan buffer and
   the other is the free buffer, and they are dumped and loaded from
   disk as necessary.  At the beginning and at the end a single buffer
   is used, since transporting will occur into the area being scanned.
*/

/* Exports */

extern void EXFUN (Clear_Memory, (int, int, int));
extern void EXFUN (Setup_Memory, (int, int, int));
extern void EXFUN (Reset_Memory, (void));

long
  absolute_gc_file_end_position,
  gc_file_end_position,
  gc_file_current_position,
  gc_file_start_position;

unsigned long
  gc_buffer_size,
  gc_buffer_bytes,
  gc_buffer_shift,
  gc_buffer_mask,
  gc_buffer_byte_shift;

static unsigned long
  gc_extra_buffer_size,
  gc_buffer_overlap_bytes,
  gc_buffer_remainder_bytes,
  gc_total_buffer_size;

SCHEME_OBJECT
  * scan_buffer_top,		* scan_buffer_bottom,
  * free_buffer_top,		* free_buffer_bottom,
  * virtual_scan_pointer;

static SCHEME_OBJECT
  * virtual_scan_base;

static char
  * gc_file_name = 0;

CONST char
  * drone_file_name = 0;

static int
  keep_gc_file_p = 0,
  gc_file = -1,
  read_overlap = 0,
  write_overlap = 0;

static SCHEME_OBJECT
  * aligned_heap;

static Boolean
  can_dump_directly_p,
  extension_overlap_p,
  scan_buffer_extended_p;

static long
  scan_position,
  free_position,
  pre_read_position,
  extension_overlap_length;

static long
  saved_heap_size,
  saved_constant_size,
  saved_stack_size;

static unsigned long
  read_queue_bitmask; /* Change MAX_READ_OVERLAP if you change this. */

static struct buffer_info
  * free_buffer,
  * scan_buffer,
  * next_scan_buffer;

int
DEFUN (io_error_always_abort, (operation_name, noise),
       char * operation_name AND char * noise)
{
  return (1);
}

#ifdef __WIN32__
#include <windows.h>

int 
DEFUN (io_error_retry_p, (operation_name, noise),
       char * operation_name AND char * noise)
{
  char buf[512];
  extern HANDLE master_tty_window;

  sprintf (&buf[0],
	   "%s: GC file error (code = %d) when manipulating %s.\n"
	   "Choose an option (Cancel = Exit Scheme)",
	   operation_name, (GetLastError ()), noise);
  switch (MessageBox (master_tty_window,
		      &buf[0],
		      "MIT/GNU Scheme garbage-collection problem description",
		      (MB_ICONSTOP | MB_ABORTRETRYIGNORE | MB_APPLMODAL)))
  {
    case IDABORT:
      return (1);

    case IDRETRY:
      return (0);

    case IDIGNORE:
      Microcode_Termination (TERM_EXIT);
  }
  /*NOTREACHED*/
  return (0);
}

#else /* not __WIN32__ */
#ifdef __OS2__

int
io_error_retry_p (char * operation_name, char * noise)
{
  char buf [512];
  sprintf ((&buf[0]),
	   "%s: GC file error (code = %d) when manipulating %s.\n"
	   "Choose an option (Cancel = Exit Scheme)",
	   operation_name, errno, noise);
  switch
    (WinMessageBox (HWND_DESKTOP,
		    NULLHANDLE,
		    (&buf[0]),
		    "MIT/GNU Scheme garbage-collection problem description",
		    0,
		    (MB_ICONHAND | MB_ABORTRETRYIGNORE | MB_APPLMODAL)))
    {
    case MBID_ABORT: return (1);
    case MBID_RETRY: return (0);
    case MBID_IGNORE: Microcode_Termination (TERM_EXIT);
    }
}

#else /* not __OS2__ */

extern char EXFUN (userio_choose_option,
		   (CONST char *, CONST char *, CONST char **));
extern int EXFUN (userio_confirm, (CONST char *));

int 
DEFUN (io_error_retry_p, (operation_name, noise),
       char * operation_name AND char * noise)
{
  static CONST char * retry_choices [] =
    {
      "A = abort the operation",
      "E = exit scheme",
      "K = kill scheme",
      "Q = quit scheme",
      "R = retry the operation",
      "S = sleep for 1 minute and retry the operation",
      "X = exit scheme",
      0};

  outf_error ("\n%s (%s): GC file error (errno = %s) when manipulating %s.\n",
	      scheme_program_name, operation_name, (error_name (errno)),
	      noise);

  while (1)
  {
    switch (userio_choose_option
	    ("Choose one of the following actions:",
	     "Action -> ", retry_choices))
    {
      case 'A':
	return (1);

      case '\0':
	/* IO problems, assume everything is scrod. */
	outf_fatal 
	  ("%s (io_error_retry_p): Problems reading the keyboard; Exitting.\n",
	   scheme_program_name);
	termination_eof ();
	/*NOTREACHED*/

      case 'E': case 'K': case 'Q': case 'X':
	if (!(userio_confirm ("Kill Scheme (Y/N)? ")))
	  continue;
	Microcode_Termination (TERM_EXIT);
	/*NOTREACHED*/

      case 'S':
	sleep (60);
	/* fall through */

      case 'R':
      default:
	return (0);
    }
  }
}

#endif /* not __OS2__ */
#endif /* not __WIN32__ */

static int
DEFUN (verify_write, (position, size, success),
       long position AND long size AND Boolean * success)
{
  if ((position >= gc_file_start_position)
      && ((position + size) <= gc_file_end_position))
    return (0);
  outf_error (
	   "\n%s (verify_write): attempting to write outside allowed area.\n",
	   scheme_program_name);
  outf_error("\tlow position = 0x%lx; high position = 0x%lx.\n",
	     gc_file_start_position, gc_file_end_position);
  outf_error("\twrite position = 0x%lx; size = 0x%lx = %d bytes.\n",
	     position, size, size);
  outf_flush_error();
  if (success == ((Boolean *) NULL))
  {
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }
  *success = ((Boolean) false);
  return (-1);
}

static void
DEFUN (write_data, (from, position, nbytes, noise, success),
       char * from AND long position AND long nbytes
       AND char * noise AND Boolean * success)
{
  if (((verify_write (position, nbytes, success)) != -1)
      && ((retrying_file_operation (((file_operation_t *) write),
				    gc_file,
				    from,
				    position,
				    nbytes,
				    "write",
				    noise,
				    &gc_file_current_position,
				    ((success == ((Boolean *) NULL))
				     ? io_error_retry_p
				     : io_error_always_abort)))
	  == -1)
      && (success != ((Boolean *) NULL)))
    *success = false;
  return;
}

static void
DEFUN (load_data, (position, to, nbytes, noise, success),
       long position AND char * to AND long nbytes
       AND char * noise AND Boolean * success)
{
  (void) (retrying_file_operation (((file_operation_t *) read),
				   gc_file,
				   to,
				   position,
				   nbytes,
				   "read",
				   noise,
				   &gc_file_current_position,
				   ((success == ((Boolean *) NULL))
				    ? io_error_retry_p
				    : io_error_always_abort)));
}

static int
DEFUN (parameterization_termination, (kill_p, init_p),
       int kill_p AND int init_p)
{
  fflush (stderr);
  if (init_p)
    termination_init_error ();			/*NOTREACHED*/
  if (kill_p)
    Microcode_Termination (TERM_EXIT);		/*NOTREACHED*/
  return (-1);
}

struct bch_GC_statistic
{
  char * name;
  long * counter;
};

#ifdef RECORD_GC_STATISTICS

static void EXFUN (statistics_clear, (void));
static void EXFUN (statistics_print, (int, char *));

#  define STATISTICS_INCR(name)			name += 1
#  define STATISTICS_CLEAR()			statistics_clear ()
#  define STATISTICS_PRINT(level, noise)	statistics_print (level, noise)

#else

static struct bch_GC_statistic all_gc_statistics[] =
{ { "invalid last statistic",		((long *) NULL) } };

#  define STATISTICS_INCR(name)			do { } while (0)
#  define STATISTICS_CLEAR()			do { } while (0)
#  define STATISTICS_PRINT(level, noise)	do { } while (0)

#endif

#ifdef USE_SYSV_SHARED_MEMORY

#ifdef RECORD_GC_STATISTICS

static long
  reads_not_overlapped,
  reads_overlapped,
  reads_ready,
  reads_queued,
  reads_pending,
  reads_overlapped_aborted,
  reads_found_in_write_queue,
  reads_found_ready,
  read_wait_cycles,
  writes_not_overlapped,
  writes_overlapped,
  writes_not_deferred,
  writes_restarted,
  writes_retried,
  writes_pending,
  write_wait_cycles,
  pre_reads_aborted,
  pre_reads_ignored,
  pre_reads_found_in_write_queue,
  pre_reads_found_ready,
  pre_reads_not_started,
  pre_reads_started,
  pre_reads_deferred,
  pre_reads_restarted,
  pre_reads_retried,
  pre_reads_not_retried,
  pre_reads_requeued_as_writes,
  ready_buffers_enqueued,
  ready_buffers_not_enqueued,
  drone_wait_cycles,
  drone_request_failures,
  drones_found_dead,
  sleeps_interrupted,  
  await_io_cycles,
  gc_start_time,
  gc_end_transport_time,
  gc_end_weak_update_time,
  gc_start_reload_time,
  gc_end_time;

#define START_TRANSPORT_HOOK()						\
  gc_start_time = ((long) (OS_real_time_clock ()))
  
#define END_TRANSPORT_HOOK()						\
  gc_end_transport_time = ((long) (OS_real_time_clock ()))

#define END_WEAK_UPDATE_HOOK()						\
  gc_end_weak_update_time = ((long) (OS_real_time_clock ()))

#define START_RELOAD_HOOK()						\
  gc_start_reload_time = ((long) (OS_real_time_clock ()))

#define END_GC_HOOK()							\
  gc_end_time = ((long) (OS_real_time_clock ()))

static struct bch_GC_statistic all_gc_statistics[] =
{
  { "reads not overlapped",		&reads_not_overlapped },
  { "reads overlapped",			&reads_overlapped },
  { "reads ready",			&reads_ready },
  { "reads queued",			&reads_queued },
  { "reads pending",			&reads_pending },
  { "reads overlapped aborted",		&reads_overlapped_aborted },
  { "reads found in write queue",	&reads_found_in_write_queue },
  { "reads found ready",		&reads_found_ready },
  { "read wait cycles",			&read_wait_cycles },
  { "writes not overlapped",		&writes_not_overlapped },
  { "writes overlapped",		&writes_overlapped },
  { "writes retried",			&writes_retried },
  { "writes not deferred",		&writes_not_deferred },
  { "writes restarted",			&writes_restarted },
  { "writes retried",			&writes_retried },
  { "writes pending",			&writes_pending },
  { "write wait cycles",		&write_wait_cycles },
  { "pre-reads aborted",		&pre_reads_aborted },
  { "pre-reads ignored",		&pre_reads_ignored },
  { "pre-reads found in write queue",	&pre_reads_found_in_write_queue },
  { "pre-reads found ready",		&pre_reads_found_ready },
  { "pre-reads not started",		&pre_reads_not_started },
  { "pre-reads started",		&pre_reads_started },
  { "pre-reads deferred",		&pre_reads_deferred },
  { "pre-reads restarted",		&pre_reads_restarted },
  { "pre-reads retried",		&pre_reads_retried },
  { "pre-reads not retried",		&pre_reads_not_retried },
  { "pre-reads requeued as writes",	&pre_reads_requeued_as_writes },
  { "ready buffers enqueued",		&ready_buffers_enqueued },
  { "ready buffers not enqueued",	&ready_buffers_not_enqueued },
  { "drone wait cycles",		&drone_wait_cycles },
  { "drone request failures",		&drone_request_failures },
  { "drones found dead",		&drones_found_dead },
  { "sleeps interrupted",		&sleeps_interrupted },
  { "cycles awaiting I/O completion",	&await_io_cycles },
  { "time at gc start",			&gc_start_time },
  { "time at end of transport",		&gc_end_transport_time },
  { "time at end of weak update",	&gc_end_weak_update_time },
  { "time at start of reload",		&gc_start_reload_time },
  { "time at gc end",			&gc_end_time },
  { "invalid last statistic",		((long *) NULL) }
};

#endif /* RECORD_GC_STATISTICS */

/* The limit on MAX_READ_OVERLAP is the number of bits in read_queue_bitmask.
   The limit on MAX_GC_DRONES is the number of bits in (* wait_mask).
   There is no direct limit on MAX_WRITE_OVERLAP.
   On the other hand, the explicit searches through the queues
   will become slower as the numbers are increased.
 */

#define MAX_READ_OVERLAP	((sizeof (long)) * CHAR_BIT)
#define MAX_WRITE_OVERLAP	MAX_READ_OVERLAP
#define MAX_GC_DRONES		((sizeof (long)) * CHAR_BIT)
#define MAX_OVERLAPPED_RETRIES	2

static char * shared_memory = ((char *) -1);
static char * malloc_memory = ((char *) NULL);
static int drones_initialized_p = 0;
static int shmid = -1;
static int n_gc_buffers, n_gc_drones, gc_next_buffer, gc_next_drone;
static struct gc_queue_entry * gc_read_queue, * gc_write_queue;
static struct drone_info * gc_drones;
static struct buffer_info * gc_buffers;
static unsigned long * wait_mask, * drone_version;

static long default_sleep_period = 20 MILLISEC;

#define GET_SLEEP_DELTA()	default_sleep_period
#define SET_SLEEP_DELTA(value)	default_sleep_period = (value)

static void
DEFUN (sleep_awaiting_drones, (microsec, mask),
       unsigned int microsec AND unsigned long mask)
{
  int saved_errno;
  int retval;

  *wait_mask = mask;
#ifdef HAVE_POLL
  retval = (poll (0, 0, (microsec / 1000)));
#else
  {
    int dummy = 0;
    struct timeval timeout;
    timeout.tv_sec = 0;
    timeout.tv_usec = microsec;
    retval
      = (select (0,
		 ((SELECT_TYPE *) &dummy),
		 ((SELECT_TYPE *) &dummy),
		 ((SELECT_TYPE *) &dummy),
		 &timeout));
  }
#endif
  *wait_mask = ((unsigned long) 0);
  saved_errno = errno;

  if ((retval == -1) && (saved_errno == EINTR))
    STATISTICS_INCR (sleeps_interrupted);
}

#ifndef _SUNOS4
#  define SYSV_SPRINTF sprintf
#else
/* Losing SunOS sprintf */

#  define SYSV_SPRINTF sysV_sprintf

static int
DEFUN (sysV_sprintf, (string, format, value),
       char * string AND char * format AND long value)
{
  sprintf (string, format, value);
  return (strlen (string));
}

#endif /* _SUNOS4 */

#ifdef SIGCONT
static void
DEFUN (continue_running, (sig), int sig)
{
  RE_INSTALL_HANDLER (SIGCONT, continue_running);
}
#endif

static void
DEFUN (start_gc_drones, (first_drone, how_many, restarting),
       int first_drone AND int how_many AND int restarting)
{
  pid_t pid;
  char arguments[512];
  struct drone_info *drone;
  char
    * shmid_string,		/* shared memory handle */
    * tdron_string,		/* total number of drones */
    * nbuf_string,		/* total number of buffers */
    * bufsiz_string,		/* size of each buffer in bytes */
    * sdron_string,		/* index of first drone to start */
    * ndron_string;		/* number of drones to start */

  shmid_string = &arguments[0];
  tdron_string =
    (shmid_string + (1 + (SYSV_SPRINTF (shmid_string, "%d", shmid))));
  nbuf_string =
    (tdron_string + (1 + (SYSV_SPRINTF (tdron_string, "%d", n_gc_drones))));
  bufsiz_string =
    (nbuf_string + (1 + (SYSV_SPRINTF (nbuf_string, "%d", n_gc_buffers))));
  sdron_string =
    (bufsiz_string
     + (1 + (SYSV_SPRINTF (bufsiz_string, "%ld",
			   (gc_total_buffer_size
			    * (sizeof (SCHEME_OBJECT)))))));
  ndron_string =
    (sdron_string + (1 + (SYSV_SPRINTF (sdron_string, "%d", first_drone))));
  (void) (SYSV_SPRINTF (ndron_string, "%d", how_many));

  drone = (gc_drones + first_drone);
  if (restarting && (drone->state != drone_dead))
    (void) (kill (drone->DRONE_PID, SIGTERM));
  drone->state = drone_not_ready;
  (* drone_version) = ((unsigned long) DRONE_VERSION_NUMBER);

  if ((pid = (vfork ())) == 0)
  {
    execlp (drone_file_name, drone_file_name, gc_file_name, shmid_string,
	    tdron_string, nbuf_string, bufsiz_string,
	    sdron_string, ndron_string, (keep_gc_file_p ? "1" : "0"),
	    ((char *) 0));
    outf_error ("\n%s (start_gc_drones): execlp (%s) failed (errno = %s).\n",
		scheme_program_name, drone_file_name, (error_name (errno)));
    drone->state = drone_dead;
    (void) (kill ((getppid ()), SIGCONT));
    _exit (1);
  }
  else if (pid == -1)
  {
    outf_error ("\n%s (start_gc_drones): vfork failed (errno = %s).\n",
		scheme_program_name, (error_name (errno)));
    drone->state = drone_dead;
  }
  else
  {
    sigset_t old_mask, new_mask;

    UX_sigemptyset (&new_mask);
    UX_sigaddset ((&new_mask), SIGCONT);
    UX_sigprocmask (SIG_BLOCK, (&new_mask), (&old_mask));
    if (drone->state == drone_not_ready)
      UX_sigsuspend (&old_mask);
    UX_sigprocmask (SIG_SETMASK, (&old_mask), 0);

    if ((drone->state != drone_idle) && !restarting)
    {
      /* Do the wait only at startup since Scheme handles SIGCHLD
	 for all children. */
      ((void) (waitpid (pid, ((int *) 0), WNOHANG)));
      drone->state = drone_dead;
    }
  }
  return;
}

static int
DEFUN (invoke_gc_drone,
       (entry, operation, buffer, position, size),
       struct gc_queue_entry * entry
       AND enum drone_state operation
       AND struct buffer_info * buffer
       AND long position
       AND long size)
{
  int result, drone_index;
  struct drone_info * drone;
  enum buffer_state old_state;

  drone_index = (entry->drone_index);
  drone = (gc_drones + drone_index);
  drone->buffer_index = buffer->index;
  drone->entry_offset = (((char *) entry) - ((char *) drone));
  
  old_state = buffer->state;
  buffer->state = ((operation == drone_reading)
		   ? buffer_being_read
		   : buffer_being_written);
  buffer->position = position;
  buffer->size = size;
  entry->buffer = buffer;
  entry->state = entry_busy;

  drone->state = operation;	/* Previously drone_idle */
  if ((result = (kill (drone->DRONE_PID, SIGCONT))) == -1)
  {
    entry->state = entry_idle;
    buffer->state = old_state;
    drone->state = drone_dead;
    if (errno != ESRCH)
      outf_error
	("\n%s (invoke_gc_drone): kill (%d, SIGCONT) failed; errno = %s.\n",
	 scheme_program_name, drone->DRONE_PID, (error_name (errno)));
    start_gc_drones (drone_index, 1, 1);
  }
  return (result != -1);
}

/* The following don't do a wait/waitpid because Scheme handles SIGCHLD. */

static void
DEFUN_VOID (kill_all_gc_drones)
{
  int count;
  struct drone_info * drone;

  for (count = 0, drone = gc_drones; count < n_gc_drones; count++, drone++)
    (void) (kill (drone->DRONE_PID, SIGTERM));
  return;
}

static int
DEFUN (probe_gc_drone, (drone), struct drone_info * drone)
{
  int result;

  if ((result = (kill ((drone->DRONE_PID), 0))) == -1)
  {
    if (errno != ESRCH)
      (void) (kill ((drone->DRONE_PID), SIGTERM));
    drone->state = drone_dead;
  }
  return (result == 0);
}

static void EXFUN (handle_drone_death, (struct drone_info *));

static void
DEFUN (probe_all_gc_drones, (wait_p), int wait_p)
{
  int count;
  unsigned long running;
  struct drone_info * drone;

  do {
    for (count = 0, drone = gc_drones, running = ((unsigned long) 0);
	 count < n_gc_drones;
	 count++, drone++)
    {
      if (drone->state != drone_idle)
      {
	running |= (((unsigned long) 1) << drone->index);
	if ((kill (drone->DRONE_PID, 0)) == -1)
	{
	  if (errno != ESRCH)
	    (void) (kill (drone->DRONE_PID, SIGTERM));
	  drone->state = drone_dead;
	  start_gc_drones (drone->index, 1, 1);
	  handle_drone_death (drone);
	}
      }
    }
    if (wait_p && (running != ((unsigned long) 0)))
    {
      sleep_awaiting_drones (default_sleep_period, running);
      STATISTICS_INCR (await_io_cycles);
    }
  } while (wait_p && (running != ((unsigned long) 0)));
  return;
}

static void EXFUN (open_gc_file, (long, int));

static int
DEFUN (sysV_initialize, (first_time_p, size, r_overlap, w_overlap, drfnam),
       int first_time_p
       AND long size AND int r_overlap AND int w_overlap
       AND CONST char * drfnam)
{
  SCHEME_OBJECT * bufptr;
  int cntr;
  long buffer_space, shared_size, malloc_size;
  struct buffer_info * buffer;

  if (r_overlap < 0)
    r_overlap = 0;
  else if (r_overlap > MAX_READ_OVERLAP)
    r_overlap = MAX_READ_OVERLAP;
  read_overlap = r_overlap;

  if (w_overlap < 0)
    w_overlap = 0;
  else if (w_overlap > MAX_WRITE_OVERLAP)
    w_overlap = MAX_WRITE_OVERLAP;
  write_overlap = w_overlap;

  if ((n_gc_drones = (read_overlap + write_overlap)) > MAX_GC_DRONES)
  {
    read_overlap = ((read_overlap * MAX_GC_DRONES) / n_gc_drones);
    write_overlap = ((write_overlap * MAX_GC_DRONES) / n_gc_drones);
    n_gc_drones = (read_overlap + write_overlap);
  }
  n_gc_buffers = (2 + n_gc_drones);

  /* The second argument to open_gc_file should be (n_gc_drones == 0),
     but we can't do this since we can change the number of drones.
   */

  if (first_time_p)
  {
    open_gc_file (size, 0);
#ifdef F_SETFD
    /* Set the close on exec flag, the drones re-open it to get a
       different file pointer so that all the processes can independently
       lseek without clobbering each other.
     */
    (void) (fcntl (gc_file, F_SETFD, 1));
#endif
  }

  buffer_space = (n_gc_buffers
		  * (gc_total_buffer_size * (sizeof (SCHEME_OBJECT))));
  shared_size =
    (ALIGN_UP_TO_IO_PAGE (buffer_space
			  + (n_gc_buffers * (sizeof (struct buffer_info)))
			  + (n_gc_drones * (sizeof (struct drone_info)))
			  + (sizeof (long))
			  + (sizeof (long))
			  + (r_overlap * (sizeof (struct gc_queue_entry)))
			  + (w_overlap * (sizeof (struct gc_queue_entry)))
			  + IO_PAGE_SIZE));

  malloc_size = ((n_gc_drones == 0)
		 ? shared_size
		 : (first_time_p ? MALLOC_SPACE : 0));

  if (malloc_size > 0)
  {
    malloc_memory = ((char *) (malloc (malloc_size)));
    if (malloc_memory == ((char *) NULL))
    {
      outf_error
	("%s (sysV_initialize): Unable to allocate %d bytes (errno = %s).\n",
	 scheme_program_name, malloc_size, (error_name (errno)));
      return (parameterization_termination (1, first_time_p));
    }
  }

  if (n_gc_drones == 0)
    shared_memory = ((char *) (ALIGN_UP_TO_IO_PAGE (malloc_memory)));
  else
  {
    if ((shmid = (shmget (IPC_PRIVATE, shared_size, 0600))) == -1)
    {
      outf_error
	("%s (sysV_initialize): shmget (-, %d, -) failed (errno = %s).\n\
          \tUnable to allocate shared memory for drone processes.\n",
	 scheme_program_name, shared_size, (error_name (errno)));
      return (parameterization_termination (0, first_time_p));
    }
    shared_memory = (shmat (shmid, ATTACH_POINT, 0));
    if (shared_memory == ((char *) -1))
    {
      int saved_errno = errno;

      (void) (shmctl (shmid, IPC_RMID, 0));
      shmid = -1;
      outf_error
	("%s (sysV_initialize): shmat (%d, 0x%lx, 0) failed. (errno = %s).\n\
	  \tUnable to attach shared memory for drone processes.\n",
	 scheme_program_name, shmid, shared_size, (error_name (saved_errno)));
      return (parameterization_termination (0, first_time_p));
    }
    signal (SIGCONT, continue_running);
  }

  if (!(ALIGNED_TO_IO_PAGE_P (shared_memory)))
  {
    outf_error
      ("%s (sysV_initialize): buffer space is not aligned properly.\n\
        \taddress = 0x%lx; IO_PAGE_SIZE = 0x%lx.\n",
       ((long) shared_memory), ((long) IO_PAGE_SIZE));
    return (parameterization_termination (0, first_time_p));
  }

  if ((n_gc_drones != 0) && (malloc_size > 0)
      && (malloc_memory != ((char *) NULL)))
  {
    free (malloc_memory);
    malloc_memory = ((char *) NULL);
  }

  gc_buffers = ((struct buffer_info *) (shared_memory + buffer_space));
  gc_drones = ((struct drone_info *) (gc_buffers + n_gc_buffers));
  drone_version = ((unsigned long *) (gc_drones + n_gc_drones));
  wait_mask = (drone_version + 1);
  gc_read_queue = ((struct gc_queue_entry *) (drone_version + 2));
  gc_write_queue = (gc_read_queue + r_overlap);

  /* Initialize structures. */

  *wait_mask = ((unsigned long) 0);
  gc_next_drone = 0;
  gc_next_buffer = 0;

  drone_file_name = ((char *) drfnam);
  if ((drfnam != ((char *) NULL)) && (drfnam[0] != SUB_DIRECTORY_DELIMITER))
  {
    CONST char * temp = (search_for_library_file (drfnam));

    if (temp != ((char *) NULL))
    {
      drone_file_name = temp;
      if (drfnam != option_gc_drone)
	free ((PTR) drfnam);
    }
  }

  for (bufptr = ((SCHEME_OBJECT *) shared_memory), cntr = 0,
       buffer = gc_buffers;
       (cntr < n_gc_buffers);
       bufptr = buffer->end, cntr++, buffer++)
  {
    buffer->index = cntr;
    buffer->state = buffer_idle;
    buffer->position = -1;
    buffer->bottom = ((PTR) bufptr);
    buffer->top = ((PTR) (bufptr + gc_buffer_size));
    buffer->end = ((PTR) (bufptr + gc_total_buffer_size));
  }

  if (n_gc_drones == 0)
    shared_memory = ((char *) -1);
  else
  {
    struct gc_queue_entry * entry;
    struct drone_info * drone;

    /* Make sure that SIGCONT is enabled. */
    {
      sigset_t mask;

      UX_sigemptyset (&mask);
      UX_sigaddset ((&mask), SIGCONT);
      UX_sigprocmask (SIG_UNBLOCK, (&mask), 0);
    }

    for (cntr = 0, entry = gc_read_queue;
	 cntr < read_overlap;
	 cntr++, entry++)
    {
      entry->index = cntr;
      entry->state = entry_idle;
      entry->retry_count = 0;
    }

    for (cntr = 0, entry = gc_write_queue;
	 cntr < write_overlap;
	 cntr++, entry++)
    {
      entry->index = cntr;
      entry->state = entry_idle;
      entry->retry_count = 0;
    }

    for (cntr = 0, drone = gc_drones;
	 cntr < n_gc_drones;
	 cntr++, drone++)
    {
      drone->index = cntr;
      drone->state = drone_not_ready;
    }

    start_gc_drones (0, n_gc_drones, 0);
    if (gc_drones->state != drone_idle)
    {
      outf_error
	("%s (sysV_initialize): Problems starting up the GC drones%s.\n",
	 scheme_program_name,
	 (((* drone_version) != ((unsigned long) DRONE_VERSION_NUMBER))
	  ? " (wrong drone version)"
	  : ""));
      return (parameterization_termination (0, first_time_p));
    }
    drones_initialized_p = 1;
  }
  return (0);
}

static void EXFUN (close_gc_file, (int));

static void
DEFUN (sysV_shutdown, (final_time_p), int final_time_p)
{
  /* arg should be (n_gc_drones > 0), see sysV_initialize */
  if (final_time_p)
    close_gc_file (1);

  if (malloc_memory != ((char *) NULL))
  {
    free (malloc_memory);
    malloc_memory = ((char *) NULL);    
  }

  if ((n_gc_drones != 0) && (drones_initialized_p))
  {
    kill_all_gc_drones ();
    drones_initialized_p = 0;
  }

  if ((shared_memory != ((char *) -1)) && ((shmdt (shared_memory)) == -1))
    outf_error ("\n%s (sysV_shutdown): shmdt failed.  errno = %s.\n",
		scheme_program_name, (error_name (errno)));
  shared_memory = ((char *) -1);

  if ((shmid != -1)
      && (shmctl (shmid, IPC_RMID, ((struct shmid_ds *) 0))) == -1)
    outf_error ("\n%s (sysV_shutdown): shmctl failed.  errno = %s.\n",
		scheme_program_name, (error_name (errno)));
  shmid = -1;

  return;
}

static int
DEFUN (find_idle_drone, (wait_p), int wait_p)
{
  int drone_index, next_drone_index, count = 0;
  struct drone_info * drone;

  drone_index = gc_next_drone;
  while (1)
  {
    count += 1;
    do
    {
      next_drone_index = (drone_index + 1);
      if (next_drone_index >= n_gc_drones)
	next_drone_index = 0;

      drone = (gc_drones + drone_index);
      switch (drone->state)
      {
      case drone_idle:
	gc_next_drone = next_drone_index;
	return (drone_index);

      case drone_dead:
	start_gc_drones (drone_index, 1, 1);
	/* fall through, look at it on next pass. */

      default:
	break;	    
      }
      drone_index = next_drone_index;
    } while (drone_index != gc_next_drone);

    /* All the drones are busy... */

    if (!wait_p)
    {
      STATISTICS_INCR (drone_request_failures);
      return (-1);
    }

    if (count == 10)
    {
      probe_all_gc_drones (0);
      count = 0;
    }
    else
    {
      /* Use -1 as the mask to awaken when any drone becomes idle. */

      sleep_awaiting_drones (default_sleep_period, ((unsigned long) -1));
      STATISTICS_INCR (drone_wait_cycles);
    }
  }
}

static void
DEFUN (abort_gc_drone, (drone), struct drone_info * drone)
{
  int restart_p = 0;
  sigset_t block_mask, signal_mask;
  
  UX_sigemptyset (&block_mask);
  UX_sigaddset ((&block_mask), SIGCONT);
  UX_sigprocmask (SIG_BLOCK, (&block_mask), (&signal_mask));

  *wait_mask = (((unsigned long) 1) << drone->index);
  if (drone->state != drone_idle)
  {
    if ((kill (drone->DRONE_PID, SIGQUIT)) == -1)
      restart_p = 1;
    else if (drone->state != drone_idle)
      UX_sigsuspend (&signal_mask);
  }
  *wait_mask = ((unsigned long) 0);
  UX_sigprocmask (SIG_SETMASK, (&signal_mask), 0);
  if (restart_p)
    start_gc_drones (drone->index, 1, 1);
  return;
}

static struct gc_queue_entry *
DEFUN (find_queue_entry, (queue, queue_size, position, drone_index),
       struct gc_queue_entry * queue AND int queue_size
       AND long position AND int drone_index)
{
  struct gc_queue_entry * entry; 
  int cntr;

  for (cntr = 0, entry = queue; cntr < queue_size; cntr++, entry++)
  {
    if ((entry->state != entry_idle)
	&& (((entry->buffer)->position == position)
	    || (entry->drone_index == drone_index)))
      return (entry);
  }
  return ((struct gc_queue_entry *) NULL);
}

enum allocate_request
{
  request_read,
  request_write,
  request_ready
};

static struct gc_queue_entry *
DEFUN (allocate_queue_entry, (queue, queue_size, position, request, mask),
       struct gc_queue_entry * queue AND int queue_size AND long position
       AND enum allocate_request request AND unsigned long * mask)
{
  struct gc_queue_entry * entry; 
  int cntr, queue_index, drone_index;
  unsigned long drone_mask;

  /* Examine all entries for duplicates, ergo no `break' */

  queue_index = -1;
  drone_mask = ((unsigned long) 0);
  for (cntr = 0, entry = queue; cntr < queue_size; cntr++, entry++)
  {
    if (entry->state == entry_idle)
      queue_index = cntr;
    else if ((entry->buffer)->position == position)
      return (entry);
    else if (entry->state == entry_error)
    {
      struct buffer_info * buffer = entry->buffer;

      entry->retry_count += 1;
      if (entry->retry_count <= MAX_OVERLAPPED_RETRIES)
      {
	if (request == request_write)
	{
	  /* This was done when originally queued, but we are paranoid. */
	  (void) (verify_write (buffer->position, buffer->size,
				((Boolean *) NULL)));
	  do
	    entry->drone_index = (find_idle_drone (1));
	  while (!(invoke_gc_drone (entry, drone_writing, entry->buffer,
				    buffer->position, buffer->size)));
	  STATISTICS_INCR (writes_retried);
	}
	else
	{
	  entry->drone_index = (find_idle_drone (0));
	  if ((entry->drone_index != -1)
	      && (invoke_gc_drone (entry, drone_reading, entry->buffer,
				   buffer->position, buffer->size)))
	    STATISTICS_INCR (pre_reads_retried);
	  else
	    STATISTICS_INCR (pre_reads_not_retried);
	}
      }
      else if (request == request_write)
      {
	STATISTICS_INCR (writes_not_deferred);
	write_data (((char *) (buffer->bottom)),
		    buffer->position, buffer->size,
		    "a queued buffer", ((Boolean *) NULL));
	buffer->state = buffer_idle;
	entry->state = entry_idle;
	entry->retry_count = 0;
	queue_index = cntr;
      }
      else
	/* If pre-reading, it will be taken care of later. */
	STATISTICS_INCR (pre_reads_deferred);
    }
    else if ((drone_index = (entry->drone_index)) != -1)
      drone_mask |= (((unsigned long) 1) << drone_index);
  }

  if (queue_index == -1)
  {
    probe_all_gc_drones (0);
    if (mask != ((unsigned long *) NULL))
      (* mask) = drone_mask;
    return ((struct gc_queue_entry *) NULL);
  }

  entry = (queue + queue_index);
  entry->buffer = ((struct buffer_info *) NULL);
  return (entry);
}

static struct buffer_info *
DEFUN_VOID (find_idle_buffer)
{
  int next_buffer, new_next_buffer;
  struct buffer_info *buffer;

  next_buffer = gc_next_buffer;
  do
  {
    new_next_buffer = (next_buffer + 1);
    if (new_next_buffer >= n_gc_buffers)
      new_next_buffer = 0;
    buffer = (gc_buffers + next_buffer);
    if (buffer->state == buffer_idle)
    {
      gc_next_buffer = new_next_buffer;
      return (buffer);
    }
    next_buffer = new_next_buffer;
  } while (next_buffer != gc_next_buffer);

  outf_fatal ("\n%s (find_idle_buffer): All buffers are in use!\n",
	      scheme_program_name);
  Microcode_Termination (TERM_GC_OUT_OF_SPACE);
  /*NOTREACHED*/
  return (0);
}

static struct buffer_info * 
DEFUN (find_ready_buffer, (position, size), long position AND long size)
{
  int next_buffer, new_next_buffer;
  struct buffer_info *buffer;

  next_buffer = gc_next_buffer;
  do
  {
    new_next_buffer = (next_buffer + 1);
    if (new_next_buffer >= n_gc_buffers)
      new_next_buffer = 0;
    buffer = (gc_buffers + next_buffer);
    if ((buffer->state == buffer_idle) /* && (buffer->size == size) */
	&& (buffer->position == position))
    {
      gc_next_buffer = new_next_buffer;
      return (buffer);
    }
    next_buffer = new_next_buffer;
  } while (next_buffer != gc_next_buffer);
  return ((struct buffer_info *) NULL);
}

static struct buffer_info *
DEFUN_VOID (get_gc_buffer)
{
  struct buffer_info * buffer;

  buffer = (find_idle_buffer ());
  buffer->state = buffer_busy;
  return (buffer);
}

static struct buffer_info *
DEFUN (read_buffer, (posn, size, noise),
       long posn AND long size AND char * noise)
{
  struct gc_queue_entry * entry;
  struct buffer_info * buffer;

  if ((read_overlap > 0)
      && ((entry = (find_queue_entry (gc_read_queue, read_overlap, posn, -2)))
	  != ((struct gc_queue_entry *) NULL))
      && ((buffer = entry->buffer) != ((struct buffer_info *) NULL)))
  {
    switch (buffer->state)
    {
      default:
        outf_error
	  ("\n%s (read_buffer %s): invalid state.\n\
	    \tindex = %d; state = %d; position = 0x%lx.\n",
	   scheme_program_name, noise, buffer->index, buffer->state, posn);
	/* fall through */

      case buffer_read_error:
	/* Try synchronously, and complain then if the condition persists. */
	break;

      case buffer_being_read:
      {
	int count;
	struct drone_info * drone = (gc_drones + entry->drone_index);

	for (count = 1; (buffer->state == buffer_being_read) ; count++)
	{
	  if (count == 10)
	  {
	    if (probe_gc_drone (drone))
	      count = 0;
	    else
	    {
	      start_gc_drones (drone->index, 1, 1);
	      goto buffer_failed;
	    }
	  }
	  else
	    sleep_awaiting_drones (default_sleep_period,
				   (((unsigned long) 1) << drone->index));
	  STATISTICS_INCR (read_wait_cycles);
	}

	if (buffer->state != buffer_ready)
	{
buffer_failed:
	  entry->state = entry_idle;
	  entry->retry_count = 0;
	  buffer->state = buffer_idle;
	  buffer->position = -1;
	  STATISTICS_INCR (reads_overlapped_aborted);
	  break;
	}
	STATISTICS_INCR (reads_pending);
	goto buffer_available;
      }

      case buffer_queued:
	STATISTICS_INCR (reads_queued);
	goto buffer_available;

      case buffer_ready:
	STATISTICS_INCR (reads_ready);

buffer_available:
	/* This should check size, but they are all the same. */
	entry->state = entry_idle;
	entry->retry_count = 0;
	buffer->state = buffer_busy;
	STATISTICS_INCR (reads_overlapped);
	return (buffer);
    }
  }
  else if ((write_overlap > 0)
	   && ((entry = (find_queue_entry (gc_write_queue, write_overlap,
					   posn, -2)))
	       != ((struct gc_queue_entry *) NULL)))
  {
    int index;

    /* This should check size, but they are all the same. */

    entry->state = entry_idle;
    entry->retry_count = 0;
    buffer = entry->buffer;
    index = entry->drone_index;
    if (index != -1)
      abort_gc_drone (gc_drones + index);
    buffer->state = buffer_busy;
    STATISTICS_INCR (reads_found_in_write_queue);
    return (buffer);
  }
  else if ((buffer = (find_ready_buffer (posn, size)))
	   != ((struct buffer_info *) NULL))
  {
    /* This should check size, but they are all the same. */

    buffer->state = buffer_busy;
    STATISTICS_INCR (reads_found_ready);
    return (buffer);
  }

  /* (read_overlap == 0) or not pre-read. */
  {
    buffer = (find_idle_buffer ());

    load_data (posn, ((char *) buffer->bottom), size,
	       noise, ((Boolean *) NULL));
    buffer->state = buffer_busy;
    STATISTICS_INCR (reads_not_overlapped);
    return (buffer);
  }
}

static void
DEFUN (write_buffer, (buffer, position, size, success, noise),
       struct buffer_info * buffer AND long position
       AND long size AND Boolean * success AND char * noise)
{
  if ((write_overlap > 0) && ((verify_write (position, size, success)) != -1))
  {
    unsigned long drone_mask;
    struct gc_queue_entry * entry =
      (allocate_queue_entry (gc_write_queue, write_overlap,
			     position, request_write, (& drone_mask)));

    if (entry == ((struct gc_queue_entry *) NULL))
    {
      STATISTICS_INCR (writes_pending);
      do
      {
	sleep_awaiting_drones (default_sleep_period, drone_mask);
	entry =
	  (allocate_queue_entry (gc_write_queue, write_overlap,
				 position, request_write, (& drone_mask)));
	STATISTICS_INCR (write_wait_cycles);
      } while (entry == ((struct gc_queue_entry *) NULL));
    }
    else if (entry->buffer != NULL)
    {
      int index = entry->drone_index;
      struct buffer_info * old_buffer;

      if (index != -1)
	abort_gc_drone (gc_drones + index);
      old_buffer = entry->buffer;
      old_buffer->state = buffer_idle;
      entry->buffer = buffer;
      outf_error ("\n%s (write_buffer %s): duplicate write at 0x%lx.\n",
		  scheme_program_name, noise, position);
    }
    do
      entry->drone_index = (find_idle_drone (1));
    while (!(invoke_gc_drone (entry, drone_writing, buffer, position, size)));
    STATISTICS_INCR (writes_overlapped);
    return;
  }

  STATISTICS_INCR (writes_not_overlapped);
  write_data (((char *) buffer->bottom), position, size, noise, success);
  buffer->state = buffer_idle;
  return;
}

static void
DEFUN (enqueue_buffer, (entry, buffer, position, size, state),
       struct gc_queue_entry * entry AND struct buffer_info * buffer
       AND long position AND long size AND enum buffer_state state)
{
  buffer->state = state;
  buffer->position = position;
  buffer->size = size;
  entry->buffer = buffer;
  entry->drone_index = -1;
  entry->state = entry_busy;
  return;
}

static void
DEFUN (enqueue_ready_buffer, (buffer, position, size),
       struct buffer_info * buffer AND long position AND long size)
{
  struct gc_queue_entry * entry;

  if ((read_overlap == 0)
      || ((entry = (allocate_queue_entry (gc_read_queue, read_overlap,
					  position, request_ready,
					  ((unsigned long *) NULL))))
	  == ((struct gc_queue_entry *) NULL)))
  {
    write_buffer (buffer, position, size, ((char *) NULL), "a ready buffer");
    STATISTICS_INCR (ready_buffers_not_enqueued);
    return;
  }
  if (entry->buffer != NULL)  
  {
    int index = entry->drone_index;
    struct buffer_info * old_buffer = entry->buffer;

    if (index != -1)
      abort_gc_drone (gc_drones + index);
    old_buffer->state = buffer_idle;
    outf_error ("\n%s (enqueue_ready_buffer): Duplicate pre-read at 0x%lx.\n",
		scheme_program_name, old_buffer->position);
  }
  enqueue_buffer (entry, buffer, position, size, buffer_queued);
  STATISTICS_INCR (ready_buffers_enqueued);
  return;
}

static void
DEFUN (abort_pre_read, (position), long position)
{
  int index;
  struct gc_queue_entry * entry;
  struct buffer_info * buffer;
  
  entry = (find_queue_entry (gc_read_queue, read_overlap, position, -2));
  if (entry == ((struct gc_queue_entry *) NULL))
    return;
  buffer = entry->buffer;
  if (buffer->state == buffer_queued)
  {
    entry->state = entry_idle;
    entry->retry_count = 0;
    write_buffer (buffer, buffer->position, buffer->size,
		  ((Boolean *) NULL), "a queued buffer");
    STATISTICS_INCR (pre_reads_requeued_as_writes);
    return;
  }
  index = entry->drone_index;
  if (index != -1)
    abort_gc_drone (gc_drones + index);
  buffer->state = buffer_idle;
  buffer->position = -1;
  entry->state = entry_idle;
  entry->retry_count = 0;
  STATISTICS_INCR (pre_reads_aborted);
  return;
}

static int
DEFUN (pre_read_buffer, (position, size), long position AND long size)
{
  struct gc_queue_entry * rentry, * wentry;
  struct buffer_info * buffer;

  if (read_overlap <= 0)
    return (0);

  /* Do this first, to guarantee that we can insert it in the queue.
     Otherwise there is no point in aborting a write, etc.
     It is not really allocated until enqueue_buffer or invoke_gc_drone.
   */

  rentry = (allocate_queue_entry (gc_read_queue, read_overlap,
				  position, request_read,
				  ((unsigned long *) NULL)));
  if (rentry == ((struct gc_queue_entry *) NULL))
  {
    STATISTICS_INCR (pre_reads_ignored);
    return (0);
  }
  else if (rentry->buffer != NULL)
    /* Already being pre-read */
    return (1);

  if ((write_overlap > 0)
      && ((wentry = (find_queue_entry (gc_write_queue, write_overlap,
				       position, -2)))
	  != ((struct gc_queue_entry *) NULL)))
  {
    int index = wentry->drone_index;

    buffer = wentry->buffer;
    if (index != -1)
      abort_gc_drone (gc_drones + index);
    wentry->state = entry_idle;
    wentry->retry_count = 0;
    enqueue_buffer (rentry, buffer, position, size, buffer_queued);
    STATISTICS_INCR (pre_reads_found_in_write_queue);
    return (1);
  }
  else if ((buffer = (find_ready_buffer (position, size)))
	   != ((struct buffer_info *) NULL))
  {
    enqueue_buffer (rentry, buffer, position, size, buffer_ready);
    STATISTICS_INCR (pre_reads_found_ready);
    return (1);
  }

  if (((rentry->drone_index = (find_idle_drone (0))) == -1)
      || (!(invoke_gc_drone (rentry, drone_reading, (find_idle_buffer ()),
			     position, size))))
  {
    STATISTICS_INCR (pre_reads_not_started);
    return (0);
  }
  STATISTICS_INCR (pre_reads_started);
  return (1);
}

static void
DEFUN (handle_drone_death, (drone), struct drone_info * drone)
{
  struct buffer_info * buffer;
  struct gc_queue_entry * entry;

  STATISTICS_INCR (drones_found_dead);
  if ((entry = (find_queue_entry (gc_write_queue, write_overlap,
				  -1, drone->index)))
      != ((struct gc_queue_entry *) NULL))
  {
    buffer = entry->buffer;
    entry->state = entry_idle;
    entry->retry_count = 0;
    if (buffer->state != buffer_idle)
    {
      write_buffer (buffer, buffer->position, buffer->size,
		    ((Boolean *) NULL), "a queued buffer whose drone died");
      STATISTICS_INCR (writes_restarted);
    }
  }
  else if ((entry = (find_queue_entry (gc_read_queue, read_overlap,
				       -1, drone->index)))
	   != ((struct gc_queue_entry *) NULL))
  {
    buffer = entry->buffer;
    if (buffer->state != buffer_ready)
    {
      entry->state = entry_idle;
      entry->retry_count = 0;
      buffer->state = buffer_idle;
      STATISTICS_INCR (pre_reads_restarted);
      (void) (pre_read_buffer (buffer->position, buffer->size));
    }
  }
  return;
}

static void
DEFUN (await_io_completion, (start_p), int start_p)
{
  int cntr;
  struct buffer_info * buffer;
  struct gc_queue_entry * entry;

  if (n_gc_drones != 0)
    probe_all_gc_drones (1);
  if (start_p)
  {
    for (cntr = 0, buffer = gc_buffers; cntr < n_gc_buffers; cntr++, buffer++)
    {
      buffer->state = buffer_idle;
      buffer->position = -1;
    }
    for (cntr = 0, entry = gc_read_queue; cntr < read_overlap; cntr++, entry++)
      entry->state = entry_idle;
    for (cntr = 0, entry = gc_write_queue; cntr < write_overlap;
	 cntr++, entry++)
      entry->state = entry_idle;
  }
  return;
}

#define CAN_RECONFIGURE_GC_BUFFERS	1

#define GC_BUFFER_ALLOCATION(space)	0

#define INITIALIZE_GC_BUFFERS(ft, start, size, ro, wo, gcd)		\
 sysV_initialize (ft, size, ro, wo, gcd)

#define RE_INITIALIZE_GC_BUFFERS(ft, start, size, ro, wo, gcd)		\
 sysV_initialize (ft, size, ro, wo, gcd)

#define BUFFER_SHUTDOWN(lt)		sysV_shutdown (lt)

#define INITIALIZE_IO()			await_io_completion (1)
#define AWAIT_IO_COMPLETION()		await_io_completion (0)

#define INITIAL_SCAN_BUFFER()		free_buffer		/* NOP */
#define INITIAL_FREE_BUFFER()		get_gc_buffer ()
#define OTHER_BUFFER(buffer)		get_gc_buffer ()

#define GC_BUFFER_BOTTOM(buffer) 	((SCHEME_OBJECT *) buffer->bottom)
#define GC_BUFFER_TOP(buffer) 		((SCHEME_OBJECT *) buffer->top)

#define READ_BUFFER			read_buffer
#define DUMP_BUFFER			write_buffer
#define PRE_READ_BUFFER			pre_read_buffer
#define ABORT_PRE_READ			abort_pre_read
#define ENQUEUE_READY_BUFFER		enqueue_ready_buffer

#define LOAD_BUFFER(buffer, position, size, noise)			\
  buffer = (read_buffer (position, size, noise))

#else /* not USE_SYSV_SHARED_MEMORY */

static struct buffer_info
  * gc_disk_buffer_1,
  * gc_disk_buffer_2;

#define CAN_RECONFIGURE_GC_BUFFERS	0

#define GC_BUFFER_ALLOCATION(space)	space

#define INITIALIZE_GC_BUFFERS(ft, start, size, ro, wo, gcd)		\
do {									\
  SCHEME_OBJECT * ptr = (start);					\
									\
  gc_disk_buffer_1 = ((struct buffer_info *) ptr);			\
  gc_disk_buffer_2 = ((struct buffer_info *)				\
		      (ptr + gc_total_buffer_size));			\
  open_gc_file (size, 1);						\
} while (0)

#define BUFFER_SHUTDOWN(lt)	close_gc_file (lt)

#define INITIALIZE_IO()		do { } while (0)
#define AWAIT_IO_COMPLETION()	do { } while (0)

#define INITIAL_FREE_BUFFER()	gc_disk_buffer_1
#define INITIAL_SCAN_BUFFER()	OTHER_BUFFER(free_buffer)

/* (gc_disk_buffer_1 - (gc_disk_buffer_2 - (buffer))) does not work
   because scan_buffer is not initialized until after scanning
   constant space.  */

#define OTHER_BUFFER(buffer)	(((buffer) == gc_disk_buffer_1)		\
				 ? gc_disk_buffer_2			\
				 : gc_disk_buffer_1)

#define GC_BUFFER_BOTTOM(buffer) ((SCHEME_OBJECT *) (buffer))
#define GC_BUFFER_TOP(buffer) (((SCHEME_OBJECT *) (buffer)) + gc_buffer_size)

static int
DEFUN (catastrophic_failure, (name), char * name)
{
  outf_fatal ("\n%s: Procedure %s should never be called!\n",
	      scheme_program_name, name);
  Microcode_Termination (TERM_EXIT);
  /*NOTREACHED*/
  return (0);
}

#define GCDIE(m)			catastrophic_failure (m)

#define RE_INITIALIZE_GC_BUFFERS(f,s,z,r,w,g)				\
					GCDIE ("RE_INITIALIZE_GC_BUFFERS")
#define READ_BUFFER(p,s,n)		GCDIE ("read_buffer")
#define PRE_READ_BUFFER(p,s)		GCDIE ("pre_read_buffer")
#define ABORT_PRE_READ(p)		GCDIE ("abort_pre_read")
#define ENQUEUE_READY_BUFFER(b,p,s)	GCDIE ("enqueue_ready_buffer")

#define LOAD_BUFFER(buffer, position, size, noise)			\
  load_data (position, ((char *) buffer), size, noise, ((Boolean *) NULL))

#define DUMP_BUFFER(buffer, position, size, successp, noise)		\
  write_data (((char *) buffer), position, size, noise, successp)

#endif /* not USE_SYSV_SHARED_MEMORY */

#define DUMP_SCAN_BUFFER(success)					\
  DUMP_BUFFER (scan_buffer, scan_position, gc_buffer_bytes,		\
	       success, "the scan buffer")

#define DUMP_FREE_BUFFER(success)					\
  DUMP_BUFFER (free_buffer, free_position, gc_buffer_bytes,		\
	       success, "the free buffer")

#define LOAD_SCAN_BUFFER()						\
  LOAD_BUFFER (scan_buffer, scan_position, gc_buffer_bytes,		\
	       "the scan buffer")

#define LOAD_FREE_BUFFER()						\
  LOAD_BUFFER (free_buffer, free_position, gc_buffer_bytes,		\
	       "the free buffer")

static int
DEFUN (next_exponent_of_two, (value), int value)
{
  unsigned int power;
  int exponent;

  if (value < 0)
    return (0);
  
  for (power = 1, exponent = 0;
       power < ((unsigned int) value);
       power = (power << 1), exponent += 1)
    ;
  return (exponent);
}

/* Hacking the gc file */

static int
  saved_gc_file = -1,
  saved_read_overlap,
  saved_write_overlap;

static long
  saved_start_position,
  saved_end_position;

int
DEFUN (swap_gc_file, (fid), int fid)
{
  /* Do not use overlapped I/O for fasdump because the drone processes
     will continue writing to the same old file!
   */
  saved_gc_file = gc_file;
  saved_read_overlap = read_overlap;
  saved_write_overlap = write_overlap;
  saved_start_position = gc_file_start_position;
  saved_end_position = gc_file_end_position;
  gc_file = fid;
  read_overlap = 0;
  write_overlap = 0;
  gc_file_end_position
    = (absolute_gc_file_end_position - gc_file_start_position);
  gc_file_start_position = 0L;
  return (saved_gc_file);
}

void
DEFUN_VOID (restore_gc_file)
{
  gc_file = saved_gc_file;
  read_overlap = saved_read_overlap;
  write_overlap = saved_write_overlap;
  gc_file_start_position = saved_start_position;
  gc_file_end_position = saved_end_position;
  saved_gc_file = -1;
  return;
}

static void
DEFUN (close_gc_file, (unlink_p), int unlink_p)
{
#ifdef HAVE_LOCKF
  if (gc_file != -1)
    {
      if ((lseek (gc_file, gc_file_start_position, SEEK_SET)) < 0)
	perror ("lseek");
      if ((lockf (gc_file, F_ULOCK,
		  (gc_file_end_position - gc_file_start_position)))
	  < 0)
	perror ("lockf");
    }
#endif
  if ((gc_file != -1) && ((close (gc_file)) == -1))
    outf_error ("\n%s (close_gc_file): error: GC file = \"%s\"; errno = %s.\n",
		scheme_program_name, gc_file_name, (error_name (errno)));
  gc_file = -1;
  if (!keep_gc_file_p && unlink_p)
    unlink (gc_file_name);
  OS_free (gc_file_name);
  gc_file_name = 0;
  keep_gc_file_p = 0;
}

#define EMPTY_STRING_P(string)						\
  (((string) == ((char *) NULL)) || ((*(string)) == '\0'))

static void
DEFUN (termination_open_gc_file, (operation, extra),
       CONST char * operation AND CONST char * extra)
{
  if ((! (EMPTY_STRING_P (operation))) && (! (EMPTY_STRING_P (extra))))
    outf_fatal
      ("%s (open_gc_file): %s (\"%s\") failed, (errno = %s).\n\t%s.\n",
       scheme_program_name, operation, gc_file_name, (error_name (errno)),
       extra);
  else if (! (EMPTY_STRING_P (operation)))
    outf_fatal
      ("%s (open_gc_file): %s (\"%s\") failed, (errno = %s).\n",
       scheme_program_name, operation, gc_file_name, (error_name (errno)));
  else if (! (EMPTY_STRING_P (extra)))
    outf_fatal ("\t%s.\n", extra);
  termination_init_error ();
  /*NOTREACHED*/
}

char *
DEFUN (make_gc_file_name, (suffix), CONST char * suffix)
{
  unsigned int s = (strlen (suffix));
  if ((option_gc_file[0]) == SUB_DIRECTORY_DELIMITER)
    {
      unsigned int n
	= (((strrchr (option_gc_file, SUB_DIRECTORY_DELIMITER))
	    - option_gc_file)
	   + 1);
      char * result = (OS_malloc (n + s + 1));
      strncpy (result, option_gc_file, n);
      (result[n]) = '\0';
      strcat (result, suffix);
      return (result);
    }
  {
    unsigned int l = (strlen (option_gc_directory));
    if ((option_gc_directory [l - 1]) == SUB_DIRECTORY_DELIMITER)
      {
	unsigned int n = l;
	char * result = (OS_malloc (n + s + 1));
	sprintf (result, "%s%s", option_gc_directory, suffix);
	return (result);
      }
    else
      {
	unsigned int n = (l + 1);
	char * result = (OS_malloc (n + s + 1));
	sprintf (result, "%s%c%s",
		 option_gc_directory, SUB_DIRECTORY_DELIMITER, suffix);
	return (result);
      }
  }
}

int
DEFUN (allocate_gc_file, (name), char * name)
{
  /* `name' must end in 6 `X' characters.  */
  char * exxes = (name + ((strlen (name)) - 6));
  unsigned int n = 0;

  while (n < 1000000)
    {
      sprintf (exxes, "%06d", n);
      if (OS_file_touch (name))
	return (1);
      n += 1;
    }
  return (0);
}

void
DEFUN (protect_gc_file_name, (name), CONST char * name)
{
  CONST char ** p = (dstack_alloc (sizeof (char *)));
  (*p) = name;
  transaction_record_action (tat_always, OS_free, p);
}

#ifndef _POSIX_VERSION
extern off_t EXFUN (lseek, (int, off_t, int));
#endif

static void
DEFUN (open_gc_file, (size, unlink_p),
       long size AND
       int unlink_p)
{
  struct stat file_info;
  int flags;
  Boolean temp_p, exists_p;

  gc_file_name
    = (make_gc_file_name
       (((option_gc_file[0]) == SUB_DIRECTORY_DELIMITER)
	? ((strrchr (option_gc_file, SUB_DIRECTORY_DELIMITER)) + 1)
	: option_gc_file));

  {
    unsigned int n = (strlen (option_gc_file));
    if ((n >= 6) && ((strcmp ((option_gc_file + (n - 6)), "XXXXXX")) == 0))
      {
	if (!allocate_gc_file (gc_file_name))
	  {
	    outf_fatal
	      ("%s: Unable to allocate a temporary file for the spare heap.\n",
	       scheme_program_name);
	    termination_open_gc_file (0, 0);
	    /*NOTREACHED*/
	  }
	temp_p = true;
      }
    else
      temp_p = false;
  }

  flags = GC_FILE_FLAGS;
  gc_file_start_position = (ALIGN_UP_TO_IO_PAGE (option_gc_start_position));
  gc_file_end_position = option_gc_end_position;
  if (gc_file_end_position == -1)
    gc_file_end_position = (gc_file_start_position + size);
  gc_file_end_position = (ALIGN_DOWN_TO_IO_PAGE (gc_file_end_position));
  if (gc_file_end_position < gc_file_start_position)
  {
    outf_fatal
      ("%s (open_gc_file): file bounds are inconsistent.\n\
        \trequested start = 0x%lx;\taligned start = 0x%lx.\n\
	\trequested end   = 0x%lx;\taligned end   = 0x%lx.\n",
       scheme_program_name,
       option_gc_start_position, gc_file_start_position,
       option_gc_end_position, gc_file_end_position);
    termination_open_gc_file (0, 0);
  }

  absolute_gc_file_end_position = gc_file_end_position;

  if ((stat (gc_file_name, &file_info)) == -1)
  {
    exists_p = false;
    can_dump_directly_p = true;
    flags |= O_EXCL;
  }
  else
  {
#ifdef __unix__
    /* If it is S_IFCHR, it should determine the IO block
       size and make sure that it will work.
       I don't know how to do that.
       ustat(2) will do that for a mounted file system,
       but obviously, if a raw device file is used,
       there better not be a file system on the device or partition.
       Does st_blksize give the correct value? -- Apparently not.
       */

    exists_p = true;
    if ((file_info.st_mode & S_IFMT) == S_IFCHR)
      can_dump_directly_p = false;

    else if (((file_info.st_mode & S_IFMT) != S_IFREG)
	     && ((file_info.st_mode & S_IFMT) != S_IFBLK))
    {
      outf_fatal
	("%s (open_gc_file): file \"%s\" has unknown/bad type 0x%x.\n\
	  \tKnown types: S_IFREG (0x%x), S_IFBLK (0x%x), S_IFCHR (0x%x).\n",
	 scheme_program_name, gc_file_name,
	 ((int) (file_info.st_mode & S_IFMT)),
	 S_IFREG, S_IFBLK, S_IFCHR);
      termination_open_gc_file (((char *) NULL), ((char *) NULL));
    }
    else
      can_dump_directly_p = true;
#else
    /* Assume that it will be a normal file.  */
    exists_p = true;
    can_dump_directly_p = true;
#endif
  }

  gc_file = (open (gc_file_name, flags, GC_FILE_MASK));
  if (gc_file == -1)
  {
#ifndef __unix__
    /* errno does not give sufficient information except under unix. */

    int saved_errno = errno;
    char
      directory_buffer[FILE_NAME_LENGTH],
      * directory, * directory_end;

    directory = &directory_buffer[0];
    strcpy (directory, gc_file_name);
    directory_end = (strrchr (directory, SUB_DIRECTORY_DELIMITER));
    if (directory_end != ((char *) NULL))
      * directory_end = '\0';
    if ((access (directory, F_OK)) != 0)
    {
      outf_fatal
	("\n%s (open_gc_file): GC directory \"%s\" does not exist.\n",
	 scheme_program_name, directory);
      termination_open_gc_file (((char *) NULL), ((char *) NULL));
    }
    else if ((access (directory, W_OK)) != 0)
    {
      outf_fatal
	("\n%s (open_gc_file): GC directory \"%s\" is read protected.\n",
	 scheme_program_name, directory);
      termination_open_gc_file (((char *) NULL), ((char *) NULL));
    }      
    else
      errno = saved_errno;
#endif /* not __unix__ */
    termination_open_gc_file ("open", ((char *) NULL));
  }

  keep_gc_file_p = (option_gc_keep || (exists_p && (!temp_p)));

#ifdef UNLINK_BEFORE_CLOSE
  if (!keep_gc_file_p && unlink_p)
    unlink (gc_file_name);
#endif  

#ifdef HAVE_PREALLOC
  if (!exists_p)
    prealloc (gc_file, ((unsigned int) gc_file_end_position));
#endif

#ifdef HAVE_LOCKF
  if (exists_p)
    {
      if ((lseek (gc_file, gc_file_start_position, SEEK_SET)) < 0)
	termination_open_gc_file ("lseek", ((char *) NULL));

      if ((lockf (gc_file, F_TLOCK, size)) < 0)
	termination_open_gc_file
	  ("lockf",
	   "The GC file is probably being used by another process");
    }
#endif

  gc_file_current_position = -1;	/* Unknown position */

#ifdef __unix__
  /* Determine whether it is a seekable file. */
  if (exists_p && ((file_info.st_mode & S_IFMT) == S_IFCHR))
  {
#ifdef HAVE_FCNTL
    int fcntl_flags;
#endif
    Boolean ignore;
    static char message[] = "This is a test message to the GC file.\n";
    char * buffer;
  
    buffer = ((char *) aligned_heap);
    strcpy (buffer, &message[0]);
    strncpy ((buffer + ((sizeof (message)) - 1)),
	     buffer,
	     (IO_PAGE_SIZE - (sizeof (message))));
    (* (buffer + (IO_PAGE_SIZE - 1))) = '\n';

#ifdef HAVE_FCNTL
    fcntl_flags = (fcntl (gc_file, F_GETFL, 0));
    if (fcntl_flags != (-1))
      fcntl (gc_file, F_SETFL, (fcntl_flags | O_NONBLOCK));
#endif

    write_data (buffer,
		(gc_file_start_position + ((long) IO_PAGE_SIZE)),
		((long) IO_PAGE_SIZE),
		"a test buffer (1)",
		&ignore);
    load_data (gc_file_start_position,
	       (buffer + IO_PAGE_SIZE),
	       ((long) (2 * IO_PAGE_SIZE)),
	       "a test buffer (2)",
	       &ignore);
    if ((strncmp (buffer, (buffer + (2 * IO_PAGE_SIZE)), IO_PAGE_SIZE)) != 0)
    {
      outf_fatal ("\n%s (open_gc_file): \"%s\" is not a seek-able device.\n",
		  scheme_program_name, gc_file_name);
      termination_open_gc_file (((char *) NULL), ((char *) NULL));
    }
#ifdef HAVE_FCNTL
    if (fcntl_flags != (-1))
      fcntl (gc_file, F_SETFL, fcntl_flags);
#endif
  }
#endif /* __unix__ */
}

#define CONSTANT_SPACE_FUDGE	128

Boolean
DEFUN (update_allocator_parameters, (ctop), SCHEME_OBJECT * ctop)
{
  SCHEME_OBJECT * htop;
  long new_end;

  /* buffer for impurify, etc. */
  ctop = ((SCHEME_OBJECT *)
	  (ALIGN_UP_TO_IO_PAGE (ctop + CONSTANT_SPACE_FUDGE)));
  htop = ((SCHEME_OBJECT *)
	  (ALIGN_DOWN_TO_IO_PAGE (Highest_Allocated_Address)));
  if (ctop >= htop)
    return (FALSE);

  new_end = (((char *) htop) - ((char *) ctop));
  new_end = (CEILING (new_end, gc_buffer_bytes));
  new_end += gc_file_start_position;
  if ((new_end > absolute_gc_file_end_position)
      && (! option_gc_end_position))
    return (FALSE);

  gc_file_end_position = new_end;
  Constant_Top = ctop;
  Heap_Bottom = Constant_Top;
  Heap_Top = htop;
  aligned_heap = Heap_Bottom;
  Local_Heap_Base = Heap_Bottom;
  Unused_Heap_Bottom = Heap_Top;
  Unused_Heap_Top = Highest_Allocated_Address;
  Free = Heap_Bottom;
  SET_MEMTOP (Heap_Top - GC_Reserve);
  return (TRUE);
}

Boolean
DEFUN_VOID (recompute_gc_end_position)
{
  SCHEME_OBJECT * htop;
  long new_end, delta;

  if ((((gc_file_end_position - gc_file_start_position) % gc_buffer_bytes)
       == 0)
      || option_gc_end_position)
    return (TRUE);

  htop = ((SCHEME_OBJECT *)
	  (ALIGN_DOWN_TO_IO_PAGE (Highest_Allocated_Address)));
  new_end = (CEILING ((((char *) htop) - ((char *) Constant_Top)),
		      gc_buffer_bytes));
  new_end += gc_file_start_position;
  if (new_end <= absolute_gc_file_end_position)
  {
    gc_file_end_position = new_end;
    return (TRUE);
  }
  delta = (FLOOR ((absolute_gc_file_end_position - gc_file_start_position),
		  gc_buffer_bytes));
  if ((((char *) Constant_Top) + delta) <= (((char *) Free) + GC_Reserve))
    /* This should really GC and retry, but ... */
    return (FALSE);
  Heap_Top = ((SCHEME_OBJECT *) (((char *) Constant_Top) + delta));
  SET_MEMTOP (Heap_Top - GC_Reserve);
  return (TRUE);
}

void
DEFUN_VOID (reset_allocator_parameters)
{
  GC_Reserve = 4500;
  GC_Space_Needed = 0;
  Stack_Bottom = ((SCHEME_OBJECT *)
		  (ALIGN_UP_TO_IO_PAGE (Lowest_Allocated_Address)));
  Stack_Top = ((SCHEME_OBJECT *)
	       (ALIGN_DOWN_TO_IO_PAGE
		(Stack_Bottom + (STACK_ALLOCATION_SIZE (saved_stack_size)))));
  Constant_Space = Stack_Top;
  Free_Constant = Constant_Space;
  (void) update_allocator_parameters (Free_Constant);
  SET_CONSTANT_TOP ();
  ALIGN_FLOAT (Free);
  INITIALIZE_STACK ();
  STACK_RESET ();
  return;
}

void
DEFUN (Clear_Memory, (heap_size, stack_size, constant_space_size),
       int heap_size
       AND int stack_size
       AND int constant_space_size)
{
  saved_heap_size = heap_size;
  saved_constant_size = constant_space_size;
  saved_stack_size = stack_size;
  reset_allocator_parameters ();
}

void
DEFUN_VOID (Reset_Memory)
{
  BUFFER_SHUTDOWN (1);
  HEAP_FREE (Lowest_Allocated_Address);
  DEALLOCATE_REGISTERS ();
  return;
}

#define BLOCK_TO_IO_SIZE(size)						\
  ((ALIGN_UP_TO_IO_PAGE ((size) * (sizeof (SCHEME_OBJECT))))		\
   / (sizeof (SCHEME_OBJECT)))

static int
DEFUN (set_gc_buffer_sizes, (new_buffer_shift), unsigned long new_buffer_shift)
{
  unsigned long
    new_buffer_size, new_buffer_bytes, new_buffer_byte_shift,
    new_buffer_overlap_bytes, new_extra_buffer_size;
  
  new_buffer_size = (1L << new_buffer_shift);
  new_buffer_bytes = (new_buffer_size * (sizeof (SCHEME_OBJECT)));
  if (! (ALIGNED_TO_IO_PAGE_P (new_buffer_bytes)))
  {
    outf_error
      ("%s (Setup_Memory): improper new_buffer_size.\n\
	\tIO_PAGE_SIZE   = 0x%lx bytes.\n\
	\tgc_buffer_size = 0x%lx bytes = 0x%lx objects.\n\
	\tIO_PAGE_SIZE should divide gc_buffer_size.\n",
       scheme_program_name,
       ((long) IO_PAGE_SIZE),
       new_buffer_bytes, new_buffer_size);
    return (-1);
  }

  new_buffer_byte_shift = (next_exponent_of_two (new_buffer_bytes));
  if ((((unsigned long) 1L) << new_buffer_byte_shift) != new_buffer_bytes)
  {
    outf_error
      ("%s (Setup_Memory): gc_buffer_bytes (0x%lx) is not a power of 2.\n",
       scheme_program_name, new_buffer_bytes);
    return (-1);
  }

  new_buffer_overlap_bytes = IO_PAGE_SIZE;
  new_extra_buffer_size
    = (new_buffer_overlap_bytes / (sizeof (SCHEME_OBJECT)));
  if ((new_extra_buffer_size * (sizeof (SCHEME_OBJECT)))
      != new_buffer_overlap_bytes)
  {
    outf_error
      (" %s (Setup_Memory): improper IO_PAGE_SIZE.\n\
	\tIO_PAGE_SIZE = 0x%lx; (sizeof (SCHEME_OBJECT)) = 0x%lx.\n\
	\t(sizeof (SCHEME_OBJECT)) should divide IO_PAGE_SIZE.\n",
       scheme_program_name,
       ((long) IO_PAGE_SIZE), ((long) (sizeof (SCHEME_OBJECT))));
    return (-1);
  }

  gc_buffer_shift = new_buffer_shift;
  gc_buffer_size = new_buffer_size;
  gc_buffer_bytes = new_buffer_bytes;
  gc_buffer_mask = (gc_buffer_size - 1);
  gc_buffer_byte_shift = new_buffer_byte_shift;
  gc_buffer_overlap_bytes = new_buffer_overlap_bytes;
  gc_extra_buffer_size = new_extra_buffer_size;
  gc_buffer_remainder_bytes = (gc_buffer_bytes - gc_buffer_overlap_bytes);
  gc_total_buffer_size = (gc_buffer_size + gc_extra_buffer_size);
  return (0);
}

void
DEFUN (Setup_Memory, (heap_size, stack_size, constant_space_size),
       int heap_size
       AND int stack_size
       AND int constant_space_size)
{
  SCHEME_OBJECT test_value;
  int real_stack_size;
  long gc_buffer_allocation;

  ALLOCATE_REGISTERS ();

  /* Consistency check 1 */
  if (heap_size == 0)
  {
    outf_fatal ("%s (Setup_Memory): Configuration won't hold initial data.\n",
		scheme_program_name);
    termination_init_error ();
    /*NOTREACHED*/
  }

  real_stack_size = (STACK_ALLOCATION_SIZE (stack_size));

  /* add log(1024)/log(2) to exponent */
  if ((set_gc_buffer_sizes (10
			    + (next_exponent_of_two (option_gc_window_size))))
      != 0)
    parameterization_termination (1, 1);

  /* Use multiples of IO_PAGE_SIZE. */

  heap_size = (BLOCK_TO_IO_SIZE (heap_size));
  constant_space_size = (BLOCK_TO_IO_SIZE (constant_space_size));
  real_stack_size = (BLOCK_TO_IO_SIZE (real_stack_size));
  gc_buffer_allocation = (GC_BUFFER_ALLOCATION (2 * gc_total_buffer_size));

  /* Allocate. */

  ALLOCATE_HEAP_SPACE ((heap_size
			+ constant_space_size + real_stack_size
			+ gc_buffer_allocation
			+ (IO_PAGE_SIZE / (sizeof (SCHEME_OBJECT)))),
		       Lowest_Allocated_Address,
		       Highest_Allocated_Address);

  /* Consistency check 2 */
  if (Lowest_Allocated_Address == NULL)
  {
    outf_fatal
      ("%s (Setup_Memory): Not enough memory for this configuration.\n",
       scheme_program_name);
    termination_init_error ();
    /*NOTREACHED*/
  }

  Highest_Allocated_Address -= gc_buffer_allocation;

  /* Consistency check 3 */
  test_value =
    (MAKE_POINTER_OBJECT (LAST_TYPE_CODE, Highest_Allocated_Address));

  if (((OBJECT_TYPE (test_value)) != LAST_TYPE_CODE) ||
      ((OBJECT_ADDRESS (test_value)) != Highest_Allocated_Address))
  {
    outf_fatal
      ("%s (Setup_Memory): \
	Largest address does not fit in datum field of object.\n\
	\tAllocate less space or re-configure without HEAP_IN_LOW_MEMORY.\n",
       scheme_program_name);
    Reset_Memory ();
    termination_init_error ();
    /*NOTREACHED*/
  }

  Clear_Memory (heap_size, stack_size, constant_space_size);
  INITIALIZE_GC_BUFFERS (1,
			 Highest_Allocated_Address,
			 ((sizeof (SCHEME_OBJECT))
			  * (CEILING ((heap_size + constant_space_size),
				      gc_buffer_size))),
			 option_gc_read_overlap,
			 option_gc_write_overlap,
			 option_gc_drone);
  return;
}

/* Utilities for the GC proper. */ 

static void
DEFUN (enqueue_free_buffer, (success), Boolean * success)
{
  int diff;

  diff = ((free_position - pre_read_position) >> gc_buffer_byte_shift);
  if (diff >= read_overlap)
    DUMP_FREE_BUFFER (success);
  else
  {
    ENQUEUE_READY_BUFFER (free_buffer, free_position, gc_buffer_bytes);
    read_queue_bitmask |= (1L << diff);
  }
  return;
}

static void
DEFUN_VOID (schedule_pre_reads)
{
  int cntr;
  long position;
  unsigned long bit;

  if (pre_read_position == scan_position)
  {
    read_queue_bitmask = (read_queue_bitmask >> 1);
    pre_read_position += gc_buffer_bytes;
  }
  for (cntr = 0, bit = 1L, position = pre_read_position;
       ((cntr < read_overlap) && (position < free_position));
       cntr++, bit = (bit << 1), position += gc_buffer_bytes)
  {
    if ((read_queue_bitmask & bit) != bit)
      if (PRE_READ_BUFFER (position, gc_buffer_bytes))
	read_queue_bitmask |= bit;
  }
  return;
}

static void
DEFUN_VOID (abort_pre_reads)
{
  while (scan_position > pre_read_position)
  {
    ABORT_PRE_READ (pre_read_position);
    pre_read_position += gc_buffer_bytes;
    read_queue_bitmask = (read_queue_bitmask >> 1);
  }
  schedule_pre_reads ();
  return;
}

static void
DEFUN (reload_scan_buffer, (skip), unsigned long skip)
{
  scan_position += (skip << gc_buffer_byte_shift);
  virtual_scan_pointer += (skip << gc_buffer_shift);

  if ((read_overlap > 0) && (scan_position > pre_read_position))
    abort_pre_reads ();

  if (scan_position == free_position)
  {
    pre_read_position = (free_position + gc_buffer_bytes);
    read_queue_bitmask = 0L;
    scan_buffer = free_buffer;
    scan_buffer_bottom = free_buffer_bottom;
    scan_buffer_top = free_buffer_top;
    return;
  }
  LOAD_SCAN_BUFFER ();
  scan_buffer_bottom = (GC_BUFFER_BOTTOM (scan_buffer));
  scan_buffer_top = (GC_BUFFER_TOP (scan_buffer));
  *scan_buffer_top = (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
  
  if (read_overlap > 0)
    schedule_pre_reads ();
}

SCHEME_OBJECT *
DEFUN (dump_and_reload_scan_buffer, (end, success),
       SCHEME_OBJECT * end AND
       Boolean * success)
{
  unsigned long number_to_skip = (end - scan_buffer_top);
  DUMP_SCAN_BUFFER (success);
  reload_scan_buffer (1 + (number_to_skip >> gc_buffer_shift));
  return (scan_buffer_bottom + (number_to_skip & gc_buffer_mask));
}

SCHEME_OBJECT *
DEFUN (dump_and_reset_free_buffer, (current_free, success),
       SCHEME_OBJECT * current_free AND
       Boolean * success)
{
  unsigned long overflow = (current_free - free_buffer_top);
  SCHEME_OBJECT * from = free_buffer_top;
  Boolean buffer_overlap_p = extension_overlap_p;
  Boolean same_buffer_p = (scan_buffer == free_buffer);

  if (read_overlap > 0)
    {
      if (buffer_overlap_p)
	{
	  extension_overlap_p = false;
	  next_scan_buffer = free_buffer;
	}
      else if (!same_buffer_p)
	enqueue_free_buffer (success);
    }
  else if (!same_buffer_p)
    DUMP_FREE_BUFFER (success);

  /* Otherwise there is no need to dump now, it will be dumped
     when scan is dumped.  Note that the next buffer may be dumped
     before this one, but there should be no problem lseeking past the
     end of file.  */
  free_position += gc_buffer_bytes;
  free_buffer = (OTHER_BUFFER (scan_buffer));
  free_buffer_bottom = (GC_BUFFER_BOTTOM (free_buffer));
  free_buffer_top = (GC_BUFFER_TOP (free_buffer));
  {
    SCHEME_OBJECT * into = free_buffer_bottom;
    SCHEME_OBJECT * end = (into + overflow);
    while (into < end)
      (*into++) = (*from++);
    if (same_buffer_p && (!buffer_overlap_p))
      (*scan_buffer_top)
	= (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
    return (into);
  }
}

/* These utilities are needed when pointers fall accross window boundaries.

   Between both they effectively do a dump_and_reload_scan_buffer, in two
   stages.
*/

void
DEFUN (extend_scan_buffer, (to_where, current_free),
       char * to_where AND
       SCHEME_OBJECT * current_free)
{
  fast char * source, * dest;
  long new_scan_position = (scan_position + gc_buffer_bytes);

  /* Is there buffer overlap?, i.e. is the next bufferful the one cached
     in the free pointer window?
   */

  scan_buffer_extended_p = true;
  dest = ((char *) scan_buffer_top);
  extension_overlap_length = (to_where - dest);
  extension_overlap_p = (new_scan_position == free_position);

  if (extension_overlap_p)
  {
    long temp;

    source = ((char *) free_buffer_bottom);
    temp = (((char *) current_free) - source);
    if (temp < extension_overlap_length)
    {
      /* This should only happen when Scan and Free are very close. */
      extension_overlap_length = temp;
    }
  }
  else if (read_overlap == 0)
  {
    load_data (new_scan_position, dest, gc_buffer_overlap_bytes,
	       "the next scan buffer", ((Boolean *) NULL));
    return;
  }
  else
  {
    LOAD_BUFFER (next_scan_buffer, new_scan_position,
		 gc_buffer_bytes, "the next scan buffer");
    source = ((char *) (GC_BUFFER_BOTTOM (next_scan_buffer)));
  }

  while (dest < to_where)
    *dest++ = *source++;
  return;
}

char *
DEFUN (end_scan_buffer_extension, (to_relocate), char * to_relocate)
{
  char * result;
  if (extension_overlap_p)
  {
    /* There was overlap between the scan buffer and the free buffer,
       there may no longer be, but dump_and_reload_scan_buffer will
       get us the correct next buffer.
       The old scan buffer may be written, but the while loop below
       will read storage contiguous to it (in the buffer extension).
     */
    SCHEME_OBJECT old, new;
    fast char * source, * dest, * limit;

    extension_overlap_p = false;
    source = ((char *) scan_buffer_top);
    old = (* ((SCHEME_OBJECT *) source));
    limit = (source + extension_overlap_length);
    dest = ((char *) (dump_and_reload_scan_buffer (scan_buffer_top, 0)));
    /* The following is only necesary if we are reusing the scan buffer. */
    new = (* scan_buffer_top);
    (* ((SCHEME_OBJECT *) source)) = old;
    result = (dest + (to_relocate - source));
    while (source < limit)
      *dest++ = *source++;
    (* scan_buffer_top) = new;
  }
  else if (next_scan_buffer == ((struct buffer_info *) NULL))
  {
    /* There was no buffer overlap and no read overlap */

    fast SCHEME_OBJECT * source, * dest, * limit;

    source = scan_buffer_top;
    limit = (source + gc_extra_buffer_size);

    DUMP_SCAN_BUFFER (0);
    scan_position += gc_buffer_bytes;
    virtual_scan_pointer += gc_buffer_size;

    scan_buffer = (OTHER_BUFFER (free_buffer));
    scan_buffer_bottom = (GC_BUFFER_BOTTOM (scan_buffer));
    scan_buffer_top = (GC_BUFFER_TOP (scan_buffer));

    dest = scan_buffer_bottom;
    result = (((char *) dest) + (to_relocate - ((char *) source)));

    while (source < limit)
      *dest++ = *source++;

    if (gc_buffer_remainder_bytes != 0)
      load_data ((scan_position + gc_buffer_overlap_bytes),
		 ((char *) dest), gc_buffer_remainder_bytes,
		 "the scan buffer", ((Boolean *) NULL));

    (* scan_buffer_top) =
      (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
  }
  else
  {
    /* There is overlap with the next bufferful (not the free bufferful). */

    fast char * source, * dest, * limit;

    source = ((char *) scan_buffer_top);
    limit = (source + extension_overlap_length);
    dest = ((char *) (GC_BUFFER_BOTTOM (next_scan_buffer)));
    result = (dest + (to_relocate - source));

    while (source < limit)
      *dest++ = *source++;
    
    DUMP_SCAN_BUFFER (0);
    scan_position += gc_buffer_bytes;
    virtual_scan_pointer += gc_buffer_size;

    scan_buffer = next_scan_buffer;
    next_scan_buffer = NULL;
    scan_buffer_bottom = (GC_BUFFER_BOTTOM (scan_buffer));
    scan_buffer_top = (GC_BUFFER_TOP (scan_buffer));
    (* scan_buffer_top) =
      (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
    schedule_pre_reads ();
  }
  scan_buffer_extended_p = false; 
  return (result);
}

/* This is used to avoid unnecessary copying when copying a large
   non-marked area.
 */

SCHEME_OBJECT *
DEFUN (dump_free_directly, (from, nbuffers, success),
       fast SCHEME_OBJECT * from
       AND fast long nbuffers
       AND Boolean * success)
{
  if (((read_overlap + write_overlap) == 0)
      && (can_dump_directly_p || (ALIGNED_TO_IO_PAGE_P (from))))
  {
    long byte_length = (nbuffers << gc_buffer_byte_shift);

    write_data (((char *) from), free_position, byte_length,
		"free buffers", success);
    free_position += byte_length;
  }
  else
  {
    /* This assumes that the free buffer has no valid data, so it can be
       used as scratch.
       This code is executed when there is I/O overlap, or when the
       data is not aligned to be written to a raw (character) device.
     */

    while ((--nbuffers) >= 0)
    {
      fast SCHEME_OBJECT * to, * bufend;

      for (to = free_buffer_bottom, bufend = free_buffer_top; to != bufend; )
	*to++ = *from++;

      (void) (dump_and_reset_free_buffer (to, success));
    }
  }
  return (free_buffer_bottom);
}

/* This code is needed by purify.  After the purified object is
   copied, the next step is to scan constant space.  In order to do
   this, it's necessary to save the current scan position, reset the
   scan limit pointers to scan constant space, then restore the saved
   scan position and finish scanning the heap.  These procedures
   provide the necessary functionality to do this.  */

static void
DEFUN_VOID (reset_scan_buffer)
{
  virtual_scan_pointer = 0;
  scan_position = (-1L);
  scan_buffer = 0;
  scan_buffer_bottom = 0;
  scan_buffer_top = Highest_Allocated_Address;
  next_scan_buffer = 0;
  scan_buffer_extended_p = false;
  extension_overlap_p = false;
  extension_overlap_length = 0;
}

void
DEFUN (save_scan_state, (state, scan),
       struct saved_scan_state * state AND
       SCHEME_OBJECT * scan)
{
  (state -> virtual_scan_pointer) = virtual_scan_pointer;
  (state -> scan_position) = scan_position;
  (state -> scan_offset) = (scan - scan_buffer_bottom);
  if (scan_position != free_position)
    DUMP_SCAN_BUFFER (0);
  reset_scan_buffer ();
}

SCHEME_OBJECT *
DEFUN (restore_scan_state, (state), struct saved_scan_state * state)
{
  virtual_scan_pointer = (state -> virtual_scan_pointer);
  scan_position = (state -> scan_position);
  if (scan_position == free_position)
    {
      scan_buffer = free_buffer;
      scan_buffer_bottom = free_buffer_bottom;
      scan_buffer_top = free_buffer_top;
    }
  else
    {
      scan_buffer = (OTHER_BUFFER (free_buffer));
      scan_buffer_bottom = (GC_BUFFER_BOTTOM (scan_buffer));
      scan_buffer_top = (GC_BUFFER_TOP (scan_buffer));
      LOAD_SCAN_BUFFER ();
    }
  return (scan_buffer_bottom + (state -> scan_offset));
}

void
DEFUN (set_fixed_scan_area, (bottom, top),
       SCHEME_OBJECT * bottom AND
       SCHEME_OBJECT * top)
{
  virtual_scan_pointer = bottom;
  scan_buffer_bottom = bottom;
  scan_buffer_top = top;
}

#ifndef START_TRANSPORT_HOOK
#define START_TRANSPORT_HOOK()		do { } while (0)
#endif

#ifndef END_TRANSPORT_HOOK
#define END_TRANSPORT_HOOK()		do { } while (0)
#endif

#ifndef END_WEAK_UPDATE_HOOK
#define END_WEAK_UPDATE_HOOK()		do { } while (0)
#endif

#ifndef START_RELOAD_HOOK
#define START_RELOAD_HOOK()		do { } while (0)
#endif

#ifndef END_GC_HOOK
#define END_GC_HOOK()			do { } while (0)
#endif

/* This hacks the scan buffer also so that Scan is always below
   scan_buffer_top until the scan buffer is initialized.
   Various parts of the garbage collector depend on scan_buffer_top
   having an aligned value.
*/

SCHEME_OBJECT *
DEFUN_VOID (initialize_free_buffer)
{
  STATISTICS_CLEAR ();
  START_TRANSPORT_HOOK ();
  read_queue_bitmask = 0L;
  pre_read_position = gc_file_start_position;
  free_position = gc_file_start_position;
  INITIALIZE_IO ();
  free_buffer = (INITIAL_FREE_BUFFER ());
  free_buffer_bottom = (GC_BUFFER_BOTTOM (free_buffer));
  free_buffer_top = (GC_BUFFER_TOP (free_buffer));
  reset_scan_buffer ();
  /* Force first write to do an lseek. */
  gc_file_current_position = -1;
  return (free_buffer_bottom);
}

SCHEME_OBJECT *
DEFUN (initialize_scan_buffer, (block_start), SCHEME_OBJECT * block_start)
{
  virtual_scan_base = block_start;
  virtual_scan_pointer = virtual_scan_base;
  scan_position = gc_file_start_position;
  scan_buffer = (INITIAL_SCAN_BUFFER ());
  scan_buffer_bottom = (GC_BUFFER_BOTTOM (scan_buffer));
  scan_buffer_top = (GC_BUFFER_TOP (scan_buffer));
  reload_scan_buffer (0);
  return (scan_buffer_bottom);
}

void
DEFUN (end_transport, (success), Boolean * success)
{
  DUMP_SCAN_BUFFER (success);
  scan_position += gc_buffer_bytes;
  virtual_scan_pointer += gc_buffer_size;
  free_position = scan_position;
  END_TRANSPORT_HOOK ();
  STATISTICS_PRINT (2, "after transport");
  return;
}

void
DEFUN (final_reload, (to, length, noise),
       SCHEME_OBJECT * to AND unsigned long length AND char * noise)
{
  unsigned long byte_length;

  byte_length = (ALIGN_UP_TO_IO_PAGE (length * (sizeof (SCHEME_OBJECT))));
  END_WEAK_UPDATE_HOOK ();
  AWAIT_IO_COMPLETION ();
  START_RELOAD_HOOK ();
  load_data (gc_file_start_position, ((char *) to), byte_length,
	     noise, ((Boolean *) NULL));
  END_GC_HOOK ();
  STATISTICS_PRINT (1, "after final reload");
  return;
}

static int
  weak_buffer_pre_read_count;

static long
  weak_pair_buffer_position;

static struct buffer_info
  * weak_pair_buffer;

static SCHEME_OBJECT
  weak_pair_break;

/* This procedure is not very smart.

   It does not attempt to figure out whether the position being
   requested is already being pre-read, nor does it look further down
   the weak chain list for duplicate positions, to avoid early writes.

   On the other hand, pre_read_buffer will ignore the request if it is
   a duplicate, and will abort a pending write if a read for the same
   position is requested.
 */   

static void
DEFUN (pre_read_weak_pair_buffers, (low_heap), SCHEME_OBJECT * low_heap)
{
  SCHEME_OBJECT next, * pair_addr, * obj_addr;
  long position, last_position;

  last_position = -1;
  next = weak_pair_break;
  while (next != EMPTY_WEAK_CHAIN)
  {
    pair_addr = (OBJECT_ADDRESS (next));
    obj_addr = (OBJECT_ADDRESS (*pair_addr++));
    if (! (obj_addr < low_heap))
    {
      position = (obj_addr - aligned_heap);
      position = (position >> gc_buffer_shift);
      position = (position << gc_buffer_byte_shift);
      position += gc_file_start_position;

      if ((position != last_position)
	  && (position != weak_pair_buffer_position))
      {
	last_position = position;
	if ((weak_buffer_pre_read_count >= read_overlap)
	    || (!(PRE_READ_BUFFER (position, gc_buffer_bytes))))
	  break;
	weak_buffer_pre_read_count += 1;
      }
    }
    next = (OBJECT_NEW_TYPE (TC_NULL, (*pair_addr)));
  }
  weak_pair_break = next;
  return;
}

/* The following code depends on being called in between copying objects,
   so that the "free" pointer points to the middle of the free buffer,
   and thus the overlap area at the end of the free buffer is available
   as temporary storage.  In addition, because we have not yet moved free,
   next_scan_buffer has not been set even if we are in the middle of a
   scan buffer extension.
 */

SCHEME_OBJECT
DEFUN (read_newspace_address, (addr), SCHEME_OBJECT * addr)
{
  long position;
  unsigned long offset;
  SCHEME_OBJECT result;

  if ((addr >= Constant_Space) && (addr < Free_Constant))
    return (* addr);

  position = (addr - virtual_scan_base);
  offset = (position & gc_buffer_mask);
  position = (position >> gc_buffer_shift);
  position = (position << gc_buffer_byte_shift);
  position += gc_file_start_position;

  if (position > free_position)
  {
    outf_fatal
      ("\n%s (read_newspace_address): Reading outside of GC window!\n\
	\t         addr = 0x%lx;\t     position = 0x%lx.\n\
	\tscan_position = 0x%lx;\tfree_position = 0x%lx.\n",
       scheme_program_name,
       addr, position,
       scan_position, free_position);
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/    
  }
  if (position == scan_position)
    result = (* (scan_buffer_bottom + offset));
  else if (position == free_position)
    result = (* (free_buffer_bottom + offset));
  else if ((position == ((long) (scan_position + gc_buffer_bytes)))
	   && scan_buffer_extended_p
	   && ((read_overlap != 0) || (offset < gc_extra_buffer_size)))
  {
    /* Note: we need not worry about the state of extension_overlap_p,
       because if there is overlap between the scan extension and the free
       buffer, then (position == free_position) would be true,
       and that case has already been taken care of.
     */
       
    result = ((read_overlap == 0)
	      ? (* (scan_buffer_top + offset))
	      : (* ((GC_BUFFER_BOTTOM (next_scan_buffer)) + offset)));
  }
  else if ((read_overlap <= 0) || (position > pre_read_position))
  {
    unsigned long position2;

    position = (((char *) addr) - ((char *) virtual_scan_base));
    position2 = (ALIGN_DOWN_TO_IO_PAGE (position));
    offset = (position - position2);
    position2 += gc_file_start_position;
    
    load_data (position2,
	       ((char *) free_buffer_top),
	       IO_PAGE_SIZE,
	       "a buffer for read_newspace_address",
	       ((Boolean *) NULL));
    result = (* ((SCHEME_OBJECT *) (((char *) free_buffer_top) + offset)));
  }
  else
  {
    /* The buffer is pre-read or in the process of being pre-read.
       Force completion of the read, fetch the location,
       and re-queue the buffer as ready.
     */

    LOAD_BUFFER (next_scan_buffer, position, gc_buffer_bytes,
		 "a buffer for read_newspace_address");
    result = ((GC_BUFFER_BOTTOM (next_scan_buffer)) [offset]);
    ENQUEUE_READY_BUFFER (next_scan_buffer, position, gc_buffer_bytes);
    next_scan_buffer = ((struct buffer_info *) NULL);
  }
  return (result);
}

static void
DEFUN (initialize_new_space_buffer, (chain, low_heap),
       SCHEME_OBJECT chain AND
       SCHEME_OBJECT * low_heap)
{
  if (read_overlap == 0)
  {
    weak_pair_break = EMPTY_WEAK_CHAIN;
    weak_pair_buffer = (INITIAL_FREE_BUFFER ());
    weak_pair_buffer_position = -1;
  }
  else
  {
    weak_pair_break = chain;
    weak_pair_buffer = ((struct buffer_info *) NULL);
    weak_pair_buffer_position = -1;
    weak_buffer_pre_read_count = 0;
    pre_read_weak_pair_buffers (low_heap);
  }
}

static void
DEFUN_VOID (flush_new_space_buffer)
{
  if (weak_pair_buffer_position == -1)
    return;
  DUMP_BUFFER (weak_pair_buffer, weak_pair_buffer_position,
	       gc_buffer_bytes, ((Boolean *) NULL),
	       "the weak pair buffer");
  weak_pair_buffer_position = -1;
  return;
}

static SCHEME_OBJECT *
DEFUN (guarantee_in_memory, (addr, low_heap),
       SCHEME_OBJECT * addr AND
       SCHEME_OBJECT * low_heap)
{
  long position, offset;

  if (addr < low_heap)
    return (addr);

  position = (addr - aligned_heap);
  offset = (position & gc_buffer_mask);
  position = (position >> gc_buffer_shift);
  position = (position << gc_buffer_byte_shift);
  position += gc_file_start_position;

  if (position != weak_pair_buffer_position)
  {
    flush_new_space_buffer ();
    LOAD_BUFFER (weak_pair_buffer, position, gc_buffer_bytes,
		 "the weak pair buffer");
    weak_pair_buffer_position = position;
    if (weak_pair_break != EMPTY_WEAK_CHAIN)
    {
      weak_buffer_pre_read_count -= 1;
      pre_read_weak_pair_buffers (low_heap);
    }
  }
  return ((GC_BUFFER_BOTTOM (weak_pair_buffer)) + offset);
}

/* For a description of the algorithm, see memmag.c and gccode.h.
   This has been modified only to account for the fact that new space
   is on disk.  Old space is in memory.
   Note: Compiled_BH requires the names Temp and Old!
*/

static SCHEME_OBJECT
DEFUN (update_weak_pointer, (Temp, low_heap),
       SCHEME_OBJECT Temp AND
       SCHEME_OBJECT * low_heap)
{
  SCHEME_OBJECT * Old;

  switch (GC_Type (Temp))
  {
    case GC_Non_Pointer:
      return (Temp);
  
    case GC_Special:
      if ((OBJECT_TYPE (Temp)) != TC_REFERENCE_TRAP)
	/* No other special type makes sense here. */
	goto fail;
      if ((OBJECT_DATUM (Temp)) <= TRAP_MAX_IMMEDIATE)
	return (Temp);
      /* Otherwise, it is a pointer.  Fall through */

    /* Normal pointer types, the broken heart is in the first word.
       Note that most special types are treated normally here.
       The BH code updates *Scan if the object has been relocated.
       Otherwise it falls through and we replace it with a full SHARP_F.
       Eliminating this assignment would keep old data (pl. of datum).
     */
    case GC_Cell:
    case GC_Pair:
    case GC_Triple:
    case GC_Quadruple:
    case GC_Vector:
      Old = (OBJECT_ADDRESS (Temp));
      if (Old < low_heap)
	return (Temp);

      if ((OBJECT_TYPE (*Old)) == TC_BROKEN_HEART)
	return (MAKE_OBJECT_FROM_OBJECTS (Temp, *Old));
      else
	return (SHARP_F);

    case GC_Compiled:
      Old = (OBJECT_ADDRESS (Temp));
      if (Old < low_heap)
	return (Temp);
      Compiled_BH (false, { return Temp; });
      return (SHARP_F);

    default:			/* Non Marked Headers and Broken Hearts */
    case GC_Undefined:
    fail:
      outf_error ("\n%s (update_weak_pointer): Clearing bad object 0x%08lx.\n",
		  scheme_program_name, Temp);
      return (SHARP_F);
  }
}

SCHEME_OBJECT
  Weak_Chain,
  * weak_pair_stack_ptr,
  * weak_pair_stack_limit;

void
DEFUN (initialize_weak_pair_transport, (limit), SCHEME_OBJECT * limit)
{
  Weak_Chain = EMPTY_WEAK_CHAIN;
  weak_pair_stack_ptr = sp_register;
  weak_pair_stack_limit = (limit + 1); /* in case it's odd */
  return;
}

void
DEFUN (fix_weak_chain_1, (low_heap), SCHEME_OBJECT * low_heap)
{
  fast SCHEME_OBJECT chain, * old_weak_cell, * scan, * ptr, * limit;

  chain = Weak_Chain;
  initialize_new_space_buffer (chain, low_heap);

  limit = sp_register;
  for (ptr = weak_pair_stack_ptr; ptr < limit ; ptr += 2)
    *ptr = (update_weak_pointer (*ptr, low_heap));

  while (chain != EMPTY_WEAK_CHAIN)
  {
    old_weak_cell = (OBJECT_ADDRESS (Weak_Chain));
    scan
      = (guarantee_in_memory ((OBJECT_ADDRESS (*old_weak_cell++)), low_heap));
    Weak_Chain = (* old_weak_cell);
    *scan
      = (update_weak_pointer
	 ((MAKE_OBJECT_FROM_OBJECTS (Weak_Chain, (* scan))), low_heap));
    Weak_Chain = (OBJECT_NEW_TYPE (TC_NULL, Weak_Chain));
  }
  flush_new_space_buffer ();
  Weak_Chain = chain;
  return;
}

void
DEFUN_VOID (fix_weak_chain_2)
{
  SCHEME_OBJECT * ptr, * limit, new_car, * addr;

  limit = sp_register;
  for (ptr = weak_pair_stack_ptr; ptr < limit ; )
  {
    new_car = *ptr++;
    addr = ((SCHEME_OBJECT *) (*ptr++));
    if (new_car != SHARP_F)
      *addr = new_car;
  }
  weak_pair_stack_ptr = limit;
  return;
}

long
DEFUN (GC_relocate_root, (free_buffer_ptr), SCHEME_OBJECT ** free_buffer_ptr)
{
  long skip;
  SCHEME_OBJECT * initial_free_buffer, * free_buffer;

  free_buffer = * free_buffer_ptr;
  initial_free_buffer = free_buffer;
  SET_MEMTOP (Heap_Top - GC_Reserve);

  /* Save the microcode registers so that they can be relocated */

  Set_Fixed_Obj_Slot (Precious_Objects, SHARP_F);
  Set_Fixed_Obj_Slot (Lost_Objects_Base, SHARP_F);

  *free_buffer++ = Fixed_Objects;
  *free_buffer++ = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, history_register));
  *free_buffer++ = (Get_Current_Stacklet ());
  *free_buffer++ = ((Prev_Restore_History_Stacklet == NULL) ?
		    SHARP_F :
		    (MAKE_POINTER_OBJECT (TC_CONTROL_POINT,
					  Prev_Restore_History_Stacklet)));

  *free_buffer++ = Current_State_Point;
  *free_buffer++ = Fluid_Bindings;
  skip = (free_buffer - initial_free_buffer);
  if (free_buffer >= free_buffer_top)
    free_buffer = (dump_and_reset_free_buffer (free_buffer, 0));
  * free_buffer_ptr = free_buffer;
  return (skip);
}

void
DEFUN (GC_end_root_relocation, (root, root2),
       SCHEME_OBJECT * root AND SCHEME_OBJECT * root2)
{
  /* Make the microcode registers point to the copies in new-space. */

  Fixed_Objects = *root++;
  Set_Fixed_Obj_Slot (Precious_Objects, *root2);
  Set_Fixed_Obj_Slot
    (Lost_Objects_Base, (LONG_TO_UNSIGNED_FIXNUM (ADDRESS_TO_DATUM (root2))));

  history_register = (OBJECT_ADDRESS (*root++));
  Set_Current_Stacklet (* root);
  root += 1;
  if ((* root) != SHARP_F)
    Prev_Restore_History_Stacklet = (OBJECT_ADDRESS (*root++));
  else
  {
    Prev_Restore_History_Stacklet = NULL;
    root += 1;
  }
  Current_State_Point = *root++;
  Fluid_Bindings = *root++;
  Free_Stacklets = NULL;
  COMPILER_TRANSPORT_END ();
  CLEAR_INTERRUPT (INT_GC);
  return;
}

/* Here is the set up for the full garbage collection:

   - First it makes the constant space and stack into one large area
   by "hiding" the gap between them with a non-marked header.

   - Then it saves away all the relevant microcode registers into new
   space, making this the root for garbage collection.

   - Then it does the actual garbage collection in 4 steps:
     1) Trace constant space.
     2) Trace objects pointed out by the root and constant space.
     3) Trace the precious objects, remembering where consing started.
     4) Update all weak pointers.

   - Load new space to memory.

   - Finally it restores the microcode registers from the copies in
   new space.
*/

void
DEFUN (GC, (weak_pair_transport_initialized_p),
       int weak_pair_transport_initialized_p)
{
  SCHEME_OBJECT * root;
  SCHEME_OBJECT * end_of_constant_area;
  SCHEME_OBJECT the_precious_objects;
  SCHEME_OBJECT * root2;
  SCHEME_OBJECT * free_buffer;
  SCHEME_OBJECT * block_start;
  SCHEME_OBJECT * saved_ctop;
  long skip_length;

  saved_ctop = Constant_Top;
  if (((Constant_Top - Free_Constant) < CONSTANT_SPACE_FUDGE)
      && (update_allocator_parameters (Free_Constant)))
    Constant_Top = saved_ctop;

  if (!weak_pair_transport_initialized_p)
    initialize_weak_pair_transport (Stack_Bottom);

  free_buffer = (initialize_free_buffer ());
  Free = Heap_Bottom;
  ALIGN_FLOAT (Free);
  block_start = aligned_heap;
  skip_length = (Free - block_start);
  free_buffer += skip_length;

  Terminate_Old_Stacklet ();
  SEAL_CONSTANT_SPACE ();
  end_of_constant_area = (CONSTANT_AREA_END ());
  the_precious_objects = (Get_Fixed_Obj_Slot (Precious_Objects));
  root = Free;

  /* The 4 step GC */

  Free += (GC_relocate_root (&free_buffer));

  {
    SCHEME_OBJECT * new_scan
      = (gc_loop ((CONSTANT_AREA_START ()), (&free_buffer), (&Free),
		  Constant_Top, NORMAL_GC, 0));
    if (new_scan != end_of_constant_area)
      {
	gc_death (TERM_EXIT, "gc_loop ended too early", new_scan, free_buffer);
	/*NOTREACHED*/
      }
  }

  {
    SCHEME_OBJECT * scan
      = (gc_loop (((initialize_scan_buffer (block_start)) + skip_length),
		  (&free_buffer), (&Free), Constant_Top, NORMAL_GC, 1));

    root2 = Free;
    (*free_buffer++) = the_precious_objects;
    Free += 1;
    if (free_buffer >= free_buffer_top)
      free_buffer = (dump_and_reset_free_buffer (free_buffer, 0));

    gc_loop (scan, (&free_buffer), (&Free), Constant_Top, NORMAL_GC, 1);
  }

  end_transport (0);
  fix_weak_chain_1 (Constant_Top);

  /* Load new space into memory. */
  final_reload (block_start, (Free - block_start), "new space");

  fix_weak_chain_2 ();
  GC_end_root_relocation (root, root2);
  Constant_Top = saved_ctop;
  SET_CONSTANT_TOP ();
}

/* (GARBAGE-COLLECT SLACK)
   Requests a garbage collection leaving the specified amount of slack
   for the top of heap check on the next GC.  The primitive ends by
   invoking the GC daemon if there is one.
*/

DEFINE_PRIMITIVE ("GARBAGE-COLLECT", Prim_garbage_collect, 1, 1, 0)
{
  extern unsigned long gc_counter;
  SCHEME_OBJECT daemon;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();

  STACK_SANITY_CHECK ("GC");
  if (Free > Heap_Top)
    termination_gc_out_of_space ();

  GC_Reserve = (arg_nonnegative_integer (1));
  POP_PRIMITIVE_FRAME (1);

  ENTER_CRITICAL_SECTION ("garbage collector");
  run_pre_gc_hooks ();
  gc_counter += 1;
  GC (0);
  run_post_gc_hooks ();
  daemon = (Get_Fixed_Obj_Slot (GC_Daemon));

 Will_Push (CONTINUATION_SIZE);
  Store_Return (RC_NORMAL_GC_DONE);
  exp_register = (LONG_TO_UNSIGNED_FIXNUM (MemTop - Free - GC_Space_Needed));
  Save_Cont ();
 Pushed ();

  RENAME_CRITICAL_SECTION ("garbage collector daemon");
  if (daemon == SHARP_F)
    PRIMITIVE_ABORT (PRIM_POP_RETURN);
    /*NOTREACHED*/

 Will_Push (2);
  STACK_PUSH (daemon);
  STACK_PUSH (STACK_FRAME_HEADER);
 Pushed ();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  return (0);
}

#ifdef RECORD_GC_STATISTICS

static void
DEFUN_VOID (statistics_clear)
{
  int cntr, arlen;
  struct bch_GC_statistic * ptr;

  arlen = (((sizeof (all_gc_statistics))
	    / (sizeof (struct bch_GC_statistic)))
	   - 1);
  for (cntr = 0, ptr = &all_gc_statistics[0]; cntr < arlen; cntr++, ptr++)
    (* (ptr->counter)) = 0;
  return;
}

static int statistics_print_level = 0;

static void
DEFUN (statistics_print, (level, noise), int level AND char * noise)
{
  char format[30];
  int cntr, arlen, len, name_len;
  struct bch_GC_statistic * ptr;

  if (level > statistics_print_level)
    return;
  arlen = (((sizeof (all_gc_statistics))
	    / (sizeof (struct bch_GC_statistic)))
	   - 1);
  name_len = -1;
  for (cntr = 0, ptr = &all_gc_statistics[0];
       cntr < arlen;
       cntr++, ptr++)
    if ((* (ptr->counter)) != 0L)
    {
      len = (strlen (ptr->name));
      if (len > name_len)
	name_len = len;
    }

  if (name_len >= 0)
  {
    sprintf (&format[0], "\t%%-%ds : %%ld\n", name_len);

    outf_console ("\nGC I/O statistics %s:\n", noise);
    for (cntr = 0, ptr = &all_gc_statistics[0]; cntr < arlen; cntr++, ptr++)
      if ((* (ptr->counter)) != 0L)
	outf_console (&format[0], ptr->name, (* (ptr->counter)));
    outf_flush_console ();
  }
  return;
}
#endif /* RECORD_GC_STATISTICS */

static SCHEME_OBJECT
DEFUN_VOID (statistics_names)
{
  SCHEME_OBJECT vector, * scan;
  struct bch_GC_statistic * ptr;
  int len, cntr;

  len = (((sizeof (all_gc_statistics))
	  / (sizeof (struct bch_GC_statistic)))
	 - 1);
  if (len == 0)
    return (SHARP_F);

  vector = (allocate_marked_vector (TC_VECTOR, len, true));
  for (cntr = 0, ptr = &all_gc_statistics[0], scan = (VECTOR_LOC (vector, 0));
       cntr < len;
       cntr++, ptr++)
    *scan++ = (char_pointer_to_string (ptr->name));
  return (vector);
}

static void
DEFUN_VOID (statistics_read)
{
  SCHEME_OBJECT vector, *scan;
  struct bch_GC_statistic * ptr;
  int len, cntr;

  len = (((sizeof (all_gc_statistics))
	  / (sizeof (struct bch_GC_statistic)))
	 - 1);
  if (len == 0)
    signal_error_from_primitive (ERR_UNDEFINED_PRIMITIVE);

  vector = (VECTOR_ARG (1));
  if (len != ((int) (VECTOR_LENGTH (vector))))
    error_bad_range_arg (1);
  
  for (cntr = 0, ptr = &all_gc_statistics[0], scan = (VECTOR_LOC (vector, 0));
       cntr < len;
       cntr++, ptr++)
    *scan++ = (long_to_integer (* (ptr->counter)));
  return;
}

/* Additional primitives for statistics collection and
   manipulation of parameters from Scheme
 */

DEFINE_PRIMITIVE ("BCHSCHEME-STATISTICS-NAMES", Prim_bchscheme_stat_names, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (statistics_names ());
}

DEFINE_PRIMITIVE ("BCHSCHEME-STATISTICS-READ!", Prim_bchscheme_read_stats, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  statistics_read ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* There are other parameters that could be set, especially the drone program
   to run, and the file to gc from, but...
 */

#ifndef GET_SLEEP_DELTA
#define GET_SLEEP_DELTA()	-1
#define SET_SLEEP_DELTA(v)	do { } while (0)
#endif

#define N_PARAMS	6

DEFINE_PRIMITIVE ("BCHSCHEME-PARAMETERS-GET", Prim_bchscheme_get_params, 0, 0, 0)
{
  SCHEME_OBJECT vector;
  PRIMITIVE_HEADER (0);

  vector = (allocate_marked_vector (TC_VECTOR, N_PARAMS, true));

  VECTOR_SET (vector, 0,
	      (long_to_integer ((long) CAN_RECONFIGURE_GC_BUFFERS)));
  VECTOR_SET (vector, 1, (long_to_integer ((long) gc_buffer_size)));
  VECTOR_SET (vector, 2, (long_to_integer ((long) read_overlap)));
  VECTOR_SET (vector, 3, (long_to_integer ((long) write_overlap)));
  VECTOR_SET (vector, 4, (long_to_integer ((long) (GET_SLEEP_DELTA ()))));
  VECTOR_SET (vector, 5, (char_pointer_to_string (drone_file_name)));

  PRIMITIVE_RETURN (vector);
}

#if CAN_RECONFIGURE_GC_BUFFERS
static long
DEFUN (bchscheme_long_parameter, (vector, index),
       SCHEME_OBJECT vector AND int index)
{
  SCHEME_OBJECT temp;
  long value;

  temp = (VECTOR_REF (vector, index));
  if ((! (INTEGER_P (temp))) || (! (integer_to_long_p (temp))))
    error_bad_range_arg (1);
  value = (integer_to_long (temp));
  if (value < 0)
    error_bad_range_arg (1);
  return (value);
}
#endif /* CAN_RECONFIGURE_GC_BUFFERS */

DEFINE_PRIMITIVE ("BCHSCHEME-PARAMETERS-SET!", Prim_bchscheme_set_params, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

#if !CAN_RECONFIGURE_GC_BUFFERS
  signal_error_from_primitive (ERR_UNDEFINED_PRIMITIVE);
  /*NOTREACHED*/
  return (0);
#else

  {
    char * new_drone_ptr;
    SCHEME_OBJECT vector, new_drone;
    long
      new_buffer_size, new_read_overlap,
      new_write_overlap, new_sleep_period,
      old_buffer_size = gc_buffer_size,
      old_buffer_shift = gc_buffer_shift;

    vector = (VECTOR_ARG (1));
    if ((VECTOR_LENGTH (vector)) != N_PARAMS)
      error_bad_range_arg (1);

    /* Slot 0 ignored. */
    new_buffer_size = (bchscheme_long_parameter (vector, 1));
    new_read_overlap = (bchscheme_long_parameter (vector, 2));
    new_write_overlap = (bchscheme_long_parameter (vector, 3));
    new_sleep_period = (bchscheme_long_parameter (vector, 4));
    new_drone = (VECTOR_REF (vector, 5));
    if (! (STRING_P (new_drone)))
      error_bad_range_arg (1);
    if ((STRING_LENGTH (new_drone)) == 0)
      new_drone_ptr = ((char *) NULL);
    else
    {
      new_drone_ptr = ((char *) (malloc ((STRING_LENGTH (new_drone)) + 1)));
      if (new_drone_ptr != ((char *) NULL))
	strcpy (new_drone_ptr, ((char *) (STRING_LOC (new_drone, 0))));
    }

    if (new_buffer_size != old_buffer_size)
    {
      int power = (next_exponent_of_two (new_buffer_size));

      if (((1L << power) != new_buffer_size)
	  || ((set_gc_buffer_sizes (power)) != 0))
	error_bad_range_arg (1);
      if (! (recompute_gc_end_position ()))
      {
	set_gc_buffer_sizes (old_buffer_shift);
	error_bad_range_arg (1);
      }
    }

    BUFFER_SHUTDOWN (0);
    SET_SLEEP_DELTA (new_sleep_period);
    if ((drone_file_name != ((char *) NULL))
	&& (drone_file_name != option_gc_drone))
      free ((PTR) drone_file_name);

    if ((RE_INITIALIZE_GC_BUFFERS (0,
				   Highest_Allocated_Address,
				   ((sizeof (SCHEME_OBJECT))
				    * (CEILING ((saved_heap_size
						 + saved_constant_size),
						gc_buffer_size))),
				   new_read_overlap,
				   new_write_overlap,
				   new_drone_ptr))
	== 0)
      PRIMITIVE_RETURN (UNSPECIFIC);
    else
    {
      if (new_buffer_size != old_buffer_size)
      {
	set_gc_buffer_sizes (old_buffer_shift);
	recompute_gc_end_position ();
      }

      BUFFER_SHUTDOWN (0);
      if (new_drone_ptr != ((char *) NULL))
	free (new_drone_ptr);

      if ((RE_INITIALIZE_GC_BUFFERS (0,
				     Highest_Allocated_Address,
				     (saved_heap_size
				      * (sizeof (SCHEME_OBJECT))),
				     0, 0,
				     option_gc_drone)) != 0)
	Microcode_Termination (TERM_EXIT);
      else
	signal_error_from_primitive (ERR_EXTERNAL_RETURN);
    }
    /*NOTREACHED*/
    return (0);
  }
#endif /* (CAN_RECONFIGURE_GC_BUFFERS == 0) */
}
