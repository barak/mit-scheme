/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchmmg.c,v 9.66 1991/11/04 16:52:09 jinx Exp $

Copyright (c) 1987-1991 Massachusetts Institute of Technology

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

/* Memory management top level.  Garbage collection to disk. */

#include "scheme.h"
#include "prims.h"
#include "bchgcc.h"
#include "option.h"
#include "ux.h"
#include <sys/stat.h>
#include "bchdrn.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#ifdef HAVE_SYSV_SHARED_MEMORY
#  define RECORD_GC_STATISTICS
#endif
#define MILLISEC * 1000

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
   - Purify kills Scheme if there is not enough space in constant space
     for the new object.
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
   ------------------------------------------
   |         Control Stack        ||        |
   |                              \/        |
   ------------------------------------------
   |     Constant + Pure Space    /\        |
   |                              ||        |
   ------------------------------------------
   |          Heap Space                    |
   |                                        |
   ------------------------------------------
0
   Each area has a pointer to its starting address and a pointer to
   the next free cell.  The GC buffer space contains two (or more)
   buffers used during the garbage collection process.  One is the
   scan buffer and the other is the free buffer, and they are dumped
   and loaded from disk as necessary.  At the beginning and at the end
   a single buffer is used, since transporting will occur into the
   area being scanned.
*/

/* Exports */

extern void EXFUN (Clear_Memory, (int, int, int));
extern void EXFUN (Setup_Memory, (int, int, int));
extern void EXFUN (Reset_Memory, (void));

long
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
  * free_buffer_top,		* free_buffer_bottom;

static char
  * gc_file_name = ((char *) NULL),
  gc_file_name_buffer[FILE_NAME_LENGTH];

CONST char
  * drone_file_name = ((char *) NULL);

static int
  keep_gc_file_p = 0,
  gc_file = -1,
  read_overlap = 0,
  write_overlap = 0;

static SCHEME_OBJECT
  * aligned_heap;

static Boolean
  can_dump_directly_p,
  extension_overlap_p;

static long
  scan_position,
  free_position,
  pre_read_position,
  extension_overlap_length,
  saved_heap_size;

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

int 
DEFUN (io_error_retry_p, (operation_name, noise),
       char * operation_name AND char * noise)
{
  extern char EXFUN (userio_choose_option,
		     (const char *, const char *, const char **));
  extern int EXFUN (userio_confirm, (const char *));

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

  fprintf (stderr,
	   "\n%s (%s): GC file error (errno = %s) when manipulating %s.\n",
	   scheme_program_name, operation_name, (error_name (errno)), noise);
  fflush (stderr);
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
	fprintf
	  (stderr,
	   "%s (io_error_retry_p): Problems reading the keyboard; Exitting.\n",
	   scheme_program_name);
	fflush (stderr);
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

static int
DEFUN (verify_write, (position, size, success),
       long position AND long size AND Boolean * success)
{
  if ((position >= gc_file_start_position)
      && ((position + size) <= gc_file_end_position))
    return (0);
  fprintf (stderr,
	   "\n%s (verify_write): attempting to write outside allowed area.\n",
	   scheme_program_name);
  fprintf (stderr, "\tlow position = 0x%lx; high position = 0x%lx.\n",
	   gc_file_start_position, gc_file_end_position);
  fprintf (stderr, "\twrite position = 0x%lx; size = 0x%lx = %d bytes.\n",
	   position, size, size);
  fflush (stderr);
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
      && ((retrying_file_operation (write,
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
  (void) (retrying_file_operation (read,
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
  return;
}

static int
DEFUN (parameterization_termination, (kill_p, init_p),
       int kill_p AND int init_p)
{
  fflush (stderr);
  if (init_p)
    termination_init_error ();			/*NOTREACHED*/
  else if (kill_p)
    Microcode_Termination (TERM_EXIT);		/*NOTREACHED*/
  else
    return (-1);
}

#ifdef SIGCONT
static void
DEFUN (continue_running, (sig), int sig)
{
  RE_INSTALL_HANDLER (SIGCONT, continue_running);
  return;
}
#endif

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

#ifdef HAVE_SYSV_SHARED_MEMORY

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
  int dummy, saved_errno;
  struct timeval timeout;
  extern int EXFUN (select, (int, int *, int *, int *, struct timeval *));

  dummy = 0;
  timeout.tv_sec = 0;
  timeout.tv_usec = microsec;

  *wait_mask = mask;
  dummy = (select (0, &dummy, &dummy, &dummy, &timeout));
  *wait_mask = ((unsigned long) 0);
  saved_errno = errno;

  if ((dummy == -1) && (saved_errno == EINTR))
    STATISTICS_INCR (sleeps_interrupted);
  return;
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

static void
DEFUN (start_gc_drones, (first_drone, how_many, restarting),
       int first_drone AND int how_many AND int restarting)
{
  pid_t pid;
  long signal_mask;
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
    fprintf (stderr,
	     "\n%s (start_gc_drones): execlp (%s) failed (errno = %s).\n",
	     scheme_program_name, drone_file_name, (error_name (errno)));
    fflush (stderr);
    drone->state = drone_dead;
    (void) (kill ((getppid ()), SIGCONT));
    _exit (1);
  }
  else if (pid == -1)
  {
    fprintf (stderr, "\n%s (start_gc_drones): vfork failed (errno = %s).\n",
	     scheme_program_name, (error_name (errno)));
    fflush (stderr);
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
    {
      fprintf
	(stderr,
	 "\n%s (invoke_gc_drone): kill (%d, SIGCONT) failed; errno = %s.\n",
	 scheme_program_name, drone->DRONE_PID, (error_name (errno)));
      fflush (stderr);
    }
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

static void
DEFUN (probe_all_gc_drones, (wait_p), int wait_p)
{
  int count;
  unsigned long running;
  struct drone_info * drone;
  static void EXFUN (handle_drone_death, (struct drone_info *));

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

static int
DEFUN (sysV_initialize, (first_time_p, size, r_overlap, w_overlap, drfnam),
       int first_time_p
       AND long size AND int r_overlap AND int w_overlap
       AND CONST char * drfnam)
{
  static void EXFUN (open_gc_file, (long, int));

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
      fprintf
	(stderr,
	 "%s (sysV_initialize): Unable to allocate %d bytes (errno = %s).\n",
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
      fprintf
	(stderr,
	 "%s (sysV_initialize): shmget (-, %d, -) failed (errno = %s).\n",
	 scheme_program_name, shared_size, (error_name (errno)));
      fprintf (stderr,
	       "\tUnable to allocate shared memory for drone processes.\n");
      return (parameterization_termination (0, first_time_p));
    }
    shared_memory = (shmat (shmid, ATTACH_POINT, 0));
    if (shared_memory == ((char *) -1))
    {
      int saved_errno = errno;

      (void) (shmctl (shmid, IPC_RMID, 0));
      shmid = -1;
      fprintf
	(stderr,
	 "%s (sysV_initialize): shmat (%d, 0x%lx, 0) failed. (errno = %s).\n",
	 scheme_program_name, shmid, shared_size, (error_name (saved_errno)));
      fprintf (stderr,
	       "\tUnable to attach shared memory for drone processes.\n");
      return (parameterization_termination (0, first_time_p));
    }
    signal (SIGCONT, continue_running);
  }

  if (!(ALIGNED_TO_IO_PAGE_P (shared_memory)))
  {
    fprintf (stderr,
	     "%s (sysV_initialize): buffer space is not aligned properly.\n",
	     scheme_program_name);
    fprintf (stderr,
	     "\taddress = 0x%lx; IO_PAGE_SIZE = 0x%lx.\n",
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
  if ((drfnam != ((char *) NULL)) && (drfnam[0] != '/'))
  {
    CONST char * temp = (search_for_library_file (drfnam));

    if (temp != ((char *) NULL))
    {
      drone_file_name = temp;
      if (drfnam != option_gc_drone)
	free (drfnam);
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
      fprintf (stderr,
	       "%s (sysV_initialize): Problems starting up the GC drones%s.\n",
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

static void
DEFUN (sysV_shutdown, (final_time_p), int final_time_p)
{
  static void EXFUN (close_gc_file, (int));

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
  {
    fprintf (stderr, "\n%s (sysV_shutdown): shmdt failed.  errno = %s.\n",
	     scheme_program_name, (error_name (errno)));
    fflush (stderr);
  }
  shared_memory = ((char *) -1);

  if ((shmid != -1)
      && (shmctl (shmid, IPC_RMID, ((struct shmid_ds *) 0))) == -1)
  {
    fprintf (stderr, "\n%s (sysV_shutdown): shmctl failed.  errno = %s.\n",
	     scheme_program_name, (error_name (errno)));
    fflush (stderr);
  }
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

  fprintf (stderr, "\n%s (find_idle_buffer): All buffers are in use!\n",
	   scheme_program_name);
  fflush (stderr);
  Microcode_Termination (TERM_GC_OUT_OF_SPACE);
  /*NOTREACHED*/
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
	fprintf (stderr, "\n%s (read_buffer %s): invalid state.\n",
		 scheme_program_name, noise);
	fprintf (stderr, "\tindex = %d; state = %d; position = 0x%lx.\n",
		 buffer->index, buffer->state, posn);
	fflush (stderr);
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
      fprintf (stderr,
	       "\n%s (write_buffer %s): duplicate write at 0x%lx.\n",
	       scheme_program_name, noise, position);
      fflush (stderr);
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
    fprintf (stderr,
	     "\n%s (enqueue_ready_buffer): Duplicate pre-read at 0x%lx.\n",
	     scheme_program_name, old_buffer->position);
    fflush (stderr);
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

#endif /* HAVE_SYSV_SHARED_MEMORY */



#ifndef GC_BUFFER_ALLOCATION

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

#define BUFFER_SHUTDOWN(lt)	close_gc_file (0)

#define INITIALIZE_IO()		do { } while (0)
#define AWAIT_IO_COMPLETION()	do { } while (0)

#define INITIAL_SCAN_BUFFER()	gc_disk_buffer_2
#define INITIAL_FREE_BUFFER()	gc_disk_buffer_1

/* (gc_disk_buffer_1 - (gc_disk_buffer_2 - (buffer))) does not work
   because scan_buffer is not initialized until after scanning
   constant space.
*/

#define OTHER_BUFFER(buffer)	(((buffer) == gc_disk_buffer_1)		\
				 ? gc_disk_buffer_2			\
				 : gc_disk_buffer_1)

#define GC_BUFFER_BOTTOM(buffer) ((SCHEME_OBJECT *) (buffer))
#define GC_BUFFER_TOP(buffer) (((SCHEME_OBJECT *) (buffer)) + gc_buffer_size)

static int
DEFUN (catastrophic_failure, (name), char * name)
{
  fprintf (stderr,
	   "\n%s: Procedure %s should never be called!\n",
	   scheme_program_name, name);
  fflush (stderr);
  Microcode_Termination (TERM_EXIT);
  /*NOTREACHED*/
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

#endif /* GC_BUFFER_ALLOCATION */

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
  gc_file_start_position = 0L;
  gc_file_end_position = (saved_heap_size * (sizeof (SCHEME_OBJECT)));
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
#ifdef F_ULOCK
  if (gc_file != -1)
  {
    (void) (lseek (gc_file, gc_file_start_position, SEEK_SET));
    (void) (lockf (gc_file, F_ULOCK,
		   (gc_file_end_position - gc_file_start_position)));
  }
#endif
  if ((gc_file != -1) && ((close (gc_file)) == -1))
  {
    fprintf (stderr,
	     "\n%s (close_gc_file): error: GC file = \"%s\"; errno = %s.\n",
	     scheme_program_name, gc_file_name, (error_name (errno)));
    fflush (stderr);
  }
  gc_file = -1;
  if (!keep_gc_file_p && unlink_p)
    unlink (gc_file_name);
  gc_file_name = ((char *) NULL);
  keep_gc_file_p = 0;
  return;
}

static void
DEFUN (termination_open_gc_file, (operation, extra),
       CONST char * operation AND CONST char * extra)
{
  if ((operation != ((char *) NULL)) && (*operation != '\0'))
    fprintf
      (stderr,
       "%s (open_gc_file): %s (\"%s\") failed, (errno = %s).\n",
       scheme_program_name, operation, gc_file_name, (error_name (errno)));
  if ((extra != ((char *) NULL)) && (*extra != '\0'))
    fprintf (stderr, "\t%s.\n", extra);
  fflush (stderr);
  termination_init_error ();
  /*NOTREACHED*/
}

static void
DEFUN (open_gc_file, (size, unlink_p),
       long size AND int unlink_p)
{
  extern char * EXFUN (mktemp, (char *));
  extern long EXFUN (lseek, (int, long, int));
  struct stat file_info;
  int position, flags;
  Boolean exists_p;

  gc_file_name = &gc_file_name_buffer[0];
  if (option_gc_file[0] == '/')
    strcpy (gc_file_name, option_gc_file);
  else
  {
    position = (strlen (option_gc_directory));
    if ((position == 0) || (option_gc_directory[position - 1] != '/'))
      sprintf (gc_file_name, "%s/%s", option_gc_directory, option_gc_file);
    else
      sprintf (gc_file_name, "%s%s", option_gc_directory, option_gc_file);
  }

  /* mktemp supposedly only clobbers Xs from the end.
     If the string does not end in Xs, it is untouched. 
     This presents a quoting problem, but...
     Well, it seems to clobber the string if there are no Xs.
   */

#if 1
  position = (strlen (option_gc_file));
  if ((position >= 6)
      && ((strncmp ((option_gc_file + (position - 6)), "XXXXXX", 6)) == 0))
#endif
    (void) (mktemp (gc_file_name));

  flags = GC_FILE_FLAGS;
  gc_file_start_position = option_gc_start_position;
  gc_file_end_position = option_gc_end_position;
  if (gc_file_end_position == -1)
    gc_file_end_position = (gc_file_start_position + size);

  if ((stat (gc_file_name, &file_info)) == -1)
  {
    exists_p = false;
    can_dump_directly_p = true;
    flags |= O_EXCL;
  }
  else
  {
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
      fprintf (stderr,
	       "%s (open_gc_file): file \"%s\" has unknown/bad type 0x%x.\n",
	       scheme_program_name, gc_file_name,
	       ((int) (file_info.st_mode & S_IFMT)));
      fprintf
	(stderr,
	 "\tKnown types: S_IFREG (0x%x), S_IFBLK (0x%x), S_IFCHR (0x%x).\n",
	 S_IFREG, S_IFBLK, S_IFCHR);
      termination_open_gc_file (((char *) NULL), ((char *) NULL));
    }
    else
      can_dump_directly_p = true;
  }

  gc_file = (open (gc_file_name, flags, GC_FILE_MASK));
  if (gc_file == -1)
    termination_open_gc_file ("open", ((char *) NULL));

  keep_gc_file_p = (exists_p || option_gc_keep);
  if (!keep_gc_file_p && unlink_p)
  {
    extern int EXFUN (unlink, (const char *));

    (void) (unlink (gc_file_name));
  }

#ifdef HAVE_PREALLOC
  if (!exists_p)
  {
    extern int EXFUN (prealloc, (int, unsigned int));

    (void) (prealloc (gc_file, ((unsigned int) gc_file_end_position)));
  }
#endif /* HAVE_PREALLOC */

#ifdef F_TLOCK
  if (exists_p)
  {
    extern int EXFUN (locfk, (int, int, long));

    if ((lseek (gc_file, gc_file_start_position, SEEK_SET)) == -1)
      termination_open_gc_file ("lseek", ((char *) NULL));

    if ((lockf (gc_file, F_TLOCK, size)) == -1)
      termination_open_gc_file
	("lockf",
	 "The GC file is probably being used by another process");
  }
#endif /* F_TLOCK */

  gc_file_current_position = -1;	/* Unknown position */

  /* Determine whether it is a seekable file. */

  if (exists_p && ((file_info.st_mode & S_IFMT) == S_IFCHR))
  {
    int flags;
    Boolean ignore;
    static char message[] = "This is a test message to the GC file.\n";
    char * buffer;
  
    buffer = ((char *) aligned_heap);
    strcpy (buffer, &message[0]);
    strncpy ((buffer + ((sizeof (message)) - 1)),
	     buffer,
	     (IO_PAGE_SIZE - (sizeof (message))));
    (* (buffer + (IO_PAGE_SIZE - 1))) = '\n';

    if ((flags = (fcntl (gc_file, F_GETFL, 0))) != -1)
      (void) (fcntl (gc_file, F_SETFL, (flags | O_NONBLOCK)));

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
      fprintf (stderr,
	       "\n%s (open_gc_file): \"%s\" is not a seek-able device.\n",
	       scheme_program_name, gc_file_name);
      termination_open_gc_file (((char *) NULL), ((char *) NULL));
    }
    if (flags != -1)
      (void) (fcntl (gc_file, F_SETFL, (flags | O_NONBLOCK)));
  }
  return;
}

void
DEFUN (Clear_Memory, (heap_size, stack_size, constant_space_size),
       int heap_size
       AND int stack_size
       AND int constant_space_size)
{
  GC_Reserve = 4500;
  GC_Space_Needed = 0;
  Heap_Top = (Heap_Bottom + heap_size);
  SET_MEMTOP (Heap_Top - GC_Reserve);
  Free = Heap_Bottom;
  Constant_Top = (Constant_Space + constant_space_size);
  Initialize_Stack ();
  Free_Constant = Constant_Space;
  SET_CONSTANT_TOP ();
  return;
}

void
DEFUN_VOID (Reset_Memory)
{
  BUFFER_SHUTDOWN (1);
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
  if (!ALIGNED_TO_IO_PAGE_P (new_buffer_bytes))
  {
    fprintf (stderr,
	     "%s (Setup_Memory): improper new_buffer_size.\n",
	     scheme_program_name);
    fprintf (stderr, "\tIO_PAGE_SIZE   = 0x%lx bytes.\n",
	     ((long) IO_PAGE_SIZE));
    fprintf (stderr, "\tgc_buffer_size = 0x%lx bytes = 0x%lx objects.\n",
	     new_buffer_bytes, new_buffer_size);
    fprintf (stderr, "\tIO_PAGE_SIZE should divide gc_buffer_size.\n");
    return (-1);
  }

  new_buffer_byte_shift = (next_exponent_of_two (new_buffer_bytes));
  if ((1L << new_buffer_byte_shift) != new_buffer_bytes)
  {
    fprintf
      (stderr,
       "%s (Setup_Memory): gc_buffer_bytes (= 0x%lx) is not a power of 2.\n",
       scheme_program_name, new_buffer_bytes);
    return (-1);
  }

  new_buffer_overlap_bytes = IO_PAGE_SIZE;
  new_extra_buffer_size = (new_buffer_overlap_bytes / (sizeof (SCHEME_OBJECT)));
  if ((new_extra_buffer_size * (sizeof (SCHEME_OBJECT))) != new_buffer_overlap_bytes)
  {
    fprintf (stderr, " %s (Setup_Memory): improper IO_PAGE_SIZE.\n",
	     scheme_program_name);
    fprintf (stderr,
	     "\tIO_PAGE_SIZE = 0x%lx; (sizeof (SCHEME_OBJECT)) = 0x%lx.\n",
	     ((long) IO_PAGE_SIZE), ((long) (sizeof (SCHEME_OBJECT))));
    fprintf (stderr,
	     "\t(sizeof (SCHEME_OBJECT)) should divide IO_PAGE_SIZE.\n");
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
  int real_stack_size, fudge_space;

  /* Consistency check 1 */
  if (heap_size == 0)
  {
    fprintf (stderr,
	     "%s (Setup_Memory): Configuration won't hold initial data.\n",
	     scheme_program_name);
    fflush (stderr);
    termination_init_error ();
    /*NOTREACHED*/
  }

  real_stack_size = (Stack_Allocation_Size (stack_size));

  /* add log(1024)/log(2) to exponent */
  if ((set_gc_buffer_sizes (10 + (next_exponent_of_two
				   (option_gc_window_size))))
      != 0)
    parameterization_termination (1, 1);

  /* Use multiples of IO_PAGE_SIZE. */

  fudge_space = ((BLOCK_TO_IO_SIZE (HEAP_BUFFER_SPACE + 1))
		 + (IO_PAGE_SIZE / (sizeof (SCHEME_OBJECT))));
  heap_size = (BLOCK_TO_IO_SIZE (heap_size));
  constant_space_size = (BLOCK_TO_IO_SIZE (constant_space_size));
  real_stack_size = (BLOCK_TO_IO_SIZE (real_stack_size));

  /* Allocate. */

  ALLOCATE_HEAP_SPACE (fudge_space + heap_size
		       + constant_space_size + real_stack_size
		       + (GC_BUFFER_ALLOCATION (2 * gc_total_buffer_size)));

  /* Consistency check 2 */
  if (Heap == NULL)
  {
    fprintf (stderr,
	     "%s (Setup_Memory): Not enough memory for this configuration.\n",
	     scheme_program_name);
    fflush (stderr);
    termination_init_error ();
    /*NOTREACHED*/
  }

  Heap += HEAP_BUFFER_SPACE;
  Heap = ((SCHEME_OBJECT *) (ALIGN_UP_TO_IO_PAGE (Heap)));
  aligned_heap = Heap;
  Constant_Space = (Heap + heap_size);

  /*
     The two GC buffers are not included in the valid Scheme memory.
  */

  Highest_Allocated_Address = ((Constant_Space + constant_space_size
				+ real_stack_size) - 1);

  /* Consistency check 3 */
  test_value =
    (MAKE_POINTER_OBJECT (LAST_TYPE_CODE, Highest_Allocated_Address));

  if (((OBJECT_TYPE (test_value)) != LAST_TYPE_CODE) ||
      ((OBJECT_ADDRESS (test_value)) != Highest_Allocated_Address))
  {
    fprintf (stderr,
	     "\
%s (Setup_Memory): Largest address does not fit in datum field of object.\n",
	     scheme_program_name);
    fprintf (stderr,
	     "\
\tAllocate less space or re-configure without HEAP_IN_LOW_MEMORY.\n");
    fflush (stderr);
    termination_init_error ();
    /*NOTREACHED*/
  }

  /* This does not use INITIAL_ALIGN_HEAP because it would
     make Heap point to the previous GC_BUFFER frame.
     INITIAL_ALIGN_HEAP should have its phase changed so that it would
     be a NOP below, and constant space should use it too.
   */     

  ALIGN_FLOAT (Heap);
  ALIGN_FLOAT (Constant_Space);
  heap_size = (Constant_Space - Heap);
  constant_space_size = ((Highest_Allocated_Address - Constant_Space)
			 - real_stack_size);
  saved_heap_size = ((long) heap_size);

  Heap_Bottom = Heap;
  Clear_Memory (heap_size, stack_size, constant_space_size);

  INITIALIZE_GC_BUFFERS (1,
			 (Highest_Allocated_Address + 1),
			 (heap_size * (sizeof (SCHEME_OBJECT))),
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
    DUMP_BUFFER (free_buffer, free_position, gc_buffer_bytes,
		  success, "the free buffer");
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
DEFUN (reload_scan_buffer, (skip), int skip)
{
  scan_position += (skip << gc_buffer_byte_shift);

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
  LOAD_BUFFER (scan_buffer, scan_position,
	       gc_buffer_bytes, "the scan buffer");
  scan_buffer_bottom = (GC_BUFFER_BOTTOM (scan_buffer));
  scan_buffer_top = (GC_BUFFER_TOP (scan_buffer));
  *scan_buffer_top = (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
  
  if (read_overlap > 0)
    schedule_pre_reads ();
  return;
}

SCHEME_OBJECT *
DEFUN (dump_and_reload_scan_buffer, (number_to_skip, success),
       long number_to_skip AND Boolean * success)
{
  DUMP_BUFFER (scan_buffer, scan_position, gc_buffer_bytes,
		success, "the scan buffer");
  reload_scan_buffer (1 + number_to_skip);
  return (scan_buffer_bottom);
}

SCHEME_OBJECT *
DEFUN (dump_and_reset_free_buffer, (overflow, success),
       fast long overflow AND Boolean * success)
{
  Boolean buffer_overlap_p, same_buffer_p;
  fast SCHEME_OBJECT *into, *from;

  from = free_buffer_top;
  buffer_overlap_p = extension_overlap_p;
  same_buffer_p = (scan_buffer == free_buffer);

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
    DUMP_BUFFER (free_buffer, free_position, gc_buffer_bytes,
		 success, "the free buffer");

  /* Otherwise there is no need to dump now, it will be dumped
     when scan is dumped.  Note that the next buffer may be dumped
     before this one, but there should be no problem lseeking past the
     end of file.
   */

  free_position += gc_buffer_bytes;
  free_buffer = (OTHER_BUFFER (scan_buffer));
  free_buffer_bottom = (GC_BUFFER_BOTTOM (free_buffer));
  free_buffer_top = (GC_BUFFER_TOP (free_buffer));

  for (into = free_buffer_bottom; --overflow >= 0; )
    *into++ = *from++;

  if (same_buffer_p && !buffer_overlap_p)
    *scan_buffer_top =
      (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
  return (into);
}

/* These utilities are needed when pointers fall accross window boundaries.

   Between both they effectively do a dump_and_reload_scan_buffer, in two
   stages.
*/

void
DEFUN (extend_scan_buffer, (to_where, current_free),
       fast char * to_where AND SCHEME_OBJECT * current_free)
{
  fast char * source, * dest;
  long new_scan_position = (scan_position + gc_buffer_bytes);

  /* Is there buffer overlap?, i.e. is the next bufferful the one cached
     in the free pointer window?
   */

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
    dest = ((char *) (dump_and_reload_scan_buffer (0, ((Boolean *) NULL))));
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

    DUMP_BUFFER (scan_buffer, scan_position, gc_buffer_bytes,
		 ((Boolean *) NULL), "the scan buffer");
    scan_position += gc_buffer_bytes;

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
    
    DUMP_BUFFER (scan_buffer, scan_position, gc_buffer_bytes,
		 ((Boolean *) NULL), "the scan buffer");
    scan_position += gc_buffer_bytes;

    scan_buffer = next_scan_buffer;
    next_scan_buffer = NULL;
    scan_buffer_bottom = (GC_BUFFER_BOTTOM (scan_buffer));
    scan_buffer_top = (GC_BUFFER_TOP (scan_buffer));
    (* scan_buffer_top) =
      (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
    schedule_pre_reads ();
  }
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

      (void) (dump_and_reset_free_buffer (0, success));
    }
  }
  return (free_buffer_bottom);
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
  scan_position = -1L;
  scan_buffer = NULL;
  scan_buffer_bottom = NULL;
  scan_buffer_top = (Highest_Allocated_Address + 2);
  /* Force first write to do an lseek. */
  gc_file_current_position = -1;
  next_scan_buffer = NULL;
  extension_overlap_p = false;
  extension_overlap_length = 0;
  return (free_buffer_bottom);
}

SCHEME_OBJECT *
DEFUN_VOID (initialize_scan_buffer)
{
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
  DUMP_BUFFER (scan_buffer, scan_position, gc_buffer_bytes,
	       success, "the final scan buffer");
  scan_position += gc_buffer_bytes;
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
DEFUN_VOID (pre_read_weak_pair_buffers)
{
  SCHEME_OBJECT next, * pair_addr, * obj_addr;
  long position, last_position;

  last_position = -1;
  next = weak_pair_break;
  while (next != EMPTY_LIST)
  {
    pair_addr = (OBJECT_ADDRESS (next));
    obj_addr = (OBJECT_ADDRESS (*pair_addr++));
    if (! (obj_addr >= Constant_Space))
    {
      position = (obj_addr - aligned_heap);
      position = (position >> gc_buffer_shift);
      position = (position << gc_buffer_byte_shift);
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

static void
DEFUN (initialize_new_space_buffer, (chain), SCHEME_OBJECT chain)
{
  if (read_overlap == 0)
  {
    weak_pair_break = EMPTY_LIST;
    weak_pair_buffer = (INITIAL_FREE_BUFFER ());
    weak_pair_buffer_position = -1;
  }
  else
  {
    weak_pair_break = chain;
    weak_pair_buffer = ((struct buffer_info *) NULL);
    weak_pair_buffer_position = -1;
    weak_buffer_pre_read_count = 0;
    pre_read_weak_pair_buffers ();
  }
  return;
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
DEFUN (guarantee_in_memory, (addr), SCHEME_OBJECT * addr)
{
  long position, offset;

  if (addr >= Constant_Space)
    return (addr);

  position = (addr - aligned_heap);
  offset = (position & gc_buffer_mask);
  position = (position >> gc_buffer_shift);
  position = (position << gc_buffer_byte_shift);
  if (position != weak_pair_buffer_position)
  {
    flush_new_space_buffer ();
    LOAD_BUFFER (weak_pair_buffer, position, gc_buffer_bytes,
		 "the weak pair buffer");
    weak_pair_buffer_position = position;
    if (weak_pair_break != EMPTY_LIST)
    {
      weak_buffer_pre_read_count -= 1;
      pre_read_weak_pair_buffers ();
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
DEFUN (update_weak_pointer, (Temp), SCHEME_OBJECT Temp)
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
      if (Old >= Constant_Space)
	return (Temp);

      if ((OBJECT_TYPE (*Old)) == TC_BROKEN_HEART)
	return (MAKE_OBJECT_FROM_OBJECTS (Temp, *Old));
      else
	return (SHARP_F);

    case GC_Compiled:
      Old = (OBJECT_ADDRESS (Temp));
      if (Old >= Constant_Space)
	return (Temp);
      Compiled_BH (false, { return Temp; });
      return (SHARP_F);

    default:			/* Non Marked Headers and Broken Hearts */
    case GC_Undefined:
    fail:
      fprintf (stderr,
	       "\n%s (update_weak_pointer): Clearing bad object 0x%08lx.\n",
	       scheme_program_name, Temp);
      fflush (stderr);
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
  Weak_Chain = EMPTY_LIST;
  weak_pair_stack_ptr = Stack_Pointer;
  weak_pair_stack_limit = (limit + 1); /* in case it's odd */
  return;
}

void
DEFUN_VOID (fix_weak_chain_1)
{
  fast SCHEME_OBJECT chain, * old_weak_cell, * scan, * ptr, * limit;

  chain = Weak_Chain;
  initialize_new_space_buffer (chain);

  limit = Stack_Pointer;
  for (ptr = weak_pair_stack_ptr; ptr < limit ; ptr += 2)
    *ptr = (update_weak_pointer (*ptr));

  while (chain != EMPTY_LIST)
  {
    old_weak_cell = (OBJECT_ADDRESS (Weak_Chain));
    scan = (guarantee_in_memory (OBJECT_ADDRESS (*old_weak_cell++)));
    Weak_Chain = (* old_weak_cell);
    *scan = (update_weak_pointer
	     (MAKE_OBJECT_FROM_OBJECTS (Weak_Chain, (* scan))));
    Weak_Chain = (OBJECT_NEW_TYPE (TC_NULL, Weak_Chain));
  }
  flush_new_space_buffer ();
  Weak_Chain = chain;
  return;
}

void
DEFUN_VOID (fix_weak_chain_2)
{
  fast SCHEME_OBJECT * ptr, * limit, new_car, * addr;

  limit = Stack_Pointer;
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
  SCHEME_OBJECT
    * root, * result, * end_of_constant_area,
    the_precious_objects, * root2,
    * free_buffer, * block_start, * initial_free_buffer;

  if (!weak_pair_transport_initialized_p)
    initialize_weak_pair_transport (Free_Constant + 2);

  free_buffer = (initialize_free_buffer ());
  Free = Heap_Bottom;
  block_start = aligned_heap;
  if (block_start != Free)
    free_buffer += (Free - block_start);
  initial_free_buffer = free_buffer;

  SET_MEMTOP (Heap_Top - GC_Reserve);

  /* Save the microcode registers so that they can be relocated */

  Terminate_Old_Stacklet ();
  SEAL_CONSTANT_SPACE ();
  end_of_constant_area = (CONSTANT_SPACE_SEAL ());
  root = Free;
  the_precious_objects = (Get_Fixed_Obj_Slot (Precious_Objects));
  Set_Fixed_Obj_Slot (Precious_Objects, SHARP_F);
  Set_Fixed_Obj_Slot (Lost_Objects_Base, SHARP_F);

  *free_buffer++ = Fixed_Objects;
  *free_buffer++ = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, History));
  *free_buffer++ = Undefined_Primitives;
  *free_buffer++ = Undefined_Primitives_Arity;
  *free_buffer++ = Get_Current_Stacklet ();
  *free_buffer++ = ((Prev_Restore_History_Stacklet == NULL) ?
		    SHARP_F :
		    (MAKE_POINTER_OBJECT (TC_CONTROL_POINT,
					  Prev_Restore_History_Stacklet)));

  *free_buffer++ = Current_State_Point;
  *free_buffer++ = Fluid_Bindings;
  Free += (free_buffer - initial_free_buffer);

  if (free_buffer >= free_buffer_top)
    free_buffer =
      (dump_and_reset_free_buffer ((free_buffer - free_buffer_top),
				   NULL));
  /* The 4 step GC */

  result = (GCLoop (Constant_Space, &free_buffer, &Free));
  if (result != end_of_constant_area)
  {
    fprintf (stderr,
	     "\n%s (GC): The Constant Space scan ended too early.\n",
	     scheme_program_name);
    fflush (stderr);
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }

  result = (GCLoop (((initialize_scan_buffer ())
		     + (Heap_Bottom - block_start)),
		    &free_buffer, &Free));
  if (free_buffer != result)
  {
    fprintf (stderr,
	     "\n%s (GC): The Heap scan ended too early.\n",
	     scheme_program_name);
    fflush (stderr);
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }

  root2 = Free;
  *free_buffer++ = the_precious_objects;
  Free += (free_buffer - result);
  if (free_buffer >= free_buffer_top)
    free_buffer =
      (dump_and_reset_free_buffer ((free_buffer - free_buffer_top), NULL));

  result = (GCLoop (result, &free_buffer, &Free));
  if (free_buffer != result)
  {
    fprintf (stderr,
	     "\n%s (GC): The Precious Object scan ended too early.\n",
	     scheme_program_name);
    fflush (stderr);
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }
  end_transport (NULL);
  fix_weak_chain_1 ();

  /* Load new space into memory. */

  final_reload (block_start, (Free - block_start), "new space");
  fix_weak_chain_2 ();

  /* Make the microcode registers point to the copies in new-space. */

  Fixed_Objects = *root++;
  Set_Fixed_Obj_Slot (Precious_Objects, *root2);
  Set_Fixed_Obj_Slot
    (Lost_Objects_Base, (LONG_TO_UNSIGNED_FIXNUM (ADDRESS_TO_DATUM (root2))));

  History = (OBJECT_ADDRESS (*root++));
  Undefined_Primitives = *root++;
  Undefined_Primitives_Arity = *root++;

  Set_Current_Stacklet (*root);
  root += 1;
  if (*root == SHARP_F)
  {
    Prev_Restore_History_Stacklet = NULL;
    root += 1;
  }
  else
    Prev_Restore_History_Stacklet = (OBJECT_ADDRESS (*root++));
  Current_State_Point = *root++;
  Fluid_Bindings = *root++;
  Free_Stacklets = NULL;
  COMPILER_TRANSPORT_END ();
  CLEAR_INTERRUPT (INT_GC);
  return;
}

/* (GARBAGE-COLLECT SLACK)
   Requests a garbage collection leaving the specified amount of slack
   for the top of heap check on the next GC.  The primitive ends by invoking
   the GC daemon if there is one.
*/

DEFINE_PRIMITIVE ("GARBAGE-COLLECT", Prim_garbage_collect, 1, 1, 0)
{
  long new_gc_reserve;
  extern unsigned long gc_counter;
  SCHEME_OBJECT GC_Daemon_Proc;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();

  STACK_SANITY_CHECK ("GC");
  new_gc_reserve = (arg_nonnegative_integer (1));
  if (Free > Heap_Top)
    termination_gc_out_of_space ();

  ENTER_CRITICAL_SECTION ("garbage collector");
  gc_counter += 1;
  GC_Reserve = new_gc_reserve;
  GC (0);
  POP_PRIMITIVE_FRAME (1);
  GC_Daemon_Proc = (Get_Fixed_Obj_Slot (GC_Daemon));

  RENAME_CRITICAL_SECTION ("garbage collector daemon");
  if (GC_Daemon_Proc == SHARP_F)
  {
   Will_Push (CONTINUATION_SIZE);
    Store_Return (RC_NORMAL_GC_DONE);
    Store_Expression (LONG_TO_UNSIGNED_FIXNUM(MemTop - Free));
    Save_Cont ();
   Pushed ();
    PRIMITIVE_ABORT (PRIM_POP_RETURN);
    /*NOTREACHED*/
  }
 Will_Push (CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS + 1));
  Store_Return (RC_NORMAL_GC_DONE);
  Store_Expression (LONG_TO_UNSIGNED_FIXNUM (MemTop - Free));
  Save_Cont ();
  STACK_PUSH (GC_Daemon_Proc);
  STACK_PUSH (STACK_FRAME_HEADER);
 Pushed ();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /* The following comment is by courtesy of LINT, your friendly sponsor. */
  /*NOTREACHED*/
}

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

    printf ("\nGC I/O statistics %s:\n", noise);
    for (cntr = 0, ptr = &all_gc_statistics[0]; cntr < arlen; cntr++, ptr++)
      if ((* (ptr->counter)) != 0L)
	printf (&format[0], ptr->name, (* (ptr->counter)));
    fflush (stdout);
  }
  return;
}

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
    *scan++ = (char_pointer_to_string ((unsigned char *) ptr->name));
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
  if (len != (VECTOR_LENGTH (vector)))
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
  VECTOR_SET (vector, 5, (char_pointer_to_string
			  ((unsigned char *) drone_file_name)));

  PRIMITIVE_RETURN (vector);
}

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

DEFINE_PRIMITIVE ("BCHSCHEME-PARAMETERS-SET!", Prim_bchscheme_set_params, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

#if (CAN_RECONFIGURE_GC_BUFFERS == 0)
  signal_error_from_primitive (ERR_UNDEFINED_PRIMITIVE);
  /*NOTREACHED*/
#else

  {
    char * new_drone_ptr;
    SCHEME_OBJECT vector, new_drone;
    int power;
    long
      new_buffer_size, new_read_overlap,
      new_write_overlap, new_sleep_period;

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

    power = (next_exponent_of_two (new_buffer_size));
    if (((1L << power) != new_buffer_size)
	|| ((set_gc_buffer_sizes (power)) != 0))
      error_bad_range_arg (1);

    BUFFER_SHUTDOWN (0);
    SET_SLEEP_DELTA (new_sleep_period);
    if ((drone_file_name != ((char *) NULL))
	&& (drone_file_name != option_gc_drone))
      free (drone_file_name);

    if ((RE_INITIALIZE_GC_BUFFERS (0,
				   (Highest_Allocated_Address + 1),
				   (saved_heap_size
				    * (sizeof (SCHEME_OBJECT))),
				   new_read_overlap,
				   new_write_overlap,
				   new_drone_ptr)) == 0)
      PRIMITIVE_RETURN (UNSPECIFIC);
    else
    {
      BUFFER_SHUTDOWN (0);
      if (new_drone_ptr != ((char *) NULL))
	free (new_drone_ptr);

      if ((RE_INITIALIZE_GC_BUFFERS (0,
				     (Highest_Allocated_Address + 1),
				     (saved_heap_size
				      * (sizeof (SCHEME_OBJECT))),
				     0, 0,
				     option_gc_drone)) != 0)
	Microcode_Termination (TERM_EXIT);
      else
	signal_error_from_primitive (ERR_EXTERNAL_RETURN);
    }
    /*NOTREACHED*/
  }
#endif (CAN_RECONFIGURE_GC_BUFFERS == 0)
}
