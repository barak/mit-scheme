/* -*- C -*-

$Id: bchdrn.c,v 1.13 2007/01/05 15:33:06 cph Exp $

Copyright (c) 1991-2000 Massachusetts Institute of Technology

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

/* Drone program for overlapped I/O in bchscheme. */

#include "ux.h"
#include "bchdrn.h"
#if 0
/* ux.h includes <setjmp.h> indirectly */
#  include <setjmp.h>
#endif

#define DEBUG
/* #define DEBUG_1 */
/* #define DEBUG_2 */

extern char * EXFUN (error_name, (int));
extern int EXFUN (retrying_file_operation,
		  (ssize_t EXFUN ((*), (int, char *, int)),
		   int, char *, long, long, char *, char *, long *,
		   int EXFUN ((*), (char *, char *))));

#ifdef USE_SYSV_SHARED_MEMORY

static struct
{
  char * program_name;
  char * file_name;		/* gc file name */
  int shmid;			/* shared memory descriptor */
  int tdron;			/* total number of drones */
  int nbuf;			/* total number of buffers */
  long bufsiz;			/* size of each buffer in bytes */
  int sdron;			/* index of first drone to start */
  int ndron;			/* number of drones to start */
  int keep_p;			/* keep the gc file if Scheme dies? */
} arguments;

struct argdesc
{
  char * name;
  char * format;
  PTR location;
};

static char string_a[] = "%s";
#define STRING_FMT &string_a[0]

static char decimal_int_a[] = "%d";
#define DECIMAL_INT_FMT &decimal_int_a[0]

static char decimal_long_a[] = "%ld";
#define DECIMAL_LONG_FMT &decimal_long_a[0]

static struct argdesc command_line[] =
{
  { "program_name",	STRING_FMT,		&arguments.program_name },
  { "file_name",	STRING_FMT,		&arguments.file_name },
  { "shmid",		DECIMAL_INT_FMT,	&arguments.shmid },
  { "tdron",		DECIMAL_INT_FMT,	&arguments.tdron },
  { "nbuf",		DECIMAL_INT_FMT,	&arguments.nbuf },
  { "bufsiz",		DECIMAL_LONG_FMT,	&arguments.bufsiz },
  { "sdron",		DECIMAL_INT_FMT,	&arguments.sdron },
  { "ndron",		DECIMAL_INT_FMT,	&arguments.ndron },
  { "keep_gc_file",	DECIMAL_INT_FMT,	&arguments.keep_p }
};

static int gc_fid = -1;
static char * shared_memory;
static struct buffer_info * gc_buffers;
static struct drone_info * myself;
static unsigned long * drone_version, * wait_mask;
static jmp_buf abort_point;
static pid_t boss_pid;

static void EXFUN (kill_program, (int sig));

static void 
DEFUN (posix_signal, (signum, handler),
       int signum AND void EXFUN ((*handler), ()))
{
  struct sigaction new;

  new.sa_handler = handler;
  UX_sigemptyset (&new.sa_mask);
  UX_sigaddset ((&new.sa_mask), SIGCONT);
  UX_sigaddset ((&new.sa_mask), SIGQUIT);
  new.sa_flags = SA_NOCLDSTOP;
  
  if ((UX_sigaction (signum, &new, 0)) == -1)
  {
    fprintf (stderr, "%s (%d, posix_signal): sigaction failed. errno = %s.\n",
	     arguments.program_name, myself->index, (error_name (errno)));
    fflush (stderr);
    kill_program (0);
    /*NOTREACHED*/
  }
  return;
}

static void
DEFUN (kill_program, (sig), int sig)
{
  myself->state = drone_dead;
  if (gc_fid != -1)
    close (gc_fid);
  shmdt (shared_memory);
  if (sig == -1)
  {
    shmctl (arguments.shmid, IPC_RMID, ((struct shmid_ds *) 0));
    if (!arguments.keep_p)
      unlink (arguments.file_name);
  }
  exit (0);
  /*NOTREACHED*/
}

static void
DEFUN (abort_operation, (sig), int sig)
{
  RE_INSTALL_HANDLER (SIGQUIT, abort_operation);
  myself->state = drone_aborting;
  longjmp (abort_point, 1);
  /*NOTREACHED*/
}

static void
DEFUN (continue_running, (sig), int sig)
{
  RE_INSTALL_HANDLER (SIGCONT, continue_running);
  longjmp (abort_point, 1);
  /*NOTREACHED*/
}

static int
DEFUN (always_one, (operation_name, noise),
       char * operation_name AND char * noise)
{
  return (1);
}

static void
DEFUN (process_requests, (drone), struct drone_info * drone)
{
  sigset_t non_blocking_signal_mask, blocking_signal_mask;
  int result, count, buffer_index, flags;
  long current_position = -1;
  struct timeval timeout;
  struct stat file_info;
  unsigned long read_mask, my_mask;

  myself = drone;
  my_mask = (((unsigned long) 1) << drone->index);
  drone->DRONE_PID = (getpid ());
  gc_fid = (open (arguments.file_name, O_RDWR, 0644));
  if (gc_fid == -1)
  {
    fprintf (stderr,
	     "%s (%d, process_requests): open failed. errno = %s.\n",	
	     arguments.program_name, drone->index, (error_name (errno)));
    fflush (stderr);
    if (drone->DRONE_PPID == boss_pid)
      (void) (kill (boss_pid, SIGCONT));
    kill_program (0);
    /*NOTREACHED*/
  }
#ifdef DEBUG_1
  printf ("%s (%d, process_requests): Starting (pid = %d, ppid = %d).\n",
	  arguments.program_name, drone->index,
	  drone->DRONE_PID, drone->DRONE_PPID);
  fflush (stdout);
#endif
  if ((result = (fstat (gc_fid, &file_info))) == -1)
  {
    fprintf (stderr,
	     "%s (%d, process_requests): fstat failed. errno = %s.\n",
	     arguments.program_name, drone->index, (error_name (errno)));
    fflush (stderr);
  }
  /* Force O_SYNC only if we are dealing with a raw device. */
    
  if ((result == -1) || ((file_info.st_mode & S_IFMT) == S_IFCHR))
  {
    if ((flags = (fcntl (gc_fid, F_GETFL, 0))) == -1)
    {
      fprintf
	(stderr,
	 "%s (%d, process_requests): fcntl (F_GETFL) failed. errno = %s.\n",
	 arguments.program_name, drone->index, (error_name (errno)));
      fflush (stderr);
    }
    else
    {
      flags |= O_SYNC;
      if ((fcntl (gc_fid, F_SETFL, flags)) == -1)
      {
	fprintf
	  (stderr,
	   "%s (%d, process_requests): fcntl (F_SETFL) failed. errno = %s.\n",
	   arguments.program_name, drone->index, (error_name (errno)));
	fflush (stderr);
      }
    }
  }

  UX_sigemptyset (&non_blocking_signal_mask);
  UX_sigemptyset (&blocking_signal_mask);
  UX_sigaddset ((&blocking_signal_mask), SIGCONT);
  UX_sigaddset ((&blocking_signal_mask), SIGQUIT);
  UX_sigprocmask (SIG_SETMASK, (&blocking_signal_mask), 0);
  posix_signal (SIGQUIT, abort_operation);
  posix_signal (SIGCONT, continue_running);

  if ((setjmp (abort_point)) == 0)
  {
    count = drone->index; 
    drone->state = drone_idle;
    if (drone->DRONE_PPID == boss_pid)
      (void) (kill (boss_pid, SIGCONT));
  }
  else
    goto redo_dispatch;

  for (; 1; count++)
  {
    timeout.tv_sec = 6;
    timeout.tv_usec = 0;

    UX_sigprocmask (SIG_SETMASK, (&non_blocking_signal_mask), 0);
    result = (select (0, 0, 0, 0, &timeout));
    UX_sigprocmask (SIG_SETMASK, (&blocking_signal_mask), 0);

    if ((drone->state != drone_idle)
	|| ((result == -1) && (errno == EINTR)))
    {
      if (result != -1)
      {
	fprintf (stderr,
		 "\n%s (%d, process_requests): request after timeout %d.\n",
		 arguments.program_name, drone->index, drone->state);
	fflush (stderr);
      }
redo_dispatch:
      switch (drone->state)
      {
	default:
	  fprintf (stderr,
		   "\n%s (%d, process_requests): Unknown/bad operation %d.\n",
		   arguments.program_name, drone->index, drone->state);
	  fflush (stderr);
	  kill_program (0);
	  /*NOTREACHED*/

	case drone_idle:
	  break;

	case drone_aborting:
#ifdef DEBUG_1
	  printf ("\n%s (%d, process_requests): Aborting.",
		  arguments.program_name, drone->index);
	  fflush (stdout);
#endif
	  drone->buffer_index = -1;
	  current_position = -1;
	  break;

	case drone_reading:
	case drone_writing:
	{
	  /* Can't use buffer->bottom because the shared memory may be
	     attached at a different address!
	   */

	  int saved_errno;
	  enum drone_state operation;
	  char * operation_name, * buffer_address;
	  struct buffer_info * buffer;
	  struct gc_queue_entry * entry;

	  operation = drone->state;
	  buffer_index = (drone->buffer_index);
	  buffer = (gc_buffers + buffer_index);

	  entry = ((struct gc_queue_entry *)
		   (((char *) drone) + (drone->entry_offset)));
	  entry->error_code = 0;

	  operation_name = ((operation == drone_reading) ? "read" : "write");
	  buffer_address = (shared_memory + (arguments.bufsiz * buffer_index));
#ifdef DEBUG_1
	  printf ("\n%s (%d, process_requests %s): Buffer index = %d.\n",
		  arguments.program_name, drone->index, operation_name,
		  buffer_index);
	  printf ("\tBuffer address = 0x%lx; Position = 0x%lx; Size = 0x%lx.",
		  buffer_address, buffer->position, buffer->size);
	  fflush (stdout);
#endif

	  UX_sigprocmask (SIG_SETMASK, (&non_blocking_signal_mask), 0);
	  result = (retrying_file_operation
		    (((operation == drone_reading) ? read : write),
		     gc_fid, buffer_address,
		     buffer->position, buffer->size, operation_name, NULL,
		     &current_position, always_one));
	  saved_errno = errno;
	  UX_sigprocmask (SIG_SETMASK, (&blocking_signal_mask), 0);

	  if (result == -1)
	  {
	    buffer->state = ((operation == drone_reading)
			     ? buffer_read_error
			     : buffer_write_error);
	    drone->buffer_index = -1;
	    entry->drone_index = -1;
	    entry->error_code = saved_errno;
	    entry->state = entry_error;
	    current_position = -1;
#ifdef DEBUG
	    printf ("\n%s (%d, process_requests): %s error (errno = %s).\n",
		    arguments.program_name, drone->index, operation_name,
		    (error_name (saved_errno)));
	    fflush (stdout);
#endif
	  }

	  else
	  {
	    buffer->state = ((operation == drone_reading)
			     ? buffer_ready
			     : buffer_idle);
	    drone->buffer_index = -1;
	    entry->drone_index = -1;
	    if (operation == drone_writing)
	    {
	      entry->retry_count = 0;
	      entry->state = entry_idle;
	    }

#ifdef DEBUG_1
	  printf ("\n%s (%d, process_requests %s): Done.",
		  arguments.program_name, drone->index, operation_name);
	  fflush (stdout);
#endif
	  }
	}
      }

      count = 0;
      drone->state = drone_idle;
      read_mask = (* wait_mask);
      if ((read_mask & my_mask) == my_mask)
	(void) (kill (boss_pid, SIGCONT));
    }
    else if (result == 0)
    {
      if (count == arguments.tdron)
      {
	count = 0;
	if ((kill (boss_pid, 0)) == -1)
	  kill_program (-1);
      }
      read_mask = (* wait_mask);
      if ((read_mask & my_mask) == my_mask)
      {
	fprintf (stderr,
		 "\n%s (%d, process_requests): signal deadlock (%s)!\n",
		 arguments.program_name, drone->index,
		 ((read_mask == ((unsigned long) -1)) ? "any" : "me"));
	fflush (stderr);
	drone->state = drone_idle; /* !! */
	(void) (kill (boss_pid, SIGCONT));
      }
    }
  }
}

static void
DEFUN_VOID (start_drones)
{
  pid_t my_pid;
  int counter, cpid;
  struct drone_info *gc_drones, *drone;

  my_pid = (getpid ());

  shared_memory = (shmat (arguments.shmid, ((char *) 0), 0));
  if (shared_memory == ((char *) -1))
  {
    fprintf (stderr,
	     "\
%s (start_drones): Unable to attach shared memory segment %d (errno = %s).\n",
	     arguments.program_name, arguments.shmid, (error_name (errno)));
    fflush (stderr);
    sleep (10);
    kill (boss_pid, SIGCONT);
    exit (1);
  }
#ifdef DEBUG_1
  printf ("%s (start_drones): Attached shared memory at address = 0x%lx.\n",
	  arguments.program_name, ((long) shared_memory));
  fflush (stdout);
#endif
  posix_signal (SIGINT, SIG_IGN);
  posix_signal (SIGQUIT, SIG_IGN);
  posix_signal (SIGHUP, kill_program);
  posix_signal (SIGTERM, kill_program);

  gc_buffers = ((struct buffer_info *)
		(shared_memory + (arguments.nbuf * arguments.bufsiz)));
  gc_drones = ((struct drone_info *) (gc_buffers + arguments.nbuf));
  drone_version = ((unsigned long *) (gc_drones + arguments.tdron));
  wait_mask = (drone_version + 1);
  if ((* drone_version) != ((unsigned long) DRONE_VERSION_NUMBER))
  {
    fprintf (stderr,
	     "%s (start_drones): stored drone version != drone version.\n",
	     arguments.program_name);
    fprintf (stderr, "\t*drone_version = %ld; DRONE_VERSION_NUMBER = %ld.\n",
	     (* drone_version), ((unsigned long) DRONE_VERSION_NUMBER));
    fflush (stderr);
    kill (boss_pid, SIGCONT);
    exit (1);
  }

  for (counter = 1, drone = (gc_drones + (arguments.sdron + 1));
       counter < arguments.ndron;
       counter++, drone ++)
  {
    if ((cpid = (fork ())) == 0)
    {
      drone->DRONE_PPID = my_pid;
      process_requests (drone);
      /*NOTREACHED*/
    }
    else if (cpid == -1)
    {
      fprintf (stderr,
	       "%s (start_drones): fork failed; errno = %s.\n",
	       arguments.program_name, (error_name (errno)));
      fflush (stderr);
    }
  }
  drone = (gc_drones + arguments.sdron);
  drone->DRONE_PPID = boss_pid;
  /* This is non-portable behavior to prevent zombies from being created. */
  if (arguments.ndron != 1)
    posix_signal (SIGCHLD, SIG_IGN);
  process_requests (drone);
  /*NOTREACHED*/
}

int
DEFUN (main, (argc, argv), int argc AND char ** argv)
{
  int count, nargs;
  static char err_buf[1024];
#if defined(DEBUG) || defined(DEBUG_1) || defined(DEBUG_2)
  static char out_buf[1024];

  setvbuf (stdout, &out_buf[0], _IOFBF, (sizeof (out_buf)));
#endif
  setvbuf (stderr, &err_buf[0], _IOFBF, (sizeof (err_buf)));

#ifdef DEBUG_2
  printf ("%s (main): Arguments =\n", argv[0]);
  for (count = 1; count < argc; count++)
    printf ("\t%s\n", argv[count]);
  fflush (stdout);
#endif

  nargs = ((sizeof (command_line)) / (sizeof (struct argdesc)));
  boss_pid = (getppid ());
  if (argc != nargs)
  {
    fprintf (stderr,
	     "%s (main): Wrong number of arguments (got %d, expected %d).\n",
	     argv[0], (argc - 1), (nargs - 1));
    fflush (stderr);
    kill (boss_pid, SIGCONT);
    exit (1);
  }
  for (count = 0; count < nargs; count++)
  {
    if (command_line[count].format == STRING_FMT)
      (* ((char **) command_line[count].location)) = argv[count];
    else
      sscanf (argv[count],
	      command_line[count].format,
	      command_line[count].location);
  }

#ifdef DEBUG_2
  printf ("%s (main): Parsed arguments =\n", argv[0]);
  for (count = 0; count < nargs; count++)
  {
    if (command_line[count].format == STRING_FMT)
      printf ("\t%s\t= %s\n",
	      command_line[count].name,
	      (* ((char **) (command_line[count].location))));
    else
      printf ("\t%s\t= %d\n",
	      command_line[count].name,
	      (* ((int *) (command_line[count].location))));
  }
  fflush (stdout);
#endif

  start_drones ();
  /*NOTREACHED*/
  return (0);
}

#define MAIN main

#endif /* USE_SYSV_SHARED_MEMORY */

#ifndef MAIN

int
DEFUN (main, (argc, argv), int argc AND char ** argv)
{
  fprintf (stderr, "%s: Not implemented.\n", (argv[0]));
  fflush (stderr);
  exit (1);
  return (1);
}

#endif /* MAIN */
