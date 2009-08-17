/* -*-C-*-

$Id: c125eacce19b64cdf4caa5124e0c16a354c19aab $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
#include "nt.h"
#include "ntio.h"
#include "ntgui.h"
#include "osterm.h"
#include "osfile.h"
#include "osproc.h"
#include "ostty.h"
#include "outf.h"
#include "ossig.h"
#include "intrpt.h"
#include "ntscreen.h"

#undef TRACE_NTIO
#ifdef TRACE_NTIO
extern FILE * trace_file;
#endif

extern HANDLE master_tty_window;

channel_class_t * NT_channel_class_generic;
channel_class_t * NT_channel_class_file;
channel_class_t * NT_channel_class_screen;
channel_class_t * NT_channel_class_anonymous_pipe;
channel_class_t * NT_channel_class_named_pipe;

static Tchannel channel_allocate (void);
static long cooked_channel_write (Tchannel, const void *, unsigned long) ;
static int wait_on_multiple_objects (struct select_registry_s *);
static int test_multiple_objects (struct select_registry_s *);
static int wait_on_single_object (Tchannel, unsigned int);
static int test_single_object (Tchannel, unsigned int);
static unsigned int test_single_object_1 (Tchannel, unsigned int);
static int test_for_pending_event (void);

#ifndef NT_DEFAULT_CHANNEL_TABLE_SIZE
#define NT_DEFAULT_CHANNEL_TABLE_SIZE 1024
#endif

Tchannel OS_channel_table_size;
struct channel * NT_channel_table;

Tchannel
NT_make_channel (HANDLE handle, channel_class_t * class)
{
  Tchannel channel;
  transaction_begin ();
  NT_handle_close_on_abort (handle);
  channel = (channel_allocate ());
  (CHANNEL_CLASS (channel)) = class;
  (CHANNEL_HANDLE (channel)) = handle;
  (CHANNEL_INTERNAL (channel)) = 0;
  (CHANNEL_NONBLOCKING (channel)) = 0;
  (CHANNEL_BUFFERED (channel)) = 1;
  (CHANNEL_COOKED (channel)) = 0;
  transaction_commit ();
  return (channel);
}

channel_class_t *
NT_handle_channel_class (HANDLE handle)
{
  if (Screen_IsScreenHandle (handle))
    return (NT_channel_class_screen);
  /* If GetFileType returns FILE_TYPE_PIPE, assume that it is a named
     pipe.  This procedure won't be called with an anonymous-pipe
     handle.  */
  switch (GetFileType (handle))
    {
    case FILE_TYPE_DISK: return (NT_channel_class_file);
    case FILE_TYPE_CHAR: return (NT_channel_class_generic);
    case FILE_TYPE_PIPE: return (NT_channel_class_named_pipe);
    default: return (NT_channel_class_generic);
    }
}

Tchannel
NT_open_handle (HANDLE handle)
{
  Tchannel channel
    = (NT_make_channel (handle, (NT_handle_channel_class (handle))));
  /* Like Unix, all terminals initialize to cooked mode.  */
  if ((CHANNEL_TYPE (channel)) == channel_type_terminal)
    (CHANNEL_COOKED (channel)) = 1;
  return (channel);
}

long
OS_channel_read (Tchannel channel, void * buffer, size_t n_bytes)
{
  return
    ((n_bytes == 0)
     ? 0
     : ((* (CHANNEL_CLASS_OP_READ (CHANNEL_CLASS (channel))))
	(channel, buffer, n_bytes)));
}

long
OS_channel_write (Tchannel channel, const void * buffer, size_t n_bytes)
{
  return
    ((n_bytes == 0)
     ? 0
     : (CHANNEL_COOKED (channel))
     ? (cooked_channel_write (channel, buffer, n_bytes))
     : ((* (CHANNEL_CLASS_OP_WRITE (CHANNEL_CLASS (channel))))
	(channel, buffer, n_bytes)));
}

void
OS_channel_close (Tchannel channel)
{
  if (! ((CHANNEL_CLOSED_P (channel)) || (CHANNEL_INTERNAL (channel))))
    {
      (* (CHANNEL_CLASS_OP_CLOSE (CHANNEL_CLASS (channel)))) (channel, 1);
      MARK_CHANNEL_CLOSED (channel);
    }
}

void
OS_channel_close_noerror (Tchannel channel)
{
  if (! ((CHANNEL_CLOSED_P (channel)) || (CHANNEL_INTERNAL (channel))))
    {
      (* (CHANNEL_CLASS_OP_CLOSE (CHANNEL_CLASS (channel)))) (channel, 0);
      MARK_CHANNEL_CLOSED (channel);
    }
}

long
NT_channel_n_read (Tchannel channel)
{
  if (CHANNEL_CLOSED_P (channel))
    return (0);
  return ((* (CHANNEL_CLASS_OP_N_READ (CHANNEL_CLASS (channel)))) (channel));
}

static void
NT_channel_close_all (void)
{
  Tchannel channel;
  for (channel = 0; (channel < OS_channel_table_size); channel += 1)
    if (CHANNEL_OPEN_P (channel))
      OS_channel_close_noerror (channel);
}

static Tchannel
channel_allocate (void)
{
  Tchannel channel = 0;
  while (1)
  {
    if (channel == OS_channel_table_size)
      error_out_of_channels ();
    if (CHANNEL_CLOSED_P (channel))
      return (channel);
    channel += 1;
  }
}

int
OS_channel_open_p (Tchannel channel)
{
  return (CHANNEL_OPEN_P (channel));
}

static void
channel_close_on_abort_1 (void * cp)
{
  OS_channel_close (* ((Tchannel *) cp));
}

void
OS_channel_close_on_abort (Tchannel channel)
{
  Tchannel * cp = ((Tchannel *) (dstack_alloc (sizeof (Tchannel))));
  (*cp) = (channel);
  transaction_record_action (tat_abort, channel_close_on_abort_1, cp);
}

static void
NT_handle_close_on_abort_1 (void * hp)
{
  (void) CloseHandle (* ((HANDLE *) hp));
}

void
NT_handle_close_on_abort (HANDLE h)
{
  HANDLE * hp = (dstack_alloc (sizeof (HANDLE)));
  (*hp) = h;
  transaction_record_action (tat_abort, NT_handle_close_on_abort_1, hp);
}

enum channel_type
OS_channel_type (Tchannel channel)
{
  return (CHANNEL_TYPE (channel));
}

void
OS_channel_synchronize (Tchannel channel)
{
}

static void
generic_channel_close (Tchannel channel, int errorp)
{
  if ((!CloseHandle (CHANNEL_HANDLE (channel))) && errorp)
    NT_error_api_call ((GetLastError ()), apicall_CloseHandle);
}

static long
generic_channel_read (Tchannel channel, void * buffer, unsigned long n_bytes)
{
  DWORD bytes_read;
  if ((!ReadFile ((CHANNEL_HANDLE (channel)),
		  buffer, n_bytes, (&bytes_read), 0))
      && (bytes_read > 0))
    NT_error_api_call ((GetLastError ()), apicall_ReadFile);
  return (bytes_read);
}

static long
generic_channel_write (Tchannel channel, const void * buffer,
		       unsigned long n_bytes)
{
  DWORD n_written;
  STD_BOOL_API_CALL
    (WriteFile,
     ((CHANNEL_HANDLE (channel)), ((LPCVOID) buffer), n_bytes, (&n_written),
      0));
  return (n_written);
}

static long
generic_channel_n_read (Tchannel channel)
{
  return (CHANNEL_N_READ_UNKNOWN);
}

static void
initialize_channel_class_generic (void)
{
  channel_class_t * class = (OS_malloc (sizeof (channel_class_t)));
  (CHANNEL_CLASS_TYPE (class)) = channel_type_unknown;
  (CHANNEL_CLASS_OP_READ (class)) = generic_channel_read;
  (CHANNEL_CLASS_OP_WRITE (class)) = generic_channel_write;
  (CHANNEL_CLASS_OP_CLOSE (class)) = generic_channel_close;
  (CHANNEL_CLASS_OP_N_READ (class)) = generic_channel_n_read;
  NT_channel_class_generic = class;
}

static long
file_channel_n_read (Tchannel channel)
{
  DWORD length = (GetFileSize ((CHANNEL_HANDLE (channel)), 0));
  off_t position;
  if (length == 0xFFFFFFFF)
    return (0);
  position = (OS_file_position (channel));
  return ((position < ((off_t) length)) ? (((off_t) length) - position) : 0);
}

static void
initialize_channel_class_file (void)
{
  channel_class_t * class = (OS_malloc (sizeof (channel_class_t)));
  (*class) = (*NT_channel_class_generic);
  (CHANNEL_CLASS_TYPE (class)) = channel_type_file;
  (CHANNEL_CLASS_OP_N_READ (class)) = file_channel_n_read;
  NT_channel_class_file = class;
}

static long
screen_channel_read (Tchannel channel, void * buffer, unsigned long n_bytes)
{
  DWORD bytes_read
    = (Screen_Read ((CHANNEL_HANDLE (channel)),
		    ((BOOL) (CHANNEL_BUFFERED (channel))),
		    buffer,
		    n_bytes));
  if (bytes_read == 0xFFFFFFFF)
    {
      /* For pleasantness give up rest of this timeslice.  */
      Sleep (0);
      REQUEST_INTERRUPT (INT_Global_1);	/* windows polling */
      return (-1);
    }
  return (bytes_read);
}

static long
screen_channel_write (Tchannel channel, const void * buffer,
		      unsigned long n_bytes)
{
  HANDLE h = (CHANNEL_HANDLE (channel));
  SendMessage (h, SCREEN_WRITE, ((WPARAM) n_bytes), ((LPARAM) buffer));
  if (h == master_tty_window)
    SendMessage (h, WM_PAINT, 0, 0);
  return (n_bytes);
}

static long
screen_channel_n_read (Tchannel channel)
{
  /* This is incorrect.  However, it's a pain to do the right thing.
     Furthermore, NT_channel_n_read is only used by "select", and for
     that particular case, this is the correct value.  */
  return (CHANNEL_N_READ_WOULD_BLOCK);
}

static void
initialize_channel_class_screen (void)
{
  channel_class_t * class = (OS_malloc (sizeof (channel_class_t)));
  (CHANNEL_CLASS_TYPE (class)) = channel_type_terminal;
  (CHANNEL_CLASS_OP_READ (class)) = screen_channel_read;
  (CHANNEL_CLASS_OP_WRITE (class)) = screen_channel_write;
  (CHANNEL_CLASS_OP_CLOSE (class)) = 0;
  (CHANNEL_CLASS_OP_N_READ (class)) = screen_channel_n_read;
  NT_channel_class_screen = class;
}

void
OS_make_pipe (Tchannel * readerp, Tchannel * writerp)
{
  HANDLE hread;
  HANDLE hwrite;
  STD_BOOL_API_CALL (CreatePipe, ((&hread), (&hwrite), 0, 0));
  transaction_begin ();
  NT_handle_close_on_abort (hwrite);
  (*readerp) = (NT_make_channel (hread, NT_channel_class_anonymous_pipe));
  transaction_commit ();
  transaction_begin ();
  OS_channel_close_on_abort (*readerp);
  (*writerp) = (NT_make_channel (hwrite, NT_channel_class_anonymous_pipe));
  transaction_commit ();
}

static long
pipe_channel_read (Tchannel channel, void * buffer, unsigned long n_bytes)
{
#ifdef TRACE_NTIO
  fprintf (trace_file, "pipe_channel_read: channel=%d blocking=%s\n",
	   channel,
	   ((CHANNEL_NONBLOCKING (channel)) ? "no" : "yes"));
  fflush (trace_file);
#endif
  if (CHANNEL_NONBLOCKING (channel))
    {
      long n = (NT_channel_n_read (channel));
#ifdef TRACE_NTIO
      fprintf (trace_file, "pipe_channel_read: n=%d\n", n);
      fflush (trace_file);
#endif
      if (n <= 0)
	return (n);
    }
  return (generic_channel_read (channel, buffer, n_bytes));
}

static long
pipe_channel_n_read (Tchannel channel)
{
  DWORD n;
#ifdef TRACE_NTIO
  fprintf (trace_file, "pipe_channel_n_read: channel=%d\n", channel);
  fflush (trace_file);
#endif
  if (!PeekNamedPipe ((CHANNEL_HANDLE (channel)), 0, 0, 0, (&n), 0))
    {
      DWORD code = (GetLastError ());
      if ((code == ERROR_INVALID_HANDLE)
	  || (code == ERROR_BROKEN_PIPE))
	/* ERROR_BROKEN_PIPE means the other end of the pipe has been
	   closed, so return zero which means "end of file".  */
	return (0);
      NT_error_api_call (code, apicall_PeekNamedPipe);
    }
#ifdef TRACE_NTIO
  fprintf (trace_file, "pipe_channel_n_read: n=%d\n", n);
  fflush (trace_file);
#endif
  return ((n == 0) ? CHANNEL_N_READ_WOULD_BLOCK : n);
}

static void
initialize_channel_class_anonymous_pipe (void)
{
  channel_class_t * class = (OS_malloc (sizeof (channel_class_t)));
  (CHANNEL_CLASS_TYPE (class)) = channel_type_win32_anonymous_pipe;
  (CHANNEL_CLASS_OP_READ (class)) = pipe_channel_read;
  (CHANNEL_CLASS_OP_WRITE (class)) = generic_channel_write;
  (CHANNEL_CLASS_OP_CLOSE (class)) = generic_channel_close;
  (CHANNEL_CLASS_OP_N_READ (class)) = pipe_channel_n_read;
  NT_channel_class_anonymous_pipe = class;
}

static void
initialize_channel_class_named_pipe (void)
{
  channel_class_t * class = (OS_malloc (sizeof (channel_class_t)));
  (*class) = (*NT_channel_class_anonymous_pipe);
  (CHANNEL_CLASS_TYPE (class)) = channel_type_win32_named_pipe;
  NT_channel_class_named_pipe = class;
}

static long
cooked_channel_write (Tchannel channel, const void * buffer,
		      unsigned long n_bytes)
{
  /* Map LF to CR/LF */
  static const unsigned char crlf [] = {CARRIAGE_RETURN, LINEFEED};
  const unsigned char * bstart = buffer;
  const unsigned char * start = bstart;
  const unsigned char * end = (start + n_bytes);
  while (start < end)
    {
      const unsigned char * scan = start;
      while ((scan < end) && ((*scan) != LINEFEED))
	scan += 1;
      if (scan > start)
	{
	  unsigned int n_bytes = (scan - start);
	  long n_written
	    = ((* (CHANNEL_CLASS_OP_WRITE (CHANNEL_CLASS (channel))))
	       (channel, start, n_bytes));
	  if (n_written < 0)
	    return (start - bstart);
	  if (((unsigned int) n_written) < n_bytes)
	    return ((start - bstart) + n_written);
	}
      if (scan < end)
	{
	  unsigned int n_bytes = (sizeof (crlf));
	  long n_written
	    = ((* (CHANNEL_CLASS_OP_WRITE (CHANNEL_CLASS (channel))))
	       (channel, crlf, n_bytes));
	  if (n_written < ((long) n_bytes))
	    /* This backs out incorrectly if only CR is written out.  */
	    return (scan - bstart);
	}
      start = (scan + 1);
    }
  return (n_bytes);
}

size_t
OS_channel_read_load_file (Tchannel channel, void * buffer, size_t nbytes)
{
  DWORD scr;
  return ((ReadFile (CHANNEL_HANDLE (channel), buffer, nbytes, &scr, 0))
	  ? scr : 0);
}

size_t
OS_channel_write_dump_file (Tchannel channel, const void * buffer,
			    size_t nbytes)
{
  DWORD  scr;
  return ((WriteFile (CHANNEL_HANDLE (channel), ((LPCVOID) buffer), nbytes,
		      &scr, 0))
	  ? scr : 0);
}

void
OS_channel_write_string (Tchannel channel, const char * string)
{
  long length = (strlen (string));
  if ((OS_channel_write (channel, string, length)) != length)
    error_external_return ();
}

int
OS_channel_nonblocking_p (Tchannel channel)
{
  return (CHANNEL_NONBLOCKING (channel));
}

void
OS_channel_nonblocking (Tchannel channel)
{
  (CHANNEL_NONBLOCKING (channel)) = 1;
}

void
OS_channel_blocking (Tchannel channel)
{
  (CHANNEL_NONBLOCKING (channel)) = 0;
}

int
OS_terminal_buffered_p (Tchannel channel)
{
  return (CHANNEL_BUFFERED (channel));
}

void
OS_terminal_buffered (Tchannel channel)
{
  (CHANNEL_BUFFERED (channel)) = 1;
}

void
OS_terminal_nonbuffered (Tchannel channel)
{
  (CHANNEL_BUFFERED (channel)) = 0;
}

int
OS_terminal_cooked_output_p (Tchannel channel)
{
  return (CHANNEL_COOKED (channel));
}

void
OS_terminal_cooked_output (Tchannel channel)
{
  CHANNEL_COOKED (channel) = 1;
}

void
OS_terminal_raw_output (Tchannel channel)
{
  CHANNEL_COOKED (channel) = 0;
}

void
OS_terminal_flush_input (Tchannel channel)
{
}

void
OS_terminal_flush_output (Tchannel channel)
{
}

void
OS_terminal_drain_output (Tchannel channel)
{
}

unsigned int
arg_baud_index (unsigned int argument)
{
  return (arg_index_integer (argument, 1));
}

unsigned int
OS_terminal_get_ispeed (Tchannel channel)
{
  return (0);
}

unsigned int
OS_terminal_get_ospeed (Tchannel channel)
{
  return (0);
}

void
OS_terminal_set_ispeed (Tchannel channel, unsigned int baud)
{
}

void
OS_terminal_set_ospeed (Tchannel channel, unsigned int baud)
{
}

unsigned int
OS_baud_index_to_rate (unsigned int index)
{
  return (9600);
}

int
OS_baud_rate_to_index (unsigned int rate)
{
  return ((rate == 9600) ? 0 : -1);
}

unsigned int
OS_terminal_state_size (void)
{
  return (3);
}

void
OS_terminal_get_state (Tchannel channel, void * state_ptr)
{
  unsigned char * statep = ((unsigned char *) state_ptr);
  (*statep++) = (CHANNEL_NONBLOCKING (channel));
  (*statep++) = (CHANNEL_BUFFERED (channel));
  (*statep)   = (CHANNEL_COOKED (channel));
}

void
OS_terminal_set_state (Tchannel channel, void * state_ptr)
{
  unsigned char * statep = ((unsigned char *) state_ptr);
  (CHANNEL_NONBLOCKING (channel)) = (*statep++);
  (CHANNEL_BUFFERED (channel))    = (*statep++);
  (CHANNEL_COOKED (channel))      = (*statep);
}

int
OS_job_control_p (void)
{
  return (0);
}

int
OS_have_ptys_p (void)
{
  return (0);
}

/* Initialization/Termination code. */

int OS_have_select_p = 0;

extern HANDLE master_tty_window;
extern void NT_initialize_channels (void);
extern void NT_reset_channels (void);
extern void NT_restore_channels (void);

void
NT_reset_channels (void)
{
  OS_free (NT_channel_table);
  NT_channel_table = 0;
  OS_channel_table_size = 0;
}

void
NT_restore_channels (void)
{
  if (master_tty_window != ((HANDLE) NULL))
    Screen_Destroy (TRUE, master_tty_window);
  master_tty_window = ((HANDLE) NULL);
}

void
NT_initialize_channels (void)
{
  master_tty_window = (Screen_Create (NULL, "MIT/GNU Scheme", SW_SHOWNORMAL));
  if (win32_under_win32s_p ())
    OS_have_select_p = 0;
  else
    OS_have_select_p = 1;
  /* The following API call boosts the number of available handles to
     its maximum value.  This has no effect under NT, which does not
     place a limit on the number of handles.  */
  (void) SetHandleCount (255);
  OS_channel_table_size = NT_DEFAULT_CHANNEL_TABLE_SIZE;
  NT_channel_table
    = (OS_malloc (OS_channel_table_size * (sizeof (struct channel))));
  {
    Tchannel channel;
    for (channel = 0; (channel < OS_channel_table_size); channel += 1)
      MARK_CHANNEL_CLOSED (channel);
  }
  add_reload_cleanup (NT_channel_close_all);
  initialize_channel_class_generic ();
  initialize_channel_class_file ();
  initialize_channel_class_screen ();
  initialize_channel_class_anonymous_pipe ();
  initialize_channel_class_named_pipe ();
}

struct select_registry_s
{
  unsigned int n_channels;
  unsigned int length;
  Tchannel * channels;
  unsigned char * qmodes;
  unsigned char * rmodes;
};

select_registry_t
OS_allocate_select_registry (void)
{
  struct select_registry_s * r
    = (OS_malloc (sizeof (struct select_registry_s)));
  (r -> n_channels) = 0;
  (r -> length) = 16;
  (r -> channels) = (OS_malloc ((sizeof (Tchannel)) * (r -> length)));
  (r -> qmodes) = (OS_malloc ((sizeof (unsigned char)) * (r -> length)));
  (r -> rmodes) = (OS_malloc ((sizeof (unsigned char)) * (r -> length)));
  return (r);
}

void
OS_deallocate_select_registry (select_registry_t registry)
{
  struct select_registry_s * r = registry;
  OS_free (r -> rmodes);
  OS_free (r -> qmodes);
  OS_free (r -> channels);
  OS_free (r);
}

static void
resize_select_registry (struct select_registry_s * r, int growp)
{
  if (growp)
    (r -> length) *= 2;
  else
    (r -> length) /= 2;
  (r -> channels)
    = (OS_realloc ((r -> channels),
		   ((sizeof (Tchannel)) * (r -> length))));
  (r -> qmodes)
    = (OS_realloc ((r -> qmodes),
		   ((sizeof (unsigned char)) * (r -> length))));
  (r -> rmodes)
    = (OS_realloc ((r -> rmodes),
		   ((sizeof (unsigned char)) * (r -> length))));
}

void
OS_add_to_select_registry (select_registry_t registry, int fd,
			   unsigned int mode)
{
  struct select_registry_s * r = registry;
  Tchannel channel = fd;
  unsigned int i = 0;

  while (i < (r -> n_channels))
    {
      if (((r -> channels) [i]) == channel)
	{
	  ((r -> qmodes) [i]) |= mode;
	  return;
	}
      i += 1;
    }
  if (i == (r -> length))
    resize_select_registry (r, 1);
  ((r -> channels) [i]) = channel;
  ((r -> qmodes) [i]) = mode;
  (r -> n_channels) += 1;
}

void
OS_remove_from_select_registry (select_registry_t registry, int fd,
				unsigned int mode)
{
  struct select_registry_s * r = registry;
  Tchannel channel = fd;
  unsigned int i = 0;

  while (1)
    {
      if (i == (r -> n_channels))
	return;
      if (((r -> channels) [i]) == channel)
	{
	  ((r -> qmodes) [i]) &=~ mode;
	  if (((r -> qmodes) [i]) == 0)
	    break;
	  else
	    return;
	}
      i += 1;
    }
  while (i < (r -> n_channels))
    {
      ((r -> channels) [i]) = ((r -> channels) [(i + 1)]);
      ((r -> qmodes) [i]) = ((r -> qmodes) [(i + 1)]);
      i += 1;
    }
  (r -> n_channels) -= 1;

  if (((r -> length) > 16) && ((r -> n_channels) < ((r -> length) / 2)))
    resize_select_registry (r, 0);
}

unsigned int
OS_select_registry_length (select_registry_t registry)
{
  struct select_registry_s * r = registry;
  return (r -> n_channels);
}

void
OS_select_registry_result (select_registry_t registry, unsigned int index,
			   int * fd_r, unsigned int * mode_r)
{
  struct select_registry_s * r = registry;
  (*fd_r) = ((r -> channels) [index]);
  (*mode_r) = ((r -> rmodes) [index]);
}

int
OS_test_select_registry (select_registry_t registry, int blockp)
{
  struct select_registry_s * r = registry;
  if (win32_trace_level > 1)
    {
      fprintf (win32_trace_file, "OS_test_select_registry: ");
      fprintf (win32_trace_file, "n_channels=%d blockp=%d\n",
	       (r -> n_channels), blockp);
      fflush (win32_trace_file);
    }
  {
    int result
      = (blockp
	 ? (wait_on_multiple_objects (r))
	 : (test_multiple_objects (r)));
    if (win32_trace_level > 1)
      {
	fprintf (win32_trace_file, "OS_test_select_registry: ");
	fprintf (win32_trace_file, "result=%d\n", result);
	fflush (win32_trace_file);
      }
    return (result);
  }
}

static int
wait_on_multiple_objects (struct select_registry_s * r)
{
  while (1)
    {
      {
	int result = (test_multiple_objects (r));
	if (result != 0)
	  return (result);
      }
      /* Block waiting for a message to arrive.  The asynchronous
	 interrupt thread guarantees that a message will arrive in a
	 reasonable amount of time.  */
      if ((MsgWaitForMultipleObjects (0, 0, FALSE, INFINITE, QS_ALLINPUT))
	  == WAIT_FAILED)
	NT_error_api_call
	  ((GetLastError ()), apicall_MsgWaitForMultipleObjects);
    }
}

static int
test_multiple_objects (struct select_registry_s * r)
{
  unsigned int i;
  unsigned int j;

  j = 0;
  for (i = 0; (i < (r -> n_channels)); i += 1)
    {
      ((r -> rmodes) [i])
	= (test_single_object_1 (((r -> channels) [i]),
				 ((r -> qmodes) [i])));
      if (((r -> rmodes) [i]) != 0)
	j += 1;
    }
  return
    ((j > 0)
     ? j
     : (pending_interrupts_p ())
     ? SELECT_INTERRUPT
     : (OS_process_any_status_change ())
     ? SELECT_PROCESS_STATUS_CHANGE
     : 0);
}

int
OS_test_select_descriptor (int fd, int blockp, unsigned int qmode)
{
  Tchannel channel = fd;
  return
    (blockp
     ? (wait_on_single_object (channel, qmode))
     : (test_single_object (channel, qmode)));
}

static int
wait_on_single_object (Tchannel channel, unsigned int qmode)
{
  while (1)
    {
      int result = (test_single_object (channel, qmode));
      if (result != 0)
	return (result);

      /* Block waiting for a message to arrive.  The asynchronous
	 interrupt thread guarantees that a message will arrive in a
	 reasonable amount of time.  */
      if ((MsgWaitForMultipleObjects (0, 0, FALSE, INFINITE, QS_ALLINPUT))
	  == WAIT_FAILED)
	NT_error_api_call
	  ((GetLastError ()), apicall_MsgWaitForMultipleObjects);
    }
}

static int
test_single_object (Tchannel channel, unsigned int qmode)
{
  unsigned int rmode = (test_single_object_1 (channel, qmode));
  return
    ((rmode > 0)
     ? rmode
     : (pending_interrupts_p ())
     ? SELECT_INTERRUPT
     : (OS_process_any_status_change ())
     ? SELECT_PROCESS_STATUS_CHANGE
     : 0);
}


static unsigned int
test_single_object_1 (Tchannel channel, unsigned int qmode)
{
  unsigned int rmode = (qmode & SELECT_MODE_WRITE);
  if (((qmode & SELECT_MODE_READ) != 0)
      && ((channel == (OS_tty_input_channel ()))
	  ? (test_for_pending_event ())
	  : ((NT_channel_n_read (channel)) > 0)))
    rmode |= SELECT_MODE_READ;
  return (rmode);
}

static int
test_for_pending_event (void)
{
  MSG m;
  while (PeekMessage ((&m), 0, 0, 0, PM_REMOVE))
    DispatchMessage (&m);
  return (Screen_pending_events_p ());
}
