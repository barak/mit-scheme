/* -*-C-*-

$Id: os2io.c,v 1.4 1995/04/28 06:45:37 cph Exp $

Copyright (c) 1994-95 Massachusetts Institute of Technology

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

#include "os2.h"

extern void add_reload_cleanup (void (*) (void));
extern void OS2_initialize_console_channel (Tchannel);
extern void OS2_initialize_pipe_channel (Tchannel);

static enum channel_type handle_channel_type (LHANDLE);
static void handle_noinherit (LHANDLE);

size_t OS_channel_table_size;
struct channel * OS2_channel_table;
Tchannel * OS2_channel_pointer_table;
const int OS_have_select_p = 1;

void
OS2_initialize_channels (void)
{
  OS_channel_table_size = (OS2_MAX_FILE_HANDLES ());
  OS2_channel_table =
    (OS_malloc (OS_channel_table_size * (sizeof (struct channel))));
  OS2_channel_pointer_table =
    (OS_malloc (OS_channel_table_size * (sizeof (Tchannel))));
  {
    Tchannel channel;
    for (channel = 0; (channel < OS_channel_table_size); channel += 1)
      {
	(CHANNEL_OPEN (channel)) = 0;
	(OS2_channel_pointer_table [channel]) = channel;
      }
  }
  add_reload_cleanup (OS2_channel_close_all_noerror);
}

void
OS2_reset_channels (void)
{
  OS_free (OS2_channel_table);
  OS2_channel_table = 0;
  OS_channel_table_size = 0;
}

void
OS2_channel_operation (Tchannel channel, chop_t operation,
		       choparg_t arg1, choparg_t arg2, choparg_t arg3)
{
  ((* (CHANNEL_OPERATOR (channel))) (channel, operation, arg1, arg2, arg3));
}

Tchannel
OS2_make_channel (LHANDLE handle, unsigned int mode)
{
  Tchannel channel = 0;
  enum channel_type type;
  transaction_begin ();
  OS2_handle_close_on_abort (handle);
  while (1)
    {
      if (channel == OS_channel_table_size)
	OS2_error_out_of_channels ();
      if (! (CHANNEL_OPEN (channel)))
	break;
      channel += 1;
    }
  type = (handle_channel_type (handle));
  handle_noinherit (handle);
  (CHANNEL_HANDLE (channel)) = handle;
  (CHANNEL_TYPE (channel)) = type;
  (CHANNEL_OPEN (channel)) = 1;
  (CHANNEL_INTERNAL (channel)) = 0;
  (CHANNEL_NONBLOCKING (channel)) = 0;
  (CHANNEL_INPUTP (channel)) = ((mode & CHANNEL_READ) != 0);
  (CHANNEL_OUTPUTP (channel)) = ((mode & CHANNEL_WRITE) != 0);
  (CHANNEL_OPERATOR (channel)) = 0;
  switch (type)
    {
    case channel_type_console:
      OS2_initialize_console_channel (channel);
      break;
    case channel_type_unnamed_pipe:
      OS2_initialize_pipe_channel (channel);
      break;
    }
  transaction_commit ();
  return (channel);
}

static enum channel_type
handle_channel_type (LHANDLE handle)
{
  /* **** For now, limit channel types to those that we know how to
     handle in a reasonable way.  Later we can add other types if
     needed.  However, we probably won't need other types since pipes
     and files are sufficient to do nearly anything, and the console
     will be flushed when the PM support is installed.  */
  ULONG type;
  ULONG flags;
  if ((dos_query_h_type (handle, (&type), (&flags))) == NO_ERROR)
    switch (type & 0xff)
      {
      case FHT_DISKFILE:
	return (channel_type_file);
      case FHT_CHRDEV:
	if ((flags & 0x3) != 0)
	  return (channel_type_console);
	else if ((flags & 0x4) != 0)
	  /* return (channel_type_null); */
	  break;
	else if ((flags & 0x8) != 0)
	  /* return (channel_type_clock); */
	  break;
	else
	  /* return (channel_type_character_device); */
	  break;
      case FHT_PIPE:
	{
	  APIRET rc = (dos_query_n_p_h_state (handle, (&flags)));
	  if ((rc == NO_ERROR) || (rc == ERROR_PIPE_NOT_CONNECTED))
	    /* return (channel_type_named_pipe); */
	    break;
	  else
	    return (channel_type_unnamed_pipe);
	}
      }
  OS2_error_anonymous ();
  return (channel_type_unknown);
}

static void
handle_noinherit (LHANDLE handle)
{
  ULONG state;
  STD_API_CALL (dos_query_fh_state, (handle, (& state)));
  /* Magic mask 0xFF88 zeroes out high bits and two fields
     required to be zero by the spec.  When testing, the high
     bits were not zero, and this caused the system call to
     complain.  */
  state &= 0xFF88;
  STD_API_CALL
    (dos_set_fh_state, (handle, (state | OPEN_FLAGS_NOINHERIT)));
}

void
OS_channel_close (Tchannel channel)
{
  if (! (CHANNEL_INTERNAL (channel)))
    {
      if (CHANNEL_ABSTRACT_P (channel))
	OS2_channel_operation (channel, chop_close, 0, 0, 0);
      else
	STD_API_CALL (dos_close, (CHANNEL_HANDLE (channel)));
      (CHANNEL_OPEN (channel)) = 0;
    }
}

void
OS2_channel_close_all_noerror (void)
{
  Tchannel channel;
  for (channel = 0; (channel < OS_channel_table_size); channel += 1)
    if (CHANNEL_OPEN (channel))
      OS_channel_close_noerror (channel);
}

void
OS_channel_close_noerror (Tchannel channel)
{
  transaction_begin ();
  OS2_ignore_errors ();
  OS_channel_close (channel);
  transaction_commit ();
}

static void
OS_channel_close_on_abort_1 (void * cp)
{
  OS_channel_close_noerror (* ((Tchannel *) cp));
}

void
OS_channel_close_on_abort (Tchannel channel)
{
  Tchannel * cp = (dstack_alloc (sizeof (Tchannel)));
  (*cp) = (channel);
  transaction_record_action (tat_abort, OS_channel_close_on_abort_1, cp);
}

static void
OS2_handle_close_on_abort_1 (void * hp)
{
  (void) dos_close (* ((LHANDLE *) hp));
}

void
OS2_handle_close_on_abort (LHANDLE h)
{
  LHANDLE * hp = (dstack_alloc (sizeof (LHANDLE)));
  (*hp) = h;
  transaction_record_action (tat_abort, OS2_handle_close_on_abort_1, hp);
}

int
OS_channel_open_p (Tchannel channel)
{
  return (CHANNEL_OPEN (channel));
}

enum channel_type
OS_channel_type (Tchannel channel)
{
  return (CHANNEL_TYPE (channel));
}

long
OS_channel_read (Tchannel channel, void * buffer, size_t nbytes)
{
  long n;
  if (nbytes == 0)
    return (0);
  if (CHANNEL_ABSTRACT_P (channel))
    OS2_channel_operation (channel, chop_read,
			   ((choparg_t) buffer),
			   ((choparg_t) nbytes),
			   ((choparg_t) (& n)));
  else
    STD_API_CALL
      (dos_read, ((CHANNEL_HANDLE (channel)), buffer, nbytes,
		  ((ULONG *) (& n))));
  return (n);
}

long
OS_channel_write (Tchannel channel, const void * buffer, size_t nbytes)
{
  long n;
  if (nbytes == 0)
    return (0);
  if (CHANNEL_ABSTRACT_P (channel))
    OS2_channel_operation (channel,
			   chop_write,
			   ((choparg_t) buffer),
			   ((choparg_t) nbytes),
			   ((choparg_t) (& n)));
  else
    STD_API_CALL
      (dos_write, ((CHANNEL_HANDLE (channel)), ((void *) buffer), nbytes,
		   ((ULONG *) (& n))));
  return (n);
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

size_t
OS_channel_read_load_file (Tchannel channel, void * buffer, size_t nbytes)
{
  ULONG nread;
  if ((dos_read ((CHANNEL_HANDLE (channel)), buffer, nbytes, (&nread))) != 0)
    return (0);
  return (nread);
}

size_t
OS_channel_write_dump_file (Tchannel channel,
			    const void * buffer,
			    size_t nbytes)
{
  ULONG nwrite;
  if ((dos_write
       ((CHANNEL_HANDLE (channel)), ((void *) buffer), nbytes, (&nwrite)))
      != 0)
    return (0);
  return (nwrite);
}

void
OS_channel_write_string (Tchannel channel, const char * string)
{
  unsigned long length = (strlen (string));
  if ((OS_channel_write (channel, string, length)) != length)
    OS2_error_anonymous ();
}
