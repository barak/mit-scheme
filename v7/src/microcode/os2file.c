/* -*-C-*-

$Id: os2file.c,v 1.1 1994/11/28 03:42:57 cph Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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
#include "osfile.h"

static ULONG set_file_pointer (Tchannel, ULONG, LONG);

#define OS2_OPEN_MODE(m)						\
  ((((m) & CHANNEL_READ) == 0)						\
   ? (OPEN_ACCESS_WRITEONLY | OPEN_SHARE_DENYWRITE)			\
   : (((m) & CHANNEL_WRITE) == 0)					\
   ? (OPEN_ACCESS_READONLY  | OPEN_SHARE_DENYNONE)			\
   : (OPEN_ACCESS_READWRITE | OPEN_SHARE_DENYWRITE))

static Tchannel
open_file (const char * filename, ULONG attr, ULONG flags, unsigned int mode)
{
  HFILE handle;
  ULONG action;
  STD_API_CALL
    (dos_open, (((char *) filename), (&handle), (&action), 0, attr, flags,
		(OS2_OPEN_MODE (mode)), 0));
  return (OS2_make_channel (handle, mode));
}

Tchannel
OS_open_input_file (const char * filename)
{
  return
    (open_file (filename,
		FILE_NORMAL,
		(OPEN_ACTION_OPEN_IF_EXISTS | OPEN_ACTION_FAIL_IF_NEW),
		CHANNEL_READ));
}

Tchannel
OS_open_output_file (const char * filename)
{
  return
    (open_file (filename,
		FILE_NORMAL,
		(OPEN_ACTION_REPLACE_IF_EXISTS | OPEN_ACTION_CREATE_IF_NEW),
		CHANNEL_WRITE));
}

Tchannel
OS_open_io_file (const char * filename)
{
  return
    (open_file (filename,
		FILE_NORMAL,
		(OPEN_ACTION_OPEN_IF_EXISTS | OPEN_ACTION_CREATE_IF_NEW),
		(CHANNEL_READ | CHANNEL_WRITE)));
}

Tchannel
OS_open_append_file (const char * filename)
{
  Tchannel channel =
    (open_file (filename,
		FILE_NORMAL,
		(OPEN_ACTION_OPEN_IF_EXISTS | OPEN_ACTION_CREATE_IF_NEW),
		CHANNEL_WRITE));
  transaction_begin ();
  OS_channel_close_on_abort (channel);
  (void) set_file_pointer (channel, FILE_END, 0);
  transaction_commit ();
  return (channel);
}

static Tchannel
open_file_noerror (const char * filename, ULONG attr, ULONG flags,
		   unsigned int mode)
{
  HFILE handle;
  ULONG action;
  if ((dos_open (((char *) filename), (&handle), (&action), 0, attr, flags,
		 (OS2_OPEN_MODE (mode)), 0))
      != NO_ERROR)
    return (NO_CHANNEL);
  {
    Tchannel channel = (OS2_make_channel (handle, mode));
    if ((CHANNEL_TYPE (channel)) == channel_type_file)
      return (channel);
    OS_channel_close_noerror (channel);
    return (NO_CHANNEL);
  }
}

Tchannel
OS_open_load_file (const char * filename)
{
  return
    (open_file_noerror
     (filename,
      FILE_NORMAL,
      (OPEN_ACTION_OPEN_IF_EXISTS | OPEN_ACTION_FAIL_IF_NEW),
      CHANNEL_READ));
}

Tchannel
OS_open_dump_file (const char * filename)
{
  return
    (open_file_noerror
     (filename,
      FILE_NORMAL,
      (OPEN_ACTION_REPLACE_IF_EXISTS | OPEN_ACTION_CREATE_IF_NEW),
      CHANNEL_WRITE));
}

off_t
OS_file_length (Tchannel channel)
{
  FILESTATUS3 buffer;
  if ((CHANNEL_TYPE (channel)) != channel_type_file)
    OS2_error_system_call (ERROR_INVALID_HANDLE, syscall_dos_query_file_info);
  STD_API_CALL
    (dos_query_file_info,
     ((CHANNEL_HANDLE (channel)), FIL_STANDARD,
      (&buffer), (sizeof (buffer))));
  return (buffer.cbFile);
}

off_t
OS_file_position (Tchannel channel)
{
  return (set_file_pointer (channel, FILE_CURRENT, 0));
}

void
OS_file_set_position (Tchannel channel, off_t position)
{
  if ((set_file_pointer (channel, FILE_BEGIN, position)) != position)
    OS2_error_anonymous ();
}

static ULONG
set_file_pointer (Tchannel channel, ULONG type, LONG distance)
{
  ULONG fp;
  if ((CHANNEL_TYPE (channel)) != channel_type_file)
    OS2_error_system_call (ERROR_INVALID_HANDLE, syscall_dos_set_file_ptr);
  STD_API_CALL
    (dos_set_file_ptr, ((CHANNEL_HANDLE (channel)), distance, type, (&fp)));
  return (fp);
}
