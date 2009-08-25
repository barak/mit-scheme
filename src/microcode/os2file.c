/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

#include "os2.h"
#include "osfile.h"

static ULONG set_file_pointer (Tchannel, ULONG, LONG);

#define OS2_OPEN_MODE(m)						\
  (((((m) & CHANNEL_READ) == 0)						\
    ? (OPEN_ACCESS_WRITEONLY | OPEN_SHARE_DENYWRITE)			\
    : (((m) & CHANNEL_WRITE) == 0)					\
    ? (OPEN_ACCESS_READONLY  | OPEN_SHARE_DENYNONE)			\
    : (OPEN_ACCESS_READWRITE | OPEN_SHARE_DENYWRITE))			\
   | OPEN_FLAGS_NOINHERIT)

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

Tchannel
OS_open_exclusive_output_file (const char * filename)
{
  error_unimplemented_primitive ();
  return (0);
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
