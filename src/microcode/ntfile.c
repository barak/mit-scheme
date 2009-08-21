/* -*-C-*-

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

#include "nt.h"
#include "osfile.h"
#include "ntio.h"

#define DEFUN_OPEN_FILE(name, args)					\
Tchannel								\
name (const char * filename)						\
{									\
  HANDLE hFile;								\
  STD_HANDLE_API_CALL (hFile, CreateFile, args);			\
  return (NT_open_handle (hFile));					\
}

// In the following we specify FILE_SHARE_READ | FILE_SHARE_WRITE
// so that we can edit and save out a file while we are still in a
// error REPL from a buggy source file.

DEFUN_OPEN_FILE (OS_open_input_file,
  (filename, GENERIC_READ, (FILE_SHARE_READ | FILE_SHARE_WRITE), 0,
   OPEN_EXISTING, (FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN), 0));

DEFUN_OPEN_FILE (OS_open_output_file,
  (filename, GENERIC_WRITE, FILE_SHARE_READ, 0,
   CREATE_ALWAYS, (FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN), 0));

DEFUN_OPEN_FILE (OS_open_io_file,
  (filename, (GENERIC_READ | GENERIC_WRITE),
   (FILE_SHARE_READ | FILE_SHARE_WRITE), 0,
   OPEN_ALWAYS, (FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS), 0));

Tchannel
OS_open_append_file (const char * filename)
{
  HANDLE hFile;
  STD_HANDLE_API_CALL
    (hFile,
     CreateFile, (filename,
	          GENERIC_WRITE,
		  FILE_SHARE_READ	/*sharing*/,
		  0,			/*security*/
		  OPEN_ALWAYS,
		  FILE_ATTRIBUTE_NORMAL /*attributes&flags*/,
		  0			/*Template*/
		  ));
  if ((SetFilePointer (hFile, 0, 0, FILE_END)) == 0xFFFFFFFF)
    NT_error_api_call ((GetLastError ()), apicall_SetFilePointer);
  return (NT_open_handle (hFile));
}

Tchannel
OS_open_exclusive_output_file (const char * filename)
{
  error_unimplemented_primitive ();
  return (0);
}

static Tchannel
make_load_channel (HANDLE handle)
{
  channel_class_t * class = (NT_handle_channel_class (handle));
  return
    ((((CHANNEL_CLASS_TYPE (class)) == channel_type_terminal)
      || ((CHANNEL_CLASS_TYPE (class)) == channel_type_directory))
     ? NO_CHANNEL
     : (NT_make_channel (handle, class)));
}

Tchannel
OS_open_load_file (const char * filename)
{
  HANDLE handle
    = (CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, 0,
		   OPEN_EXISTING, 0, 0));
  return
    ((handle != INVALID_HANDLE_VALUE)
     ? (make_load_channel (handle))
     : NO_CHANNEL);
}

Tchannel
OS_open_dump_file (const char * filename)
{
  HANDLE handle
    = (CreateFile (filename, GENERIC_WRITE, FILE_SHARE_READ, 0,
		   CREATE_ALWAYS, 0, 0));
  return
    ((handle != INVALID_HANDLE_VALUE)
     ? (make_load_channel (handle))
     : NO_CHANNEL);
}

off_t
OS_file_length (Tchannel channel)
{
  DWORD result = (GetFileSize ((CHANNEL_HANDLE (channel)), 0));
  if (result == 0xFFFFFFFF)
    NT_error_api_call ((GetLastError ()), apicall_GetFileSize);
  return (result);
}

off_t
OS_file_position (Tchannel channel)
{
  DWORD position
    = (SetFilePointer ((CHANNEL_HANDLE (channel)), 0, 0, FILE_CURRENT));
  if (position == 0xFFFFFFFF)
    NT_error_api_call ((GetLastError ()), apicall_SetFilePointer);
  return (position);
}

void
OS_file_set_position (Tchannel channel, off_t position)
{
  DWORD old_position
    = (SetFilePointer ((CHANNEL_HANDLE (channel)), position, 0, FILE_BEGIN));
  if (old_position == 0xFFFFFFFF)
    NT_error_api_call ((GetLastError ()), apicall_SetFilePointer);
  if (old_position != ((DWORD) position))
    error_external_return ();
}

void
OS_file_truncate (Tchannel channel, off_t length)
{
  OS_file_set_position (channel, length);
  STD_BOOL_API_CALL (SetEndOfFile, (CHANNEL_HANDLE (channel)));
}
