/* -*-C-*-

$Id: ntfile.c,v 1.12 1998/04/14 05:13:16 cph Exp $

Copyright (c) 1992-98 Massachusetts Institute of Technology

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
