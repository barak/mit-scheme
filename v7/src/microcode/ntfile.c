/* -*-C-*-

$Id: ntfile.c,v 1.9 1997/06/19 05:55:34 cph Exp $

Copyright (c) 1992-97 Massachusetts Institute of Technology

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

extern void EXFUN (terminal_open, (Tchannel channel));

static enum channel_type
DEFUN (handle_channel_type, (hFile), HANDLE hFile)
{
  if (Screen_IsScreenHandle (hFile))
    return  channel_type_terminal;
//  if (IsConsoleHandle (hFile))
//    return  channel_type_terminal;
  switch (GetFileType (hFile))
  {
    default:
    case  FILE_TYPE_UNKNOWN:	return  channel_type_unknown;
    case  FILE_TYPE_DISK:	return  channel_type_file;
    case  FILE_TYPE_CHAR:	return  channel_type_win32_char;
    case  FILE_TYPE_PIPE:	return  channel_type_win32_pipe;
  }
}

Tchannel
DEFUN (OS_open_handle, (hFile), HANDLE hFile)
{
  enum channel_type type;
  Tchannel channel;
  
//  if (hFile == STDIN_HANDLE) {
//    channel = (NT_make_channel (STDIN_HANDLE, channel_type_terminal));
//    CHANNEL_COOKED(channel) = 1;
//  }
//
//  else if (hFile == STDOUT_HANDLE) {
//    channel = (NT_make_channel (STDOUT_HANDLE, channel_type_terminal));
//    CHANNEL_COOKED(channel) = 1;
//  }
//
//  else if (hFile == STDERR_HANDLE) {
//    channel = (NT_make_channel (STDERR_HANDLE, channel_type_terminal));
//    CHANNEL_COOKED(channel) = 1;
//  }

//  else
  {
    type = handle_channel_type (hFile);
    channel = (NT_make_channel (hFile, type));

    /* Like Unix, all terminals initialize to cooked mode. */
    if (type == channel_type_terminal)
      CHANNEL_COOKED(channel) = 1;
  }
  return  channel;
}


#define DEFUN_OPEN_FILE(name, args)					\
Tchannel								\
DEFUN (name, (filename), CONST char * filename)				\
{									\
  HANDLE hFile;								\
  STD_HANDLE_API_CALL (hFile, CreateFile, args);			\
  return (OS_open_handle (hFile));					\
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


#ifdef HAVE_APPEND

Tchannel
DEFUN (OS_open_append_file, (filename), CONST char * filename)
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
  return (OS_open_handle (hFile));
}

#else

Tchannel
DEFUN (OS_open_append_file, (filename), CONST char * filename)
{
  error_unimplemented_primitive ();
  return (0);
}

#endif

static Tchannel
DEFUN (make_load_channel, (handle), HANDLE handle)
{
  enum channel_type type = handle_channel_type (handle);
  return
    (((type == channel_type_terminal)
      || (type == channel_type_directory))
     ? NO_CHANNEL
     : (NT_make_channel (handle, type)));
}

Tchannel
DEFUN (OS_open_load_file, (filename), CONST char * filename)
{
      /*SRA:*/
   HANDLE  hFile;
   
   hFile = CreateFile (filename, GENERIC_READ,
				 FILE_SHARE_READ /*FILE_SHARE_READ?*/,
				 0 /*security?*/,
				 OPEN_EXISTING,
				 0,
				 0);
   if (hFile != INVALID_HANDLE_VALUE)
     return  make_load_channel (hFile);

   /* try to truncate extension for .bcon hack*/
   {
     char newname [MAX_PATH+10];
     int i;
     strncpy (newname, filename, MAX_PATH);
     for (i=0; newname[i]; i++);
     if (i<4)  return  NO_CHANNEL;
     if (newname[i-5]=='.') {
       newname[i-1] = 0;
       hFile = CreateFile (newname, GENERIC_READ, FILE_SHARE_READ,
			   0, OPEN_EXISTING, 0, 0);
       if (hFile != INVALID_HANDLE_VALUE)
	 return  make_load_channel (hFile);
     }
   }
 
   return  NO_CHANNEL;
}

Tchannel
DEFUN (OS_open_dump_file, (filename), CONST char * filename)
{
   HANDLE  hFile = CreateFile (	filename,
			        GENERIC_WRITE,
				FILE_SHARE_READ /*no sharing*/,
				0 /*security?*/,
				CREATE_ALWAYS,
				0,
				0);

  if (hFile != INVALID_HANDLE_VALUE)
    return  make_load_channel (hFile);
  
  return  NO_CHANNEL;
}

off_t
DEFUN (OS_file_length, (channel), Tchannel channel)
{
  DWORD result;
  DWORD code;
  while (1)
    {
      result = (GetFileSize ((CHANNEL_HANDLE (channel)), 0));
      if (result != 0xFFFFFFFF)
	return (result);
      code = (GetLastError ());
      if (code != NO_ERROR)
	NT_error_api_call (code, apicall_GetFileSize);
    }
}

off_t
DEFUN (OS_file_position, (channel), Tchannel channel)
{
  off_t result
    = (_llseek (((HFILE) (CHANNEL_HANDLE (channel))), 0L, SEEK_CUR));
  if (result == 0)
    NT_error_unix_call (errno, syscall_lseek);
  return (result);
}

void
DEFUN (OS_file_set_position, (channel, position),
       Tchannel channel AND
       off_t position)
{
  off_t result
    = (_llseek (((HFILE) (CHANNEL_HANDLE (channel))), position, SEEK_SET));
  if (result == 0)
    NT_error_unix_call (errno, syscall_lseek);
  if (result != position)
    error_external_return ();
}
