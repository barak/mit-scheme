/* -*-C-*-

$Id: ntfile.c,v 1.4 1993/09/13 18:36:20 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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
    case  FILE_TYPE_CHAR:	return  channel_type_character_device;
    case  FILE_TYPE_PIPE:	return  channel_type_fifo;
  }
}

Tchannel
DEFUN (OS_open_handle, (hFile), HANDLE hFile)
{
  enum channel_type type;
  Tchannel channel;
  
//  if (hFile == STDIN_HANDLE) {
//    MAKE_CHANNEL (STDIN_HANDLE, channel_type_terminal, channel=);
//    CHANNEL_COOKED(channel) = 1;
//  }
//
//  else if (hFile == STDOUT_HANDLE) {
//    MAKE_CHANNEL (STDOUT_HANDLE, channel_type_terminal, channel=);
//    CHANNEL_COOKED(channel) = 1;
//  }
//
//  else if (hFile == STDERR_HANDLE) {
//    MAKE_CHANNEL (STDERR_HANDLE, channel_type_terminal, channel=);
//    CHANNEL_COOKED(channel) = 1;
//  }

//  else
  {
    type = handle_channel_type (hFile);
    MAKE_CHANNEL (hFile, type, channel =);

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
  HANDLE  hFile;							\
  STD_HANDLE_SYSTEM_CALL (syscall_open, hFile, CreateFile args);	\
  return  OS_open_handle (hFile);					\
}

DEFUN_OPEN_FILE (OS_open_input_file,
  (filename, GENERIC_READ, FILE_SHARE_READ, 0,
   OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0));

DEFUN_OPEN_FILE (OS_open_output_file,
  (filename, GENERIC_WRITE, FILE_SHARE_READ, 0,
   CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));

DEFUN_OPEN_FILE (OS_open_io_file,
  (filename, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ, 0,
   OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));


//DEFUN_OPEN_FILE (OS_open_input_file, O_RDONLY | _O_BINARY)
//DEFUN_OPEN_FILE (OS_open_output_file, (O_WRONLY | O_CREAT | O_TRUNC | _O_BINARY))
//DEFUN_OPEN_FILE (OS_open_io_file, (O_RDWR | O_CREAT))


#ifdef HAVE_APPEND

Tchannel
DEFUN (OS_open_append_file, (filename), CONST char * filename)
{
  HANDLE    hFile;
  STD_HANDLE_SYSTEM_CALL
    (syscall_open, hFile,
      CreateFile (filename,
	          GENERIC_WRITE,
		  FILE_SHARE_READ	/*sharing*/,
		  0	/*security*/,
		  OPEN_ALWAYS,
		  FILE_ATTRIBUTE_NORMAL /*attributes&flags*/,
		  0	/*Template*/));
  SetFilePointer (hFile, 0, 0, FILE_END);
  return  OS_open_handle (hFile);
}


//DEFUN_OPEN_FILE (OS_open_append_file, (O_WRONLY | O_CREAT | O_APPEND | _O_BINARY))

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
  if ((type == channel_type_terminal)
      || (type == channel_type_directory)
      )
    return (NO_CHANNEL);
  MAKE_CHANNEL (handle, type, return);
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
  DWORD  result;
  while ((result = GetFileSize (CHANNEL_HANDLE (channel), 0)) == 0xffffffffL
         && GetLastError() != NO_ERROR)
    error_system_call (GetLastError(), syscall_fstat);
  
  return  result;
}

off_t
DEFUN (OS_file_position, (channel), Tchannel channel)
{
  off_t result;
  STD_UINT_SYSTEM_CALL
    (syscall_lseek,
     result,
     (_llseek ((CHANNEL_HANDLE (channel)), 0L, SEEK_CUR)));
  return (result);
}

void
DEFUN (OS_file_set_position, (channel, position),
       Tchannel channel AND
       off_t position)
{
  LONG result;
  STD_UINT_SYSTEM_CALL
    (syscall_lseek,
     result,
     (_llseek ((CHANNEL_HANDLE (channel)), position, SEEK_SET)));
  if (result != position)
    error_external_return ();
}
