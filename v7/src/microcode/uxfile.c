/* -*-C-*-

$Id: uxfile.c,v 1.14 2007/01/05 15:33:08 cph Exp $

Copyright 1990,1991,1993,1997,2000,2004 Massachusetts Institute of Technology

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

#include "ux.h"
#include "osfile.h"
#include "uxio.h"

extern void EXFUN (terminal_open, (Tchannel channel));

static enum channel_type
DEFUN (fd_channel_type, (fd), int fd)
{
  struct stat stat_buf;
  if ((UX_fstat (fd, (&stat_buf))) < 0)
    return (channel_type_unknown);
  {
    mode_t type = ((stat_buf . st_mode) & S_IFMT);
    return
      ((type == S_IFREG) ? channel_type_file
       : (type == S_IFCHR)
       ? ((isatty (fd))
	  ? channel_type_terminal
	  : channel_type_unix_character_device)
#ifdef S_IFIFO
       : (type == S_IFIFO) ? channel_type_unix_fifo
#endif
#ifdef S_IFBLK
       : (type == S_IFBLK) ? channel_type_unix_block_device
#endif
       : (type == S_IFDIR) ? channel_type_directory
       : channel_type_unknown);
  }
}

Tchannel
DEFUN (OS_open_fd, (fd), int fd)
{
  enum channel_type type = (fd_channel_type (fd));
  Tchannel channel;
  MAKE_CHANNEL (fd, type, channel =);
  if (type == channel_type_terminal)
    terminal_open (channel);
  return (channel);
}

static Tchannel
DEFUN (open_file, (filename, oflag), CONST char * filename AND int oflag)
{
  int fd;
  STD_UINT_SYSTEM_CALL
    (syscall_open, fd, (UX_open (filename, oflag, MODE_REG)));
#ifdef SLAVE_PTY_P
  if ((SLAVE_PTY_P (filename)) && (!UX_setup_slave_pty (fd)))
    {
      int xerrno = errno;
      UX_close (fd);
      error_system_call (xerrno, syscall_open);
    }
#endif
  return (OS_open_fd (fd));
}

#define DEFUN_OPEN_FILE(name, oflag)					\
Tchannel								\
DEFUN (name, (filename), CONST char * filename)				\
{									\
  return (open_file (filename, oflag));					\
}

DEFUN_OPEN_FILE (OS_open_input_file, O_RDONLY)
DEFUN_OPEN_FILE (OS_open_output_file, (O_WRONLY | O_CREAT | O_TRUNC))
DEFUN_OPEN_FILE (OS_open_io_file, (O_RDWR | O_CREAT))

#ifdef O_APPEND

DEFUN_OPEN_FILE (OS_open_append_file, (O_WRONLY | O_CREAT | O_APPEND))

#else

Tchannel
DEFUN (OS_open_append_file, (filename), CONST char * filename)
{
  error_unimplemented_primitive ();
  return (0);
}

#endif

static Tchannel
DEFUN (make_load_channel, (fd), int fd)
{
  enum channel_type type = (fd_channel_type (fd));
  if ((type == channel_type_terminal)
      || (type == channel_type_directory)
      || (type == channel_type_unknown))
    return (NO_CHANNEL);
  MAKE_CHANNEL (fd, type, return);
}

Tchannel
DEFUN (OS_open_load_file, (filename), CONST char * filename)
{
  while (1)
    {
      int fd = (UX_open (filename, O_RDONLY, MODE_REG));
      if (fd >= 0)
	return (make_load_channel (fd));
      if (errno != EINTR)
	return (NO_CHANNEL);
    }
}

Tchannel
DEFUN (OS_open_dump_file, (filename), CONST char * filename)
{
  while (1)
    {
      int fd = (UX_open (filename, (O_WRONLY | O_CREAT | O_TRUNC), MODE_REG));
      if (fd >= 0)
	return (make_load_channel (fd));
      if (errno != EINTR)
	return (NO_CHANNEL);
    }
}

off_t
DEFUN (OS_file_length, (channel), Tchannel channel)
{
  struct stat stat_buf;
  STD_VOID_SYSTEM_CALL
    (syscall_fstat, (UX_fstat ((CHANNEL_DESCRIPTOR (channel)), (&stat_buf))));
  return (stat_buf . st_size);
}

off_t
DEFUN (OS_file_position, (channel), Tchannel channel)
{
  off_t result;
  STD_UINT_SYSTEM_CALL
    (syscall_lseek,
     result,
     (UX_lseek ((CHANNEL_DESCRIPTOR (channel)), 0L, SEEK_CUR)));
  return (result);
}

void
DEFUN (OS_file_set_position, (channel, position),
       Tchannel channel AND
       off_t position)
{
  off_t result;
  STD_UINT_SYSTEM_CALL
    (syscall_lseek,
     result,
     (UX_lseek ((CHANNEL_DESCRIPTOR (channel)), position, SEEK_SET)));
  if (result != position)
    error_external_return ();
}

void
DEFUN (OS_file_truncate, (channel, length),
       Tchannel channel AND
       off_t length)
{
  STD_VOID_SYSTEM_CALL
    (syscall_ftruncate,
     (UX_ftruncate ((CHANNEL_DESCRIPTOR (channel)), length)));
}
