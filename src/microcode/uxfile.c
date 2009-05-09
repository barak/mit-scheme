/* -*-C-*-

$Id: uxfile.c,v 1.20 2009/04/15 13:00:29 riastradh Exp $

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
#include "ux.h"
#include "osfile.h"
#include "uxio.h"

extern void terminal_open (Tchannel channel);

static enum channel_type
fd_channel_type (int fd)
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
OS_open_fd (int fd)
{
  enum channel_type type = (fd_channel_type (fd));
  Tchannel channel;
  MAKE_CHANNEL (fd, type, channel =);
  if (type == channel_type_terminal)
    terminal_open (channel);
  return (channel);
}

static Tchannel
open_file (const char * filename, int oflag)
{
  int fd;
  STD_FD_SYSTEM_CALL (syscall_open, fd, (UX_open (filename, oflag, MODE_REG)));
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
name (const char * filename)						\
{									\
  return (open_file (filename, oflag));					\
}

DEFUN_OPEN_FILE (OS_open_input_file, O_RDONLY)
DEFUN_OPEN_FILE (OS_open_output_file, (O_WRONLY | O_CREAT | O_TRUNC))
DEFUN_OPEN_FILE (OS_open_exclusive_output_file, (O_WRONLY | O_CREAT | O_EXCL))
DEFUN_OPEN_FILE (OS_open_io_file, (O_RDWR | O_CREAT))

#ifdef O_APPEND

DEFUN_OPEN_FILE (OS_open_append_file, (O_WRONLY | O_CREAT | O_APPEND))

#else

Tchannel
OS_open_append_file (const char * filename)
{
  error_unimplemented_primitive ();
  return (0);
}

#endif

static Tchannel
make_load_channel (int fd)
{
  enum channel_type type = (fd_channel_type (fd));
  if ((type == channel_type_terminal)
      || (type == channel_type_directory)
      || (type == channel_type_unknown))
    return (NO_CHANNEL);
  MAKE_CHANNEL (fd, type, return);
}

Tchannel
OS_open_load_file (const char * filename)
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
OS_open_dump_file (const char * filename)
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
OS_file_length (Tchannel channel)
{
  struct stat stat_buf;
  STD_VOID_SYSTEM_CALL
    (syscall_fstat, (UX_fstat ((CHANNEL_DESCRIPTOR (channel)), (&stat_buf))));
  return (stat_buf . st_size);
}

off_t
OS_file_position (Tchannel channel)
{
  off_t result;
  STD_UINT_SYSTEM_CALL
    (syscall_lseek,
     result,
     (UX_lseek ((CHANNEL_DESCRIPTOR (channel)), 0L, SEEK_CUR)));
  return (result);
}

void
OS_file_set_position (Tchannel channel, off_t position)
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
OS_file_truncate (Tchannel channel, off_t length)
{
  STD_VOID_SYSTEM_CALL
    (syscall_ftruncate,
     (UX_ftruncate ((CHANNEL_DESCRIPTOR (channel)), length)));
}
