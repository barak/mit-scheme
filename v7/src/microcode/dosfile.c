/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosfile.c,v 1.1 1992/05/05 06:55:13 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

#include "msdos.h"
#include "osfile.h"
#include "dosio.h"

extern void EXFUN (terminal_open, (Tchannel channel));

static enum channel_type
DEFUN (fd_channel_type, (fd), int fd)
{
  struct stat stat_buf;
  if ((DOS_fstat (fd, (&stat_buf))) < 0)
    return (channel_type_unknown);
  {
    mode_t type = ((stat_buf . st_mode) & S_IFMT);
    return
      ((type == S_IFREG) ? channel_type_file
       : (type == S_IFCHR)
       ? ((isatty (fd))
	  ? channel_type_terminal
	  : channel_type_character_device)
#ifdef S_IFIFO
       : (type == S_IFIFO) ? channel_type_fifo
#endif
#ifdef S_IFBLK
       : (type == S_IFBLK) ? channel_type_block_device
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

  /* Like Unix, all terminals initialize to cooked mode. */
  if (type == channel_type_terminal) CHANNEL_COOKED(channel) = 1;

  return (channel);
}

static Tchannel
DEFUN (open_file, (filename, oflag), CONST char * filename AND int oflag)
{
  int fd;
  STD_UINT_SYSTEM_CALL
    (syscall_open, fd, (DOS_open (filename, oflag, MODE_REG)));
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

#ifdef HAVE_APPEND

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
      int fd = (DOS_open (filename, O_RDONLY, MODE_REG));
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
      int fd = (DOS_open (filename, (O_WRONLY | O_CREAT | O_TRUNC), MODE_REG));
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
    (syscall_fstat, (DOS_fstat ((CHANNEL_DESCRIPTOR (channel)), (&stat_buf))));
  return (stat_buf . st_size);
}

off_t
DEFUN (OS_file_position, (channel), Tchannel channel)
{
  off_t result;
  STD_UINT_SYSTEM_CALL
    (syscall_lseek,
     result,
     (DOS_lseek ((CHANNEL_DESCRIPTOR (channel)), 0L, SEEK_CUR)));
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
     (DOS_lseek ((CHANNEL_DESCRIPTOR (channel)), position, SEEK_SET)));
  if (result != position)
    error_external_return ();
}
