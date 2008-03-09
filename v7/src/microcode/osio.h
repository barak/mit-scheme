/* -*-C-*-

$Id: osio.h,v 1.25 2008/03/09 20:24:30 cph Exp $

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

#ifndef SCM_OSIO_H
#define SCM_OSIO_H

#include "os.h"

/* Must match definition of `channel_type_names' in "prosio.c".  */
enum channel_type
{
  channel_type_unknown,
  channel_type_file,
  channel_type_unix_pipe,
  channel_type_unix_fifo,
  channel_type_terminal,
  channel_type_unix_pty_master,
  channel_type_unix_stream_socket,
  channel_type_tcp_stream_socket,
  channel_type_tcp_server_socket,
  channel_type_directory,
  channel_type_unix_character_device,
  channel_type_unix_block_device,
  channel_type_os2_console,
  channel_type_os2_unnamed_pipe,
  channel_type_os2_named_pipe,
  channel_type_win32_anonymous_pipe,
  channel_type_win32_named_pipe
};

extern Tchannel OS_channel_table_size;
#define NO_CHANNEL ((Tchannel) -1)
extern int OS_channel_open_p (Tchannel channel);
extern void OS_channel_close (Tchannel channel);
extern void OS_channel_close_noerror (Tchannel channel);
extern void OS_channel_close_on_abort (Tchannel channel);
extern enum channel_type OS_channel_type (Tchannel channel);
extern size_t OS_channel_read_load_file
  (Tchannel channel, void * buffer, size_t nbytes);
extern size_t OS_channel_write_dump_file
  (Tchannel channel, const void * buffer, size_t nbytes);
extern long OS_channel_read
  (Tchannel channel, void * buffer, size_t nbytes);
extern long OS_channel_write
  (Tchannel channel, const void * buffer, size_t nbytes);
extern void OS_channel_write_string
  (Tchannel channel, const char * string);
extern void OS_make_pipe
  (Tchannel * readerp, Tchannel * writerp);
extern int OS_channel_nonblocking_p (Tchannel channel);
extern void OS_channel_nonblocking (Tchannel channel);
extern void OS_channel_blocking (Tchannel channel);

/* Interface to poll(2) or select(2) */

#ifdef __WIN32__
extern int OS_have_select_p;
#else
extern const int OS_have_select_p;
#endif

typedef void * select_registry_t;
#define SELECT_MODE_READ 1
#define SELECT_MODE_WRITE 2
#define SELECT_MODE_ERROR 4
#define SELECT_MODE_HUP 8

#define SELECT_INTERRUPT (-1)
#define SELECT_PROCESS_STATUS_CHANGE (-2)

extern select_registry_t OS_allocate_select_registry
  (void);
extern void OS_deallocate_select_registry
  (select_registry_t registry);
extern void OS_add_to_select_registry
  (select_registry_t registry, int fd, unsigned int mode);
extern void OS_remove_from_select_registry
  (select_registry_t registry, int fd, unsigned int mode);
extern unsigned int OS_select_registry_length
  (select_registry_t registry);
extern void OS_select_registry_result
  (select_registry_t registry, unsigned int index,
    int * fd_r, unsigned int * mode_r);
extern int OS_test_select_registry
  (select_registry_t registry, int blockp);
extern int OS_test_select_descriptor
  (int fd, int blockp, unsigned int mode);

#endif /* SCM_OSIO_H */
