/* -*-C-*-

$Id: osio.h,v 1.16 2002/11/20 19:46:12 cph Exp $

Copyright (c) 1990-2000 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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

extern size_t OS_channel_table_size;
#define NO_CHANNEL OS_channel_table_size
extern int EXFUN (OS_channel_open_p, (Tchannel channel));
extern void EXFUN (OS_channel_close, (Tchannel channel));
extern void EXFUN (OS_channel_close_noerror, (Tchannel channel));
extern void EXFUN (OS_channel_close_on_abort, (Tchannel channel));
extern enum channel_type EXFUN (OS_channel_type, (Tchannel channel));
extern size_t EXFUN
  (OS_channel_read_load_file, (Tchannel channel, PTR buffer, size_t nbytes));
extern size_t EXFUN
  (OS_channel_write_dump_file,
   (Tchannel channel, CONST PTR buffer, size_t nbytes));
extern long EXFUN
  (OS_channel_read, (Tchannel channel, PTR buffer, size_t nbytes));
extern long EXFUN
  (OS_channel_write, (Tchannel channel, CONST PTR buffer, size_t nbytes));
extern void EXFUN
  (OS_channel_write_string, (Tchannel channel, CONST char * string));
extern void EXFUN
  (OS_make_pipe, (Tchannel * readerp, Tchannel * writerp));
extern int EXFUN (OS_channel_nonblocking_p, (Tchannel channel));
extern void EXFUN (OS_channel_nonblocking, (Tchannel channel));
extern void EXFUN (OS_channel_blocking, (Tchannel channel));

#ifdef __WIN32__
extern int OS_have_select_p;
#else
extern CONST int OS_have_select_p;
#endif

#endif /* SCM_OSIO_H */
