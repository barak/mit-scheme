/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/osio.h,v 1.2 1990/06/21 20:01:42 cph Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

#ifndef SCM_OSIO_H
#define SCM_OSIO_H

#include "os.h"

enum channel_type
{
  channel_type_unknown,
  channel_type_file,
  channel_type_pipe,
  channel_type_fifo,
  channel_type_terminal,
  channel_type_pty_master,
  channel_type_unix_stream_socket,
  channel_type_tcp_stream_socket
};

extern size_t OS_channel_table_size;
#define NO_CHANNEL OS_channel_table_size
extern int EXFUN (OS_channel_open_p, (Tchannel channel));
extern void EXFUN (OS_channel_close, (Tchannel channel));
extern void EXFUN (OS_channel_close_noerror, (Tchannel channel));
extern void EXFUN (OS_channel_close_all, (void));
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
extern int EXFUN (OS_channel_nonblocking_p, (Tchannel channel));
extern void EXFUN (OS_channel_nonblocking, (Tchannel channel));
extern void EXFUN (OS_channel_blocking, (Tchannel channel));

#endif /* SCM_OSIO_H */
