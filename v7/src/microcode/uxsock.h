/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxsock.h,v 1.2 1990/11/08 11:11:57 cph Rel $

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

#ifndef SCM_UXSOCK_H
#define SCM_UXSOCK_H

#include "os.h"

extern Tchannel EXFUN (OS_open_tcp_stream_socket, (char * host, int port));
extern int EXFUN
  (OS_get_service_by_name,
   (CONST char * service_name, CONST char * protocol_name));
extern unsigned int EXFUN (OS_host_address_length, (void));
extern char ** EXFUN (OS_get_host_by_name, (CONST char * host_name));

#ifdef HAVE_UNIX_SOCKETS
extern Tchannel EXFUN (OS_open_unix_stream_socket, (CONST char * filename));
#endif

extern Tchannel EXFUN (OS_open_server_socket, (int port));
extern Tchannel EXFUN
  (OS_server_connection_accept,
   (Tchannel channel, char * peer_host, int * peer_port));

#endif /* SCM_UXSOCK_H */
