/* -*-C-*-

$Id: uxsock.h,v 1.7 1998/08/31 04:00:26 cph Exp $

Copyright (c) 1990-98 Massachusetts Institute of Technology

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

#include "osio.h"

extern Tchannel EXFUN (OS_open_tcp_stream_socket, (char *, int));
extern int EXFUN (OS_get_service_by_name, (CONST char *, CONST char *));
extern unsigned long EXFUN (OS_get_service_by_number, (CONST unsigned long));
extern unsigned int EXFUN (OS_host_address_length, (void));
extern char ** EXFUN (OS_get_host_by_name, (CONST char *));
CONST char * EXFUN (OS_get_host_name, (void));
CONST char * EXFUN (OS_canonical_host_name, (CONST char *));

#ifdef HAVE_UNIX_SOCKETS
extern Tchannel EXFUN (OS_open_unix_stream_socket, (CONST char *));
#endif

extern Tchannel EXFUN (OS_open_server_socket, (unsigned int, int));
extern Tchannel EXFUN (OS_server_connection_accept, (Tchannel, char *, int *));

#endif /* SCM_UXSOCK_H */
