/* -*-C-*-

$Id: uxsock.h,v 1.13 2006/11/22 18:50:46 cph Exp $

Copyright 1990,1992,1993,1997,1998,1999 Massachusetts Institute of Technology
Copyright 2001,2006 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#ifndef SCM_UXSOCK_H
#define SCM_UXSOCK_H

#include "osio.h"

extern Tchannel EXFUN (OS_open_tcp_stream_socket, (PTR, unsigned int));
extern void EXFUN (OS_shutdown_socket, (Tchannel, unsigned long));
extern int EXFUN (OS_get_service_by_name, (CONST char *, CONST char *));
extern unsigned long EXFUN (OS_get_service_by_number, (CONST unsigned long));
extern unsigned int EXFUN (OS_host_address_length, (void));
extern char ** EXFUN (OS_get_host_by_name, (CONST char *));
extern CONST char * EXFUN (OS_get_host_name, (void));
extern CONST char * EXFUN (OS_canonical_host_name, (CONST char *));
extern CONST char * EXFUN (OS_get_host_by_address, (CONST char *));
extern void EXFUN (OS_host_address_any, (PTR));
extern void EXFUN (OS_host_address_loopback, (PTR));

#ifdef HAVE_UNIX_SOCKETS
extern Tchannel EXFUN (OS_open_unix_stream_socket, (CONST char *));
#endif

extern Tchannel EXFUN (OS_create_tcp_server_socket, (void));
extern void EXFUN (OS_bind_tcp_server_socket, (Tchannel, PTR, unsigned int));
extern void EXFUN (OS_listen_tcp_server_socket, (Tchannel));
extern Tchannel EXFUN
 (OS_server_connection_accept, (Tchannel, PTR, unsigned int *));

#endif /* SCM_UXSOCK_H */
