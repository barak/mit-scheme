/* -*-C-*-

$Id: uxsock.h,v 1.8 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

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
