/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

#ifndef SCM_UXSOCK_H
#define SCM_UXSOCK_H

#include "osio.h"

extern Tchannel OS_open_tcp_stream_socket (void *, unsigned int);
extern void OS_shutdown_socket (Tchannel, unsigned long);
extern int OS_get_service_by_name (const char *, const char *);
extern unsigned long OS_get_service_by_number (const unsigned long);
extern unsigned int OS_host_address_length (void);
extern char ** OS_get_host_by_name (const char *);
extern const char * OS_get_host_name (void);
extern const char * OS_canonical_host_name (const char *);
extern const char * OS_get_host_by_address (const char *);
extern void OS_host_address_any (void *);
extern void OS_host_address_loopback (void *);

#ifdef HAVE_UNIX_SOCKETS
  extern Tchannel OS_open_unix_stream_socket (const char *);
  extern Tchannel OS_create_unix_server_socket (const char *);
  extern Tchannel OS_unix_server_connection_accept (Tchannel);
#endif

extern Tchannel OS_create_tcp_server_socket (void);
extern void OS_bind_tcp_server_socket (Tchannel, void *, unsigned int);
extern void OS_listen_tcp_server_socket (Tchannel);
extern Tchannel OS_tcp_server_connection_accept (Tchannel, void *, unsigned int *);

#endif /* SCM_UXSOCK_H */
