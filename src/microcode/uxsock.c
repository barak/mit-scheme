/* -*-C-*-

$Id: uxsock.c,v 1.36 2007/04/22 16:31:23 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

#include "ux.h"
#include "osio.h"

#ifdef HAVE_SOCKETS

#include "uxsock.h"
#include "uxio.h"
#include "prims.h"

static void do_connect (int, struct sockaddr *, socklen_t);

Tchannel
OS_open_tcp_stream_socket (void * host, unsigned int port)
{
  int s;
  Tchannel channel;

  transaction_begin ();
  STD_UINT_SYSTEM_CALL
    (syscall_socket, s, (UX_socket (AF_INET, SOCK_STREAM, 0)));
  MAKE_CHANNEL (s, channel_type_tcp_stream_socket, channel =);
  OS_channel_close_on_abort (channel);
  {
    struct sockaddr_in address;
    (address . sin_family) = AF_INET;
    memcpy ((& (address . sin_addr)), host, (sizeof (address . sin_addr)));
    (address . sin_port) = port;
    do_connect (s, ((struct sockaddr *) (&address)), (sizeof (address)));
  }
  transaction_commit ();
  return (channel);
}

static void
do_connect (int s, struct sockaddr * address, socklen_t addr_len)
{
  if ((UX_connect (s, address, addr_len)) < 0)
    {
      if (errno != EINTR)
	error_system_call (errno, syscall_connect);
      while (1)
	{
	  deliver_pending_interrupts ();
	  /* Yuk; lots of hair because connect can't be restarted.
	     Instead, we must wait for the connection to finish, then
	     examine the SO_ERROR socket option.  */
#ifdef HAVE_POLL
	  {
	    struct pollfd fds;
	    int nfds;

	    (fds . fd) = s;
	    (fds . events) = (POLLIN | POLLOUT);
	    nfds = (poll ((&fds), 1, 0));
	    if ((nfds > 0) && (((fds . revents) & (POLLIN | POLLOUT)) != 0))
	      break;
	    if ((nfds < 0) && (errno != EINTR))
	      error_system_call (errno, syscall_select);
	  }
#else /* not HAVE_POLL */
#ifdef HAVE_SELECT
	  {
	    fd_set readers;
	    fd_set writers;
	    int result;

	    FD_ZERO (&readers);
	    FD_SET (s, (&readers));
	    writers = readers;
	    result = (UX_select ((s + 1), (&readers), (&writers), 0, 0));
	    if ((result > 0)
		&& ((FD_ISSET (s, (&readers))) || (FD_ISSET (s, (&writers)))))
	      break;
	    if ((result < 0) && (errno != EINTR))
	      error_system_call (errno, syscall_select);
	  }
#else /* not HAVE_SELECT */
	  error_system_call (errno, syscall_connect);
	  break;
#endif /* not HAVE_SELECT */
#endif /* not HAVE_POLL */
	}
      {
	int error;
	socklen_t len = (sizeof (error));
	if (((getsockopt (s, SOL_SOCKET, SO_ERROR, (&error), (&len))) < 0)
	    || (error != 0))
	  error_system_call (error, syscall_connect);
      }
    }
}

void
OS_shutdown_socket (Tchannel channel, unsigned long stype)
{
  STD_VOID_SYSTEM_CALL
    (syscall_shutdown,
     (shutdown ((CHANNEL_DESCRIPTOR (channel)),
		(((stype & 0x3) == 0x1)
		 ? SHUT_RD
		 : ((stype & 0x3) == 0x2)
		 ? SHUT_WR
		 : SHUT_RDWR))));
}

int
OS_get_service_by_name (const char * service_name, const char * protocol_name)
{
  struct servent * entry = (UX_getservbyname (service_name, protocol_name));
  return ((entry == 0) ? (-1) : (entry -> s_port));
}

unsigned long
OS_get_service_by_number (const unsigned long port_number)
{
  return ((unsigned long) (htons ((unsigned short) port_number)));
}

unsigned int
OS_host_address_length (void)
{
  return (sizeof (struct in_addr));
}

char **
OS_get_host_by_name (const char * host_name)
{
  struct hostent * entry = (UX_gethostbyname (host_name));
  if (entry == 0)
    return (0);
#ifdef HAVE_HOSTENT_H_ADDR_LIST
  return (entry -> h_addr_list);
#else
  {
    static char * addresses [2];
    (addresses[0]) = (entry -> h_addr);
    (addresses[1]) = 0;
    return (addresses);
  }
#endif
}

#define HOSTNAMESIZE 1024

const char *
OS_get_host_name (void)
{
  char host_name [HOSTNAMESIZE];
  STD_VOID_SYSTEM_CALL
    (syscall_gethostname, (UX_gethostname (host_name, HOSTNAMESIZE)));
  {
    char * result = (OS_malloc ((strlen (host_name)) + 1));
    strcpy (result, host_name);
    return (result);
  }
}

const char *
OS_canonical_host_name (const char * host_name)
{
  struct hostent * entry = (gethostbyname (host_name));
  if (entry == 0)
    return (0);
  {
    char * result = (OS_malloc ((strlen (entry -> h_name)) + 1));
    strcpy (result, (entry -> h_name));
    return (result);
  }
}

const char *
OS_get_host_by_address (const char * host_addr)
{
  struct hostent * entry
    = (gethostbyaddr (host_addr, (OS_host_address_length ()), AF_INET));
  if (entry == 0)
    return (0);
  {
    char * result = (OS_malloc ((strlen (entry -> h_name)) + 1));
    strcpy (result, (entry -> h_name));
    return (result);
  }
}

void
OS_host_address_any (void * addr)
{
  (((struct in_addr *) addr) -> s_addr) = (htonl (INADDR_ANY));
}

void
OS_host_address_loopback (void * addr)
{
  (((struct in_addr *) addr) -> s_addr) = (htonl (INADDR_LOOPBACK));
}

#ifdef HAVE_UNIX_SOCKETS
Tchannel
OS_open_unix_stream_socket (const char * filename)
{
  int s;
  Tchannel channel;

  transaction_begin ();
  STD_UINT_SYSTEM_CALL
    (syscall_socket, s, (UX_socket (AF_UNIX, SOCK_STREAM, 0)));
  MAKE_CHANNEL (s, channel_type_unix_stream_socket, channel =);
  OS_channel_close_on_abort (channel);
  {
    struct sockaddr_un address;
    (address . sun_family) = AF_UNIX;
    strncpy ((address . sun_path), filename, (sizeof (address . sun_path)));
    do_connect (s, ((struct sockaddr *) (&address)), (sizeof (address)));
  }
  transaction_commit ();
  return (channel);
}
#endif /* HAVE_UNIX_SOCKETS */

Tchannel
OS_create_tcp_server_socket (void)
{
  int s;
  STD_UINT_SYSTEM_CALL
    (syscall_socket, s, (UX_socket (AF_INET, SOCK_STREAM, 0)));
  MAKE_CHANNEL (s, channel_type_tcp_server_socket, return);
}

void
OS_bind_tcp_server_socket (Tchannel channel, void * host, unsigned int port)
{
  struct sockaddr_in address;
  int one = 1;

  (address . sin_family) = AF_INET;
  memcpy ((& (address . sin_addr)), host, (sizeof (address . sin_addr)));
  (address . sin_port) = port;

  STD_VOID_SYSTEM_CALL
    (syscall_setsockopt,
     (setsockopt ((CHANNEL_DESCRIPTOR (channel)),
		  SOL_SOCKET,
		  SO_REUSEADDR,
		  (&one),
		  (sizeof (one)))));
  STD_VOID_SYSTEM_CALL
    (syscall_bind,
     (UX_bind ((CHANNEL_DESCRIPTOR (channel)),
	       ((struct sockaddr *) (&address)),
	       (sizeof (struct sockaddr_in)))));
}

#ifndef SOCKET_LISTEN_BACKLOG
#define SOCKET_LISTEN_BACKLOG 1024
#endif

void
OS_listen_tcp_server_socket (Tchannel channel)
{
  STD_VOID_SYSTEM_CALL
    (syscall_listen,
     (UX_listen ((CHANNEL_DESCRIPTOR (channel)), SOCKET_LISTEN_BACKLOG)));
}

Tchannel
OS_server_connection_accept (Tchannel channel,
			     void * peer_host,
			     unsigned int * peer_port)
{
  static struct sockaddr_in address;
  socklen_t address_length = (sizeof (struct sockaddr_in));
  int s;
  while (1)
    {
      s = (UX_accept ((CHANNEL_DESCRIPTOR (channel)),
		      ((struct sockaddr *) (&address)),
		      (&address_length)));
      if (s >= 0)
	break;
      if (errno != EINTR)
	{
#ifdef EAGAIN
	  if (errno == EAGAIN)
	    return (NO_CHANNEL);
#endif
#ifdef EWOULDBLOCK
	  if (errno == EWOULDBLOCK)
	    return (NO_CHANNEL);
#endif
	  error_system_call (errno, syscall_accept);
	}
      deliver_pending_interrupts ();
    }
  if (peer_host != 0)
    memcpy (peer_host,
	    (& (address . sin_addr)),
	    (sizeof (address . sin_addr)));
  if (peer_port != 0)
    (*peer_port) = (address . sin_port);
  MAKE_CHANNEL (s, channel_type_tcp_stream_socket, return);
}

#endif /* not HAVE_SOCKETS */
