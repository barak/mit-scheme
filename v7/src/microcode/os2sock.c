/* -*-C-*-

$Id: os2sock.c,v 1.5 1997/11/01 07:26:23 cph Exp $

Copyright (c) 1990-97 Massachusetts Institute of Technology

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

/* This conditional encompasses the entire file.  */
#ifndef DISABLE_SOCKET_SUPPORT

#include "os2.h"
#include "uxsock.h"

#include <types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/un.h>

static Tchannel initialize_stream_socket (int, enum channel_type);
static msg_t * stream_socket_reader (LHANDLE, qid_t, msg_t *, int *);
static void stream_socket_operator
  (Tchannel, chop_t, choparg_t, choparg_t, choparg_t);

#define VOID_SOCKET_CALL(proc, args)					\
{									\
  while ((proc args) < 0)						\
    {									\
      if ((sock_errno ()) != SOCEINTR)					\
	OS2_error_system_call ((sock_errno ()), syscall_ ## proc);	\
      deliver_pending_interrupts ();					\
    }									\
}

#define VALUE_SOCKET_CALL(proc, args, var)				\
{									\
  while (1)								\
    {									\
      int rc = (proc args);						\
      if (rc >= 0)							\
	{								\
	  var = rc;							\
	  break;							\
	}								\
      if ((sock_errno ()) != SOCEINTR)					\
	OS2_error_system_call ((sock_errno ()), syscall_ ## proc);	\
      deliver_pending_interrupts ();					\
    }									\
}

static void
socket_close_on_abort_1 (void * sp)
{
  (void) soclose (* ((int *) sp));
}

static void
socket_close_on_abort (int s)
{
  int * sp = (dstack_alloc (sizeof (int)));
  (*sp) = s;
  transaction_record_action (tat_abort, socket_close_on_abort_1, sp);
}

Tchannel
OS_open_tcp_stream_socket (char * host, int port)
{
  int s;
  struct sockaddr_in address;

  transaction_begin ();
  VALUE_SOCKET_CALL (socket, (PF_INET, SOCK_STREAM, 0), s);
  socket_close_on_abort (s);
  memset ((&address), 0, (sizeof (address)));
  (address . sin_family) = AF_INET;
  memcpy ((& (address . sin_addr)), host, (sizeof (struct in_addr)));
  (address . sin_port) = port;
  VOID_SOCKET_CALL
    (connect, (s,
	       ((struct sockaddr *) (&address)),
	       (sizeof (address))));
  return (initialize_stream_socket (s, channel_type_tcp_stream_socket));
}

Tchannel
OS_open_unix_stream_socket (const char * filename)
{
  int s;
  struct sockaddr_un address;

  transaction_begin ();
  VALUE_SOCKET_CALL (socket, (PF_OS2, SOCK_STREAM, 0), s);
  socket_close_on_abort (s);
  memset ((&address), 0, (sizeof (address)));
  (address . sun_family) = AF_OS2;
  strncpy ((address . sun_path), filename, (sizeof (address . sun_path)));
  VOID_SOCKET_CALL
    (connect, (s,
	       ((struct sockaddr *) (&address)),
	       (sizeof (address))));
  return (initialize_stream_socket (s, channel_type_unix_stream_socket));
}

int
OS_get_service_by_name (const char * service_name, const char * protocol_name)
{
  struct servent * entry
    = (getservbyname (((char *) service_name),
		      ((char *) protocol_name)));
  return ((entry == 0) ? (-1) : (entry -> s_port));
}

unsigned int
OS_host_address_length (void)
{
  return (sizeof (struct in_addr));
}

char **
OS_get_host_by_name (const char * host_name)
{
  struct hostent * entry = (gethostbyname ((char *) host_name));
  return ((entry == 0) ? 0 : (entry -> h_addr_list));
}

#define HOSTNAMESIZE 1024

const char *
OS_get_host_name (void)
{
  char host_name [HOSTNAMESIZE];
  VOID_SOCKET_CALL (gethostname, (host_name, HOSTNAMESIZE));
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

#ifndef SOCKET_LISTEN_BACKLOG
#define SOCKET_LISTEN_BACKLOG 5
#endif

Tchannel
OS_open_server_socket (unsigned int port, int arg_number)
{
  int s;
  struct sockaddr_in address;
  {
    unsigned int nb_port = (sizeof (((struct sockaddr_in *) 0) -> sin_port));
    if (((sizeof (unsigned int)) > nb_port)
	&& (port >= (1 << (CHAR_BIT * nb_port))))
      error_bad_range_arg (arg_number);
  }
  transaction_begin ();
  VALUE_SOCKET_CALL (socket, (PF_INET, SOCK_STREAM, 0), s);
  socket_close_on_abort (s);
  memset ((&address), 0, (sizeof (address)));
  (address . sin_family) = AF_INET;
  (address . sin_addr . s_addr) = INADDR_ANY;
  (address . sin_port) = port;
  VOID_SOCKET_CALL
    (bind, (s,
	    ((struct sockaddr *) (& address)),
	    (sizeof (struct sockaddr_in))));
  VOID_SOCKET_CALL (listen, (s, SOCKET_LISTEN_BACKLOG));
  return (initialize_stream_socket (s, channel_type_tcp_server_socket));
}

Tchannel
OS_server_connection_accept (Tchannel channel,
			     char * peer_host, int * peer_port)
{
  static struct sockaddr_in address;
  int s;

  transaction_begin ();
  while (1)
    {
      int address_length = (sizeof (struct sockaddr_in));
      s = (accept (((int) (CHANNEL_HANDLE (channel))),
		   ((struct sockaddr *) (&address)),
		   (&address_length)));
      if (s >= 0)
	break;
      if ((sock_errno ()) == SOCEWOULDBLOCK)
	return (NO_CHANNEL);
      if ((sock_errno ()) != SOCEINTR)
	OS2_error_system_call ((sock_errno ()), syscall_accept);
      deliver_pending_interrupts ();
    }
  socket_close_on_abort (s);
  if (peer_host != 0)
    memcpy (peer_host, (& (address . sin_addr)), (sizeof (struct in_addr)));
  if (peer_port != 0)
    (*peer_port) = (address . sin_port);
  return (initialize_stream_socket (s, channel_type_tcp_stream_socket));
}

static Tchannel
initialize_stream_socket (int s, enum channel_type type)
{
  Tchannel channel = (OS2_allocate_channel ());
  OS2_initialize_channel (channel, s, (CHANNEL_READ | CHANNEL_WRITE), type);
  OS2_start_channel_thread (channel,
			    stream_socket_reader,
			    stream_socket_operator);
  transaction_commit ();
  return (channel);
}

static msg_t *
stream_socket_reader (LHANDLE handle, qid_t qid, msg_t * message, int * eofp)
{
  int nread;
  do
    nread = (recv (((int) handle),
		   (SM_READAHEAD_DATA (message)),
		   (sizeof (SM_READAHEAD_DATA (message))),
		   0));
  while ((nread < 0) && ((sock_errno ()) == SOCEINTR));
  if (nread >= 0)
    {
      (SM_READAHEAD_SIZE (message)) = nread;
      (*eofp) = (nread == 0);
      return (message);
    }
  OS2_destroy_message (message);
  if ((sock_errno ()) == SOCENOTSOCK)
    /* Socket was closed on us -- no need to do anything else.  */
    return (0);
  (*eofp) = 0;
  return (OS2_make_syscall_error ((sock_errno ()), syscall_recv));
}

static void
stream_socket_operator (Tchannel channel, chop_t operation,
			choparg_t arg1, choparg_t arg2, choparg_t arg3)
{
  switch (operation)
    {
    case chop_read:
      OS2_channel_thread_read_op (channel, arg1, arg2, arg3);
      break;
    case chop_write:
      VALUE_SOCKET_CALL
	(send, (((int) (CHANNEL_HANDLE (channel))),
		((char *) arg1),
		((size_t) arg2),
		0),
	 (* ((long *) arg3)));
      break;
    case chop_close:
      OS2_channel_thread_close (channel);
      VOID_SOCKET_CALL (soclose, ((int) (CHANNEL_HANDLE (channel))));
      break;
    default:
      OS2_logic_error ("Unknown operation for stream socket.");
      break;
    }
}

#endif /* not DISABLE_SOCKET_SUPPORT */
