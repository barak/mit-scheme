/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosio.c,v 1.1 1992/05/05 06:55:13 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

#include "msdos.h"
#include "dosio.h"
#include "osterm.h"

#ifdef __STDC__
#define fileno(fp)	((fp)->_file)
#endif

size_t OS_channel_table_size;
struct channel * channel_table;

unsigned int OS_channels_registered;

static void
DEFUN_VOID (DOS_channel_close_all)
{
  Tchannel channel;
  for (channel = 0; (channel < OS_channel_table_size); channel += 1)
    if (CHANNEL_OPEN_P (channel))
      OS_channel_close_noerror (channel);
}

void
DEFUN_VOID (DOS_initialize_channels)
{
  OS_channel_table_size = (DOS_SC_OPEN_MAX ());
  channel_table =
    (DOS_malloc (OS_channel_table_size * (sizeof (struct channel))));
  if (channel_table == 0)
    {
      fprintf (stderr, "\nUnable to allocate channel table.\n");
      fflush (stderr);
      termination_init_error ();
    }
  {
    Tchannel channel;
    for (channel = 0; (channel < OS_channel_table_size); channel += 1)
      MARK_CHANNEL_CLOSED (channel);
  }
  add_reload_cleanup (DOS_channel_close_all);
  OS_channels_registered = 0;
}

void
DEFUN_VOID (DOS_reset_channels)
{
  DOS_free (channel_table);
  channel_table = 0;
  OS_channel_table_size = 0;
}

Tchannel
DEFUN_VOID (channel_allocate)
{
  Tchannel channel = 0;
  while (1)
    {
      if (channel == OS_channel_table_size)
	error_out_of_channels ();
      if (CHANNEL_CLOSED_P (channel))
	return (channel);
      channel += 1;
    }
}

int
DEFUN (OS_channel_open_p, (channel), Tchannel channel)
{
  return (CHANNEL_OPEN_P (channel));
}

void
DEFUN (OS_channel_close, (channel), Tchannel channel)
{
  if (! (CHANNEL_INTERNAL (channel)))
    {
      if (CHANNEL_REGISTERED (channel))
	OS_channel_unregister (channel);
      STD_VOID_SYSTEM_CALL
	(syscall_close, (DOS_close (CHANNEL_DESCRIPTOR (channel))));
      MARK_CHANNEL_CLOSED (channel);
    }
}

void
DEFUN (OS_channel_close_noerror, (channel), Tchannel channel)
{
  if (! (CHANNEL_INTERNAL (channel)))
    {
      if (CHANNEL_REGISTERED (channel))
	OS_channel_unregister (channel);
      DOS_close (CHANNEL_DESCRIPTOR (channel));
      MARK_CHANNEL_CLOSED (channel);
    }
}

static void
DEFUN (channel_close_on_abort_1, (cp), PTR cp)
{
  OS_channel_close (* ((Tchannel *) cp));
}

void
DEFUN (OS_channel_close_on_abort, (channel), Tchannel channel)
{
  Tchannel * cp = (dstack_alloc (sizeof (Tchannel)));
  (*cp) = (channel);
  transaction_record_action (tat_abort, channel_close_on_abort_1, cp);
}

enum channel_type
DEFUN (OS_channel_type, (channel), Tchannel channel)
{
  return (CHANNEL_TYPE (channel));
}

void
DEFUN (OS_terminal_flush_input, (channel), Tchannel channel)
{ extern void EXFUN (flush_conio_buffers, (void));

  if ((CHANNEL_DESCRIPTOR (channel)) == (fileno (stdin)))
    flush_conio_buffers();
  return;
}

void
DEFUN (OS_terminal_flush_output, (channel), Tchannel channel)
{
  return;
}

void
DEFUN (OS_terminal_drain_output, (channel), Tchannel channel)
{
  return;
}

DEFUN (dos_channel_read, (channel, buffer, nbytes),
       Tchannel channel AND PTR buffer AND size_t nbytes)
{
  if (nbytes == 0)
    return 0;
  else if (CHANNEL_DESCRIPTOR (channel) == fileno(stdin))
    return console_read(buffer, nbytes, 
			CHANNEL_BUFFERED(channel), CHANNEL_BLOCKING_P(channel));
  else
    return DOS_read ((CHANNEL_DESCRIPTOR (channel)), buffer, nbytes);
}

long
DEFUN (OS_channel_read, (channel, buffer, nbytes),
       Tchannel channel AND PTR buffer AND size_t nbytes)
{
  while (1)
  {
    long scr = dos_channel_read(channel, buffer, nbytes);    
    if (scr < 0)
    {
      if (errno == ERRNO_NONBLOCK)
	return -1;
      DOS_prim_check_errno (syscall_read);
      continue;
    }
    else if (scr > nbytes)
      error_external_return ();
    else
      return (scr);
  }
}

static int
DEFUN (dos_write, (fd, buffer, nbytes),
       int fd AND CONST unsigned char * buffer AND size_t nbytes)
{
  return ( (fd == fileno(stdout))
	   ? dos_console_write(buffer, nbytes)
	   : write(fd, buffer, nbytes) );
}

#define Syscall_Write(fd, buffer, size, so_far)		\
do							\
{ size_t _size = (size);				\
  int _written;						\
  _written = dos_write((fd), (buffer), (_size));	\
  if (_size != _written)				\
    return ((_written < 0) ? -1 : (so_far) + _written); \
} while (0)

long
DEFUN (text_write, (fd, buffer, nbytes),
       int fd AND CONST unsigned char * buffer AND size_t nbytes)
{ /* Map LF to CR/LF */
  static CONST unsigned char crlf[] = {CARRIAGE_RETURN, LINEFEED};
  CONST unsigned char *start;
  size_t i;

  for (i=0, start=buffer; i < nbytes; start = &buffer[i])
  { size_t len;

    while ((i < nbytes)&&(buffer[i] != LINEFEED)) i++;
    len = (&buffer[i] - start);

    Syscall_Write(fd, start, len, (i - len));

    if ((i < nbytes)&&(buffer[i] == LINEFEED))
    { /* We are sitting on a linefeed. Write out CRLF */
      /* This backs out incorrectly if only CR is written out */
      Syscall_Write(fd, crlf, sizeof(crlf), i);
      i = i + 1; /* Skip over special character */
    }
  }

  return nbytes;
}

#undef Syscall_Write

long
DEFUN (OS_channel_write, (channel, buffer, nbytes),
       Tchannel channel AND CONST PTR buffer AND size_t nbytes)
{
  if (nbytes == 0) return (0);

  while (1)
  { int fd, scr;

    fd = CHANNEL_DESCRIPTOR(channel);
    scr = ((CHANNEL_COOKED(channel))
	   ? text_write(fd, buffer, nbytes)
	   : dos_write(fd, buffer, nbytes));
	      
    if (scr < 0)
    {
      DOS_prim_check_errno (syscall_write);
      continue;
    }

    if (scr > nbytes)
      error_external_return ();
    return scr;
  }
}


size_t
DEFUN (OS_channel_read_load_file, (channel, buffer, nbytes),
       Tchannel channel AND PTR buffer AND size_t nbytes)
{
  int scr;
  scr = (DOS_read ((CHANNEL_DESCRIPTOR (channel)), buffer, nbytes));
  return ((scr < 0) ? 0 : scr);
}

size_t
DEFUN (OS_channel_write_dump_file, (channel, buffer, nbytes),
       Tchannel channel AND CONST PTR buffer AND size_t nbytes)
{
  int scr = (DOS_write ((CHANNEL_DESCRIPTOR (channel)), buffer, nbytes));
  return ((scr < 0) ? 0 : scr);
}

void
DEFUN (OS_channel_write_string, (channel, string),
       Tchannel channel AND
       CONST char * string)
{
  unsigned long length = (strlen (string));
  if ((OS_channel_write (channel, string, length)) != length)
    error_external_return ();
}

void
DEFUN (OS_make_pipe, (readerp, writerp),
       Tchannel * readerp AND
       Tchannel * writerp)
{
  return;
}

int
DEFUN (OS_channel_nonblocking_p, (channel), Tchannel channel)
{
  return (CHANNEL_NONBLOCKING (channel));
}

void
DEFUN (OS_channel_nonblocking, (channel), Tchannel channel)
{
  (CHANNEL_NONBLOCKING (channel)) = 1;
  return;
}

void
DEFUN (OS_channel_blocking, (channel), Tchannel channel)
{
  (CHANNEL_NONBLOCKING (channel)) = 0;
}

int
DEFUN (OS_terminal_buffered_p, (channel), Tchannel channel)
{
  return (CHANNEL_BUFFERED(channel));
}

void
DEFUN (OS_terminal_buffered, (channel), Tchannel channel)
{
  CHANNEL_BUFFERED(channel) = 1;
}

void
DEFUN (OS_terminal_nonbuffered, (channel), Tchannel channel)
{
  CHANNEL_BUFFERED(channel) = 0;
}

int
DEFUN (OS_terminal_cooked_output_p, (channel), Tchannel channel)
{
  return (CHANNEL_COOKED(channel));
}

void
DEFUN (OS_terminal_cooked_output, (channel), Tchannel channel)
{
  CHANNEL_COOKED(channel) = 1;
}

void
DEFUN (OS_terminal_raw_output, (channel), Tchannel channel)
{
  CHANNEL_COOKED(channel) = 0;
}

unsigned int
DEFUN (arg_baud_index, (argument), unsigned int argument)
{
  return (arg_index_integer (argument, 1));
}

unsigned long
DEFUN (OS_terminal_get_ispeed, (channel), Tchannel channel)
{
  return (0);
}

unsigned long
DEFUN (OS_terminal_get_ospeed, (channel), Tchannel channel)
{
  return (0);
}

unsigned int
DEFUN (OS_baud_index_to_rate, (index), unsigned int index)
{
  return (9600);
}

int
DEFUN (OS_baud_rate_to_index, (rate), unsigned int rate)
{
  return ((rate == 9600) ? 0 : -1);
}

unsigned int
DEFUN_VOID (OS_terminal_state_size)
{
  return (3);
}

void
DEFUN (OS_terminal_get_state, (channel, state_ptr),
       Tchannel channel AND PTR state_ptr)
{
  unsigned char *statep = (unsigned char *) state_ptr;

  *statep++ = CHANNEL_NONBLOCKING(channel);
  *statep++ = CHANNEL_BUFFERED(channel);
  *statep   = CHANNEL_COOKED(channel);
  
  return;
}

void
DEFUN (OS_terminal_set_state, (channel, state_ptr),
       Tchannel channel AND PTR state_ptr)
{
  unsigned char *statep = (unsigned char *) state_ptr;

  CHANNEL_NONBLOCKING(channel) = *statep++;
  CHANNEL_BUFFERED(channel)    = *statep++;
  CHANNEL_COOKED(channel)      = *statep;
  
  return;
}

#ifndef FALSE
#  define FALSE 0
#endif

int
DEFUN_VOID (OS_job_control_p)
{
  return (FALSE);
}

int
DEFUN_VOID (OS_have_ptys_p)
{
  return (FALSE);
}

int
DEFUN (OS_channel_registered_p, (channel), Tchannel channel)
{
  return (CHANNEL_REGISTERED (channel));
}

void
DEFUN (OS_channel_register, (channel), Tchannel channel)
{
  error_unimplemented_primitive ();
}

void
DEFUN (OS_channel_unregister, (channel), Tchannel channel)
{
  if (CHANNEL_REGISTERED (channel))
    {
      OS_channels_registered -= 1;
      (CHANNEL_REGISTERED (channel)) = 0;
    }
}


/* No SELECT in DOS */
long
DEFUN (OS_channel_select_then_read, (channel, buffer, nbytes),
       Tchannel channel AND
       PTR buffer AND
       size_t nbytes)
{ /* We can't really select amonst channels in DOS, but still need
     to keep track of whether the read was interrupted. */
  while (1)
  {
    long scr = dos_channel_read(channel, buffer, nbytes);

    if (scr < 0)
    {
      if (errno == ERRNO_NONBLOCK)
	return -1;
      else if (errno == EINTR)
	return -4;
      else
      { DOS_prim_check_errno (syscall_read);
	continue;
      }
    }
    else if (scr > nbytes)
      error_external_return ();
    else
      return (scr);
  }
}
