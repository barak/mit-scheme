/* -*-C-*-

$Id: ntio.c,v 1.3 1993/06/24 02:03:59 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "nt.h"
#include "ntio.h"
#include "osterm.h"
#include "prims.h"
#include "outf.h"
#include "ossig.h"
#include "intrpt.h"

#include "ntscreen.h"

#ifndef fileno
#define fileno(fp)	((fp)->_file)
#endif

size_t OS_channel_table_size;
struct channel * channel_table;

unsigned int OS_channels_registered;

HANDLE  STDIN_HANDLE,  STDOUT_HANDLE,  STDERR_HANDLE;


static void
DEFUN_VOID (DOS_channel_close_all)
{
  Tchannel channel;
  for (channel = 0; (channel < OS_channel_table_size); channel += 1)
    if (CHANNEL_OPEN_P (channel))
      OS_channel_close_noerror (channel);
  return;
}

static BOOL _stdcall
NT_ctrl_handler(DWORD dwCtrlType)
{
    switch (dwCtrlType) {
      case CTRL_C_EVENT:
	REQUEST_INTERRUPT (INT_Character);
	return  TRUE;
      default:
	return  FALSE;
    }
}


extern  HANDLE master_tty_window;
  

void
DEFUN_VOID (NT_initialize_channels)
{
  STDIN_HANDLE  = GetStdHandle (STD_INPUT_HANDLE);
  STDOUT_HANDLE = GetStdHandle (STD_OUTPUT_HANDLE);
  STDERR_HANDLE = GetStdHandle (STD_ERROR_HANDLE);

  if (STDIN_HANDLE == INVALID_HANDLE_VALUE  ||
      STDOUT_HANDLE == INVALID_HANDLE_VALUE  ||
      STDERR_HANDLE == INVALID_HANDLE_VALUE) {
    outf_fatal ("\nUnable to get standard handles %s(%d).\n",
                __FILE__, __LINE__);
    termination_init_error ();
  }

  SetConsoleMode (STDIN_HANDLE,
        ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
  SetConsoleCtrlHandler (NT_ctrl_handler, TRUE);


  master_tty_window = Screen_Create (NULL, "MIT Scheme", SW_SHOWNORMAL);


  OS_channel_table_size = (DOS_SC_OPEN_MAX ());
  channel_table =
    (DOS_malloc (OS_channel_table_size * (sizeof (struct channel))));
  if (channel_table == 0)
    {
      outf_fatal ("\nUnable to allocate channel table.\n");
      termination_init_error ();
    }
  {
    Tchannel channel;
    for (channel = 0; (channel < OS_channel_table_size); channel += 1)
      MARK_CHANNEL_CLOSED (channel);
  }
  add_reload_cleanup (DOS_channel_close_all);
  OS_channels_registered = 0;
  return;
}

void
DEFUN_VOID (DOS_reset_channels)
{
  DOS_free (channel_table);
  channel_table = 0;
  OS_channel_table_size = 0;
  return;
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
    STD_BOOL_SYSTEM_CALL
      (syscall_close, (CloseHandle (CHANNEL_HANDLE (channel))));
    MARK_CHANNEL_CLOSED (channel);
  }
  return;
}

void
DEFUN (OS_channel_close_noerror, (channel), Tchannel channel)
{
  if (! (CHANNEL_INTERNAL (channel)))
  {
    if (CHANNEL_REGISTERED (channel))
      OS_channel_unregister (channel);
    if (! Screen_IsScreenHandle (CHANNEL_HANDLE (channel)))
      CloseHandle (CHANNEL_HANDLE (channel));
    MARK_CHANNEL_CLOSED (channel);
  }
  return;
}

static void
DEFUN (channel_close_on_abort_1, (cp), PTR cp)
{
  OS_channel_close (* ((Tchannel *) cp));
  return;
}

void
DEFUN (OS_channel_close_on_abort, (channel), Tchannel channel)
{
  Tchannel * cp = (dstack_alloc (sizeof (Tchannel)));
  (*cp) = (channel);
  transaction_record_action (tat_abort, channel_close_on_abort_1, cp);
  return;
}

enum channel_type
DEFUN (OS_channel_type, (channel), Tchannel channel)
{
  return (CHANNEL_TYPE (channel));
}

void
DEFUN (OS_terminal_flush_input, (channel), Tchannel channel)
{ extern void EXFUN (flush_conio_buffers, (void));

//  if (IsWindow (CHANNEL_HANDLE (channel)))  /*SRA:dubious*/
//    flush_conio_buffers();
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

//extern int EXFUN (dos_read, (int, PTR, size_t, int, int, int));
//
//int
//DEFUN (dos_read, (fd, buffer, nbytes, buffered_p, blocking_p, intrpt_p),
//       HANDLE  fd AND PTR buffer AND size_t nbytes
//       AND int buffered_p AND int blocking_p AND int intrpt_p)
//{
//  if (nbytes == 0)
//    return (0);
//  else if (fd == (fileno (stdin)))
//    return (console_read (buffer, nbytes, buffered_p, blocking_p, intrpt_p));
//  else
//    return (DOS_read (fd, buffer, nbytes));
//}

int
DEFUN (nt_channel_read, (channel, buffer, nbytes),
       Tchannel channel AND PTR buffer AND size_t nbytes)
{
  DWORD  bytesRead = 0;
  
  if (nbytes == 0)
    return 0;
  else if (Screen_IsScreenHandle(CHANNEL_HANDLE (channel))) {
    bytesRead = Screen_Read (CHANNEL_HANDLE (channel), buffer, nbytes);
    if (bytesRead == 0xffffffff) {
      Sleep(0);  /* for pleasantness give up rest of this timeslice */
      errno = ERRNO_NONBLOCK;
    }
    return  (int)bytesRead;
  }
  else if (IsConsoleHandle(CHANNEL_HANDLE (channel))) {
    /* fake the console being a nonblocking channel that has nothing after
       each alternate read */
    static int nonblock = 1;
    nonblock = !nonblock;
    if (nonblock) {
      errno = ERRNO_NONBLOCK;
      return  -1;
    }
    if (ReadFile (CHANNEL_HANDLE (channel), buffer, nbytes, &bytesRead, 0))
      return  (int)bytesRead;
    else
      return  -1;
  }
  else {
    if (ReadFile (CHANNEL_HANDLE (channel), buffer, nbytes, &bytesRead, 0))
      return  (int)bytesRead;
    else
      return  -1;
  }
}

long
DEFUN (OS_channel_read, (channel, buffer, nbytes),
       Tchannel channel AND PTR buffer AND size_t nbytes)
{
  while (1) 
  {
    long scr = nt_channel_read (channel, buffer, nbytes);
    if (scr < 0)
    {
      if (errno == ERRNO_NONBLOCK)
	return -1;
      DOS_prim_check_errno (syscall_read);
      continue;
    }
    else if (((size_t) scr) > nbytes)
      error_external_return ();
    else {
        return (scr);
    }
  }
}

static int
DEFUN (dos_write, (fd, buffer, nbytes),
       HANDLE fd AND CONST unsigned char * buffer AND DWORD nbytes)
{
  DWORD  bytesWritten;
  if (Screen_IsScreenHandle (fd)) {
    SendMessage (fd, SCREEN_WRITE, (WPARAM)nbytes, (LPARAM)buffer);
    return  nbytes;
  }
  if (IsConsoleHandle (fd))
    return  dos_console_write (buffer, nbytes);
  if (WriteFile(fd, buffer, nbytes, &bytesWritten, 0))
    return  bytesWritten;
  else
    return  -1;
}

#define Syscall_Write(fd, buffer, size, so_far)		\
do							\
{ size_t _size = (size);				\
  int _written;						\
  _written = dos_write ((fd), (buffer), (_size));	\
  if (_size != _written)				\
    return ((_written < 0) ? -1 : (so_far) + _written); \
} while (0)

long
DEFUN (text_write, (hFile, buffer, nbytes),
       HANDLE hFile AND CONST unsigned char * buffer AND size_t nbytes)
{ /* Map LF to CR/LF */
  static CONST unsigned char crlf[] = {CARRIAGE_RETURN, LINEFEED};
  CONST unsigned char *start;
  size_t i;

  for (i = 0, start = buffer; i < nbytes; start = &buffer[i])
  { size_t len;

    while ((i < nbytes) && (buffer[i] != LINEFEED)) i++;
    len = (&buffer[i] - start);

    Syscall_Write (hFile, start, len, (i - len));

    if ((i < nbytes) && (buffer[i] == LINEFEED))
    { /* We are sitting on a linefeed. Write out CRLF */
      /* This backs out incorrectly if only CR is written out */
      Syscall_Write (hFile, crlf, (sizeof (crlf)), i);
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
  if (nbytes == 0)
    return (0);

  while (1)
  {
    HANDLE  hFile;
    DWORD   scr;

    hFile = CHANNEL_HANDLE(channel);
    scr = ((CHANNEL_COOKED (channel))
	   ? (text_write (hFile, buffer, nbytes))
	   : (dos_write (hFile, buffer, nbytes)));

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
  DWORD  scr;
  if (ReadFile (CHANNEL_HANDLE (channel), buffer, nbytes, &scr, 0))
    return  scr;
  else
    return  0;
}

size_t
DEFUN (OS_channel_write_dump_file, (channel, buffer, nbytes),
       Tchannel channel AND CONST PTR buffer AND size_t nbytes)
{
  DWORD  scr;
  if (WriteFile (CHANNEL_HANDLE (channel), buffer, nbytes, &scr, 0))
    return  scr;
  else
    return  0;
}

void
DEFUN (OS_channel_write_string, (channel, string),
       Tchannel channel AND
       CONST char * string)
{
  long length = (strlen (string));
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

unsigned int
DEFUN (OS_terminal_get_ispeed, (channel), Tchannel channel)
{
  return  0;
}

unsigned int
DEFUN (OS_terminal_get_ospeed, (channel), Tchannel channel)
{
  return  0;
}

void
DEFUN (OS_terminal_set_ispeed, (channel, baud),
       Tchannel channel AND
       unsigned int baud)
{
}

void
DEFUN (OS_terminal_set_ospeed, (channel, baud),
       Tchannel channel AND
       unsigned int baud)
{
}

unsigned int
DEFUN (OS_baud_index_to_rate, (index), unsigned int index)
{
  return  9600;
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
CONST int OS_have_select_p = 0;

long
DEFUN (OS_channel_select_then_read, (channel, buffer, nbytes),
       Tchannel channel AND
       PTR buffer AND
       size_t nbytes)
{ /* We can't really select amongst channels in DOS, but still need
     to keep track of whether the read was interrupted.
   */
  while (1)
  {
    long scr = (nt_channel_read (channel, buffer, nbytes));

    if (scr < 0)
    {
      if (errno == ERRNO_NONBLOCK)
	return -1;
      else if (errno == EINTR)
	return -4;
      else
      {
	DOS_prim_check_errno (syscall_read);
	continue;
      }
    }
    else if (((size_t) scr) > nbytes)
      error_external_return ();
    else
      return (scr);
  }
}
