/* -*-C-*-

$Id: os2conio.c,v 1.1 1994/11/28 03:42:54 cph Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

#include "os2.h"
#ifdef USE_PMIO
#include <pmio.h>
#endif

typedef struct line_buffer_s
{
  msg_t * message;
  struct line_buffer_s * next;
} line_buffer_t;

static void console_thread (void *);
#ifndef USE_PMIO
static int  getch (void);
#endif
static void grab_console_lock (void);
static void release_console_lock (void);

static void init_line_buffer (void);
static line_buffer_t * make_line_buffer (line_buffer_t *);
static void push_line_buffer (void);
static void pop_line_buffer (void);
static line_buffer_t * reverse_line_buffer (void);

static void process_input_char (char);
static void do_rubout (void);
static void add_to_line (char);
static void do_newline (void);
static void do_self_insert (char);
static void add_char_to_line_buffer (char);
static void finish_line (void);
static void send_char (char);
static void send_readahead (msg_t *);
static void handle_console_interrupt (msg_t *);

static void console_operator
  (Tchannel, chop_t, choparg_t, choparg_t, choparg_t);;
static void flush_input (void);
static void console_input_buffered (Tchannel, int, int *);
static void console_output_cooked (Tchannel, int, int *);

static void write_char (char, int);
static void write_output (const char *, size_t, int);
static void write_output_1 (const char *, const char *);
static unsigned int char_output_length (char);

#define LINEFEED  '\012'

static HMTX console_lock;
static int input_buffered_p;
static int output_cooked_p;
static qid_t console_writer_qid;
static channel_context_t * console_context;
static line_buffer_t * line_buffer;

void
OS2_initialize_console (void)
{
#ifdef USE_PMIO
  pmio_fontspec = "6.System VIO";
  set_width (80);
  set_height (40);
  start_pmio ();
#endif
  console_lock = (OS2_create_mutex_semaphore ());
  input_buffered_p = 1;
  output_cooked_p = 1;
  console_context = (OS2_make_channel_context ());
  OS2_open_qid ((CHANNEL_CONTEXT_READER_QID (console_context)),
		OS2_scheme_tqueue);
  console_writer_qid = (CHANNEL_CONTEXT_WRITER_QID (console_context));
  OS2_open_qid (console_writer_qid, (OS2_make_std_tqueue ()));
  (void) OS2_beginthread (console_thread, 0, 0x4000);
}

static void
console_thread (void * arg)
{
  grab_console_lock ();
  init_line_buffer ();
  release_console_lock ();
  (void) OS2_thread_initialize (console_writer_qid);
  while (1)
    {
      int c = (getch ());
      if (c == EOF)
	break;
      {
	int code = (OS2_keyboard_interrupt_handler (c));
	if (code == '\0')
	  process_input_char (c);
	else
	  {
	    msg_t * message = (OS2_create_message (mt_console_interrupt));
	    (SM_CONSOLE_INTERRUPT_CODE (message)) = code;
	    OS2_send_message (OS2_interrupt_qid, message);
	    /* Flush buffers only for certain chars? */
	    flush_input ();
	  }
      }
    }
  OS2_endthread ();
}

#ifndef USE_PMIO
static int
getch (void)
{
  while (1)
    {
#if 1
      KBDKEYINFO info;
      XTD_API_CALL
	(kbd_char_in, ((&info), IO_WAIT, 0),
	 {
	   if (rc == ERROR_KBD_INVALID_HANDLE)
	     return (EOF);
	 });
      if ((info . fbStatus) == 0x40)
	return (info . chChar);
#else
      int c = (_getch ());
      if (c == EOF)
	return (EOF);
      else if ((c == 0) || (c == 0xe0))
	{
	  /* Discard extended keycodes. */
	  if ((_getch ()) == EOF)
	    return (EOF);
	}
      else
	return (c);
#endif
    }
}
#endif /* not USE_PMIO */

static void
grab_console_lock (void)
{
  OS2_request_mutex_semaphore (console_lock);
}

static void
release_console_lock (void)
{
  OS2_release_mutex_semaphore (console_lock);
}

static void
init_line_buffer (void)
{
  line_buffer = 0;
  push_line_buffer ();
}

static line_buffer_t *
make_line_buffer (line_buffer_t * next)
{
  line_buffer_t * buffer = (OS_malloc (sizeof (line_buffer_t)));
  msg_t * message = (OS2_make_readahead ());
  (SM_READAHEAD_SIZE (message)) = 0;
  (buffer -> message) = message;
  (buffer -> next) = next;
  return (buffer);
}

static void
push_line_buffer (void)
{
  line_buffer = (make_line_buffer (line_buffer));
}

static void
pop_line_buffer (void)
{
  line_buffer_t * buffer = line_buffer;
  OS2_destroy_message (buffer -> message);
  line_buffer = (buffer -> next);
  OS_free (buffer);
}

static line_buffer_t *
reverse_line_buffer (void)
{
  line_buffer_t * this = line_buffer;
  line_buffer_t * prev = 0;
  line_buffer_t * next;
  line_buffer = 0;
  while (1)
    {
      next = (this -> next);
      (this -> next) = prev;
      if (next == 0)
	break;
      prev = this;
      this = next;
    }
  push_line_buffer ();
  return (this);
}

#define LINE_BUFFER_SIZE (SM_READAHEAD_SIZE (line_buffer -> message))
#define LINE_BUFFER_DATA (SM_READAHEAD_DATA (line_buffer -> message))

static void
process_input_char (char c)
{
  if (!input_buffered_p)
    send_char (c);
  else switch (c)
    {
    case '\b':
    case '\177':
      do_rubout ();
      break;
    case '\r':
      do_self_insert (LINEFEED);
      finish_line ();
      break;
    default:
      do_self_insert (c);
      break;
    }
}

static void
do_self_insert (char c)
{
  add_char_to_line_buffer (c);
  write_char (c, 1);
}

static void
add_char_to_line_buffer (char c)
{
  grab_console_lock ();
  if (LINE_BUFFER_SIZE == SM_READAHEAD_MAX)
    push_line_buffer ();
  (LINE_BUFFER_DATA [LINE_BUFFER_SIZE ++]) = c;
  release_console_lock ();
}

static void
do_rubout (void)
{
  grab_console_lock ();
  if (LINE_BUFFER_SIZE == 0)
    {
      if ((line_buffer -> next) == 0)
	{
	  release_console_lock ();
	  write_char ('\a', 0);
	  return;
	}
      pop_line_buffer ();
    }
  {
    unsigned int n
      = (char_output_length (LINE_BUFFER_DATA [-- LINE_BUFFER_SIZE]));
    unsigned int i;
    release_console_lock ();
    for (i = 0; (i < n); i += 1)
      write_char ('\b', 0);
    for (i = 0; (i < n); i += 1)
      write_char (' ', 0);
    for (i = 0; (i < n); i += 1)
      write_char ('\b', 0);
  }
}

static void
finish_line (void)
{
  line_buffer_t * buffer;
  grab_console_lock ();
  buffer = (reverse_line_buffer ());
  release_console_lock ();
  while (buffer != 0)
    {
      send_readahead (buffer -> message);
      buffer = (buffer -> next);
      OS_free (buffer);
    }
}

static void
send_char (char c)
{
  msg_t * message = (OS2_make_readahead ());
  (SM_READAHEAD_SIZE (message)) = 1;
  ((SM_READAHEAD_DATA (message)) [0]) = c;
  send_readahead (message);
}

static void
send_readahead (msg_t * message)
{
  OS2_send_message (console_writer_qid, message);
  OS2_wait_for_readahead_ack (console_writer_qid);
}

void
OS2_initialize_console_channel (Tchannel channel)
{
  (CHANNEL_OPERATOR_CONTEXT (channel)) = console_context;
  (CHANNEL_OPERATOR (channel)) = console_operator;
}

static void
console_operator (Tchannel channel, chop_t operation,
		  choparg_t arg1, choparg_t arg2, choparg_t arg3)
{
  switch (operation)
    {
    case chop_read:
      (* ((long *) arg3))
	= (channel_thread_read (channel, ((char *) arg1), ((size_t) arg2)));
      break;
    case chop_write:
      write_output (((const char *) arg1), ((size_t) arg2), output_cooked_p);
      (* ((long *) arg3)) = ((size_t) arg2);
      break;
    case chop_close:
    case chop_output_flush:
    case chop_output_drain:
      break;
    case chop_input_flush:
      flush_input ();
      break;
    case chop_input_buffered:
      console_input_buffered (channel, ((int) arg1), ((int *) arg2));
      break;
    case chop_output_cooked:
      console_output_cooked (channel, ((int) arg1), ((int *) arg2));
      break;
    default:
      OS2_logic_error ("Unknown operation for console.");
      break;
    }
}

static void
flush_input (void)
{
  grab_console_lock ();
  while ((line_buffer -> next) != 0)
    pop_line_buffer ();
  LINE_BUFFER_SIZE = 0;
  release_console_lock ();
}

static void
console_input_buffered (Tchannel channel, int new, int * pold)
{
  if (new < 0)
    (* pold) = input_buffered_p;
  else
    {
      int old = input_buffered_p;
      input_buffered_p = new;
      if (old && (!new))
	{
	  grab_console_lock ();
	  finish_line ();
	}
    }
}

static void
console_output_cooked (Tchannel channel, int new, int * pold)
{
  if (new < 0)
    (* pold) = output_cooked_p;
  else
    output_cooked_p = new;
}

static void
write_char (char c, int cooked_p)
{
  write_output ((&c), 1, cooked_p);
}

static void
write_output (const char * data, size_t size, int cooked_p)
{
  const char * scan = data;
  const char * end = (scan + size);
  char output_translation [256];
  char * out = output_translation;
  char * out_limit = (out + ((sizeof (output_translation)) - 4));
  char c;
  if (!cooked_p)
    write_output_1 (scan, end);
  else
    while (1)
      {
	if ((scan == end) || (out >= out_limit))
	  {
	    write_output_1 (output_translation, out);
	    if (scan == end)
	      break;
	    out = output_translation;
	  }
	c = (*scan++);
	if (isprint (c))
	  (*out++) = c;
	else if (c == LINEFEED)
	  {
	    (*out++) = '\r';
	    (*out++) = c;
	  }
	else if (c < 0x20)
	  {
	    (*out++) = '^';
	    (*out++) = ('@' + c);
	  }
	else
	  {
	    (*out++) = '\\';
	    (*out++) = ('0' + ((c >> 6) & 3));
	    (*out++) = ('0' + ((c >> 3) & 7));
	    (*out++) = ('0' + (c & 7));
	  }
      }
}

static void
write_output_1 (const char * scan, const char * end)
{
#ifdef USE_PMIO
  put_raw ((end - scan), scan);
#else /* not USE_PMIO */
#if 1
  STD_API_CALL (vio_wrt_tty, (((PCH) scan), (end - scan), 0));
#else
  while (1)
    {
      ULONG n;
      APIRET rc = (dos_write (1, ((void *) scan), (end - scan), (& n)));
      if (rc != NO_ERROR)
	break;
      scan += n;
      if (scan == end)
	break;
    }
#endif
#endif /* not USE_PMIO */
}

static unsigned int
char_output_length (char c)
{
  return ((isprint (c)) ? 1 : (c < 0x20) ? 2 : 4);
}
