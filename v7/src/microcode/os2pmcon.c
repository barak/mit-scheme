/* -*-C-*-

$Id: os2pmcon.c,v 1.1 1994/12/02 20:44:41 cph Exp $

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

#define INCL_WIN
#include "os2.h"

static void process_events (int);
static int  translate_key_event (msg_t *);
static const char * find_nonprint (const char *, const char *);
static void do_carriage_return (void);
static void do_linefeed (void);
static void do_formfeed (void);
static void do_backspace (void);
static void do_alert (void);

static unsigned short console_width;
static unsigned short console_height;
static unsigned short point_x;
static unsigned short point_y;
static tqueue_t * console_tqueue;
static qid_t console_qid;
static qid_t remote_qid;
static twid_t console_twid;
static int console_closedp;
static unsigned short readahead_repeat;
static char readahead_char;
static msg_list_t * pending_events_head;
static msg_list_t * pending_events_tail;

void
OS2_initialize_pm_console (void)
{
  point_x = 0;
  point_y = 0;
  console_closedp = 0;
  readahead_repeat = 0;
  pending_events_head = 0;
  console_tqueue = (OS2_make_std_tqueue ());
  OS2_make_qid_pair ((&console_qid), (&remote_qid));
  OS2_open_qid (console_qid, console_tqueue);
  console_twid = (OS2_twindow_open (remote_qid, "Scheme"));
}

static void
process_events (int blockp)
{
  while (1)
    {
      msg_t * message = (OS2_receive_message (console_qid, blockp));
      if (message == 0)
	break;
      switch (MSG_TYPE (message))
	{
	case mt_key_event:
	case mt_close_event:
	  {
	    msg_list_t * element = (OS_malloc (sizeof (msg_list_t)));
	    (element -> message) = message;
	    (element -> next) = 0;
	    if (pending_events_head == 0)
	      pending_events_head = element;
	    else
	      (pending_events_tail -> next) = element;
	    pending_events_tail = element;
	    if (blockp)
	      return;
	    break;
	  }
	case mt_resize_event:
	  console_width = (SM_RESIZE_EVENT_WIDTH (message));
	  console_height = (SM_RESIZE_EVENT_HEIGHT (message));
	  OS2_destroy_message (message);
	  break;
	case mt_button_event:
	case mt_visibility_event:
	  OS2_destroy_message (message);
	  break;
	default:
	  OS2_logic_error ("Unknown message type received by PM console.");
	  break;
	}
    }
}

int
OS2_pm_console_getch (void)
{
  if (readahead_repeat == 0)
    while (1)
      {
	process_events (pending_events_head == 0);
	{
	  msg_list_t * element = pending_events_head;
	  msg_t * message = (element -> message);
	  pending_events_head = (element -> next);
	  OS_free (element);
	  switch (MSG_TYPE (message))
	    {
	    case mt_key_event:
	      {
		int translation = (translate_key_event (message));
		unsigned short repeat = (SM_KEY_EVENT_REPEAT (message));
		OS2_destroy_message (message);
		if ((translation >= 0) && (repeat > 0))
		  {
		    readahead_char = translation;
		    readahead_repeat = repeat;
		    goto do_read;
		  }
		break;
	      }
	    case mt_close_event:
	      {
		twid_t twid = (SM_CLOSE_EVENT_TWID (message));
		OS2_destroy_message (message);
		OS2_twindow_close (twid);
	      }
	      OS2_close_qid (remote_qid);
	      OS2_close_qid (console_qid);
	      OS2_close_std_tqueue (console_tqueue);
	      console_closedp = 1;
	      goto do_read;
	    default:
	      OS2_logic_error ("Unknown message type received by PM console.");
	      break;
	    }
	}
      }
 do_read:
  if ((readahead_repeat == 0) && console_closedp)
    return (-1);
  readahead_repeat -= 1;
  return (readahead_char);
}

static int
translate_key_event (msg_t * message)
{
  unsigned short code = (SM_KEY_EVENT_CODE (message));
  unsigned short flags = (SM_KEY_EVENT_FLAGS (message));
  if ((flags & KC_VIRTUALKEY) != 0)
    switch (code)
      {
      case VK_BACKSPACE:
	code = '\177';
	break;
      case VK_TAB:
	code = '\t';
	break;
      case VK_ESC:
	code = '\033';
	break;
      case VK_SPACE:
	code = ' ';
	break;
      case VK_NEWLINE:
      case VK_ENTER:
	code = '\r';
	break;
      default:
	return (-1);
      }
  if ((code >= 0200) || ((flags & KC_ALT) != 0))
    return (-1);
  if ((flags & KC_CTRL) != 0)
    if (code >= 040)
      code &= 037;
    else
      return (-1);
  if (code == 0)
    return (-1);
  return (code);
}

void
OS2_pm_console_write (const char * data, size_t size)
{
  const char * end = (data + size);
  while (data < end)
    {
      process_events (0);
      {
	const char * nonprint = (find_nonprint (data, end));
	if (data < nonprint)
	  {
	    unsigned short size = (nonprint - data);
	    if (size > (console_width - point_x))
	      size = (console_width - point_x);
	    OS2_twindow_write (console_twid, point_x, point_y, data, size);
	    data += size;
	    point_x += size;
	    if (point_x == console_width)
	      {
		do_carriage_return ();
		do_linefeed ();
	      }
	    if (data == end)
	      break;
	  }
      }
      switch (*data++)
	{
	case '\r':
	  do_carriage_return ();
	  break;
	case '\012':
	  do_linefeed ();
	  break;
	case '\f':
	  do_formfeed ();
	  break;
	case '\b':
	  do_backspace ();
	  break;
	case '\a':
	  do_alert ();
	  break;
	}
    }
  OS2_twindow_move_cursor (console_twid, point_x, point_y);
}

static const char *
find_nonprint (const char * start, const char * end)
{
  while (start < end)
    if (!isprint (*start++))
      return (--start);
  return (end);
}

static void
do_carriage_return (void)
{
  point_x = 0;
}

static void
do_linefeed (void)
{
  if (point_y < (console_height - 1))
    point_y += 1;
  else
    {
      point_y = (console_height - 1);
      OS2_twindow_scroll (console_twid, 0, console_width, 0, point_y, 0, 1);
    }
  OS2_twindow_clear_eol (console_twid, 0, point_y);
}

static void
do_formfeed (void)
{
  point_x = 0;
  point_y = 0;
  OS2_twindow_clear (console_twid);
}

static void
do_backspace (void)
{
  if (point_x > 0)
    {
      point_x -= 1;
    }
}

static void
do_alert (void)
{
  DosBeep (880, 50);
}
