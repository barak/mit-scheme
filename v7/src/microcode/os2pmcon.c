/* -*-C-*-

$Id: os2pmcon.c,v 1.13 1995/05/20 02:40:20 cph Exp $

Copyright (c) 1994-95 Massachusetts Institute of Technology

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

/* #define CONSOLE_WRAP */

static void grab_console_lock (void);
static void release_console_lock (void);
static unsigned short cx2x (unsigned short);
static unsigned short cy2y (unsigned short, int);
static unsigned short x2cx (unsigned short, int);
static unsigned short y2cy (unsigned short, int);
static void process_events (int);
static void console_resize (unsigned short, unsigned short);
static void console_paint
  (unsigned short, unsigned short, unsigned short, unsigned short);
static void console_clear
  (unsigned short, unsigned short, unsigned short, unsigned short);
static void console_clear_all (void);
static int translate_key_event (msg_t *);
static const char * find_nonprint (const char *, const char *);
static void do_carriage_return (void);
static void do_linefeed (void);
static unsigned short find_invalid_line (unsigned short, unsigned short);
static void do_formfeed (void);
static void do_backspace (void);
static void do_alert (void);

static HMTX console_lock;
static unsigned short console_pel_width;
static unsigned short console_pel_height;
static unsigned short console_width;
static unsigned short console_height;
static char * console_chars;
static font_metrics_t * console_metrics;
static unsigned short point_x;
static unsigned short point_y;
static int console_visiblep;
static int console_closedp;
static unsigned short readahead_repeat;
static char readahead_char;
static const char * readahead_insert;
static const char * readahead_insert_scan;
static msg_list_t * pending_events_head;
static msg_list_t * pending_events_tail;
static tqueue_t * console_tqueue;
static qid_t console_event_qid;
static qid_t console_pm_qid;
static wid_t console_wid;
static psid_t console_psid;

#define CHAR_WIDTH (FONT_METRICS_WIDTH (console_metrics))
#define CHAR_HEIGHT (FONT_METRICS_HEIGHT (console_metrics))
#define CHAR_DESCENDER (FONT_METRICS_DESCENDER (console_metrics))
#define CHAR_LOC(x, y) (& (console_chars [((y) * console_width) + (x)]))

#define FASTFILL(p, n, c)						\
{									\
  char * FASTFILL_scan = (p);						\
  char * FASTFILL_end = (FASTFILL_scan + (n));				\
  while (FASTFILL_scan < FASTFILL_end)					\
    (*FASTFILL_scan++) = (c);						\
}

void
OS2_initialize_pm_console (void)
{
  console_lock = (OS2_create_mutex_semaphore (0, 0));
  console_pel_width = 0;
  console_pel_height = 0;
  console_width = 0;
  console_height = 0;
  console_chars = 0;
  point_x = 0;
  point_y = 0;
  console_visiblep = 0;
  console_closedp = 0;
  readahead_repeat = 0;
  readahead_insert = 0;
  pending_events_head = 0;
  console_tqueue = (OS2_make_std_tqueue ());
  {
    qid_t remote;
    OS2_make_qid_pair ((&console_event_qid), (&remote));
    OS2_open_qid (console_event_qid, console_tqueue);
    console_pm_qid = (OS2_create_pm_qid (console_tqueue));
    console_wid = (OS2_window_open (console_pm_qid, remote, 0, "Scheme"));
  }
  OS2_window_permanent (console_wid);
  {
    psid_t psid = (OS2_window_client_ps (console_wid));
    /* This prevents the font-change hook from being invoked.  */
    console_psid = 0;
    console_metrics = (OS2_ps_set_font (psid, 1, "4.System VIO"));
    if (console_metrics == 0)
      OS2_logic_error ("Unable to find 4 point System VIO font.");
    console_psid = psid;
  }
  OS2_window_set_grid (console_wid, CHAR_WIDTH, CHAR_HEIGHT);
  OS2_window_shape_cursor
    (console_wid, CHAR_WIDTH, CHAR_HEIGHT, (CURSOR_SOLID | CURSOR_FLASH));
  OS2_window_show_cursor (console_wid, 1);
  OS2_window_show (console_wid, 1);
  OS2_window_activate (console_wid);
  {
    unsigned short width;
    unsigned short height;
    unsigned short max_width = (80 * CHAR_WIDTH);
    OS2_window_size (console_wid, (& width), (& height));
    console_resize (width, height);
    if (width > max_width)
      OS2_window_set_size (console_wid, max_width, height);
  }
}

wid_t
OS2_console_wid (void)
{
  return (console_wid);
}

psid_t
OS2_console_psid (void)
{
  return (console_psid);
}

void
OS2_console_font_change_hook (font_metrics_t * metrics)
{
  font_metrics_t * copy = (OS_malloc (sizeof (font_metrics_t)));
  FASTCOPY (metrics, copy, (sizeof (font_metrics_t)));
  grab_console_lock ();
  OS_free (console_metrics);
  console_metrics = copy;
  console_resize (console_pel_width, console_pel_height);
  OS2_window_set_grid (console_wid, CHAR_WIDTH, CHAR_HEIGHT);
  OS2_window_shape_cursor
    (console_wid, CHAR_WIDTH, CHAR_HEIGHT, (CURSOR_SOLID | CURSOR_FLASH));
  OS2_window_move_cursor (console_wid, (cx2x (point_x)), (cy2y (point_y, 1)));
  OS2_window_invalidate (console_wid,
			 0, console_pel_width,
			 0, console_pel_height);
  release_console_lock ();
}

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

static unsigned short
cx2x (unsigned short x)
{
  return (x * CHAR_WIDTH);
}

static unsigned short
cy2y (unsigned short y, int lowerp)
{
  /* lowerp => result is bottommost pel of cell.  Otherwise result is
     bottommost pel of cell above.  */
  unsigned short limit = (lowerp ? (console_height - 1) : console_height);
  return ((y < limit) ? ((limit - y) * CHAR_HEIGHT) : 0);
}

static unsigned short
x2cx (unsigned short x, int lowerp)
{
  /* lowerp => `x' is inclusive lower bound, and result is cell it
     falls in.  Otherwise, `x' is exclusive upper bound, and result is
     cell to its right, unless it falls on leftmost edge of cell.  If
     the argument is inclusive-lower, then the result is also;
     likewise for exclusive-upper.  */
  unsigned short cx = (x / CHAR_WIDTH);
  if (! (lowerp || ((x % CHAR_WIDTH) == 0)))
    cx += 1;
  return ((cx > console_width) ? console_width : cx);
}

static unsigned short
y2cy (unsigned short y, int lowerp)
{
  /* lowerp => `y' is inclusive lower bound, and result is cell below
     the one it falls in.  Otherwise, `y' is exclusive upper bound,
     and result is cell it falls in, unless it falls on bottommost
     edge of cell, when result is cell below.  If the argument is
     inclusive-lower, then the result is exclusive-upper, and
     vice-versa.  */
  short cy = (((short) (console_height - 1)) - ((short) (y / CHAR_HEIGHT)));
  if (lowerp || ((y % CHAR_HEIGHT) == 0))
    cy += 1;
  return ((cy < 0) ? 0 : cy);
}

static void
process_events (int blockp)
{
  while (1)
    {
      msg_t * message
	= (OS2_receive_message (console_event_qid, blockp, 0));
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
	  {
	    unsigned short new_pel_width = (SM_RESIZE_EVENT_WIDTH (message));
	    unsigned short new_pel_height = (SM_RESIZE_EVENT_HEIGHT (message));
	    OS2_destroy_message (message);
	    grab_console_lock ();
	    console_resize (new_pel_width, new_pel_height);
	    release_console_lock ();
	    break;
	  }
	case mt_paint_event:
	  {
	    unsigned short xl = (SM_PAINT_EVENT_XL (message));
	    unsigned short xh = (SM_PAINT_EVENT_XH (message));
	    unsigned short yl = (SM_PAINT_EVENT_YL (message));
	    unsigned short yh = (SM_PAINT_EVENT_YH (message));
	    OS2_destroy_message (message);
	    grab_console_lock ();
	    console_paint (xl, xh, yl, yh);
	    release_console_lock ();
	    break;
	  }
	case mt_visibility_event:
	  if ((!console_visiblep) && (SM_VISIBILITY_EVENT_SHOWNP (message)))
	    {
	      grab_console_lock ();
	      OS2_window_invalidate (console_wid,
				     0, console_pel_width,
				     0, console_pel_height);
	      release_console_lock ();
	    }
	  console_visiblep = (SM_VISIBILITY_EVENT_SHOWNP (message));
	  OS2_destroy_message (message);
	  break;
	case mt_button_event:
	  if ((BUTTON_TYPE_EVENT (SM_BUTTON_EVENT_TYPE (message)))
	       == BUTTON_EVENT_DOWN)
	    OS2_window_activate (SM_BUTTON_EVENT_WID (message));
	  OS2_destroy_message (message);
	  break;
	default:
	  OS2_destroy_message (message);
	  break;
	}
    }
}

static void
console_resize (unsigned short new_pel_width, unsigned short new_pel_height)
{
  unsigned short new_width = (new_pel_width / CHAR_WIDTH);
  unsigned short new_height = (new_pel_height / CHAR_HEIGHT);
  char * new_chars = (OS_malloc (new_width * new_height));
  FASTFILL (new_chars, (new_width * new_height), ' ');
  if ((point_x < new_width) && (point_y < new_height))
    {
      if (console_chars != 0)
	{
	  unsigned short xlim
	    = ((new_width < console_width) ? new_width : console_width);
	  unsigned short ylim
	    = ((new_height < console_height) ? new_height : console_height);
	  char * from = console_chars;
	  char * to = new_chars;
	  unsigned short y = 0;
	  while (y < ylim)
	    {
	      FASTCOPY (from, to, xlim);
	      from += console_width;
	      to += new_width;
	      y += 1;
	    }
	  OS_free (console_chars);
	}
    }
  else
    {
      point_x = 0;
      point_y = 0;
    }
  console_pel_width = new_pel_width;
  console_pel_height = new_pel_height;
  console_width = new_width;
  console_height = new_height;
  console_chars = new_chars;
}

static void
console_paint (unsigned short xl, unsigned short xh,
	       unsigned short yl, unsigned short yh)
{
  unsigned short cxl = (x2cx (xl, 1));
  unsigned short cxh = (x2cx (xh, 0));
  unsigned short cyl = (y2cy (yh, 0));
  unsigned short cyh = (y2cy (yl, 1));
  OS2_ps_clear (console_psid, xl, xh, yl, yh);
  if ((cxl < cxh) && (cyl < cyh))
    {
      unsigned short size = (cxh - cxl);
      unsigned short x = (cx2x (cxl));
      while (cyl < cyh)
	{
	  OS2_ps_draw_text (console_psid,
			    x, ((cy2y (cyl, 1)) + CHAR_DESCENDER),
			    (CHAR_LOC (cxl, cyl)), size);
	  cyl += 1;
	}
    }
}

static void
console_clear (unsigned short xl, unsigned short xh,
	       unsigned short yl, unsigned short yh)
{
  OS2_ps_clear (console_psid,
		(cx2x (xl)), (cx2x (xh)),
		(cy2y (yh, 0)), (cy2y (yl, 0)));
}

static void
console_clear_all (void)
{
  OS2_ps_clear (console_psid, 0, console_pel_width, 0, console_pel_height);
}

int
OS2_pm_console_getch (void)
{
  if (console_closedp)
    return (-1);
  if ((readahead_repeat == 0) && (readahead_insert == 0))
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
		else if (translation == (-2))
		  {
		    readahead_insert
		      = (OS2_clipboard_read_text (console_pm_qid));
		    if ((*readahead_insert) != '\0')
		      {
			readahead_insert_scan = readahead_insert;
			goto do_read;
		      }
		    else
		      OS_free ((void *) readahead_insert);
		  }
		break;
	      }
	    case mt_close_event:
	      console_closedp = 1;
	      {
		wid_t wid = (SM_CLOSE_EVENT_WID (message));
		OS2_destroy_message (message);
		OS2_window_close (wid);
	      }
	      OS2_close_qid (console_event_qid);
	      OS2_close_std_tqueue (console_tqueue);
	      goto do_read;
	    default:
	      OS2_logic_error ("Unknown message type received by PM console.");
	      break;
	    }
	}
      }
 do_read:
  if (readahead_insert != 0)
    {
      char c = (*readahead_insert_scan++);
      if ((*readahead_insert_scan) == '\0')
	{
	  OS_free ((void *) readahead_insert);
	  readahead_insert = 0;
	}
      return (c);
    }
  if (readahead_repeat != 0)
    {
      readahead_repeat -= 1;
      return (readahead_char);
    }
  return (-1);
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
      case VK_INSERT:
	return (((flags & KC_SHIFT) != 0) ? (-2) : (-1));
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
  const char * nonprint;
  if (console_closedp)
    return;
  grab_console_lock ();
  while (data < end)
    {
      nonprint = (find_nonprint (data, end));
      if (data < nonprint)
	while (1)
	  {
	    unsigned short size = (nonprint - data);
	    if (size > (console_width - point_x))
	      size = (console_width - point_x);
	    FASTCOPY (data, (CHAR_LOC (point_x, point_y)), size);
	    OS2_ps_draw_text (console_psid,
			      (cx2x (point_x)),
			      ((cy2y (point_y, 1)) + CHAR_DESCENDER),
			      data,
			      size);
	    data += size;
	    point_x += size;
	    if (point_x == console_width)
	      {
		do_carriage_return ();
		do_linefeed ();
	      }
	    if (data == nonprint)
	      break;
	  }
      if (data < end)
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
  OS2_window_move_cursor (console_wid, (cx2x (point_x)), (cy2y (point_y, 1)));
  release_console_lock ();
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
    {
      point_y += 1;
      FASTFILL ((CHAR_LOC (0, point_y)), console_width, ' ');
      console_clear (0, console_width, point_y, (point_y + 1));
    }
  else
    {
#ifdef CONSOLE_WRAP
      point_y = 0;
      FASTFILL ((CHAR_LOC (0, 0)), console_width, ' ');
      console_clear (0, console_width, 0, 1);
#else /* not CONSOLE_WRAP */
      point_y = (console_height - 1);
      FASTCOPY ((CHAR_LOC (0, 1)),
		(CHAR_LOC (0, 0)),
		(point_y * console_width));
      FASTFILL ((CHAR_LOC (0, point_y)), console_width, ' ');
      OS2_window_scroll (console_wid,
			 0, console_pel_width,
			 0, (point_y * CHAR_HEIGHT),
			 0, CHAR_HEIGHT);
      OS2_ps_clear (console_psid, 0, console_pel_width, 0, CHAR_HEIGHT);
#endif /* not CONSOLE_WRAP */
    }
}

static void
do_formfeed (void)
{
  point_x = 0;
  point_y = 0;
  FASTFILL ((CHAR_LOC (0, 0)), (console_width * console_height), ' ');
  console_clear_all ();
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
