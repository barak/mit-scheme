/* -*-C-*-

$Id: os2pmcon.c,v 1.26 2000/12/05 21:23:46 cph Exp $

Copyright (c) 1994-2000 Massachusetts Institute of Technology

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

#define INCL_WIN
#include "os2.h"
#include "os2pmcon.h"

/* For the "about" dialog box.  */
#include "version.h"

/* #define CONSOLE_WRAP */

static void grab_console_lock (void);
static void release_console_lock (void);
static unsigned short cx2x (unsigned short);
static unsigned short cy2y (unsigned short, int);
static unsigned short x2cx (short, int);
static unsigned short y2cy (short, int);
static void process_events (int);
static void initialize_marked_region (short, short);
static void update_marked_region (short, short);
static void unmark_marked_region (void);
static int marked_region_nonempty_p (void);
static char * extract_marked_region (int);
static void compute_marked_region
  (short, short, short, short,
   unsigned short *, unsigned short *, unsigned short *, unsigned short *);
static void highlight_marked_region
  (unsigned short, unsigned short, unsigned short, unsigned short, char);
static void paint_marked_region_segment
  (unsigned short, unsigned short, unsigned short, unsigned short);
static void disable_marked_region (void);
static void enable_menu_copy_items (int);
static void console_resize (unsigned short, unsigned short);
static void console_paint
  (unsigned short, unsigned short, unsigned short, unsigned short);
static unsigned short compute_run_length (const char *, const char *);
static void console_clear
  (unsigned short, unsigned short, unsigned short, unsigned short);
static void console_clear_all (void);
static int do_paste (void);
static int translate_key_event
  (MPARAM, MPARAM, unsigned short *, unsigned char *);
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
static char * console_highlights;
static unsigned short * console_line_lengths;
static font_metrics_t * console_metrics;
static unsigned short point_x;
static unsigned short point_y;
static int console_visiblep;
static int console_closedp;
static unsigned short readahead_repeat;
static char readahead_char;
static const char * readahead_insert;
static const char * readahead_insert_scan;
static void * pending_events;
static tqueue_t * console_tqueue;
static qid_t console_event_qid;
static qid_t console_pm_qid;
static wid_t console_wid;
static psid_t console_psid;
static int console_tracking_mouse_p;
static HWND console_tracking_mouse_pointer;
static int console_marked_region_active_p;
static HWND console_menu;
static short console_mark_x;
static short console_mark_y;
static short console_point_x;
static short console_point_y;

static const char * console_font_specs [] =
  { "8.Courier", "10.Courier", "12.Courier", 
    "4.System VIO", "10.System Monospaced" };

#define CHAR_WIDTH (FONT_METRICS_WIDTH (console_metrics))
#define CHAR_HEIGHT (FONT_METRICS_HEIGHT (console_metrics))
#define CHAR_DESCENDER (FONT_METRICS_DESCENDER (console_metrics))
#define CHAR_LOC(x, y) (& (console_chars [((y) * console_width) + (x)]))
#define CHAR_HL(x, y) (& (console_highlights [((y) * console_width) + (x)]))
#define LINE_LEN_LOC(y) ((char *) (& (console_line_lengths [(y)])))

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
  console_highlights = 0;
  console_line_lengths = 0;
  point_x = 0;
  point_y = 0;
  console_visiblep = 0;
  console_closedp = 0;
  console_tracking_mouse_p = 0;
  console_tracking_mouse_pointer
    = (WinQuerySysPointer (HWND_DESKTOP, SPTR_TEXT, FALSE));
  readahead_repeat = 0;
  readahead_insert = 0;
  pending_events = (OS2_create_msg_fifo ());
  console_tqueue = (OS2_make_std_tqueue ());
  {
    qid_t remote;
    OS2_make_qid_pair ((&console_event_qid), (&remote));
    OS2_open_qid (console_event_qid, console_tqueue);
    console_pm_qid = (OS2_create_pm_qid (console_tqueue));
    console_wid
      = (OS2_window_open (console_pm_qid, remote,
			  (FCF_TITLEBAR | FCF_SYSMENU
			   | FCF_SHELLPOSITION | FCF_SIZEBORDER
			   | FCF_MINMAX | FCF_TASKLIST | FCF_NOBYTEALIGN
			   | FCF_MENU | FCF_ACCELTABLE | FCF_ICON),
			  NULLHANDLE,
			  ID_PMCON_RESOURCES,
			  0, "Scheme"));
  }
  OS2_window_permanent (console_wid);
  {
    psid_t psid = (OS2_window_client_ps (console_wid));
    const char ** scan_specs = console_font_specs;
    const char ** end_specs
      = (scan_specs
	 + ((sizeof (console_font_specs)) / (sizeof (const char *))));
    console_metrics = 0;
    while (scan_specs < end_specs)
      {
	const char * spec = (*scan_specs++);
	/* This prevents the font-change hook from being invoked.  */
	console_psid = 0;
	console_metrics = (OS2_ps_set_font (psid, 1, spec));
	if (console_metrics != 0)
	  break;
      }
    if (console_metrics == 0)
      OS2_logic_error ("Unable to find usable console font.");
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
  console_menu
    = (OS2_window_handle_from_id (console_pm_qid,
				  (OS2_window_frame_handle (console_wid)),
				  FID_MENU));
  disable_marked_region ();
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
  FASTCOPY (((char *) metrics), ((char *) copy), (sizeof (font_metrics_t)));
  grab_console_lock ();
  OS_free (console_metrics);
  console_metrics = copy;
  OS2_window_set_grid (console_wid, CHAR_WIDTH, CHAR_HEIGHT);
  OS2_window_shape_cursor
    (console_wid, CHAR_WIDTH, CHAR_HEIGHT, (CURSOR_SOLID | CURSOR_FLASH));
  console_resize (console_pel_width, console_pel_height);
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
x2cx (short x, int lowerp)
{
  /* lowerp => `x' is inclusive lower bound, and result is cell it
     falls in.  Otherwise, `x' is exclusive upper bound, and result is
     cell to its right, unless it falls on leftmost edge of cell.  If
     the argument is inclusive-lower, then the result is also;
     likewise for exclusive-upper.  */
  short cx = (x / ((short) CHAR_WIDTH));
  if (! (lowerp || ((x % ((short) CHAR_WIDTH)) == 0)))
    cx += 1;
  return ((cx < 0) ? 0 : (cx > console_width) ? console_width : cx);
}

static unsigned short
y2cy (short y, int lowerp)
{
  /* lowerp => `y' is inclusive lower bound, and result is cell below
     the one it falls in.  Otherwise, `y' is exclusive upper bound,
     and result is cell it falls in, unless it falls on bottommost
     edge of cell, when result is cell below.  If the argument is
     inclusive-lower, then the result is exclusive-upper, and
     vice-versa.  */
  short cy = (((short) (console_height - 1)) - (y / ((short) CHAR_HEIGHT)));
  if (lowerp || ((y % ((short) CHAR_HEIGHT)) == 0))
    cy += 1;
  return ((cy < 0) ? 0 : (cy > console_height) ? console_height : cy);
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
	case mt_paint_event:
	  {
	    unsigned short xl = (SM_PAINT_EVENT_XL (message));
	    unsigned short xh = (SM_PAINT_EVENT_XH (message));
	    unsigned short yl = (SM_PAINT_EVENT_YL (message));
	    unsigned short yh = (SM_PAINT_EVENT_YH (message));
	    OS2_destroy_message (message);
	    grab_console_lock ();
	    OS2_ps_clear (console_psid, xl, xh, yl, yh);
	    console_paint ((x2cx (xl, 1)),
			   (x2cx (xh, 0)),
			   (y2cy (yh, 0)),
			   (y2cy (yl, 1)));
	    release_console_lock ();
	    break;
	  }
	case mt_pm_event:
	  {
	    ULONG msg = (SM_PM_EVENT_MSG (message));
	    MPARAM mp1 = (SM_PM_EVENT_MP1 (message));
	    MPARAM mp2 = (SM_PM_EVENT_MP2 (message));
	    switch (msg)
	      {
	      case WM_CHAR:
	      case WM_CLOSE:
	      postpone_event:
		OS2_msg_fifo_insert (pending_events, message);
		message = 0;
		if (blockp)
		  return;
		break;
	      case WM_SIZE:
		{
		  unsigned short new_pel_width = (SHORT1FROMMP (mp2));
		  unsigned short new_pel_height = (SHORT2FROMMP (mp2));
		  grab_console_lock ();
		  console_resize (new_pel_width, new_pel_height);
		  release_console_lock ();
		  break;
		}
	      case WM_SHOW:
		if ((!console_visiblep) && (SHORT1FROMMP (mp1)))
		  {
		    grab_console_lock ();
		    OS2_window_invalidate (console_wid,
					   0, console_pel_width,
					   0, console_pel_height);
		    release_console_lock ();
		  }
		console_visiblep = (SHORT1FROMMP (mp1));
		break;
	      case WM_BUTTON1DOWN:
		grab_console_lock ();
		if (!OS2_window_focusp (console_wid))
		  OS2_window_activate (console_wid);
		else if (OS2_window_set_capture (console_wid, 1))
		  {
		    console_tracking_mouse_p = 1;
		    initialize_marked_region ((SHORT1FROMMP (mp1)),
					      (SHORT2FROMMP (mp1)));
		    OS2_window_mousetrack (console_wid, 1);
		    OS2_set_pointer (console_pm_qid,
				     HWND_DESKTOP,
				     console_tracking_mouse_pointer);
		  }
		else
		  (void) WinAlarm (HWND_DESKTOP, WA_ERROR);
		release_console_lock ();
		break;
	      case WM_BUTTON1UP:
		if (console_tracking_mouse_p)
		  {
		    grab_console_lock ();
		    update_marked_region ((SHORT1FROMMP (mp1)),
					  (SHORT2FROMMP (mp1)));
		    (void) OS2_window_set_capture (console_wid, 0);
		    OS2_window_mousetrack (console_wid, 0);
		    enable_menu_copy_items (marked_region_nonempty_p ());
		    console_tracking_mouse_p = 0;
		    release_console_lock ();
		  }
		break;
	      case WM_MOUSEMOVE:
		if (console_tracking_mouse_p)
		  {
		    grab_console_lock ();
		    update_marked_region ((SHORT1FROMMP (mp1)),
					  (SHORT2FROMMP (mp1)));
		    OS2_set_pointer (console_pm_qid,
				     HWND_DESKTOP,
				     console_tracking_mouse_pointer);
		    release_console_lock ();
		  }
		break;
	      case WM_BUTTON2DOWN:
	      case WM_BUTTON3DOWN:
		grab_console_lock ();
		if (!OS2_window_focusp (console_wid))
		  OS2_window_activate (console_wid);
		release_console_lock ();
		break;
	      case WM_COMMAND:
		switch (SHORT1FROMMP (mp1))
		  {
		  case IDM_CUT:
		  case IDM_COPY:
		  case IDM_PASTE:
		    goto postpone_event;
		  case IDM_FONT:
		    grab_console_lock ();
		    {
		      const char * font_spec
			= (OS2_window_font_dialog (console_wid,
						   "Console Window Font"));
		      if (font_spec != 0)
			{
			  (void) OS2_ps_set_font (console_psid, 1, font_spec);
			  OS_free ((void *) font_spec);
			}
		    }
		    release_console_lock ();
		    break;
		  case IDM_EXIT:
		    termination_normal (0);
		    break;
		  case IDM_ABOUT:
		    (void) WinMessageBox
		      (HWND_DESKTOP, NULLHANDLE,
		       "This is MIT Scheme Release "
		       SCHEME_RELEASE
		       ", brought to you by the MIT Scheme Team.\n",
		       "The Uncommon Lisp", 0, MB_OK);
		    break;
		  }
	      }
	    if (message != 0)
	      OS2_destroy_message (message);
	  }
	  break;
	default:
	  OS2_destroy_message (message);
	  break;
	}
    }
}

static void
initialize_marked_region (short x, short y)
{
  unmark_marked_region ();
  console_mark_x = x;
  console_mark_y = y;
  console_point_x = x;
  console_point_y = y;
  console_marked_region_active_p = 1;
}

static void
update_marked_region (short x, short y)
{
  unsigned short cx11;
  unsigned short cy11;
  unsigned short cx21;
  unsigned short cy21;
  unsigned short cx12;
  unsigned short cy12;
  unsigned short cx22;
  unsigned short cy22;

  unsigned short i11;
  unsigned short i21;
  unsigned short i12;
  unsigned short i22;

  if (!console_marked_region_active_p)
    return;

  compute_marked_region (console_mark_x, console_mark_y,
			 console_point_x, console_point_y,
			 (&cx11), (&cy11), (&cx21), (&cy21));
  highlight_marked_region (cx11, cy11, cx21, cy21, '\0');

  compute_marked_region (console_mark_x, console_mark_y, x, y,
			 (&cx12), (&cy12), (&cx22), (&cy22));
  highlight_marked_region (cx12, cy12, cx22, cy22, '\1');

  i11 = ((cy11 * console_width) + cx11);
  i21 = ((cy21 * console_width) + cx21);
  i12 = ((cy12 * console_width) + cx12);
  i22 = ((cy22 * console_width) + cx22);

  if (i11 < i12)
    paint_marked_region_segment (cx11, cy11, cx12, cy12);
  else if (i12 < i11)
    paint_marked_region_segment (cx12, cy12, cx11, cy11);
  if (i21 < i22)
    paint_marked_region_segment (cx21, cy21, cx22, cy22);
  else if (i22 < i21)
    paint_marked_region_segment (cx22, cy22, cx21, cy21);

  console_point_x = x;
  console_point_y = y;
}

static void
unmark_marked_region (void)
{
  if (console_marked_region_active_p)
    {
      unsigned short cx1;
      unsigned short cy1;
      unsigned short cx2;
      unsigned short cy2;
      compute_marked_region (console_mark_x, console_mark_y,
			     console_point_x, console_point_y,
			     (&cx1), (&cy1), (&cx2), (&cy2));
      highlight_marked_region (cx1, cy1, cx2, cy2, '\0');
      paint_marked_region_segment (cx1, cy1, cx2, cy2);
      disable_marked_region ();
    }
}

static int
marked_region_nonempty_p (void)
{
  if (console_marked_region_active_p)
    {
      unsigned short cx1;
      unsigned short cy1;
      unsigned short cx2;
      unsigned short cy2;
      unsigned short y;
      compute_marked_region (console_mark_x, console_mark_y,
			     console_point_x, console_point_y,
			     (&cx1), (&cy1), (&cx2), (&cy2));
      return
	((cy1 < cy2)
	 || ((cx1 < cx2) && (cx1 < (console_line_lengths[cy1]))));
    }
  else
    return (0);
}

static char *
extract_marked_region (int cutp)
{
  if (console_marked_region_active_p)
    {
      unsigned short cx1;
      unsigned short cy1;
      unsigned short cx2;
      unsigned short cy2;
      unsigned short length;
      unsigned short y;
      char * result;
      char * scan;

      compute_marked_region (console_mark_x, console_mark_y,
			     console_point_x, console_point_y,
			     (&cx1), (&cy1), (&cx2), (&cy2));
      length = 1;
      for (y = cy1; (y <= cy2); y += 1)
	{
	  unsigned short xl = ((y == cy1) ? cx1 : 0);
	  unsigned short xh = ((y == cy2) ? cx2 : console_width);
	  unsigned short lx = (console_line_lengths[y]);
	  if (y > cy1)
	    length += 2;
	  if (xl < lx)
	    length += (((xh < lx) ? xh : lx) - xl);
	}
      if (length == 1)
	return (0);
      result = (OS_malloc (length));
      scan = result;
      for (y = cy1; (y <= cy2); y += 1)
	{
	  unsigned short xl = ((y == cy1) ? cx1 : 0);
	  unsigned short xh = ((y == cy2) ? cx2 : console_width);
	  unsigned short lx = (console_line_lengths[y]);
	  if (y > cy1)
	    {
	      (*scan++) = '\r';
	      (*scan++) = '\n';
	    }
	  if (xl < lx)
	    {
	      unsigned short ll = (((xh < lx) ? xh : lx) - xl);
	      FASTCOPY ((CHAR_LOC (xl, y)), scan, ll);
	      scan += ll;
	    }
	}
      (*scan) = '\0';
      if (cutp)
	{
	  unsigned short x1
	    = ((cx1 < (console_line_lengths[cy1]))
	       ? cx1
	       : (console_line_lengths[cy1]));
	  {
	    unsigned short d
	      = ((cx2 < (console_line_lengths[cy2]))
		 ? ((console_line_lengths[cy2]) - cx2)
		 : 0);
	    FASTCOPY ((CHAR_LOC (cx2, cy2)), (CHAR_LOC (x1, cy1)), d);
	    FASTFILL ((CHAR_LOC ((x1 + d), cy1)),
		      (console_width - (x1 + d)),
		      ' ');
	    FASTCOPY ((CHAR_HL (cx2, cy2)), (CHAR_HL (x1, cy1)), d);
	    FASTFILL ((CHAR_HL ((x1 + d), cy1)),
		      (console_width - (x1 + d)),
		      '\0');
	    (console_line_lengths[cy1]) = (x1 + d);
	  }
	  if (cy1 < cy2)
	    {
	      unsigned short d = (console_height - (cy2 + 1));
	      FASTCOPY ((CHAR_LOC (0, (cy2 + 1))),
			(CHAR_LOC (0, (cy1 + 1))),
			(d * console_width));
	      FASTCOPY ((CHAR_HL (0, (cy2 + 1))),
			(CHAR_HL (0, (cy1 + 1))),
			(d * console_width));
	      FASTCOPY ((LINE_LEN_LOC (cy2 + 1)),
			(LINE_LEN_LOC (cy1 + 1)),
			(d * (sizeof (unsigned short))));
	    }
	  if ((cy1 < point_y) || ((cy1 == point_y) && (x1 < point_x)))
	    {
	      if ((cy2 > point_y) || ((cy2 == point_y) && (cx2 >= point_x)))
		{
		  point_x = x1;
		  point_y = cy1;
		}
	      else if (cy2 < point_y)
		point_y -= (cy2 - cy1);
	      else
		point_x -= (cx2 - ((cy1 == cy2) ? x1 : 0));
	      OS2_window_move_cursor (console_wid,
				      (cx2x (point_x)),
				      (cy2y (point_y, 1)));
	    }
	  console_paint (0, console_width, cy1, console_height);
	}
      return (result);
    }
  else
    return (0);
}

static void
compute_marked_region (short x1, short y1, short x2, short y2,
		       unsigned short * cx1, unsigned short * cy1,
		       unsigned short * cx2, unsigned short * cy2)
{
  /* (cx1,cy1) is inclusive, and (cx2,cy2) is exclusive.  */
  unsigned short cx1a = (x2cx (x1, 1));
  unsigned short cy1a = (y2cy (y1, 0));
  unsigned short cx2a = (x2cx (x2, 1));
  unsigned short cy2a = (y2cy (y2, 0));
  if (((cy1a * console_width) + cx1a) > ((cy2a * console_width) + cx2a))
    {
      unsigned short cx = cx1a;
      unsigned short cy = cy1a;
      cx1a = cx2a;
      cy1a = cy2a;
      cx2a = cx;
      cy2a = cy;
    }
  if (cy1a >= console_height)
    {
      cx1a = (console_width - 1);
      cy1a = (console_height - 1);
    }
  else if (cx1a >= console_width)
    cx1a = (console_width - 1);
  if (cy2a >= console_height)
    {
      cx2a = 0;
      cy2a = console_height;
    }
  else if (cx2a > console_width)
    cx2a = console_width;
  (*cx1) = cx1a;
  (*cy1) = cy1a;
  (*cx2) = cx2a;
  (*cy2) = cy2a;
}

static void
highlight_marked_region (unsigned short cx1, unsigned short cy1,
			 unsigned short cx2, unsigned short cy2,
			 char hl)
{
  char * start = (CHAR_HL (cx1, cy1));
  FASTFILL (start, ((CHAR_HL (cx2, cy2)) - start), hl);
}

static void
paint_marked_region_segment (unsigned short x1, unsigned short y1,
			     unsigned short x2, unsigned short y2)
{
  if (y1 == y2)
    console_paint (x1, x2, y1, (y1 + 1));
  else
    {
      console_paint (x1, console_width, y1, (y1 + 1));
      if ((y1 + 1) < y2)
	console_paint (0, console_width, (y1 + 1), y2);
      console_paint (0, x2, y2, (y2 + 1));
    }
}

static void
disable_marked_region (void)
{
  console_marked_region_active_p = 0;
  enable_menu_copy_items (0);
}

static void
enable_menu_copy_items (int enablep)
{
  if (console_menu != NULLHANDLE)
    {
      USHORT value = (enablep ? 0 : MIA_DISABLED);
#if 0
      (void) OS2_menu_set_item_attributes
	(console_pm_qid, console_menu, IDM_CUT, TRUE, MIA_DISABLED, value);
#endif
      (void) OS2_menu_set_item_attributes
	(console_pm_qid, console_menu, IDM_COPY, TRUE, MIA_DISABLED, value);
    }
}

static void
console_resize (unsigned short new_pel_width, unsigned short new_pel_height)
{
  unsigned short new_width = (new_pel_width / CHAR_WIDTH);
  unsigned short new_height = (new_pel_height / CHAR_HEIGHT);
  char * new_chars;
  char * new_highlights;
  unsigned short * new_line_lengths;

  if ((console_chars != 0)
      && (new_width == console_width)
      && (new_height == console_height))
    return;

  new_chars = (OS_malloc (new_width * new_height));
  new_highlights = (OS_malloc (new_width * new_height));
  new_line_lengths = (OS_malloc ((sizeof (unsigned short)) * new_height));

  FASTFILL (new_chars, (new_width * new_height), ' ');
  FASTFILL (new_highlights, (new_width * new_height), '\0');
  FASTFILL (((char *) new_line_lengths),
	    ((sizeof (unsigned short)) * new_height),
	    0);

  if (console_chars != 0)
    {
      unsigned short xlim
	= ((new_width < console_width) ? new_width : console_width);
      unsigned short oy
	= (((point_y + 1) > new_height) ? ((point_y + 1) - new_height) : 0);
      unsigned short oylim
	= (oy + ((new_height < console_height) ? new_height : console_height));
      char * cfrom = (CHAR_LOC (0, oy));
      char * cto = new_chars;
      char * hfrom = (CHAR_HL (0, oy));
      char * hto = new_highlights;
      unsigned short ny = 0;
      while (oy < oylim)
	{
	  FASTCOPY (cfrom, cto, xlim);
	  FASTCOPY (hfrom, hto, xlim);
	  (new_line_lengths[ny]) = (console_line_lengths[oy]);
	  cfrom += console_width;
	  cto += new_width;
	  hfrom += console_width;
	  hto += new_width;
	  oy += 1;
	  ny += 1;
	}
      OS_free (console_chars);
      OS_free (console_highlights);
      OS_free (console_line_lengths);
    }
  console_pel_width = new_pel_width;
  console_pel_height = new_pel_height;
  console_width = new_width;
  console_height = new_height;
  console_chars = new_chars;
  console_highlights = new_highlights;
  console_line_lengths = new_line_lengths;
  if (point_x >= new_width)
    point_x = (new_width - 1);
  if ((point_y + 1) >= new_height)
    point_y -= ((point_y + 1) - new_height);
  OS2_window_move_cursor (console_wid, (cx2x (point_x)), (cy2y (point_y, 1)));
  OS2_window_invalidate (console_wid,
			 0, console_pel_width,
			 0, console_pel_height);
}

static void
console_paint (unsigned short cxl, unsigned short cxh,
	       unsigned short cyl, unsigned short cyh)
{
  if ((cxl < cxh) && (cyl < cyh))
    {
      COLOR foreground = (OS2_ps_get_foreground_color (console_psid));
      COLOR background = (OS2_ps_get_background_color (console_psid));
      unsigned short size = (cxh - cxl);
      char current_hl = '\0';
      while (cyl < cyh)
	{
	  unsigned short x = (cx2x (cxl));
	  unsigned short y = ((cy2y (cyl, 1)) + CHAR_DESCENDER);
	  char * cstart = (CHAR_LOC (cxl, cyl));
	  char * hstart = (CHAR_HL (cxl, cyl));
	  char * hend = (hstart + size);
	  while (hstart < hend)
	    {
	      unsigned short run_length = (compute_run_length (hstart, hend));
	      if (current_hl != (*hstart))
		{
		  if ((*hstart) == '\0')
		    OS2_ps_set_colors (console_psid, foreground, background);
		  else
		    OS2_ps_set_colors (console_psid, background, foreground);
		  current_hl = (*hstart);
		}
	      OS2_ps_draw_text (console_psid, x, y, cstart, run_length);
	      x += (run_length * CHAR_WIDTH);
	      cstart += run_length;
	      hstart += run_length;
	    }
	  cyl += 1;
	}
      if (current_hl != '\0')
	OS2_ps_set_colors (console_psid, foreground, background);
    }
}

static unsigned short
compute_run_length (const char * start, const char * end)
{
  if (start < end)
    {
      const char * scan = start;
      const char c = (*scan++);
      while (scan < end)
	if ((*scan) == c)
	  scan += 1;
	else
	  break;
      return (scan - start);
    }
  else
    return (0);
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
	process_events (OS2_msg_fifo_emptyp (pending_events));
	{
	  msg_t * message = (OS2_msg_fifo_remove (pending_events));
	  ULONG msg = (SM_PM_EVENT_MSG (message));
	  MPARAM mp1 = (SM_PM_EVENT_MP1 (message));
	  MPARAM mp2 = (SM_PM_EVENT_MP2 (message));
	  OS2_destroy_message (message);
	  switch (msg)
	    {
	    case WM_CHAR:
	      {
		unsigned short code;
		unsigned char repeat;
		if (translate_key_event (mp1, mp2, (&code), (&repeat)))
		  {
		    /* The feature that causes Delete and Backspace to
		       delete the marked region is disabled because it
		       is too much trouble to make the typeahead
		       buffer conform to the displayed characters.  */
#if 0
		    /* Delete and Backspace must discard the marked
		       region if there is one.  */
		    if ((code == '\177') && (repeat > 0))
		      {
			char * region = (extract_marked_region (1));
			if (region != 0)
			  {
			    OS_free (region);
			    repeat -= 1;
			  }
		      }
#endif
		    if (repeat > 0)
		      {
			readahead_char = code;
			readahead_repeat = repeat;
			goto do_read;
		      }
		  }
	      }
	      break;
	    case WM_CLOSE:
	      switch
		(WinMessageBox
		 (HWND_DESKTOP,
		  NULLHANDLE, /* client window handle */
		  "You have requested that this window be closed.\n\n"
		  "Press \"Yes\" to close this window and terminate Scheme; "
		  "doing so will discard data in unsaved Edwin buffers.\n\n"
		  "Press \"No\" to close only this window, leaving Scheme "
		  "running; the program will continue to run until the "
		  "next time it tries to read from the console.\n\n"
		  "Press \"Cancel\" if you don't want to close this window.",
		  "Terminate Scheme?",
		  0,
		  (MB_YESNOCANCEL | MB_WARNING)))
		  {
		  case MBID_YES:
		    termination_normal (0);
		    break;
		  case MBID_NO:
		    console_closedp = 1;
		    OS2_window_close (console_wid);
		    OS2_close_qid (console_event_qid);
		    OS2_close_std_tqueue (console_tqueue);
		    goto do_read;
		  }
	      break;
	    case WM_COMMAND:
	      {
		ULONG msg = (SHORT1FROMMP (mp1));
		switch (msg)
		  {
		  case IDM_PASTE:
		    if (do_paste ())
		      goto do_read;
		    break;
#if 0
		  /* IDM_CUT is disabled because it is too much
		     trouble to make the typeahead buffer conform to
		     the displayed characters.  */
		  case IDM_CUT:
#endif
		  case IDM_COPY:
		    grab_console_lock ();
		    {
		      char * region = (extract_marked_region (msg == IDM_CUT));
		      if (region != 0)
			{
			  OS2_clipboard_write_text (console_pm_qid, region);
			  OS_free (region);
			  unmark_marked_region ();
			}
		    }
		    release_console_lock ();
		    break;
		  }
	      }
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
do_paste (void)
{
  const char * text = (OS2_clipboard_read_text (console_pm_qid));
  if ((text != 0) && ((*text) != '\0'))
    {
      readahead_insert = text;
      readahead_insert_scan = text;
      return (1);
    }
  else
    {
      OS_free ((void *) text);
      return (0);
    }
}

static int
translate_key_event (MPARAM mp1, MPARAM mp2,
		     unsigned short * code, unsigned char * repeat)
{
  unsigned short flags;
  if (!OS2_translate_wm_char (mp1, mp2, code, (&flags), repeat))
    return (0);
  if ((flags & KC_VIRTUALKEY) != 0)
    switch (*code)
      {
      case VK_BACKSPACE:
      case VK_DELETE:
	(*code) = '\177';
	break;
      case VK_TAB:
	(*code) = '\t';
	break;
      case VK_ESC:
	(*code) = '\033';
	break;
      case VK_SPACE:
	(*code) = ' ';
	break;
      case VK_NEWLINE:
      case VK_ENTER:
	(*code) = '\r';
	break;
      default:
	return (0);
      }
  if (((*code) >= 0200) || ((flags & KC_ALT) != 0))
    return (0);
  if ((flags & KC_CTRL) != 0)
    if ((*code) >= 040)
      (*code) &= 037;
    else
      return (0);
  if ((*code) == 0)
    return (0);
  return (1);
}

void
OS2_pm_console_write (const char * data, size_t size)
{
  const char * end = (data + size);
  const char * nonprint;
  if (console_closedp)
    return;
  grab_console_lock ();
  unmark_marked_region ();
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
	    FASTFILL ((CHAR_HL (point_x, point_y)), size, '\0');
	    OS2_ps_draw_text (console_psid,
			      (cx2x (point_x)),
			      ((cy2y (point_y, 1)) + CHAR_DESCENDER),
			      data,
			      size);
	    data += size;
	    point_x += size;
	    (console_line_lengths[point_y]) = point_x;
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
    point_y += 1;
  else
    {
#ifdef CONSOLE_WRAP
      point_y = 0;
#else /* not CONSOLE_WRAP */
      point_y = (console_height - 1);
      FASTCOPY ((CHAR_LOC (0, 1)),
		(CHAR_LOC (0, 0)),
		(point_y * console_width));
      FASTCOPY ((CHAR_HL (0, 1)),
		(CHAR_HL (0, 0)),
		(point_y * console_width));
      FASTCOPY ((LINE_LEN_LOC (1)),
		(LINE_LEN_LOC (0)),
		(point_y * (sizeof (unsigned short))));
      OS2_window_scroll (console_wid,
			 0, console_pel_width,
			 0, (point_y * CHAR_HEIGHT),
			 0, CHAR_HEIGHT);
#endif /* not CONSOLE_WRAP */
    }
  FASTFILL ((CHAR_LOC (0, point_y)), console_width, ' ');
  FASTFILL ((CHAR_HL (0, point_y)), console_width, '\0');
  (console_line_lengths[point_y]) = 0;
  console_clear (0, console_width, point_y, (point_y + 1));
}

static void
do_formfeed (void)
{
  point_x = 0;
  point_y = 0;
  FASTFILL ((CHAR_LOC (0, 0)), (console_height * console_width), ' ');
  FASTFILL ((CHAR_HL (0, 0)), (console_height * console_width), '\0');
  FASTFILL ((LINE_LEN_LOC (0)),
	    (console_height * (sizeof (unsigned short))),
	    0);
  console_clear_all ();
}

static void
do_backspace (void)
{
  if (point_x > 0)
    {
      point_x -= 1;
      (console_line_lengths[point_y]) = point_x;
    }
}

static void
do_alert (void)
{
  WinAlarm (HWND_DESKTOP, WA_ERROR);
}
