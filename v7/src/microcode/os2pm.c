/* -*-C-*-

$Id: os2pm.c,v 1.1 1994/12/02 20:44:26 cph Exp $

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
#define INCL_GPI
#include "os2.h"

typedef struct
{
  HWND frame;			/* frame window handle */
  HWND client;			/* client window handle */
  HPS hps;			/* presentation space for client window */
  unsigned short char_width;	/* char width (max increment) */
  unsigned short char_ascender;	/* char ascender (max height above base) */
  unsigned short char_descender; /* char descender (max height below base) */
  unsigned short width;		/* width in characters */
  unsigned short height;	/* height in characters */
  char * charmap;		/* character array */
  unsigned short cursor_x;	/* x coordinate of the cursor */
  unsigned short cursor_y;	/* y coordinate of the cursor */
  qid_t event_qid;		/* qid to send input events to */
  twid_t twid;			/* twid for this twindow */
  unsigned int cursor_shownp : 1; /* nonzero if cursor is visible */
  unsigned int minimizingp : 1; /* nonzero if window being minimized */
  unsigned int minimizedp : 1;	/* nonzero if window is minimized */
} twindow_t;
#define TWINDOW_FRAME(twindow) ((twindow) -> frame)
#define TWINDOW_CLIENT(twindow) ((twindow) -> client)
#define TWINDOW_HPS(twindow) ((twindow) -> hps)
#define TWINDOW_CHAR_WIDTH(twindow) ((twindow) -> char_width)
#define TWINDOW_CHAR_ASCENDER(twindow) ((twindow) -> char_ascender)
#define TWINDOW_CHAR_DESCENDER(twindow) ((twindow) -> char_descender)
#define TWINDOW_WIDTH(twindow) ((twindow) -> width)
#define TWINDOW_HEIGHT(twindow) ((twindow) -> height)
#define TWINDOW_CHARMAP(twindow) ((twindow) -> charmap)
#define TWINDOW_CURSOR_X(twindow) ((twindow) -> cursor_x)
#define TWINDOW_CURSOR_Y(twindow) ((twindow) -> cursor_y)
#define TWINDOW_CURSOR_SHOWNP(twindow) ((twindow) -> cursor_shownp)
#define TWINDOW_EVENT_QID(twindow) ((twindow) -> event_qid)
#define TWINDOW_TWID(twindow) ((twindow) -> twid)
#define TWINDOW_MINIMIZINGP(twindow) ((twindow) -> minimizingp)
#define TWINDOW_MINIMIZEDP(twindow) ((twindow) -> minimizedp)

#define TWINDOW_CHAR_HEIGHT(twindow)					\
  ((TWINDOW_CHAR_ASCENDER (twindow)) + (TWINDOW_CHAR_DESCENDER (twindow)))

#define TWINDOW_CHAR_LOC(twindow, x, y)					\
  (& ((TWINDOW_CHARMAP (twindow)) [((y) * (TWINDOW_WIDTH (twindow))) + (x)]))

typedef struct
{
  tqueue_type_t type;
  HWND hwnd;
} pm_tqueue_t;
#define PM_TQUEUE_HWND(q) (((pm_tqueue_t *) (q)) -> hwnd)

static void simple_transaction (qid_t, msg_t *);
static void simple_reply (qid_t);

static void pm_thread_procedure (void *);
static tqueue_t * make_pm_tqueue (HWND);

static unsigned short cx2x (twindow_t *, unsigned short);
static unsigned short cy2y (twindow_t *, unsigned short, int);
static unsigned short x2cx (twindow_t *, unsigned short, int);
static unsigned short y2cy (twindow_t *, unsigned short, int);

static void initialize_twid_table (void);
static twid_t allocate_twid (twindow_t *);
static void deallocate_twid (twid_t);
static twindow_t * twid_to_twindow (twid_t);

static MRESULT EXPENTRY object_window_procedure (HWND, ULONG, MPARAM, MPARAM);

static msg_t * make_twindow_open_request (qid_t, const char *);
static void handle_twindow_open_request (msg_t *);
static msg_t * make_twindow_open_reply (twid_t);

static msg_t * make_twindow_close_request (twid_t);
static void handle_twindow_close_request (msg_t *);

static msg_t * make_twindow_write_request
  (twid_t, unsigned short, unsigned short, const char *, unsigned short);
static void handle_twindow_write_request (msg_t *);

static msg_t * make_twindow_move_cursor_request
  (twid_t, unsigned short, unsigned short);
static void handle_twindow_move_cursor_request (msg_t *);

static void handle_twindow_clear_request (msg_t *);
static msg_t * make_twindow_clear_request (twid_t);

static msg_t * make_twindow_clear_eol_request
  (twid_t, unsigned short, unsigned short);
static void handle_twindow_clear_eol_request (msg_t *);

static msg_t * make_twindow_scroll_request
  (twid_t, unsigned short, unsigned short, unsigned short, unsigned short,
   short, short);
static void handle_twindow_scroll_request (msg_t *);

static twindow_t * twindow_open (qid_t, const char *);
static twindow_t * make_twindow (qid_t);
static void twindow_close (twindow_t *);
static msg_t * make_close_event (twid_t);
static void twindow_write
  (twindow_t *, unsigned short, unsigned short, const char *, unsigned short);
static void twindow_clear (twindow_t *);
static void twindow_clear_eol (twindow_t *, unsigned short, unsigned short);
static void invalidate_partial_line
  (twindow_t *, unsigned short, unsigned short, unsigned short);
static void twindow_scroll
  (twindow_t *, unsigned short, unsigned short, unsigned short, unsigned short,
   short, short);
static void twindow_move_cursor (twindow_t *, unsigned short, unsigned short);

static MRESULT EXPENTRY frame_window_procedure (HWND, ULONG, MPARAM, MPARAM);

static MRESULT EXPENTRY twindow_procedure (HWND, ULONG, MPARAM, MPARAM);
static twindow_t * hwnd_to_twindow (HWND);

static void twindow_initialize (twindow_t *);
static void initialize_default_font (twindow_t *, PSZ, LONG, LONG);
static void initialize_attributes (twindow_t *);
static void set_twindow_char_dimensions (twindow_t *);
static void initialize_charmap (twindow_t *);

static int set_font_1 (HPS, PSZ, LONG, LONG);
static int use_font (HPS, PFONTMETRICS, LONG);

static void twindow_paint (twindow_t *);
static void draw_partial_line
  (twindow_t *, unsigned short, unsigned short, unsigned short);
static void activate_cursor (twindow_t *);
static void deactivate_cursor (twindow_t *);
static void show_cursor (twindow_t *);
static void hide_cursor (twindow_t *);
static int twindow_focusp (twindow_t *);

static void twindow_resize (twindow_t *, unsigned short, unsigned short);
static msg_t * make_resize_event (twid_t, unsigned short, unsigned short);

static int twindow_process_keychar
  (twindow_t *, unsigned short, unsigned char, unsigned char, unsigned short,
   unsigned short);
static void send_key_event
  (twindow_t *, unsigned short, unsigned short, unsigned short);
static msg_t * make_key_event
  (twid_t, unsigned short, unsigned short, unsigned short);

#define ID_RESOURCES 1
#define ID_FRAME 1

#define UWM_ENCAPSULATION WM_USER

#define QWP_TWINDOW QWL_USER

/* These should have been defined by PM header file.  */
#define MRVOID MRFROMP (0)
#define MRTRUE MRFROMLONG (TRUE)
#define MRFALSE MRFROMLONG (FALSE)

static qid_t pm_qid_local;	/* PM thread side */
static qid_t pm_qid_remote;	/* other thread side */
static TID pm_tid;
static HAB pm_hab;
static HMQ pm_hmq;
static HWND pm_object_window;
static PFNWP original_frame_window_procedure;

static const char object_class [] = "mit-scheme.object";
static const char twindow_class [] = "mit-scheme.twindow";

#define SEND_EVENT(twindow, message)					\
{									\
  if ((TWINDOW_EVENT_QID (twindow)) != QID_NONE)			\
    OS2_send_message ((TWINDOW_EVENT_QID (twindow)), (message));	\
}

#define FASTFILL(p, n, c)						\
{									\
  char * FASTFILL_scan = (p);						\
  char * FASTFILL_end = (FASTFILL_scan + (n));				\
  while (FASTFILL_scan < FASTFILL_end)					\
    (*FASTFILL_scan++) = (c);						\
}

#define window_error(name) OS2_logic_error (#name)

void
OS2_initialize_pm_thread (void)
{
  SET_MSG_TYPE_LENGTH (mt_twindow_open_request, sm_twindow_open_request_t);
  SET_MSG_TYPE_LENGTH (mt_twindow_open_reply, sm_twindow_open_reply_t);
  SET_MSG_TYPE_LENGTH (mt_twindow_close_request, sm_twindow_close_request_t);
  SET_MSG_TYPE_LENGTH (mt_twindow_write_request, sm_twindow_write_request_t);
  SET_MSG_TYPE_LENGTH (mt_twindow_move_cursor_request,
		       sm_twindow_move_cursor_request_t);
  SET_MSG_TYPE_LENGTH (mt_twindow_clear_request, sm_twindow_clear_request_t);
  SET_MSG_TYPE_LENGTH (mt_twindow_clear_eol_request,
		       sm_twindow_clear_eol_request_t);
  SET_MSG_TYPE_LENGTH (mt_twindow_scroll_request, sm_twindow_scroll_request_t);
  SET_MSG_TYPE_LENGTH (mt_key_event, sm_key_event_t);
  SET_MSG_TYPE_LENGTH (mt_button_event, sm_button_event_t);
  SET_MSG_TYPE_LENGTH (mt_close_event, sm_close_event_t);
  SET_MSG_TYPE_LENGTH (mt_visibility_event, sm_visibility_event_t);
  SET_MSG_TYPE_LENGTH (mt_resize_event, sm_resize_event_t);
  initialize_twid_table ();
  original_frame_window_procedure = 0;
  OS2_make_qid_pair ((&pm_qid_local), (&pm_qid_remote));
  OS2_open_qid (pm_qid_remote, OS2_scheme_tqueue);
  pm_tid = (OS2_beginthread (pm_thread_procedure, 0, 0x4000));
  /* Wait for init message from PM thread.  This message tells us that
     the other end of the connection is established and that it is
     safe to send messages on the connection.  */
  OS2_destroy_message (OS2_wait_for_message (pm_qid_remote, mt_init));
}

static void
simple_transaction (qid_t qid, msg_t * message)
{
  OS2_destroy_message
    (OS2_message_transaction (qid, message, mt_generic_reply));
}

static void
simple_reply (qid_t qid)
{
  OS2_send_message (qid, (OS2_create_message (mt_generic_reply)));
}

static void
pm_thread_procedure (void * arg)
{
  QMSG qmsg;

  if ((OS2_thread_initialize (pm_qid_local)) != 0)
    OS2_logic_error ("Error signalled within PM thread.");
  pm_hab = (WinInitialize (0));
  if (pm_hab == NULLHANDLE)
    window_error (WinInitialize);
  pm_hmq = (WinCreateMsgQueue (pm_hab, 0));
  if (pm_hmq == NULLHANDLE)
    window_error (WinCreateMsgQueue);
  if (!WinRegisterClass (pm_hab,
			 ((PSZ) object_class),
			 object_window_procedure,
			 0,	/* class style */
			 0))
    window_error (WinRegisterClass);
  if (!WinRegisterClass (pm_hab,
			 ((PSZ) twindow_class),
			 twindow_procedure,
			 0,	/* class style */
			 (sizeof (void *))))
    window_error (WinRegisterClass);
  pm_object_window
    = (WinCreateWindow (HWND_OBJECT,
			((PSZ) object_class),
			"",	/* text */
			0,	/* style */
			0, 0, 0, 0, /* size and position */
			NULLHANDLE, /* owner */
			HWND_BOTTOM,
			0,	/* ID */
			0,	/* control data */
			0	/* presentation parameters */
			));
  if (pm_object_window == NULLHANDLE)
    window_error (WinCreateWindow);
  OS2_open_qid (pm_qid_local, (make_pm_tqueue (pm_object_window)));
  OS2_send_message (pm_qid_local, (OS2_create_message (mt_init)));
  while (WinGetMsg (pm_hab, (&qmsg), 0, 0, 0))
    WinDispatchMsg (pm_hab, (&qmsg));
  if (!WinDestroyWindow (pm_object_window))
    window_error (WinDestroyWindow);
  WinDestroyMsgQueue (pm_hmq);
  WinTerminate (pm_hab);
}

static tqueue_t *
make_pm_tqueue (HWND hwnd)
{
  tqueue_t * tqueue = (OS_malloc (sizeof (pm_tqueue_t)));
  (TQUEUE_TYPE (tqueue)) = tqt_pm;
  (PM_TQUEUE_HWND (tqueue)) = hwnd;
  return (tqueue);
}

int
OS2_read_pm_tqueue (tqueue_t * tqueue, int blockp)
{
  OS2_logic_error ("Read from PM tqueue.");
  return (0);
}

void
OS2_write_pm_tqueue (tqueue_t * tqueue, msg_t * message)
{
  if (!WinPostMsg ((PM_TQUEUE_HWND (tqueue)),
		   UWM_ENCAPSULATION,
		   (MPFROMP (message)),
		   MPVOID))
    window_error (WinPostMsg);
}

static unsigned short
cx2x (twindow_t * twindow, unsigned short x)
{
  return (x * (TWINDOW_CHAR_WIDTH (twindow)));
}

static unsigned short
cy2y (twindow_t * twindow, unsigned short y, int lowerp)
{
  /* lowerp => result is bottommost pel of cell.  Otherwise result is
     bottommost pel of cell above.  */
  unsigned short height = (TWINDOW_HEIGHT (twindow));
  unsigned short limit = (lowerp ? (height - 1) : height);
  return (((y <= limit) ? (limit - y) : 0) * (TWINDOW_CHAR_HEIGHT (twindow)));
}

static unsigned short
x2cx (twindow_t * twindow, unsigned short x, int lowerp)
{
  /* lowerp => `x' is inclusive lower bound, and result is cell it
     falls in.  Otherwise, `x' is exclusive upper bound, and result is
     cell to its right, unless it falls on leftmost edge of cell.  If
     the argument is inclusive-lower, then the result is also;
     likewise for exclusive-upper.  */
  unsigned short cwidth = (TWINDOW_CHAR_WIDTH (twindow));
  unsigned short cx = (x / cwidth);
  return ((lowerp || ((x % cwidth) == 0)) ? cx : (cx + 1));
}

static unsigned short
y2cy (twindow_t * twindow, unsigned short y, int lowerp)
{
  /* lowerp => `y' is inclusive lower bound, and result is cell below
     the one it falls in.  Otherwise, `y' is exclusive upper bound,
     and result is cell it falls in, unless it falls on bottommost
     edge of cell, when result is cell below.  If the argument is
     inclusive-lower, then the result is exclusive-upper, and
     vice-versa.  */
  unsigned short cheight = (TWINDOW_CHAR_HEIGHT (twindow));
  short height = (TWINDOW_HEIGHT (twindow));
  short cy = ((height - 1) - ((short) (y / cheight)));
  if (lowerp || ((y % cheight) == 0))
    cy += 1;
  return ((cy < 0) ? 0 : cy);
}

static unsigned int twid_table_length;
static twindow_t ** twid_table;

static void
initialize_twid_table (void)
{
  twid_table_length = 16;
  twid_table = (OS_malloc ((sizeof (twindow_t *)) * twid_table_length));
  {
    twindow_t ** scan = twid_table;
    twindow_t ** end = (scan + twid_table_length);
    while (scan < end)
      (*scan++) = 0;
  }
}

static twid_t
allocate_twid (twindow_t * twindow)
{
  twid_t twid = 0;
  while (1)
    {
      if (twid == twid_table_length)
	{
	  twid_table_length *= 2;
	  twid_table
	    = (OS_realloc (twid_table,
			   ((sizeof (twindow_t *)) * twid_table_length)));
	  {
	    twindow_t ** scan = (twid_table + twid + 1);
	    twindow_t ** end = (twid_table + twid_table_length);
	    while (scan < end)
	      (*scan++) = 0;
	  }
	  break;
	}
      if ((twid_table [twid]) == 0)
	break;
      twid += 1;
    }
  (twid_table [twid]) = twindow;
  (TWINDOW_TWID (twindow)) = twid;
  return (twid);
}

static void
deallocate_twid (twid_t twid)
{
  (twid_table [twid]) = 0;
}

static twindow_t *
twid_to_twindow (twid_t twid)
{
  if ((twid_table [twid]) == 0)
    OS2_logic_error ("Invalid terminal window ID.");
  return (twid_table [twid]);
}

/* Implementation of the object window.  The object window handles
   encapsulated messages sent from the Scheme thread.  This defines
   the protocol used to communicate with the Scheme thread.  */

static MRESULT EXPENTRY
object_window_procedure (HWND window, ULONG msg, MPARAM mp1, MPARAM mp2)
{
  if (msg == UWM_ENCAPSULATION)
    {
      msg_t * message = (PVOIDFROMMP (mp1));
      switch (MSG_TYPE (message))
	{
	case mt_twindow_open_request:
	  handle_twindow_open_request (message);
	  break;
	case mt_twindow_close_request:
	  handle_twindow_close_request (message);
	  break;
	case mt_twindow_write_request:
	  handle_twindow_write_request (message);
	  break;
	case mt_twindow_move_cursor_request:
	  handle_twindow_move_cursor_request (message);
	  break;
	case mt_twindow_clear_request:
	  handle_twindow_clear_request (message);
	  break;
	case mt_twindow_clear_eol_request:
	  handle_twindow_clear_eol_request (message);
	  break;
	case mt_twindow_scroll_request:
	  handle_twindow_scroll_request (message);
	  break;
	default:
	  OS2_logic_error ("Unknown message type sent to PM thread.");
	  break;
	}
    }
  return (MRVOID);
}

twid_t
OS2_twindow_open (qid_t event_qid, const char * title)
{
  msg_t * reply
    = (OS2_message_transaction (pm_qid_remote,
				(make_twindow_open_request (event_qid, title)),
				mt_twindow_open_reply));
  twid_t twid = (SM_TWINDOW_OPEN_REPLY_TWID (reply));
  OS2_destroy_message (reply);
  return (twid);
}

static msg_t *
make_twindow_open_request (qid_t event_qid, const char * title)
{
  msg_t * message = (OS2_create_message (mt_twindow_open_request));
  (SM_TWINDOW_OPEN_REQUEST_EVENT_QID (message)) = event_qid;
  (SM_TWINDOW_OPEN_REQUEST_TITLE (message)) = title;
  return (message);
}

static void
handle_twindow_open_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  qid_t event_qid = (SM_TWINDOW_OPEN_REQUEST_EVENT_QID (message));
  const char * title = (SM_TWINDOW_OPEN_REQUEST_TITLE (message));
  OS2_destroy_message (message);
  OS2_send_message
    (sender,
     (make_twindow_open_reply
      (allocate_twid (twindow_open (event_qid, title)))));
}

static msg_t *
make_twindow_open_reply (twid_t twid)
{
  msg_t * message = (OS2_create_message (mt_twindow_open_reply));
  (SM_TWINDOW_OPEN_REPLY_TWID (message)) = twid;
  return (message);
}

void
OS2_twindow_close (twid_t twid)
{
  simple_transaction (pm_qid_remote, (make_twindow_close_request (twid)));
}

static msg_t *
make_twindow_close_request (twid_t twid)
{
  msg_t * message = (OS2_create_message (mt_twindow_close_request));
  (SM_TWINDOW_CLOSE_REQUEST_TWID (message)) = twid;
  return (message);
}

static void
handle_twindow_close_request (msg_t * message)
{
  qid_t qid = (MSG_SENDER (message));
  twid_t twid = (SM_TWINDOW_CLOSE_REQUEST_TWID (message));
  OS2_destroy_message (message);
  twindow_close (twid_to_twindow (twid));
  simple_reply (qid);
}

void
OS2_twindow_write (twid_t twid, unsigned short x, unsigned short y,
		   const char * data, unsigned short size)
{
  simple_transaction (pm_qid_remote,
		      (make_twindow_write_request (twid, x, y, data, size)));
}

static msg_t *
make_twindow_write_request (twid_t twid, unsigned short x, unsigned short y,
			    const char * data, unsigned short size)
{
  msg_t * message = (OS2_create_message (mt_twindow_write_request));
  (SM_TWINDOW_WRITE_REQUEST_TWID (message)) = twid;
  (SM_TWINDOW_WRITE_REQUEST_X (message)) = x;
  (SM_TWINDOW_WRITE_REQUEST_Y (message)) = y;
  (SM_TWINDOW_WRITE_REQUEST_DATA (message)) = data;
  (SM_TWINDOW_WRITE_REQUEST_SIZE (message)) = size;
  return (message);
}

static void
handle_twindow_write_request (msg_t * message)
{
  qid_t qid = (MSG_SENDER (message));
  twid_t twid = (SM_TWINDOW_WRITE_REQUEST_TWID (message));
  unsigned short x = (SM_TWINDOW_WRITE_REQUEST_X (message));
  unsigned short y = (SM_TWINDOW_WRITE_REQUEST_Y (message));
  const char * data = (SM_TWINDOW_WRITE_REQUEST_DATA (message));
  unsigned short size = (SM_TWINDOW_WRITE_REQUEST_SIZE (message));
  OS2_destroy_message (message);
  twindow_write ((twid_to_twindow (twid)), x, y, data, size);
  simple_reply (qid);
}

void
OS2_twindow_move_cursor (twid_t twid, unsigned short x, unsigned short y)
{
  simple_transaction (pm_qid_remote,
		      (make_twindow_move_cursor_request (twid, x, y)));
}

static msg_t *
make_twindow_move_cursor_request (twid_t twid, unsigned short x,
				  unsigned short y)
{
  msg_t * message = (OS2_create_message (mt_twindow_move_cursor_request));
  (SM_TWINDOW_MOVE_CURSOR_REQUEST_TWID (message)) = twid;
  (SM_TWINDOW_MOVE_CURSOR_REQUEST_X (message)) = x;
  (SM_TWINDOW_MOVE_CURSOR_REQUEST_Y (message)) = y;
  return (message);
}

static void
handle_twindow_move_cursor_request (msg_t * message)
{
  qid_t qid = (MSG_SENDER (message));
  twid_t twid = (SM_TWINDOW_MOVE_CURSOR_REQUEST_TWID (message));
  unsigned short x = (SM_TWINDOW_MOVE_CURSOR_REQUEST_X (message));
  unsigned short y = (SM_TWINDOW_MOVE_CURSOR_REQUEST_Y (message));
  OS2_destroy_message (message);
  twindow_move_cursor ((twid_to_twindow (twid)), x, y);
  simple_reply (qid);
}

void
OS2_twindow_clear (twid_t twid)
{
  simple_transaction (pm_qid_remote, (make_twindow_clear_request (twid)));
}

static void
handle_twindow_clear_request (msg_t * message)
{
  qid_t qid = (MSG_SENDER (message));
  twid_t twid = (SM_TWINDOW_CLEAR_REQUEST_TWID (message));
  OS2_destroy_message (message);
  twindow_clear (twid_to_twindow (twid));
  simple_reply (qid);
}

static msg_t *
make_twindow_clear_request (twid_t twid)
{
  msg_t * message = (OS2_create_message (mt_twindow_clear_request));
  (SM_TWINDOW_CLEAR_REQUEST_TWID (message)) = twid;
  return (message);
}

void
OS2_twindow_clear_eol (twid_t twid, unsigned short x, unsigned short y)
{
  simple_transaction (pm_qid_remote,
		      (make_twindow_clear_eol_request (twid, x, y)));
}

static void
handle_twindow_clear_eol_request (msg_t * message)
{
  qid_t qid = (MSG_SENDER (message));
  twid_t twid = (SM_TWINDOW_CLEAR_EOL_REQUEST_TWID (message));
  unsigned short x = (SM_TWINDOW_CLEAR_EOL_REQUEST_X (message));
  unsigned short y = (SM_TWINDOW_CLEAR_EOL_REQUEST_Y (message));
  OS2_destroy_message (message);
  twindow_clear_eol ((twid_to_twindow (twid)), x, y);
  simple_reply (qid);
}

static msg_t *
make_twindow_clear_eol_request (twid_t twid, unsigned short x,
				unsigned short y)
{
  msg_t * message = (OS2_create_message (mt_twindow_clear_eol_request));
  (SM_TWINDOW_CLEAR_EOL_REQUEST_TWID (message)) = twid;
  (SM_TWINDOW_CLEAR_EOL_REQUEST_X (message)) = x;
  (SM_TWINDOW_CLEAR_EOL_REQUEST_Y (message)) = y;
  return (message);
}

void
OS2_twindow_scroll (twid_t twid,
		    unsigned short x_start, unsigned short x_end,
		    unsigned short y_start, unsigned short y_end,
		    short x_delta, short y_delta)
{
  simple_transaction
    (pm_qid_remote,
     (make_twindow_scroll_request
      (twid, x_start, x_end, y_start, y_end, x_delta, y_delta)));
}

static msg_t *
make_twindow_scroll_request (twid_t twid,
			     unsigned short x_start, unsigned short x_end,
			     unsigned short y_start, unsigned short y_end,
			     short x_delta, short y_delta)
{
  msg_t * message = (OS2_create_message (mt_twindow_scroll_request));
  (SM_TWINDOW_SCROLL_REQUEST_TWID (message)) = twid;
  (SM_TWINDOW_SCROLL_REQUEST_X_START (message)) = x_start;
  (SM_TWINDOW_SCROLL_REQUEST_X_END (message)) = x_end;
  (SM_TWINDOW_SCROLL_REQUEST_Y_START (message)) = y_start;
  (SM_TWINDOW_SCROLL_REQUEST_Y_END (message)) = y_end;
  (SM_TWINDOW_SCROLL_REQUEST_X_DELTA (message)) = x_delta;
  (SM_TWINDOW_SCROLL_REQUEST_Y_DELTA (message)) = y_delta;
  return (message);
}

static void
handle_twindow_scroll_request (msg_t * message)
{
  qid_t qid = (MSG_SENDER (message));
  twid_t twid = (SM_TWINDOW_SCROLL_REQUEST_TWID (message));
  unsigned short x_start = (SM_TWINDOW_SCROLL_REQUEST_X_START (message));
  unsigned short x_end = (SM_TWINDOW_SCROLL_REQUEST_X_END (message));
  unsigned short y_start = (SM_TWINDOW_SCROLL_REQUEST_Y_START (message));
  unsigned short y_end = (SM_TWINDOW_SCROLL_REQUEST_Y_END (message));
  short x_delta = (SM_TWINDOW_SCROLL_REQUEST_X_DELTA (message));
  short y_delta = (SM_TWINDOW_SCROLL_REQUEST_Y_DELTA (message));
  OS2_destroy_message (message);
  twindow_scroll
    ((twid_to_twindow (twid)),
     x_start, x_end, y_start, y_end, x_delta, y_delta);
  simple_reply (qid);
}

static twindow_t *
twindow_open (qid_t event_qid, const char * title)
{
  twindow_t * twindow = (make_twindow (event_qid));
  FRAMECDATA frame_data;
  HWND frame_window;

  (frame_data . cb) = (sizeof (frame_data));
  (frame_data . flCreateFlags)
     = (FCF_TITLEBAR | FCF_SYSMENU | FCF_SHELLPOSITION | FCF_SIZEBORDER
	| FCF_MINMAX | FCF_TASKLIST);
  (frame_data . hmodResources) = NULLHANDLE;
  (frame_data . idResources) = ID_RESOURCES;
  frame_window
    = (WinCreateWindow (HWND_DESKTOP,
			WC_FRAME,
			((PSZ) title), /* title string */
			0,	/* window style */
			0, 0, 0, 0, /* size and position */
			NULLHANDLE, /* owner window */
			HWND_TOP,
			ID_FRAME,	/* window ID */
			(& frame_data),
			0));
  if (frame_window == NULLHANDLE)
    window_error (WinCreateWindow);
  (TWINDOW_FRAME (twindow)) = frame_window;
  {
    PFNWP procedure
      = (WinSubclassWindow (frame_window, frame_window_procedure));
    if (procedure == 0)
      window_error (WinSubclassWindow);
    if (original_frame_window_procedure == 0)
      original_frame_window_procedure = procedure;
    else if (original_frame_window_procedure != procedure)
      OS2_logic_error ("WinSubclassWindow returned two different answers.");
  }
  if ((WinCreateWindow (frame_window,
			((PSZ) twindow_class),
			0,	/* window text (class-specific) */
			0,	/* window style */
			0, 0, 0, 0, /* size and position */
			frame_window, /* owner window */
			HWND_BOTTOM,
			FID_CLIENT, /* window ID */
			twindow,
			0))
      == NULLHANDLE)
    window_error (WinCreateWindow);
  if (!WinShowWindow ((TWINDOW_FRAME (twindow)), TRUE))
    window_error (WinShowWindow);
  if (!WinShowWindow ((TWINDOW_CLIENT (twindow)), TRUE))
    window_error (WinShowWindow);
  return (twindow);
}

static twindow_t *
make_twindow (qid_t event_qid)
{
  twindow_t * twindow = (OS_malloc (sizeof (twindow_t)));
  (TWINDOW_FRAME (twindow)) = NULLHANDLE;
  (TWINDOW_CLIENT (twindow)) = NULLHANDLE;
  (TWINDOW_CHARMAP (twindow)) = 0;
  (TWINDOW_CURSOR_X (twindow)) = 0;
  (TWINDOW_CURSOR_Y (twindow)) = 0;
  (TWINDOW_CURSOR_SHOWNP (twindow)) = 1;
  (TWINDOW_EVENT_QID (twindow)) = event_qid;
  (TWINDOW_MINIMIZINGP (twindow)) = 0;
  (TWINDOW_MINIMIZEDP (twindow)) = 0;
  return (twindow);
}

static void
twindow_close (twindow_t * twindow)
{
  OS_free (TWINDOW_CHARMAP (twindow));
  if (!GpiDestroyPS (TWINDOW_HPS (twindow)))
    window_error (GpiDestroyPS);
  if (!WinDestroyWindow (TWINDOW_FRAME (twindow)))
    window_error (WinDestroyWindow);
  deallocate_twid (TWINDOW_TWID (twindow));
}

static msg_t *
make_close_event (twid_t twid)
{
  msg_t * message = (OS2_create_message (mt_close_event));
  (SM_CLOSE_EVENT_TWID (message)) = twid;
  return (message);
}

static void
twindow_write (twindow_t * twindow, unsigned short x, unsigned short y,
	       const char * data, unsigned short size)
{
  unsigned short width = (TWINDOW_WIDTH (twindow));
  unsigned short height = (TWINDOW_HEIGHT (twindow));
  if ((y < (TWINDOW_HEIGHT (twindow))) && (x < width))
    {
      char * target = (TWINDOW_CHAR_LOC (twindow, x, y));
      if (size > (width - x))
	size = (width - x);
      FASTCOPY (data, target, size);
      invalidate_partial_line (twindow, x, y, size);
    }
}

static void
twindow_clear (twindow_t * twindow)
{
  FASTFILL ((TWINDOW_CHARMAP (twindow)),
	    ((TWINDOW_WIDTH (twindow)) * (TWINDOW_HEIGHT (twindow))),
	    ' ');
  if (!WinInvalidateRect ((TWINDOW_CLIENT (twindow)), 0, FALSE))
    window_error (WinInvalidateRect);
}

static void
twindow_clear_eol (twindow_t * twindow, unsigned short x, unsigned short y)
{
  unsigned short width = (TWINDOW_WIDTH (twindow));
  if ((y < (TWINDOW_HEIGHT (twindow))) && (x < width))
    {
      unsigned short size = (width - x);
      FASTFILL ((TWINDOW_CHAR_LOC (twindow, x, y)), size, ' ');
      invalidate_partial_line (twindow, x, y, size);
    }
}

static void
invalidate_partial_line (twindow_t * twindow, unsigned short x,
			 unsigned short y, unsigned short size)
{
  RECTL rectl;
  (rectl . xLeft) = (cx2x (twindow, x));
  (rectl . xRight) = (cx2x (twindow, (x + size)));
  (rectl . yBottom) = (cy2y (twindow, y, 1));
  (rectl . yTop) = (cy2y (twindow, y, 0));
  if (!WinInvalidateRect ((TWINDOW_CLIENT (twindow)), (& rectl), FALSE))
    window_error (WinInvalidateRect);
}

static void
twindow_scroll (twindow_t * twindow,
		unsigned short x_start, unsigned short x_end,
		unsigned short y_start, unsigned short y_end,
		short x_delta, short y_delta)
{
  RECTL rectl;
  (rectl . xLeft) = (cx2x (twindow, x_start));
  (rectl . xRight) = (cx2x (twindow, x_end));
  (rectl . yBottom) = (cy2y (twindow, y_end, 1));
  (rectl . yTop) = (cy2y (twindow, y_start, 0));
  if ((WinScrollWindow ((TWINDOW_CLIENT (twindow)),
			(x_delta * (TWINDOW_CHAR_WIDTH (twindow))),
			(y_delta * (TWINDOW_CHAR_HEIGHT (twindow))),
			(& rectl),
			0,
			NULLHANDLE,
			0,
			0))
      == RGN_ERROR)
    window_error (WinScrollWindow);
}

static void
twindow_move_cursor (twindow_t * twindow, unsigned short x, unsigned short y)
{
  if ((x < (TWINDOW_WIDTH (twindow))) && (y < (TWINDOW_HEIGHT (twindow))))
    {
      (TWINDOW_CURSOR_X (twindow)) = x;
      (TWINDOW_CURSOR_Y (twindow)) = y;
      if (twindow_focusp (twindow))
	if (!WinCreateCursor ((TWINDOW_CLIENT (twindow)),
			      (cx2x (twindow, x)),
			      (cy2y (twindow, y, 1)),
			      0,
			      0,
			      CURSOR_SETPOS,
			      0))
	  window_error (WinCreateCursor);
    }
}

static MRESULT EXPENTRY
frame_window_procedure (HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
  twindow_t * twindow = (hwnd_to_twindow (WinWindowFromID (hwnd, FID_CLIENT)));
  switch (msg)
    {
    case WM_QUERYTRACKINFO:
      /* Set the tracking grid for the resize operation.  */
      {
	MRESULT mr
	  = ((* original_frame_window_procedure) (hwnd, msg, mp1, mp2));
	if (mr == MRTRUE)
	  {
	    PTRACKINFO pti = (PVOIDFROMMP (mp2));
	    if ((((pti -> fs) & TF_MOVE) != TF_MOVE)
		&& ((((pti -> fs) & TF_MOVE) != 0)
		    || (((pti -> fs) & TF_SETPOINTERPOS) != 0)))
	      {
		(pti -> fs) |= TF_GRID;
		(pti -> cxGrid) = (TWINDOW_CHAR_WIDTH (twindow));
		(pti -> cyGrid) = (TWINDOW_CHAR_HEIGHT (twindow));
		(pti -> cxKeyboard) = (TWINDOW_CHAR_WIDTH (twindow));
		(pti -> cyKeyboard) = (TWINDOW_CHAR_HEIGHT (twindow));
	      }
	  }
	return (mr);
      }
    case WM_MINMAXFRAME:
      /* If minimizing, mark the window to indicate this.  The client
	 will shortly receive a WM_SIZE which indicates that the
	 minimization has completed.  */
      {
	PSWP pswp = (PVOIDFROMMP (mp1));
	if ((!TWINDOW_MINIMIZEDP (twindow))
	    && (((pswp -> fl) & SWP_MINIMIZE) != 0))
	  {
	    (TWINDOW_MINIMIZINGP (twindow)) = 1;
	    (TWINDOW_MINIMIZEDP (twindow)) = 1;
	  }
	else if ((TWINDOW_MINIMIZEDP (twindow))
		 && (((pswp -> fl) & (SWP_RESTORE | SWP_MAXIMIZE)) != 0))
	  (TWINDOW_MINIMIZEDP (twindow)) = 0;
      }
      break;
    }
  return ((* original_frame_window_procedure) (hwnd, msg, mp1, mp2));
}

static MRESULT EXPENTRY
twindow_procedure (HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
  switch (msg)
    {
    case WM_CREATE:
      {
	twindow_t * twindow = (PVOIDFROMMP (mp1));
	(TWINDOW_CLIENT (twindow)) = hwnd;
	if (!WinSetWindowPtr (hwnd, QWP_TWINDOW, twindow))
	  window_error (WinSetWindowPtr);
	twindow_initialize (twindow);
	return (MRFALSE);
      }
    case WM_PAINT:
      {
	twindow_t * twindow = (hwnd_to_twindow (hwnd));
	if (((WinQueryWindowULong ((TWINDOW_FRAME (twindow)), QWL_STYLE))
	     & WS_MINIMIZED)
	    != 0)
	  break;
	twindow_paint (twindow);
	return (MRVOID);
      }
    case WM_SETFOCUS:
      {
	twindow_t * twindow = (hwnd_to_twindow (hwnd));
	if (SHORT1FROMMP (mp2))
	  activate_cursor (twindow);
	else
	  deactivate_cursor (twindow);
	return (MRVOID);
      }
    case WM_CHAR:
      return
	((twindow_process_keychar ((hwnd_to_twindow (hwnd)),
				   (SHORT1FROMMP (mp1)),
				   (CHAR3FROMMP (mp1)),
				   (CHAR4FROMMP (mp1)),
				   (SHORT1FROMMP (mp2)),
				   (SHORT2FROMMP (mp2))))
	 ? MRTRUE
	 : MRFALSE);
    case WM_CLOSE:
      {
	twindow_t * twindow = (hwnd_to_twindow (hwnd));
	SEND_EVENT (twindow, (make_close_event (TWINDOW_TWID (twindow))));
	return (MRVOID);
      }
    case WM_SIZE:
      {
	twindow_t * twindow = (hwnd_to_twindow (hwnd));
	/* If this message is part of a minimization, ignore it.  */
	if (TWINDOW_MINIMIZINGP (twindow))
	  {
	    (TWINDOW_MINIMIZINGP (twindow)) = 0;
	    (TWINDOW_MINIMIZEDP (twindow)) = 1;
	    break;
	  }
	twindow_resize (twindow, (SHORT1FROMMP (mp2)), (SHORT2FROMMP (mp2)));
	return (MRVOID);
      }
    case WM_SHOW:
    case WM_BUTTON1CLICK:
    case WM_BUTTON1DOWN:
    case WM_BUTTON1UP:
    case WM_BUTTON1DBLCLK:
    case WM_BUTTON2CLICK:
    case WM_BUTTON2DOWN:
    case WM_BUTTON2UP:
    case WM_BUTTON2DBLCLK:
    case WM_BUTTON3CLICK:
    case WM_BUTTON3DOWN:
    case WM_BUTTON3UP:
    case WM_BUTTON3DBLCLK:
    case WM_MOUSEMOVE:
    default:
      break;
    }
  return (WinDefWindowProc (hwnd, msg, mp1, mp2));
}

static twindow_t *
hwnd_to_twindow (HWND hwnd)
{
  twindow_t * twindow = (WinQueryWindowPtr (hwnd, QWP_TWINDOW));
  if (twindow == 0)
    window_error (WinQueryWindowPtr);
  return (twindow);
}

static void
twindow_initialize (twindow_t * twindow)
{
  SIZEL sizel;
  (sizel . cx) = 0;
  (sizel . cy) = 0;
  (TWINDOW_HPS (twindow))
    = (GpiCreatePS (pm_hab,
		    (WinOpenWindowDC (TWINDOW_CLIENT (twindow))),
		    (& sizel),
		    (PU_PELS | GPIF_DEFAULT | GPIT_MICRO | GPIA_ASSOC)));
  if ((TWINDOW_HPS (twindow)) == 0)
    window_error (GpiCreatePS);
  initialize_default_font (twindow, "System VIO", 40, 1);
  initialize_attributes (twindow);
  set_twindow_char_dimensions (twindow);
  initialize_charmap (twindow);
}

static void
initialize_default_font (twindow_t * twindow, PSZ font_name, LONG font_size,
			 LONG font_id)
{
  HPS hps = (TWINDOW_HPS (twindow));
  if ((!set_font_1 (hps, font_name, font_size, font_id))
      && (!set_font_1 (hps, "Courier", 100, font_id)))
    OS2_logic_error ("Unable to initialize default font.");
  {
    FONTMETRICS fm;
    if (!GpiQueryFontMetrics (hps, (sizeof (fm)), (& fm)))
      window_error (GpiQueryFontMetrics);
    (TWINDOW_CHAR_WIDTH (twindow)) = (fm . lMaxCharInc);
    (TWINDOW_CHAR_ASCENDER (twindow)) = (fm . lMaxBaselineExt);
    (TWINDOW_CHAR_DESCENDER (twindow)) = (fm . lMaxDescender);
  }
}

static void
initialize_attributes (twindow_t * twindow)
{
  CHARBUNDLE attrs;
#if 0
  LONG default_mask
    = (GpiQueryAttrs ((TWINDOW_HPS (twindow)),
		      PRIM_CHAR,
		      (CBB_COLOR | CBB_BACK_COLOR | CBB_MIX_MODE
		       | CBB_BACK_MIX_MODE | CBB_BOX | CBB_SET | CBB_MODE
		       | CBB_ANGLE | CBB_SHEAR | CBB_DIRECTION
		       | CBB_TEXT_ALIGN | CBB_EXTRA | CBB_BREAK_EXTRA),
		      (& attrs)));
  if (default_mask == GPI_ALTERROR)
    window_error (GpiQueryAttrs);
#endif
#if 0
  (attrs . lColor) = CLR_NEUTRAL;
  (attrs . lBackColor) = CLR_BACKGROUND;
  (attrs . usMixMode) = FM_OVERPAINT;
  (attrs . usBackMixMode) = BM_LEAVEALONE;
  if (!GpiSetAttrs ((TWINDOW_HPS (twindow)),
		    PRIM_CHAR,
		    (CBB_COLOR | CBB_BACK_COLOR | CBB_MIX_MODE
		     | CBB_BACK_MIX_MODE),
		    0,
		    (& attrs)))
    window_error (GpiSetAttrs);
#endif
}

static void
set_twindow_char_dimensions (twindow_t * twindow)
{
  SWP swp;
  if (!WinQueryWindowPos ((TWINDOW_CLIENT (twindow)), (& swp)))
    window_error (WinQueryWindowPos);
  (TWINDOW_WIDTH (twindow)) = ((swp . cx) / (TWINDOW_CHAR_WIDTH (twindow)));
  (TWINDOW_HEIGHT (twindow)) = ((swp . cy) / (TWINDOW_CHAR_HEIGHT (twindow)));
}

static void
initialize_charmap (twindow_t * twindow)
{
  unsigned short n = ((TWINDOW_WIDTH (twindow)) * (TWINDOW_HEIGHT (twindow)));
  if ((TWINDOW_CHARMAP (twindow)) != 0)
    OS_free (TWINDOW_CHARMAP (twindow));
  if (n == 0)
    (TWINDOW_CHARMAP (twindow)) = 0;
  else
    {
      (TWINDOW_CHARMAP (twindow)) = (OS_malloc (n));
      FASTFILL ((TWINDOW_CHARMAP (twindow)), n, ' ');
    }
}

static int
set_font_1 (HPS hps, PSZ font_name, LONG font_size, LONG font_id)
{
  LONG nfonts;
  ULONG index;
  PFONTMETRICS pfm;

  nfonts = 0;
  nfonts = (GpiQueryFonts (hps,
			   (QF_PUBLIC | QF_PRIVATE),
			   font_name,
			   (& nfonts),
			   (sizeof (FONTMETRICS)),
			   0));
  if (nfonts == GPI_ALTERROR)
    window_error (GpiQueryFonts);
  if (nfonts == 0)
    return (0);
  pfm = (OS_malloc (nfonts * (sizeof (FONTMETRICS))));
  if ((GpiQueryFonts (hps,
		      (QF_PUBLIC | QF_PRIVATE),
		      font_name,
		      (& nfonts),
		      (sizeof (FONTMETRICS)),
		      pfm))
      == GPI_ALTERROR)
    window_error (GpiQueryFonts);
  for (index = 0; (index < nfonts); index += 1)
    if (((((pfm [index]) . fsType) & FM_TYPE_FIXED) != 0)
	&& ((((pfm [index]) . fsDefn) & FM_DEFN_OUTLINE) == 0)
	&& (((pfm [index]) . sNominalPointSize) == font_size)
	&& (use_font (hps, (& (pfm [index])), font_id)))
      {
	OS_free (pfm);
	return (1);
      }
  OS_free (pfm);
  return (0);
}

static int
use_font (HPS hps, PFONTMETRICS pfm, LONG font_id)
{
  FATTRS font_attrs;

  (font_attrs . usRecordLength) = (sizeof (font_attrs));
  (font_attrs . fsSelection) = 0;
  (font_attrs . lMatch) = (pfm -> lMatch);
  strcpy ((font_attrs . szFacename), (pfm -> szFacename));
  (font_attrs . idRegistry) = 0;
  (font_attrs . usCodePage) = (WinQueryCp (pm_hmq));
  if ((font_attrs . usCodePage) == 0)
    window_error (WinQueryCp);
  (font_attrs . lMaxBaselineExt) = 0;
  (font_attrs . lAveCharWidth) = 0;
  (font_attrs . fsType) = 0;
  (font_attrs . fsFontUse) = 0;
  if ((GpiCreateLogFont (hps, 0, font_id, (& font_attrs))) != FONT_MATCH)
    return (0);
  GpiSetCharSet (hps, font_id);
  return (1);
}

static void
twindow_paint (twindow_t * twindow)
{
  RECTL rectl;
  HPS hps =
    (WinBeginPaint ((TWINDOW_CLIENT (twindow)),
		    (TWINDOW_HPS (twindow)),
		    (& rectl)));
  if (hps == NULLHANDLE)
    window_error (WinBeginPaint);
  if (!WinFillRect ((TWINDOW_HPS (twindow)), (& rectl), CLR_BACKGROUND))
    window_error (WinFillRect);
  {
    unsigned short xl = (x2cx (twindow, (rectl . xLeft), 1));
    unsigned short xh = (x2cx (twindow, (rectl . xRight), 0));
    unsigned short yl = (y2cy (twindow, (rectl . yTop), 0));
    unsigned short yh = (y2cy (twindow, (rectl . yBottom), 1));
    unsigned short size = (xh - xl);
    while (yl < yh)
      draw_partial_line (twindow, xl, (yl++), size);
  }
  if (!WinEndPaint (hps))
    window_error (WinEndPaint);
}

static void
draw_partial_line (twindow_t * twindow, unsigned short x, unsigned short y,
		   unsigned short size)
{
  HPS hps = (TWINDOW_HPS (twindow));
  char * target = (TWINDOW_CHAR_LOC (twindow, x, y));
  POINTL ptl;
  (ptl . x) = (cx2x (twindow, x));
  (ptl . y) = ((cy2y (twindow, y, 1)) + (TWINDOW_CHAR_DESCENDER (twindow)));
  if (size <= 512)
    GpiCharStringAt (hps, (& ptl), size, target);
  else
    {
      GpiMove (hps, (& ptl));
      while (size > 0)
	{
	  unsigned short n = ((size > 512) ? 512 : size);
	  GpiCharString (hps, n, target);
	  size -= n;
	  target += n;
	}
    }
}

static void
activate_cursor (twindow_t * twindow)
{
  if (!WinCreateCursor ((TWINDOW_CLIENT (twindow)),
			(cx2x (twindow, (TWINDOW_CURSOR_X (twindow)))),
			(cy2y (twindow, (TWINDOW_CURSOR_Y (twindow)), 1)),
			(TWINDOW_CHAR_WIDTH (twindow)),
			(TWINDOW_CHAR_HEIGHT (twindow)),
			(CURSOR_SOLID | CURSOR_FLASH),
			0))
    window_error (WinCreateCursor);
  if (TWINDOW_CURSOR_SHOWNP (twindow))
    if (!WinShowCursor ((TWINDOW_CLIENT (twindow)), TRUE))
      window_error (WinShowCursor);
}

static void
deactivate_cursor (twindow_t * twindow)
{
  if (!WinDestroyCursor (TWINDOW_CLIENT (twindow)))
    window_error (WinDestroyCursor);
}

static void
show_cursor (twindow_t * twindow)
{
  if (!TWINDOW_CURSOR_SHOWNP (twindow))
    {
      if (twindow_focusp (twindow))
	if (!WinShowCursor ((TWINDOW_CLIENT (twindow)), TRUE))
	  window_error (WinShowCursor);
      (TWINDOW_CURSOR_SHOWNP (twindow)) = 1;
    }
}

static void
hide_cursor (twindow_t * twindow)
{
  if (TWINDOW_CURSOR_SHOWNP (twindow))
    {
      if (twindow_focusp (twindow))
	if (!WinShowCursor ((TWINDOW_CLIENT (twindow)), FALSE))
	  window_error (WinShowCursor);
      (TWINDOW_CURSOR_SHOWNP (twindow)) = 0;
    }
}

static int
twindow_focusp (twindow_t * twindow)
{
  return ((TWINDOW_CLIENT (twindow)) == (WinQueryFocus (HWND_DESKTOP)));
}

static void
twindow_resize (twindow_t * twindow, unsigned short width,
		unsigned short height)
{
  unsigned short cx = (width / (TWINDOW_CHAR_WIDTH (twindow)));
  unsigned short cy = (height / (TWINDOW_CHAR_HEIGHT (twindow)));
  if ((cx == (TWINDOW_WIDTH (twindow))) && (cy == (TWINDOW_HEIGHT (twindow))))
    return;
  (TWINDOW_WIDTH (twindow)) = cx;
  (TWINDOW_HEIGHT (twindow)) = cy;
  initialize_charmap (twindow);
  SEND_EVENT (twindow, (make_resize_event ((TWINDOW_TWID (twindow)), cx, cy)));
}

static msg_t *
make_resize_event (twid_t twid, unsigned short width, unsigned short height)
{
  msg_t * message = (OS2_create_message (mt_resize_event));
  (SM_RESIZE_EVENT_TWID (message)) = twid;
  (SM_RESIZE_EVENT_WIDTH (message)) = width;
  (SM_RESIZE_EVENT_HEIGHT (message)) = height;
  return (message);
}

static int
twindow_process_keychar (twindow_t * twindow, unsigned short flags,
			 unsigned char repeat, unsigned char scan_code,
			 unsigned short char_code, unsigned short virtual_key)
{
  /* Ignore compound keys for now.  */
  if ((flags & (KC_DEADKEY | KC_COMPOSITE | KC_INVALIDCOMP | KC_KEYUP)) != 0)
    return (0);
  else if ((flags & KC_VIRTUALKEY) != 0)
    {
      send_key_event (twindow, virtual_key, flags, repeat);
      return (1);
    }
  else if ((flags & (KC_CHAR | KC_CTRL | KC_ALT)) != 0)
    {
      send_key_event (twindow, char_code, flags, repeat);
      return (1);
    }
  else
    return (0);
}

static void
send_key_event (twindow_t * twindow, unsigned short code,
		unsigned short flags, unsigned short repeat)
{
  SEND_EVENT
    (twindow,
     (make_key_event ((TWINDOW_TWID (twindow)), code, flags, repeat)));
}

static msg_t *
make_key_event (twid_t twid, unsigned short code,
		unsigned short flags, unsigned short repeat)
{
  msg_t * message = (OS2_create_message (mt_key_event));
  (SM_KEY_EVENT_TWID (message)) = twid;
  (SM_KEY_EVENT_CODE (message)) = code;
  (SM_KEY_EVENT_FLAGS (message)) = flags;
  (SM_KEY_EVENT_REPEAT (message)) = repeat;
  return (message);
}
