/* -*-C-*-

$Id: os2pm.c,v 1.35 2003/02/14 18:28:22 cph Exp $

Copyright (c) 1994-2000 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#define INCL_WIN
#define INCL_GPI
#include "os2.h"

extern void add_reload_cleanup (void (*) (void));
extern psid_t OS2_console_psid (void);
extern void OS2_console_font_change_hook (font_metrics_t *);

typedef enum { pst_window, pst_memory } pst_t;

typedef struct _ps_t
{
  psid_t id;			/* psid for this ps */
  qid_t qid;			/* qid to send commands to */
  HPS handle;
  COLOR foreground_color;
  COLOR background_color;
  pst_t visual_type;		/* window or bitmap */
  void * visual;		/* the associated window or bitmap */
  PLONG char_increments;	/* character increments for outline fonts */
} ps_t;
#define PS_ID(ps) ((ps) -> id)
#define PS_QID(ps) ((ps) -> qid)
#define PS_HANDLE(ps) ((ps) -> handle)
#define PS_FOREGROUND_COLOR(ps) ((ps) -> foreground_color)
#define PS_BACKGROUND_COLOR(ps) ((ps) -> background_color)
#define PS_VISUAL_TYPE(ps) ((ps) -> visual_type)
#define PS_VISUAL(ps) ((ps) -> visual)
#define PS_CHAR_INCREMENTS(ps) ((ps) -> char_increments)

typedef struct _window_t
{
  HWND frame;			/* frame window handle */
  HWND client;			/* client window handle */
  ps_t * client_ps;		/* presentation space for client window */
  unsigned short grid_x;	/* x dimension of resizing grid */
  unsigned short grid_y;	/* y dimension of resizing grid */
  short cursor_x;		/* x coordinate of the cursor */
  short cursor_y;		/* y coordinate of the cursor */
  unsigned short cursor_width;	/* width of the cursor */
  unsigned short cursor_height;	/* height of the cursor */
  unsigned short cursor_style;	/* style of the cursor */
  qid_t qid;			/* qid to send commands to */
  qid_t event_qid;		/* qid to send input events to */
  wid_t id;			/* wid for this window */
  unsigned int cursor_createdp : 1; /* nonzero if cursor created */
  unsigned int cursor_enabledp : 1; /* nonzero if cursor enabled */
  unsigned int minimizingp : 1; /* nonzero if window being minimized */
  unsigned int minimizedp : 1;	/* nonzero if window is minimized */
  unsigned int permanentp : 1;	/* nonzero means don't close on reload */
  unsigned int mousetrackp : 1;	/* nonzero means generate WM_MOUSEMOVE msgs */
} window_t;
#define WINDOW_FRAME(window) ((window) -> frame)
#define WINDOW_CLIENT(window) ((window) -> client)
#define WINDOW_CLIENT_PS(window) ((window) -> client_ps)
#define WINDOW_GRID_X(window) ((window) -> grid_x)
#define WINDOW_GRID_Y(window) ((window) -> grid_y)
#define WINDOW_CURSOR_X(window) ((window) -> cursor_x)
#define WINDOW_CURSOR_Y(window) ((window) -> cursor_y)
#define WINDOW_CURSOR_WIDTH(window) ((window) -> cursor_width)
#define WINDOW_CURSOR_HEIGHT(window) ((window) -> cursor_height)
#define WINDOW_CURSOR_STYLE(window) ((window) -> cursor_style)
#define WINDOW_QID(window) ((window) -> qid)
#define WINDOW_EVENT_QID(window) ((window) -> event_qid)
#define WINDOW_ID(window) ((window) -> id)
#define WINDOW_CURSOR_CREATEDP(window) ((window) -> cursor_createdp)
#define WINDOW_CURSOR_ENABLEDP(window) ((window) -> cursor_enabledp)
#define WINDOW_MINIMIZINGP(window) ((window) -> minimizingp)
#define WINDOW_MINIMIZEDP(window) ((window) -> minimizedp)
#define WINDOW_PERMANENTP(window) ((window) -> permanentp)
#define WINDOW_MOUSETRACKP(window) ((window) -> mousetrackp)

typedef struct _bitmap_t
{
  bid_t id;			/* bid for this bitmap */
  qid_t qid;			/* qid to send commands to */
  HBITMAP handle;
} bitmap_t;
#define BITMAP_ID(bitmap) ((bitmap) -> id)
#define BITMAP_QID(bitmap) ((bitmap) -> qid)
#define BITMAP_HANDLE(bitmap) ((bitmap) -> handle)

typedef struct
{
  tqueue_type_t type;
  HWND hwnd;
} pm_tqueue_t;
#define PM_TQUEUE_HWND(q) (((pm_tqueue_t *) (q)) -> hwnd)

typedef struct
{
  unsigned int length;
  void ** pointers;
} id_table_t;
#define ID_TABLE_LENGTH(table) ((table) -> length)
#define ID_TABLE_POINTERS(table) ((table) -> pointers)

/* This machine-generated file contains forward references and
   structure definitions for most of the procedures.  */
#include "os2pm-id.h"

static void window_pos (window_t *, short *, short *);
static void handle_window_pos_request (msg_t *);

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
} sm_pos_request_t;
#define SM_POS_REQUEST_WINDOW(m) (((sm_pos_request_t *) (m)) -> window)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  short x;
  short y;
} sm_pos_reply_t;
#define SM_POS_REPLY_X(m) (((sm_pos_reply_t *) (m)) -> x)
#define SM_POS_REPLY_Y(m) (((sm_pos_reply_t *) (m)) -> y)

static void window_size (window_t *, unsigned short *, unsigned short *);
static void handle_window_size_request (msg_t *);

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
} sm_size_request_t;
#define SM_SIZE_REQUEST_WINDOW(m) (((sm_size_request_t *) (m)) -> window)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  unsigned short width;
  unsigned short height;
} sm_size_reply_t;
#define SM_SIZE_REPLY_WIDTH(m) (((sm_size_reply_t *) (m)) -> width)
#define SM_SIZE_REPLY_HEIGHT(m) (((sm_size_reply_t *) (m)) -> height)

static void window_frame_size
  (window_t *, unsigned short *, unsigned short *);
static void handle_window_frame_size_request (msg_t *);

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
} sm_frame_size_request_t;
#define SM_FRAME_SIZE_REQUEST_WINDOW(m)					\
  (((sm_frame_size_request_t *) (m)) -> window)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  unsigned short width;
  unsigned short height;
} sm_frame_size_reply_t;
#define SM_FRAME_SIZE_REPLY_WIDTH(m)					\
  (((sm_frame_size_reply_t *) (m)) -> width)
#define SM_FRAME_SIZE_REPLY_HEIGHT(m)					\
  (((sm_frame_size_reply_t *) (m)) -> height)

static void handle_ps_set_bitmap_request (msg_t *);
static bitmap_t * ps_set_bitmap (ps_t *, bitmap_t *);

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  bitmap_t * bitmap;
} sm_ps_set_bitmap_request_t;
#define SM_PS_SET_BITMAP_REQUEST_PS(m)					\
  (((sm_ps_set_bitmap_request_t *) (m)) -> ps)
#define SM_PS_SET_BITMAP_REQUEST_BITMAP(m)				\
  (((sm_ps_set_bitmap_request_t *) (m)) -> bitmap)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  bitmap_t * bitmap;
} sm_ps_set_bitmap_reply_t;
#define SM_PS_SET_BITMAP_REPLY_BITMAP(m)				\
  (((sm_ps_set_bitmap_reply_t *) (m)) -> bitmap)

static void close_all_windows (void);

static void sync_transaction (qid_t, msg_t *);
static void sync_reply (qid_t);

static void pm_thread_procedure (void *);
static tqueue_t * make_pm_tqueue (HWND);

static void initialize_id_table (id_table_t *);
static unsigned int allocate_id (id_table_t *, void *);
static void deallocate_id (id_table_t *, unsigned int);
static void * id_to_pointer (id_table_t *, unsigned int);
static int id_validp (id_table_t *, unsigned int);
static ps_t * psid_to_ps (psid_t);
static window_t * wid_to_window (wid_t);
static bitmap_t * bid_to_bitmap (bid_t);

static MRESULT EXPENTRY object_window_procedure (HWND, ULONG, MPARAM, MPARAM);
static MRESULT EXPENTRY frame_window_procedure (HWND, ULONG, MPARAM, MPARAM);
static MRESULT EXPENTRY window_procedure (HWND, ULONG, MPARAM, MPARAM);

static window_t * hwnd_to_window (HWND);
static msg_t * make_pm_event (wid_t, ULONG, MPARAM, MPARAM);
static msg_t * make_paint_event
  (wid_t, unsigned short, unsigned short, unsigned short, unsigned short);

static void recreate_cursor (window_t *);
static void activate_cursor (window_t *);
static void deactivate_cursor (window_t *);

static window_t * make_window (qid_t, qid_t);

static void win_create_cursor (HWND, LONG, LONG, LONG, LONG, ULONG, PRECTL);
static void win_destroy_cursor (HWND);
static void win_show_cursor (HWND, BOOL);
static void recreate_cursor (window_t *);
static void activate_cursor (window_t *);
static void deactivate_cursor (window_t *);
static void maybe_activate_cursor (ps_t *);
static void maybe_deactivate_cursor (ps_t *);

static HDC get_ps_device (HPS);
static LONG get_device_capability (HDC, LONG);
static ps_t * create_ps (pst_t, HDC, qid_t);
static void destroy_ps (ps_t *);

static int ps_set_font (ps_t *, unsigned short, const char *);
static int parse_font_spec (const char *, PSZ *, LONG *, USHORT *);
static const char * unparse_font_spec (PSZ, LONG, USHORT);
static int ps_set_font_1 (ps_t * ps, PSZ, LONG, USHORT, LONG);
static PLONG ps_make_char_increments (LONG);
static int create_font (HPS, LONG, PFONTMETRICS, USHORT);
static void copy_fontmetrics_to_fattrs (FONTMETRICS *, FATTRS *);
static void ps_set_font_size (ps_t *, LONG);

#define ID_FRAME 1

#define UWM_ENCAPSULATION WM_USER

#define QWP_WINDOW QWL_USER

/* These should have been defined by PM header file.  */
#define MRVOID MRFROMP (0)
#define MRTRUE MRFROMLONG (TRUE)
#define MRFALSE MRFROMLONG (FALSE)

static id_table_t psid_table;
static id_table_t wid_table;
static id_table_t bid_table;
static qid_t pm_init_qid;
TID OS2_pm_tid;
static HAB pm_hab;
static HMQ pm_hmq;
static HWND pm_object_window;
static tqueue_t * pm_tqueue;
static PFNWP original_frame_window_procedure;
static window_t * capture_window;

static const char object_class [] = "mit-scheme.object";
static const char window_class [] = "mit-scheme.window";

#define SEND_EVENT(window, message)					\
{									\
  if ((WINDOW_EVENT_QID (window)) != QID_NONE)				\
    OS2_send_message ((WINDOW_EVENT_QID (window)), (message));		\
}

#define SEND_PM_EVENT(hwnd, msg, mp1, mp2)				\
{									\
  window_t * window = (hwnd_to_window (hwnd));				\
  SEND_EVENT (window,							\
	      (make_pm_event ((WINDOW_ID (window)), msg, mp1, mp2)));	\
}

#define window_error(name) window_error_1 (#name, 1)
#define window_warning(name) window_error_1 (#name, 0)

static void
window_error_1 (const char * name, int fatalp)
{
  char buffer [1024];
  ERRORID code = (WinGetLastError (pm_hab));
  if (fatalp)
    {
      sprintf (buffer, "Fatal error 0x%08x occurred in the %s procedure.",
	       code, name);
      OS2_logic_error (buffer);
    }
  else
    {
      sprintf (buffer, "Non-fatal error 0x%08x occurred in the %s procedure.  \
This indicates a bug in the Scheme implementation.  \
Please report this information to a Scheme wizard.",
	       code, name);
      (void) WinMessageBox (HWND_DESKTOP,
			    NULLHANDLE,
			    buffer,
			    "Scheme Error",
			    0,
			    (MB_OK | MB_WARNING));
    }
}

void
OS2_initialize_pm_thread (void)
{
  /* This machine-generated file contains code to initialize the
     message-type sizes for most of the procedure messages.  */
#include "os2pm-mi.h"

  SET_MSG_TYPE_LENGTH (mt_window_pos_request, sm_pos_request_t);
  SET_MSG_TYPE_LENGTH (mt_window_pos_reply, sm_pos_reply_t);
  SET_MSG_TYPE_LENGTH (mt_window_size_request, sm_size_request_t);
  SET_MSG_TYPE_LENGTH (mt_window_size_reply, sm_size_reply_t);
  SET_MSG_TYPE_LENGTH (mt_window_frame_size_request, sm_frame_size_request_t);
  SET_MSG_TYPE_LENGTH (mt_window_frame_size_reply, sm_frame_size_reply_t);

  SET_MSG_TYPE_LENGTH (mt_ps_set_bitmap_request, sm_ps_set_bitmap_request_t);
  SET_MSG_TYPE_LENGTH (mt_ps_set_bitmap_reply, sm_ps_set_bitmap_reply_t);

  SET_MSG_TYPE_LENGTH (mt_pm_event, sm_pm_event_t);
  SET_MSG_TYPE_LENGTH (mt_paint_event, sm_paint_event_t);

  initialize_id_table (& psid_table);
  initialize_id_table (& wid_table);
  initialize_id_table (& bid_table);
  original_frame_window_procedure = 0;
  capture_window = 0;
  {
    qid_t qid;
    OS2_make_qid_pair ((&pm_init_qid), (&qid));
    OS2_open_qid (qid, OS2_scheme_tqueue);
    OS2_pm_tid = (OS2_beginthread (pm_thread_procedure, 0, 0x8000));
    /* Wait for init message from PM thread.  This message tells us
       that the other end of the connection is established and that it
       is safe to send messages on the connection.  */
    OS2_destroy_message (OS2_wait_for_message (qid, mt_init));
    OS2_close_qid (qid);
  }
  add_reload_cleanup (close_all_windows);
}

static void
close_all_windows (void)
{
  window_t ** scan = ((window_t **) (ID_TABLE_POINTERS (& wid_table)));
  window_t ** end = (scan + (ID_TABLE_LENGTH (& wid_table)));
  while (scan < end)
    {
      window_t * window = (*scan++);
      if ((window != 0) && (!WINDOW_PERMANENTP (window)))
	window_close (window);
    }
}

/* Define this to cause a calling thread to wait for the PM thread to
   finish requests that have trivial replies.  Otherwise, the calling
   thread waits only when the request has a non-trivial reply.
   Usually there is no good reason to wait for trivial replies, but
   this could be useful during debugging.  */
/* #define SYNC_SIMPLE_TRANSACTIONS */
#ifdef SYNC_SIMPLE_TRANSACTIONS

#define simple_transaction sync_transaction
#define simple_reply sync_reply

#else

#define simple_transaction OS2_send_message
#define simple_reply(qid)

#endif

static void
sync_transaction (qid_t qid, msg_t * message)
{
  OS2_destroy_message
    (OS2_message_transaction (qid, message, mt_generic_reply));
}

static void
sync_reply (qid_t qid)
{
  OS2_send_message (qid, (OS2_create_message (mt_generic_reply)));
}

/* These macros simplify the code needed to perform message
   transactions, by hiding the many type-casts needed.  */

#define CREATE_MESSAGE(mt)						\
  ((void *) (OS2_create_message (mt)))

#define CREATE_MESSAGE_1(mt, extra)					\
  ((void *) (OS2_create_message_1 ((mt), (extra))))

#define DESTROY_MESSAGE(msg)						\
  OS2_destroy_message ((msg_t *) (msg))

#define SEND_MESSAGE(qid, msg)						\
  OS2_send_message ((qid), ((msg_t *) (msg)))

#define SIMPLE_TRANSACTION(qid, msg)					\
  simple_transaction ((qid), ((msg_t *) (msg)))

#define SYNC_TRANSACTION(qid, msg)					\
  sync_transaction ((qid), ((msg_t *) (msg)))

#define MESSAGE_TRANSACTION(qid, msg, mt)				\
  ((void *) (OS2_message_transaction ((qid), ((msg_t *) (msg)), (mt))))

#define MEMCPY(to, from, length)					\
  FASTCOPY (((const char *) (from)), ((char *) (to)), (length))

#define STRCPY(to, from)						\
  strcpy (((char *) (to)), (from))

static void
pm_thread_procedure (void * arg)
{
  EXCEPTIONREGISTRATIONRECORD registration;
  QMSG qmsg;

  if ((OS2_thread_initialize_1 ((&registration), QID_NONE)) != 0)
    OS2_logic_error ("Error signalled within PM thread.");
  pm_hab = (WinInitialize (0));
  if (pm_hab == NULLHANDLE)
    window_error (WinInitialize);
  pm_hmq = (WinCreateMsgQueue (pm_hab, 1000));
  if (pm_hmq == NULLHANDLE)
    window_error (WinCreateMsgQueue);
  if (!WinRegisterClass (pm_hab,
			 ((PSZ) object_class),
			 object_window_procedure,
			 0,	/* class style */
			 0))
    window_error (WinRegisterClass);
  if (!WinRegisterClass (pm_hab,
			 ((PSZ) window_class),
			 window_procedure,
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
  pm_tqueue = (make_pm_tqueue (pm_object_window));
  OS2_send_message (pm_init_qid, (OS2_create_message (mt_init)));
  while (WinGetMsg (pm_hab, (&qmsg), 0, 0, 0))
    WinDispatchMsg (pm_hab, (&qmsg));
  if (!WinDestroyWindow (pm_object_window))
    window_error (WinDestroyWindow);
  WinDestroyMsgQueue (pm_hmq);
  WinTerminate (pm_hab);
  /* There's no way to exit properly, because the normal exit depends
     on the PM thread being active enough to print the closing
     messages.  So just use exit.  */
  exit (1);
}

static tqueue_t *
make_pm_tqueue (HWND hwnd)
{
  tqueue_t * tqueue = (OS_malloc (sizeof (pm_tqueue_t)));
  (TQUEUE_TYPE (tqueue)) = tqt_pm;
  (PM_TQUEUE_HWND (tqueue)) = hwnd;
  return (tqueue);
}

msg_t *
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
    window_warning (WinPostMsg);
}

/* Object IDs

   These tables maintain data structures in the PM thread, and
   associate those structures with ID numbers that are given out to
   other threads (and to Scheme programs).  */

static void
initialize_id_table (id_table_t * table)
{
  unsigned int length = 16;
  void ** pointers = (OS_malloc ((sizeof (void *)) * length));
  void ** scan = pointers;
  void ** end = (scan + length);
  while (scan < end)
    (*scan++) = 0;
  (ID_TABLE_LENGTH (table)) = length;
  (ID_TABLE_POINTERS (table)) = pointers;
}

static unsigned int
allocate_id (id_table_t * table, void * pointer)
{
  unsigned int length = (ID_TABLE_LENGTH (table));
  void ** pointers = (ID_TABLE_POINTERS (table));
  void ** scan = (pointers + 1); /* don't allocate ID zero */
  void ** end = (pointers + length);
  while (scan < end)
    if ((*scan++) == 0)
      {
	(*--scan) = pointer;
	return (scan - pointers);
      }
  {
    unsigned int id = length;
    length *= 2;
    pointers = (OS_realloc (pointers, ((sizeof (void *)) * length)));
    scan = (pointers + id + 1);
    end = (pointers + length);
    while (scan < end)
      (*scan++) = 0;
    (ID_TABLE_LENGTH (table)) = length;
    (ID_TABLE_POINTERS (table)) = pointers;
    (pointers[id]) = pointer;
    return (id);
  }
}

static void
deallocate_id (id_table_t * table, unsigned int id)
{
  ((ID_TABLE_POINTERS (table)) [id]) = 0;
}

static void *
id_to_pointer (id_table_t * table, unsigned int id)
{
  void * pointer = ((ID_TABLE_POINTERS (table)) [id]);
  if (pointer == 0)
    OS2_logic_error ("Invalid PM ID.");
  return (pointer);
}

static int
id_validp (id_table_t * table, unsigned int id)
{
  return ((id > 0)
	  && (id < (ID_TABLE_LENGTH (table)))
	  && (((ID_TABLE_POINTERS (table)) [id]) != 0));
}

static ps_t *
psid_to_ps (psid_t psid)
{
  return (id_to_pointer ((& psid_table), psid));
}

static window_t *
wid_to_window (wid_t wid)
{
  return (id_to_pointer ((& wid_table), wid));
}

static bitmap_t *
bid_to_bitmap (bid_t bid)
{
  return (id_to_pointer ((& bid_table), bid));
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
	  /* This machine-generated file contains dispatch cases for
	     most of the procedure messages.  */
#include "os2pm-dc.h"

	case mt_window_pos_request:
	  handle_window_pos_request (message);
	  break;
	case mt_window_size_request:
	  handle_window_size_request (message);
	  break;
	case mt_window_frame_size_request:
	  handle_window_frame_size_request (message);
	  break;
	case mt_ps_set_bitmap_request:
	  handle_ps_set_bitmap_request (message);
	  break;

	default:
	  OS2_logic_error ("Unknown message type sent to PM thread.");
	  break;
	}
    }
  return (MRVOID);
}

/* Implementation of the Frame Window */

static MRESULT EXPENTRY
frame_window_procedure (HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
  window_t * window = (hwnd_to_window (WinWindowFromID (hwnd, FID_CLIENT)));
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
		(pti -> cxGrid) = (WINDOW_GRID_X (window));
		(pti -> cyGrid) = (WINDOW_GRID_Y (window));
		(pti -> cxKeyboard) = (WINDOW_GRID_X (window));
		(pti -> cyKeyboard) = (WINDOW_GRID_Y (window));
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
	if ((!WINDOW_MINIMIZEDP (window))
	    && (((pswp -> fl) & SWP_MINIMIZE) != 0))
	  {
	    (WINDOW_MINIMIZINGP (window)) = 1;
	    (WINDOW_MINIMIZEDP (window)) = 1;
	  }
	else if ((WINDOW_MINIMIZEDP (window))
		 && (((pswp -> fl) & (SWP_RESTORE | SWP_MAXIMIZE)) != 0))
	  (WINDOW_MINIMIZEDP (window)) = 0;
      }
      break;
    }
  return ((* original_frame_window_procedure) (hwnd, msg, mp1, mp2));
}

/* Implementation of the Client Window */

static MRESULT EXPENTRY
window_procedure (HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
  switch (msg)
    {
    case WM_CREATE:
      {
	window_t * window = (PVOIDFROMMP (mp1));
	if (!WinSetWindowPtr (hwnd, QWP_WINDOW, window))
	  window_error (WinSetWindowPtr);
	(WINDOW_CLIENT (window)) = hwnd;
	(WINDOW_CLIENT_PS (window))
	  = (create_ps (pst_window,
			(WinOpenWindowDC (hwnd)),
			(WINDOW_QID (window))));
	(PS_VISUAL (WINDOW_CLIENT_PS (window))) = window;
	return (MRFALSE);
      }
    case WM_PAINT:
      {
	window_t * window = (hwnd_to_window (hwnd));
	if (((WinQueryWindowULong ((WINDOW_FRAME (window)), QWL_STYLE))
	     & WS_MINIMIZED)
	    != 0)
	  break;
	{
	  HPS hps = (PS_HANDLE (WINDOW_CLIENT_PS (window)));
	  RECTL rectl;
	  if ((WinBeginPaint ((WINDOW_CLIENT (window)), hps, (& rectl)))
	      == NULLHANDLE)
	    window_error (WinBeginPaint);
	  if (!WinEndPaint (hps))
	    window_error (WinEndPaint);
	  SEND_EVENT (window,
		      (make_paint_event ((WINDOW_ID (window)),
					 (rectl . xLeft),
					 (rectl . xRight),
					 (rectl . yBottom),
					 (rectl . yTop))));
	}
	return (MRVOID);
      }
    case WM_SETFOCUS:
      {
	window_t * window = (hwnd_to_window (hwnd));
	if (SHORT1FROMMP (mp2))
	  recreate_cursor (window);
	else
	  {
	    win_destroy_cursor (WINDOW_CLIENT (window));
	    (WINDOW_CURSOR_CREATEDP (window)) = 0;
	  }
      }
      SEND_PM_EVENT (hwnd, msg, mp1, mp2);
      return (MRVOID);
    case WM_TRANSLATEACCEL:
      {
	PQMSG qmsg = (PVOIDFROMMP (mp1));
	USHORT flags = (SHORT1FROMMP (qmsg -> mp1));
	USHORT char_code = (SHORT1FROMMP (qmsg -> mp2));
	USHORT virtual_key = (SHORT2FROMMP (qmsg -> mp2));
	/* Disable specific default accelerator keys.  */
	if ((flags & KC_VIRTUALKEY) != 0)
	  switch (virtual_key)
	    {
	    case VK_ALT:
	    case VK_ALTGRAF:
	      /* Disable "Alt" keys, which normally pop up the system
		 menu.  These keys are used often in Edwin and the
		 default behavior is unacceptable.  */
	      return (MRFALSE);
	    case VK_SPACE:
	    case VK_ESC:
	    case VK_TAB:
	      /* Disable "Alt-SPC", "Alt-ESC", and "Alt-TAB", which
		 have standard key bindings in Edwin.  */
	      if ((flags & KC_ALT) != 0)
		return (MRFALSE);
	    }
	else if ((flags & KC_CHAR) != 0)
	  switch (char_code)
	    {
	    case ' ':
	    case '\033':
	    case '\t':
	      /* Disable "Alt-SPC", "Alt-ESC", and "Alt-TAB", if for
		 some reason they are reported as ASCII characters
		 rather than as virtual keys.  */
	      if ((flags & KC_ALT) != 0)
		return (MRFALSE);
	    }
	break;
      }
    case WM_DESTROY:
      {
	window_t * window = (hwnd_to_window (hwnd));
	destroy_ps (WINDOW_CLIENT_PS (window));
	(WINDOW_CLIENT_PS (window)) = 0;
	return (MRVOID);
      }
    case WM_SIZE:
      {
	window_t * window = (hwnd_to_window (hwnd));
	/* If this message is part of a minimization, ignore it.  */
	if (WINDOW_MINIMIZINGP (window))
	  {
	    (WINDOW_MINIMIZINGP (window)) = 0;
	    (WINDOW_MINIMIZEDP (window)) = 1;
	    break;
	  }
	if (WINDOW_CURSOR_CREATEDP (window))
	  {
	    win_destroy_cursor (WINDOW_CLIENT (window));
	    (WINDOW_CURSOR_CREATEDP (window)) = 0;
	    (WINDOW_CURSOR_X (window)) = 0;
	    (WINDOW_CURSOR_Y (window)) = 0;
	    recreate_cursor (window);
	  }
      }
      SEND_PM_EVENT (hwnd, msg, mp1, mp2);
      return (MRVOID);
    case WM_CLOSE:
    case WM_COMMAND:
    case WM_CONTROL:
    case WM_HELP:
    case WM_SHOW:
      SEND_PM_EVENT (hwnd, msg, mp1, mp2);
      return (MRVOID);
    case WM_CHAR:
    case WM_BUTTON1DOWN:
    case WM_BUTTON1UP:
    case WM_BUTTON1CLICK:
    case WM_BUTTON1DBLCLK:
    case WM_BUTTON2DOWN:
    case WM_BUTTON2UP:
    case WM_BUTTON2CLICK:
    case WM_BUTTON2DBLCLK:
    case WM_BUTTON3DOWN:
    case WM_BUTTON3UP:
    case WM_BUTTON3CLICK:
    case WM_BUTTON3DBLCLK:
      SEND_PM_EVENT (hwnd, msg, mp1, mp2);
      return (MRTRUE);
    case WM_MOUSEMOVE:
      if (WINDOW_MOUSETRACKP (hwnd_to_window (hwnd)))
	{
	  SEND_PM_EVENT (hwnd, msg, mp1, mp2);
	  return (MRTRUE);
	}
      break;
    default:
      break;
    }
  return (WinDefWindowProc (hwnd, msg, mp1, mp2));
}

static window_t *
hwnd_to_window (HWND hwnd)
{
  window_t * window = (WinQueryWindowPtr (hwnd, QWP_WINDOW));
  if (window == 0)
    window_error (WinQueryWindowPtr);
  return (window);
}

static msg_t *
make_pm_event (wid_t wid, ULONG msg, MPARAM mp1, MPARAM mp2)
{
  msg_t * message = (OS2_create_message (mt_pm_event));
  (SM_PM_EVENT_WID (message)) = wid;
  (SM_PM_EVENT_MSG (message)) = msg;
  (SM_PM_EVENT_MP1 (message)) = mp1;
  (SM_PM_EVENT_MP2 (message)) = mp2;
  return (message);
}

static msg_t *
make_paint_event (wid_t wid,
		  unsigned short xl, unsigned short xh,
		  unsigned short yl, unsigned short yh)
{
  msg_t * message = (OS2_create_message (mt_paint_event));
  (SM_PAINT_EVENT_WID (message)) = wid;
  (SM_PAINT_EVENT_XL (message)) = xl;
  (SM_PAINT_EVENT_XH (message)) = xh;
  (SM_PAINT_EVENT_YL (message)) = yl;
  (SM_PAINT_EVENT_YH (message)) = yh;
  return (message);
}

int
OS2_translate_wm_char (MPARAM mp1, MPARAM mp2,
		       unsigned short * code,
		       unsigned short * flags,
		       unsigned char * repeat)
{
  (*flags) = (SHORT1FROMMP (mp1));
  (*repeat) = (CHAR3FROMMP (mp1));
  /* Ignore compound keys for now.  */
  if (((*flags) & (KC_DEADKEY | KC_COMPOSITE | KC_INVALIDCOMP | KC_KEYUP))
      != 0)
    return (0);
  if (((*flags) & KC_VIRTUALKEY) != 0)
    {
      (*code) = (SHORT2FROMMP (mp2));
      return (1);
    }
  if (((*flags) & (KC_CHAR | KC_CTRL | KC_ALT)) != 0)
    {
      (*code) = (SHORT1FROMMP (mp2));
      return (1);
    }
  return (0);
}

/* Direct Operations

   These are exported operations that can be implemented directly in
   the calling thread.  Other operations that require communication
   with the PM thread appear on following pages.  */

int
OS2_psid_validp (psid_t psid)
{
  return (id_validp ((& psid_table), psid));
}

int
OS2_wid_validp (wid_t wid)
{
  return (id_validp ((& wid_table), wid));
}

int
OS2_bid_validp (bid_t bid)
{
  return (id_validp ((& bid_table), bid));
}

psid_t
OS2_window_client_ps (wid_t wid)
{
  return (PS_ID (WINDOW_CLIENT_PS (wid_to_window (wid))));
}

qid_t
OS2_create_pm_qid (tqueue_t * tqueue)
{
  qid_t pm_side;
  qid_t client_side;
  OS2_make_qid_pair ((&pm_side), (&client_side));
  OS2_open_qid (pm_side, pm_tqueue);
  OS2_open_qid (client_side, tqueue);
  return (client_side);
}

void
OS2_window_permanent (wid_t wid)
{
  (WINDOW_PERMANENTP (wid_to_window (wid))) = 1;
}

void
OS2_window_mousetrack (wid_t wid, int trackp)
{
  (WINDOW_MOUSETRACKP (wid_to_window (wid))) = trackp;
}

HWND
OS2_window_frame_handle (wid_t wid)
{
  return (WINDOW_FRAME (wid_to_window (wid)));
}

HWND
OS2_window_client_handle (wid_t wid)
{
  return (WINDOW_CLIENT (wid_to_window (wid)));
}

int
OS2_memory_ps_p (psid_t psid)
{
  return ((PS_VISUAL_TYPE (psid_to_ps (psid))) == pst_memory);
}

bid_t
OS2_ps_get_bitmap (psid_t psid)
{
  bitmap_t * bitmap = (PS_VISUAL (psid_to_ps (psid)));
  return ((bitmap == 0) ? BID_NONE : (BITMAP_ID (bitmap)));
}

/* Relayed Operations

   This page implements exported operations that require communication
   with the PM thread.  The PM-thread-side of these operations appear
   on the following pages; this page implements only the mechanism to
   communicate the operation to the PM thread.  The bulk of these
   communication procedures is machine-generated.  */

/* This macro supplies a NO-OP procedure needed by the
   machine-generated code for OS2_pm_synchronize.  */
#define pm_synchronize(qid)

/* This machine-generated file contains most of the external procedure
   definitions, and their associated handler procedures.  */
#include "os2pm-rp.h"

void
OS2_window_pos (wid_t wid, short * x, short * y)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_pos_request));
  (SM_POS_REQUEST_WINDOW (message)) = window;
  message
    = (OS2_message_transaction ((WINDOW_QID (window)),
				message,
				mt_window_pos_reply));
  (* x) = (SM_POS_REPLY_X (message));
  (* y) = (SM_POS_REPLY_Y (message));
  OS2_destroy_message (message);
}

static void
handle_window_pos_request (msg_t * request)
{
  qid_t sender = (MSG_SENDER (request));
  msg_t * reply = (OS2_create_message (mt_window_pos_reply));
  window_pos ((SM_POS_REQUEST_WINDOW (request)),
	      (& (SM_POS_REPLY_X (reply))),
	      (& (SM_POS_REPLY_Y (reply))));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

void
OS2_window_size (wid_t wid, unsigned short * width, unsigned short * height)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_size_request));
  (SM_SIZE_REQUEST_WINDOW (message)) = window;
  message
    = (OS2_message_transaction ((WINDOW_QID (window)),
				message,
				mt_window_size_reply));
  (* width) = (SM_SIZE_REPLY_WIDTH (message));
  (* height) = (SM_SIZE_REPLY_HEIGHT (message));
  OS2_destroy_message (message);
}

static void
handle_window_size_request (msg_t * request)
{
  qid_t sender = (MSG_SENDER (request));
  msg_t * reply = (OS2_create_message (mt_window_size_reply));
  window_size ((SM_SIZE_REQUEST_WINDOW (request)),
	       (& (SM_SIZE_REPLY_WIDTH (reply))),
	       (& (SM_SIZE_REPLY_HEIGHT (reply))));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

void
OS2_window_frame_size (wid_t wid,
		       unsigned short * width, unsigned short * height)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_frame_size_request));
  (SM_FRAME_SIZE_REQUEST_WINDOW (message)) = window;
  message
    = (OS2_message_transaction ((WINDOW_QID (window)),
				message,
				mt_window_frame_size_reply));
  (* width) = (SM_FRAME_SIZE_REPLY_WIDTH (message));
  (* height) = (SM_FRAME_SIZE_REPLY_HEIGHT (message));
  OS2_destroy_message (message);
}

static void
handle_window_frame_size_request (msg_t * request)
{
  qid_t sender = (MSG_SENDER (request));
  msg_t * reply = (OS2_create_message (mt_window_frame_size_reply));
  window_frame_size ((SM_FRAME_SIZE_REQUEST_WINDOW (request)),
		     (& (SM_FRAME_SIZE_REPLY_WIDTH (reply))),
		     (& (SM_FRAME_SIZE_REPLY_HEIGHT (reply))));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

bid_t
OS2_ps_set_bitmap (psid_t psid, bid_t bid)
{
  ps_t * ps = (psid_to_ps (psid));
  bitmap_t * bitmap = ((bid == BID_NONE) ? 0 : (bid_to_bitmap (bid)));
  msg_t * message = (OS2_create_message (mt_ps_set_bitmap_request));
  (SM_PS_SET_BITMAP_REQUEST_PS (message)) = ps;
  (SM_PS_SET_BITMAP_REQUEST_BITMAP (message)) = bitmap;
  message
    = (OS2_message_transaction ((PS_QID (ps)),
				message,
				mt_ps_set_bitmap_reply));
  bitmap = (SM_PS_SET_BITMAP_REPLY_BITMAP (message));
  OS2_destroy_message (message);
  return ((bitmap == 0) ? BID_NONE : (BITMAP_ID (bitmap)));
}

static void
handle_ps_set_bitmap_request (msg_t * request)
{
  qid_t sender = (MSG_SENDER (request));
  msg_t * reply = (OS2_create_message (mt_ps_set_bitmap_reply));
  (SM_PS_SET_BITMAP_REPLY_BITMAP (reply))
    = (ps_set_bitmap ((SM_PS_SET_BITMAP_REQUEST_PS (request)),
		      (SM_PS_SET_BITMAP_REQUEST_BITMAP (request))));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

font_metrics_t *
OS2_ps_set_font (psid_t psid, unsigned short id, const char * name)
{
  font_metrics_t * metrics = (OS2_ps_set_font_internal (psid, id, name));
  if ((metrics != 0) && (psid == (OS2_console_psid ())))
    OS2_console_font_change_hook (metrics);
  return (metrics);
}

/* PM-thread Operation Implementations

   All of the procedures from this point on are implementations of
   exported operations.  These implementations are the code that is
   run in the PM thread to implement the operations that are invoked
   in other threads.  */

/* Windows */

static wid_t
window_open (qid_t qid, qid_t event_qid, ULONG flags, HMODULE module, ULONG id,
	     ULONG style, const char * title)
{
  window_t * window = (make_window (qid, event_qid));
  FRAMECDATA frame_data;
  HWND frame_window;

  (frame_data . cb) = (sizeof (frame_data));
  (frame_data . flCreateFlags) = flags;
  (frame_data . hmodResources) = module;
  (frame_data . idResources) = id;
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
  (WINDOW_FRAME (window)) = frame_window;
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
			((PSZ) window_class),
			0,	/* window text (class-specific) */
			style,	/* window style */
			0, 0, 0, 0, /* size and position */
			frame_window, /* owner window */
			HWND_BOTTOM,
			FID_CLIENT, /* window ID */
			window,
			0))
      == NULLHANDLE)
    window_error (WinCreateWindow);
  return (WINDOW_ID (window));
}

static window_t *
make_window (qid_t qid, qid_t event_qid)
{
  window_t * window = (OS_malloc (sizeof (window_t)));
  (WINDOW_FRAME (window)) = NULLHANDLE;
  (WINDOW_CLIENT (window)) = NULLHANDLE;
  (WINDOW_CLIENT_PS (window)) = 0;
  (WINDOW_GRID_X (window)) = 1;
  (WINDOW_GRID_Y (window)) = 1;
  (WINDOW_CURSOR_X (window)) = 0;
  (WINDOW_CURSOR_Y (window)) = 0;
  (WINDOW_CURSOR_WIDTH (window)) = 0;
  (WINDOW_CURSOR_HEIGHT (window)) = 0;
  (WINDOW_CURSOR_STYLE (window)) = (CURSOR_SOLID | CURSOR_FLASH);
  (WINDOW_QID (window)) = qid;
  (WINDOW_EVENT_QID (window)) = event_qid;
  (WINDOW_ID (window)) = (allocate_id ((& wid_table), window));
  (WINDOW_CURSOR_CREATEDP (window)) = 0;
  (WINDOW_CURSOR_ENABLEDP (window)) = 0;
  (WINDOW_MINIMIZINGP (window)) = 0;
  (WINDOW_MINIMIZEDP (window)) = 0;
  (WINDOW_PERMANENTP (window)) = 0;
  (WINDOW_MOUSETRACKP (window)) = 0;
  return (window);
}

static void
window_close (window_t * window)
{
  if (!WinDestroyWindow (WINDOW_FRAME (window)))
    window_warning (WinDestroyWindow);
  deallocate_id ((& wid_table), (WINDOW_ID (window)));
  OS_free (window);
}

static void
window_show (window_t * window, int showp)
{
  if (!WinShowWindow ((WINDOW_FRAME (window)), showp))
    window_warning (WinShowWindow);
}

static void
window_scroll (window_t * window, short xl, short xh, short yl, short yh,
	       short x_delta, short y_delta)
{
  RECTL rectl;
  (rectl . xLeft) = xl;
  (rectl . xRight) = xh;
  (rectl . yBottom) = yl;
  (rectl . yTop) = yh;
  deactivate_cursor (window);
  if ((WinScrollWindow ((WINDOW_CLIENT (window)), x_delta, y_delta, (& rectl),
			0, NULLHANDLE, 0, 0))
      == RGN_ERROR)
    window_warning (WinScrollWindow);
  activate_cursor (window);
}

static void
window_invalidate (window_t * window, short xl, short xh, short yl, short yh)
{
  RECTL rectl;
  (rectl . xLeft) = xl;
  (rectl . xRight) = xh;
  (rectl . yBottom) = yl;
  (rectl . yTop) = yh;
  if (!WinInvalidateRect ((WINDOW_CLIENT (window)), (& rectl), FALSE))
    window_warning (WinInvalidateRect);
}

static void
window_set_grid (window_t * window, unsigned short x, unsigned short y)
{
  (WINDOW_GRID_X (window)) = x;
  (WINDOW_GRID_Y (window)) = y;
}

static void
window_activate (window_t * window)
{
  if (!WinSetActiveWindow (HWND_DESKTOP, (WINDOW_FRAME (window))))
    window_warning (WinSetActiveWindow);
}

static void
window_pos (window_t * window, short * x, short * y)
{
  SWP swp;
  if (!WinQueryWindowPos ((WINDOW_FRAME (window)), (& swp)))
    window_error (WinQueryWindowPos);
  (* x) = (swp . x);
  (* y) = (swp . y);
}

static void
window_set_pos (window_t * window, short x, short y)
{
  if (!WinSetWindowPos ((WINDOW_FRAME (window)), NULLHANDLE, x, y,
			0, 0, SWP_MOVE))
    window_warning (WinSetWindowPos);
}

static void
window_size (window_t * window,
	     unsigned short * width, unsigned short * height)
{
  SWP swp;
  if (!WinQueryWindowPos ((WINDOW_CLIENT (window)), (& swp)))
    window_error (WinQueryWindowPos);
  (* width) = (swp . cx);
  (* height) = (swp . cy);
}

static void
window_frame_size (window_t * window,
		   unsigned short * width, unsigned short * height)
{
  SWP swp;
  if (!WinQueryWindowPos ((WINDOW_FRAME (window)), (& swp)))
    window_error (WinQueryWindowPos);
  (* width) = (swp . cx);
  (* height) = (swp . cy);
}

static void
window_set_size (window_t * window,
		 unsigned short width, unsigned short height)
{
  RECTL rcl;
  (rcl . xLeft) = 0;
  (rcl . xRight) = width;
  (rcl . yBottom) = 0;
  (rcl . yTop) = height;
  if (!WinMapWindowPoints ((WINDOW_CLIENT (window)), HWND_DESKTOP,
			   ((PPOINTL) (& rcl)), 2))
    window_error (WinMapWindowPoints);
  if (!WinCalcFrameRect ((WINDOW_FRAME (window)), (& rcl), FALSE))
    window_error (WinCalcFrameRect);
  if (!WinSetWindowPos ((WINDOW_FRAME (window)),
			NULLHANDLE, 0, 0,
			((rcl . xRight) - (rcl . xLeft)),
			((rcl . yTop) - (rcl . yBottom)),
			SWP_SIZE))
    window_warning (WinSetWindowPos);
}

static int
window_focusp (window_t * window)
{
  return ((WINDOW_CLIENT (window)) == (WinQueryFocus (HWND_DESKTOP)));
}

static void
window_set_state (window_t * window, window_state_t state)
{
  ULONG op = 0;
  HWND behind = NULLHANDLE;
  switch (state)
    {
    case state_top:
      op = SWP_ZORDER;
      behind = HWND_TOP;
      break;
    case state_bottom:
      op = SWP_ZORDER;
      behind = HWND_BOTTOM;
      break;
    case state_show:
      op = SWP_SHOW;
      break;
    case state_hide:
      op = SWP_HIDE;
      break;
    case state_activate:
      op = SWP_ACTIVATE;
      break;
    case state_deactivate:
      op = SWP_DEACTIVATE;
      break;
    case state_minimize:
      op = SWP_MINIMIZE;
      break;
    case state_maximize:
      op = SWP_MAXIMIZE;
      break;
    case state_restore:
      op = SWP_RESTORE;
      break;
    }
  if (!WinSetWindowPos ((WINDOW_FRAME (window)), behind, 0, 0, 0, 0, op))
    window_warning (WinSetWindowPos);
}

static void
window_set_title (window_t * window, const char * title)
{
  if (!WinSetWindowText ((WINDOW_FRAME (window)), ((PSZ) title)))
    window_warning (WinSetWindowText);
}

static void
window_update_frame (window_t * window, USHORT flags)
{
  (void) WinSendMsg ((WINDOW_FRAME (window)), WM_UPDATEFRAME,
		     (MPFROMSHORT (flags)),
		     0);
}

static HWND
window_handle_from_id (qid_t qid, HWND window, ULONG id)
{
  return (WinWindowFromID (window, id));
}

static BOOL
window_set_capture (window_t * window, int capturep)
{
  if (capturep)
    {
      if (capture_window == 0)
	{
	  BOOL rc = (WinSetCapture (HWND_DESKTOP, (WINDOW_CLIENT (window))));
	  if (rc)
	    capture_window = window;
	  return (rc);
	}
      else
	return (capture_window == window);
    }
  else
    {
      capture_window = 0;
      return (WinSetCapture (HWND_DESKTOP, NULLHANDLE));
    }
}

static LONG
window_query_sys_value (qid_t qid, HWND window, LONG id)
{
  LONG value = (WinQuerySysValue (window, id));
  if (value == 0)
    window_error (WinQuerySysValue);
  return (value);
}

/* Text Cursors */

static void
window_move_cursor (window_t * window, short x, short y)
{
  (WINDOW_CURSOR_X (window)) = x;
  (WINDOW_CURSOR_Y (window)) = y;
  if (WINDOW_CURSOR_CREATEDP (window))
    win_create_cursor ((WINDOW_CLIENT (window)), x, y, 0, 0, CURSOR_SETPOS, 0);
}

static void
window_shape_cursor (window_t * window, unsigned short width,
		     unsigned short height, unsigned short style)
{
  (WINDOW_CURSOR_WIDTH (window)) = width;
  (WINDOW_CURSOR_HEIGHT (window)) = height;
  (WINDOW_CURSOR_STYLE (window)) = style;
  if (WINDOW_CURSOR_CREATEDP (window))
    recreate_cursor (window);
}

static void
window_show_cursor (window_t * window, int showp)
{
  if ((WINDOW_CURSOR_CREATEDP (window))
      && ((showp != 0) != (WINDOW_CURSOR_ENABLEDP (window))))
    win_show_cursor ((WINDOW_CLIENT (window)), showp);
  (WINDOW_CURSOR_ENABLEDP (window)) = (showp != 0);
}

/* Helper Procedures */

static void
win_create_cursor (HWND client, LONG x, LONG y, LONG cx, LONG cy, ULONG fs,
		   PRECTL clip_rectl)
{
  if (!WinCreateCursor (client, x, y, cx, cy, fs, clip_rectl))
    window_warning (WinCreateCursor);
}

static void
win_destroy_cursor (HWND client)
{
  if (!WinDestroyCursor (client))
    window_warning (WinDestroyCursor);
}

static void
win_show_cursor (HWND client, BOOL showp)
{
  if (!WinShowCursor (client, showp))
    window_warning (WinShowCursor);
}

static void
recreate_cursor (window_t * window)
{
  win_create_cursor ((WINDOW_CLIENT (window)),
		     (WINDOW_CURSOR_X (window)),
		     (WINDOW_CURSOR_Y (window)),
		     (WINDOW_CURSOR_WIDTH (window)),
		     (WINDOW_CURSOR_HEIGHT (window)),
		     (WINDOW_CURSOR_STYLE (window)),
		     0);
  (WINDOW_CURSOR_CREATEDP (window)) = 1;
  if (WINDOW_CURSOR_ENABLEDP (window))
    win_show_cursor ((WINDOW_CLIENT (window)), TRUE);
}

static void
activate_cursor (window_t * window)
{
  if ((WINDOW_CURSOR_CREATEDP (window)) && (WINDOW_CURSOR_ENABLEDP (window)))
    win_show_cursor ((WINDOW_CLIENT (window)), TRUE);
}

static void
deactivate_cursor (window_t * window)
{
  if ((WINDOW_CURSOR_CREATEDP (window)) && (WINDOW_CURSOR_ENABLEDP (window)))
    win_show_cursor ((WINDOW_CLIENT (window)), FALSE);
}

static void
maybe_activate_cursor (ps_t * ps)
{
  if ((PS_VISUAL_TYPE (ps)) == pst_window)
    activate_cursor (PS_VISUAL (ps));
}

static void
maybe_deactivate_cursor (ps_t * ps)
{
  if ((PS_VISUAL_TYPE (ps)) == pst_window)
    deactivate_cursor (PS_VISUAL (ps));
}

/* Presentation Spaces */

static ps_t *
create_memory_ps (qid_t qid)
{
  HDC hdc = (DevOpenDC (pm_hab, OD_MEMORY, "*", 0, 0, NULLHANDLE));
  if (hdc == DEV_ERROR)
    window_error (DevOpenDC);
  return (create_ps (pst_memory, hdc, qid));
}

static void
destroy_memory_ps (ps_t * ps)
{
  HDC hdc = (get_ps_device (PS_HANDLE (ps)));
  bitmap_t * bitmap = (PS_VISUAL (ps));
  destroy_ps (ps);
  if ((hdc != NULLHANDLE) && ((DevCloseDC (hdc)) == DEV_ERROR))
    window_warning (DevCloseDC);
  if (bitmap != 0)
    destroy_bitmap (bitmap);
}

static bitmap_t *
create_bitmap (ps_t * ps, USHORT width, USHORT height)
{
  HPS hps = (PS_HANDLE (ps));
  HDC hdc = (get_ps_device (hps));
  BITMAPINFOHEADER2 header;
  HBITMAP hbm;
  bitmap_t * bitmap;

  memset ((& header), 0, (sizeof (header)));
  (header . cbFix) = (sizeof (header));
  (header . cx) = width;
  (header . cy) = height;
  (header . cPlanes) = (get_device_capability (hdc, CAPS_COLOR_PLANES));
  (header . cBitCount) = (get_device_capability (hdc, CAPS_COLOR_BITCOUNT));
  hbm = (GpiCreateBitmap (hps, (& header), 0, 0, 0));
  if (hbm == GPI_ERROR)
    window_error (GpiCreateBitmap);
  bitmap = (OS_malloc (sizeof (bitmap_t)));
  (BITMAP_ID (bitmap)) = (allocate_id ((& bid_table), bitmap));
  (BITMAP_QID (bitmap)) = (PS_QID (ps));
  (BITMAP_HANDLE (bitmap)) = hbm;
  return (bitmap);
}

static void
destroy_bitmap (bitmap_t * bitmap)
{
  if (!GpiDeleteBitmap (BITMAP_HANDLE (bitmap)))
    window_warning (GpiDeleteBitmap);
  deallocate_id ((& bid_table), (BITMAP_ID (bitmap)));
  OS_free (bitmap);
}

static bitmap_t *
ps_set_bitmap (ps_t * ps, bitmap_t * bitmap)
{
  bitmap_t * previous_bitmap = (PS_VISUAL (ps));
  if ((GpiSetBitmap ((PS_HANDLE (ps)),
		     ((bitmap == 0) ? 0 : (BITMAP_HANDLE (bitmap)))))
      == HBM_ERROR)
    window_error (GpiSetBitmap);
  (PS_VISUAL (ps)) = bitmap;
  return (previous_bitmap);
}

static void
ps_bitblt (ps_t * target, ps_t * source, LONG npoints, PPOINTL points,
	   LONG rop, ULONG options)
{
  maybe_deactivate_cursor (target);
  if ((GpiBitBlt ((PS_HANDLE (target)), (PS_HANDLE (source)), npoints, points,
		  rop, options))
      == GPI_ERROR)
    window_warning (GpiBitBlt);
  maybe_activate_cursor (target);
}

static void
ps_draw_text (ps_t * ps, short x, short y,
	      const char * data, unsigned short size)
{
  HPS hps = (PS_HANDLE (ps));
  PLONG increments = (PS_CHAR_INCREMENTS (ps));
  POINTL ptl;
  (ptl . x) = x;
  (ptl . y) = y;
  maybe_deactivate_cursor (ps);
  if (size <= 512)
    {
      if (increments == 0)
	GpiCharStringAt (hps, (& ptl), size, ((char *) data));
      else
	GpiCharStringPosAt (hps, (& ptl), 0, CHS_VECTOR, size, ((char *) data),
			    increments);
    }
  else
    {
      const char * scan = data;
      GpiMove (hps, (& ptl));
      while (size > 0)
	{
	  unsigned short n = ((size > 512) ? 512 : size);
	  if (increments == 0)
	    GpiCharString (hps, n, ((char *) scan));
	  else
	    GpiCharStringPos (hps, 0, CHS_VECTOR, n, ((char *) scan),
			      increments);
	  size -= n;
	  scan += n;
	}
    }
  maybe_activate_cursor (ps);
}

static unsigned short
ps_text_width (ps_t * ps, const char * data, unsigned short size)
{
  if ((PS_CHAR_INCREMENTS (ps)) == 0)
    {
      POINTL points [TXTBOX_COUNT];
      if (!GpiQueryTextBox ((PS_HANDLE (ps)), size, ((char *) data),
			    TXTBOX_COUNT, points))
	window_error (GpiQueryTextBox);
      return ((points [TXTBOX_CONCAT]) . x);
    }
  else
    return (size * ((PS_CHAR_INCREMENTS (ps)) [0]));
}

static void
ps_clear (ps_t * ps, short xl, short xh, short yl, short yh)
{
  RECTL rectl;
  (rectl . xLeft) = xl;
  (rectl . xRight) = xh;
  (rectl . yBottom) = yl;
  (rectl . yTop) = yh;
  maybe_deactivate_cursor (ps);
  if (!WinFillRect ((PS_HANDLE (ps)), (&rectl), (PS_BACKGROUND_COLOR (ps))))
    window_warning (WinFillRect);
  maybe_activate_cursor (ps);
}

static COLOR
ps_get_foreground_color (ps_t * ps)
{
  return (PS_FOREGROUND_COLOR (ps));
}

static COLOR
ps_get_background_color (ps_t * ps)
{
  return (PS_BACKGROUND_COLOR (ps));
}

static void
ps_set_colors (ps_t * ps, COLOR foreground, COLOR background)
{
  if (!GpiSetColor ((PS_HANDLE (ps)), foreground))
    window_warning (GpiSetColor);
  if (!GpiSetMix ((PS_HANDLE (ps)), FM_OVERPAINT))
    window_warning (GpiSetMix);
  if (!GpiSetBackColor ((PS_HANDLE (ps)), background))
    window_warning (GpiSetBackColor);
  if (!GpiSetBackMix ((PS_HANDLE (ps)), BM_OVERPAINT))
    window_warning (GpiSetBackMix);
  (PS_FOREGROUND_COLOR (ps)) = foreground;
  (PS_BACKGROUND_COLOR (ps)) = background;
}

static void
ps_move_gcursor (ps_t * ps, short x, short y)
{
  POINTL ptl;
  (ptl . x) = x;
  (ptl . y) = y;
  if (!GpiMove ((PS_HANDLE (ps)), (& ptl)))
    window_warning (GpiMove);
}

static void
ps_draw_line (ps_t * ps, short x, short y)
{
  POINTL ptl;
  (ptl . x) = x;
  (ptl . y) = y;
  if ((GpiLine ((PS_HANDLE (ps)), (& ptl))) == GPI_ERROR)
    window_warning (GpiLine);
}

static void
ps_draw_point (ps_t * ps, short x, short y)
{
  POINTL ptl;
  (ptl . x) = x;
  (ptl . y) = y;
  if ((GpiSetPel ((PS_HANDLE (ps)), (& ptl))) == GPI_ERROR)
    window_warning (GpiSetPel);
}

static void
ps_poly_line (ps_t * ps, unsigned long npoints, PPOINTL points)
{
  if ((GpiPolyLine ((PS_HANDLE (ps)), npoints, points)) == GPI_ERROR)
    window_warning (GpiPolyLine);
}

static void
ps_poly_line_disjoint (ps_t * ps, unsigned long npoints, PPOINTL points)
{
  if ((GpiPolyLineDisjoint ((PS_HANDLE (ps)), npoints, points))
      == GPI_ERROR)
    window_warning (GpiPolyLineDisjoint);
}

static void
ps_set_line_type (ps_t * ps, LONG type)
{
  if (!GpiSetLineType ((PS_HANDLE (ps)), type))
    window_warning (GpiSetLineType);
}

static void
ps_set_mix (ps_t * ps, LONG mix)
{
  if (!GpiSetMix ((PS_HANDLE (ps)), mix))
    window_warning (GpiSetMix);
}

static void
ps_query_caps (ps_t * ps, LONG start, LONG count, PLONG values)
{
  HDC hdc = (get_ps_device (PS_HANDLE (ps)));
  if (hdc == NULLHANDLE)
    window_error (GpiQueryDevice);
  if (!DevQueryCaps (hdc, start, count, values))
    window_error (DevQueryCaps);
}

static void
ps_reset_clip_rectangle (ps_t * ps)
{
  if (!GpiSetClipPath ((PS_HANDLE (ps)), 0, SCP_RESET))
    window_error (GpiSetClipPath);
}

static void
ps_set_clip_rectangle (ps_t * ps, short xl, short xh, short yl, short yh)
{
  HPS hps = (PS_HANDLE (ps));
  ps_reset_clip_rectangle (ps);
  if (!GpiBeginPath (hps, 1))
    window_error (GpiBeginPath);
  {
    POINTL points [4];
    ((points[0]) . x) = xl;
    ((points[0]) . y) = yl;
    ((points[1]) . x) = xl;
    ((points[1]) . y) = yh;
    ((points[2]) . x) = xh;
    ((points[2]) . y) = yh;
    ((points[3]) . x) = xh;
    ((points[3]) . y) = yl;
    if (!GpiMove (hps, (&points[3])))
      window_warning (GpiMove);
    if ((GpiPolyLine (hps, 4, points)) == GPI_ERROR)
      window_warning (GpiPolyLine);
  }
  if (!GpiEndPath (hps))
    window_error (GpiEndPath);
  if (!GpiSetClipPath (hps, 1, (SCP_AND | SCP_INCL)))
    window_error (GpiSetClipPath);
}

static void
get_bitmap_parameters (bitmap_t * bitmap, PBITMAPINFOHEADER params)
{
  if (!GpiQueryBitmapParameters ((BITMAP_HANDLE (bitmap)), params))
    window_error (GpiQueryBitmapParameters);
}

static unsigned long
ps_get_bitmap_bits (ps_t * ps, unsigned long start, unsigned long length,
		    PBYTE data, PBITMAPINFO2 info)
{
  LONG r = (GpiQueryBitmapBits ((PS_HANDLE (ps)), start, length, data, info));
  if (r < 0)
    window_error (GpiQueryBitmapBits);
  return (r);
}

static unsigned long
ps_set_bitmap_bits (ps_t * ps, unsigned long start, unsigned long length,
		    PBYTE data, PBITMAPINFO2 info)
{
  LONG r = (GpiSetBitmapBits ((PS_HANDLE (ps)), start, length, data, info));
  if (r < 0)
    window_error (GpiSetBitmapBits);
  return (r);
}

/* Helper Procedures */

static HDC
get_ps_device (HPS hps)
{
  HDC hdc = (GpiQueryDevice (hps));
  if (hdc == HDC_ERROR)
    window_error (GpiQueryDevice);
  return (hdc);
}

static LONG
get_device_capability (HDC hdc, LONG index)
{
  LONG result;
  if (!DevQueryCaps (hdc, index, 1, (& result)))
    window_error (DevQueryCaps);
  return (result);
}

static ps_t *
create_ps (pst_t type, HDC hdc, qid_t qid)
{
  ps_t * ps = (OS_malloc (sizeof (ps_t)));
  SIZEL sizel;
  HPS hps;
  (sizel . cx) = 0;
  (sizel . cy) = 0;
  hps = (GpiCreatePS (pm_hab, hdc, (& sizel),
		      (PU_PELS | GPIF_DEFAULT | GPIT_MICRO | GPIA_ASSOC)));
  if (hps == 0)
    window_error (GpiCreatePS);
  /* Put color table in RGB mode so we can specify colors
     directly in RGB values rather than as indices.  */
  if (!GpiCreateLogColorTable (hps, LCOL_PURECOLOR, LCOLF_RGB, 0, 0, 0))
    window_warning (GpiCreateLogColorTable);
  (PS_HANDLE (ps)) = hps;
  (PS_ID (ps)) = (allocate_id ((& psid_table), ps));
  (PS_QID (ps)) = qid;
  (PS_VISUAL_TYPE (ps)) = type;
  (PS_VISUAL (ps)) = 0;
  (PS_CHAR_INCREMENTS (ps)) = 0;
  ps_set_colors (ps, RGB_BLACK, RGB_WHITE);
  return (ps);
}

static void
destroy_ps (ps_t * ps)
{
  if ((PS_CHAR_INCREMENTS (ps)) != 0)
    OS_free (PS_CHAR_INCREMENTS (ps));
  if (!GpiDestroyPS (PS_HANDLE (ps)))
    window_warning (GpiDestroyPS);
  deallocate_id ((& psid_table), (PS_ID (ps)));
  OS_free (ps);
}

/* Clipboard */

static void
clipboard_write_text (qid_t qid, const char * text)
{
  unsigned int len = ((strlen (text)) + 1);
  PVOID shared_copy;
  int copy_used = 0;

  STD_API_CALL
    (dos_alloc_shared_mem,
     ((&shared_copy), 0, len,
      (PAG_COMMIT | PAG_READ | PAG_WRITE | OBJ_GIVEABLE)));
  FASTCOPY (text, ((char *) shared_copy), len);

  if (WinOpenClipbrd (pm_hab))
    {
      if (WinEmptyClipbrd (pm_hab))
	copy_used
	  = (WinSetClipbrdData
	     (pm_hab, ((ULONG) shared_copy), CF_TEXT, CFI_POINTER));
      (void) WinCloseClipbrd (pm_hab);
    }
  if (!copy_used)
    STD_API_CALL (dos_free_mem, (shared_copy));
}

static const char *
clipboard_read_text (qid_t qid)
{
  char * result = 0;
  if (WinOpenClipbrd (pm_hab))
    {
      const char * shared_copy
	= ((const char *) (WinQueryClipbrdData (pm_hab, CF_TEXT)));
      if (shared_copy != 0)
	{
	  unsigned int len = ((strlen (shared_copy)) + 1);
	  result = (OS_malloc (len));
	  FASTCOPY (shared_copy, result, len);
	}
      (void) WinCloseClipbrd (pm_hab);
    }
  return (result);
}

/* Menus */

static HWND
menu_create (qid_t qid, HWND owner, USHORT style, USHORT id)
{
  return
    (WinCreateWindow (owner,	/* parent window */
		      WC_MENU,	/* class name */
		      "",	/* window text */
		      style,	/* window style */
		      0, 0, 0, 0, /* size and position */
		      owner,	/* owner window */
		      HWND_TOP, /* sibling window */
		      id,	/* ID */
		      0,	/* control data */
		      0		/* presentation parameters */
		      ));
}

static BOOL
menu_destroy (qid_t qid, HWND menu)
{
  return (WinDestroyWindow (menu));
}

static USHORT
menu_insert_item (qid_t qid, HWND menu, USHORT position, USHORT style,
		  USHORT attributes, USHORT id, HWND submenu, char * text)
{
  MENUITEM item;
  (item . iPosition) = position;
  (item . afStyle) = style;
  (item . afAttribute) = attributes;
  (item . id) = id;
  (item . hwndSubMenu) = submenu;
  (item . hItem) = 0;
  return (SHORT1FROMMR (WinSendMsg (menu, MM_INSERTITEM,
				    (MPFROMP (&item)),
				    (MPFROMP (text)))));
}

static USHORT
menu_remove_item (qid_t qid, HWND menu, USHORT id, USHORT submenup,
		  USHORT deletep)
{
  return (SHORT1FROMMR (WinSendMsg (menu,
				    (deletep ? MM_DELETEITEM : MM_REMOVEITEM),
				    (MPFROM2SHORT (id, submenup)),
				    0)));
}

static PMENUITEM
menu_get_item (qid_t qid, HWND menu, USHORT id, USHORT submenup)
{
  PMENUITEM item = (OS_malloc (sizeof (MENUITEM)));
  if (LONGFROMMR (WinSendMsg (menu, MM_QUERYITEM,
			      (MPFROM2SHORT (id, submenup)),
			      (MPFROMP (item)))))
    return (item);
  OS_free (item);
  return (0);
}

static USHORT
menu_n_items (qid_t qid, HWND menu)
{
  return (SHORT1FROMMR (WinSendMsg (menu, MM_QUERYITEMCOUNT, 0, 0)));
}

static USHORT
menu_nth_item_id (qid_t qid, HWND menu, USHORT position)
{
  return (SHORT1FROMMR (WinSendMsg (menu, MM_ITEMIDFROMPOSITION,
				    (MPFROMSHORT (position)),
				    0)));
}

static USHORT
menu_get_item_attributes (qid_t qid, HWND menu, USHORT id, USHORT submenup,
			  USHORT mask)
{
  return (SHORT1FROMMR (WinSendMsg (menu, MM_QUERYITEMATTR,
				    (MPFROM2SHORT (id, submenup)),
				    (MPFROMSHORT (mask)))));
}

static BOOL
menu_set_item_attributes (qid_t qid, HWND menu, USHORT id, USHORT submenup,
			  USHORT mask, USHORT attributes)
{
  return (LONGFROMMR (WinSendMsg (menu, MM_SETITEMATTR,
				  (MPFROM2SHORT (id, submenup)),
				  (MPFROM2SHORT (mask, attributes)))));
}

static HWND
window_load_menu (window_t * window, HMODULE module, ULONG id)
{
  return (WinLoadMenu ((WINDOW_FRAME (window)), module, id));
}

static BOOL
window_popup_menu (qid_t qid, HWND parent, HWND owner, HWND menu,
		   LONG x, LONG y, LONG id, ULONG options)
{
  return (WinPopupMenu (parent, owner, menu, x, y, id, options));
}

/* Fonts */

static font_metrics_t *
ps_get_font_metrics (ps_t * ps)
{
  font_metrics_t * metrics = (OS_malloc (sizeof (font_metrics_t)));
  FONTMETRICS fm;
  if (!GpiQueryFontMetrics ((PS_HANDLE (ps)), (sizeof (fm)), (& fm)))
    window_error (GpiQueryFontMetrics);
  (FONT_METRICS_WIDTH (metrics)) = (fm . lMaxCharInc);
  (FONT_METRICS_HEIGHT (metrics)) = (fm . lMaxBaselineExt);
  (FONT_METRICS_DESCENDER (metrics)) = (fm . lMaxDescender);
  return (metrics);
}

static font_metrics_t *
ps_set_font_internal (ps_t * ps, unsigned short id, const char * spec)
{
  return ((ps_set_font (ps, id, spec)) ? (ps_get_font_metrics (ps)) : 0);
}

static const char *
window_font_dialog (window_t * window, const char * title)
{
  ps_t * ps = (WINDOW_CLIENT_PS (window));
  HPS hps = (PS_HANDLE (ps));
  FONTDLG info;
  char name_buffer [FACESIZE];
  HWND result;

  memset ((&info), 0, (sizeof (info)));
  (name_buffer[0]) = '\0';
  (info . cbSize) = (sizeof (info));
  (info . hpsScreen) = hps;
  (info . pszTitle) = ((PSZ) title);
  (info . fl) = (FNTS_FIXEDWIDTHONLY | FNTS_CENTER); /* FNTS_INITFROMFATTRS */
  (info . pszFamilyname) = name_buffer;
  (info . usFamilyBufLen) = (sizeof (name_buffer));
  /* Because our PS is in RGB mode, the RGB color specs we are using
     are useless.  It's undocumented, but only indexed colors work in
     the font dialog, so we must override with indexes.  */
  (info . clrFore) = CLR_BLACK; /* (PS_FOREGROUND_COLOR (ps)) */
  (info . clrBack) = CLR_WHITE; /* (PS_BACKGROUND_COLOR (ps)) */
  {
    FONTMETRICS fm;
    if (GpiQueryFontMetrics (hps, (sizeof (fm)), (&fm)))
      {
	strcpy (name_buffer, (fm . szFamilyname));
	(info . usWeight) = (fm . usWeightClass);
	(info . usWidth) = (fm . usWidthClass);
	(info . fxPointSize)
	  = (MAKEFIXED (((fm . sNominalPointSize) / 10), 0));
	(info . flStyle) = (fm . fsSelection);
	copy_fontmetrics_to_fattrs ((&fm), (& (info . fAttrs)));
#if 0
	/* The following, for some unknown reason, causes the
	   selection of incorrect fonts: */
	(info . fl) |= FNTS_INITFROMFATTRS;
#endif
      }
  }
  result = (WinFontDlg (HWND_DESKTOP, (WINDOW_CLIENT (window)), (&info)));
  if ((result == NULLHANDLE) || ((info . lReturn) != DID_OK))
    return (0);
  {
    PSZ face_name;
    const char * font_spec;
    {
      FACENAMEDESC desc;
      ULONG face_name_length;
      char face_name_dummy [1];
      memset ((&desc), 0, (sizeof (desc)));
      (desc . usSize) = (sizeof (desc));
      (desc . usWeightClass) = (info . usWeight);
      (desc . usWidthClass) = (info . usWidth);
      (desc . flOptions) = (info . flType);
      face_name = face_name_dummy;
      face_name_length
	= (GpiQueryFaceString (hps, (info . pszFamilyname), (&desc),
			       0, face_name));
      if (face_name_length == GPI_ERROR)
	{
	  window_warning (GpiQueryFaceString);
	  return (0);
	}
      face_name = (OS_malloc (face_name_length));
      face_name_length
	= (GpiQueryFaceString (hps, (info . pszFamilyname), (&desc),
			       face_name_length, face_name));
      if (face_name_length == GPI_ERROR)
	{
	  OS_free (face_name);
	  window_warning (GpiQueryFaceString);
	  return (0);
	}
    }
    font_spec = (unparse_font_spec (face_name,
				    ((FIXEDINT (info . fxPointSize)) * 10),
				    (info . flStyle)));
    OS_free (face_name);
    return (font_spec);
  }
}

/* Helper Procedures */

static int
ps_set_font (ps_t * ps, unsigned short id, const char * spec)
{
  PSZ name = 0;
  LONG size;
  USHORT selection;
  if (!parse_font_spec (spec, (& name), (& size), (& selection)))
    return (0);
  if (!ps_set_font_1 (ps, name, size, selection, id))
    {
      OS_free (name);
      return (0);
    }
  {
    FONTMETRICS fm;
    if (!GpiQueryFontMetrics ((PS_HANDLE (ps)), (sizeof (fm)), (& fm)))
      window_error (GpiQueryFontMetrics);
    if ((PS_CHAR_INCREMENTS (ps)) != 0)
      OS_free (PS_CHAR_INCREMENTS (ps));
    (PS_CHAR_INCREMENTS (ps))
      = ((((fm . fsDefn) & FM_DEFN_OUTLINE) != 0)
	 ? (ps_make_char_increments (fm . lMaxCharInc))
	 : 0);
  }
  return (1);
}

static int
ps_set_font_1 (ps_t * ps, PSZ name, LONG size, USHORT selection, LONG id)
{
  HPS hps = (PS_HANDLE (ps));
  LONG nfonts;
  ULONG index;
  PFONTMETRICS pfm;

  nfonts = 0;
  nfonts = (GpiQueryFonts (hps,
			   (QF_PUBLIC | QF_PRIVATE),
			   name,
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
		      name,
		      (& nfonts),
		      (sizeof (FONTMETRICS)),
		      pfm))
      == GPI_ALTERROR)
    window_error (GpiQueryFonts);
  {
    int result = 0;
    /* Choose an image font if one is available.  */
    for (index = 0; (index < nfonts); index += 1)
      if (((((pfm [index]) . fsType) & FM_TYPE_FIXED) != 0)
	  && ((((pfm [index]) . fsDefn) & FM_DEFN_OUTLINE) == 0)
	  && (((pfm [index]) . sNominalPointSize) == size)
	  && (create_font (hps, id, (& (pfm [index])), selection)))
	{
	  GpiSetCharSet (hps, id);
	  result = 1;
	  goto done;
	}
    /* Otherwise, look for an outline font.  */
    for (index = 0; (index < nfonts); index += 1)
      if (((((pfm [index]) . fsType) & FM_TYPE_FIXED) != 0)
	  && ((((pfm [index]) . fsDefn) & FM_DEFN_OUTLINE) != 0)
	  && (create_font (hps, id, (& (pfm [index])), selection)))
	{
	  GpiSetCharSet (hps, id);
	  ps_set_font_size (ps, size);
	  result = 1;
	  goto done;
	}
  done:
    OS_free (pfm);
    return (result);
  }
}

static int
create_font (HPS hps, LONG font_id, PFONTMETRICS pfm, USHORT selection)
{
  FATTRS fa;
  copy_fontmetrics_to_fattrs (pfm, (&fa));
  (fa . fsSelection) = selection;
  return ((GpiCreateLogFont (hps, 0, font_id, (&fa))) == FONT_MATCH);
}

static void
copy_fontmetrics_to_fattrs (FONTMETRICS * pfm, FATTRS * pfa)
{
  (pfa -> usRecordLength) = (sizeof (*pfa));
  (pfa -> fsSelection) = (pfm -> fsSelection);
  (pfa -> lMatch) = (pfm -> lMatch);
  strcpy ((pfa -> szFacename), (pfm -> szFacename));
  (pfa -> idRegistry) = (pfm -> idRegistry);
  (pfa -> usCodePage) = (pfm -> usCodePage);
  (pfa -> fsType) = 0;
  if (((pfm -> fsDefn) & FM_DEFN_OUTLINE) != 0)
    {
      (pfa -> lMaxBaselineExt) = 0;
      (pfa -> lAveCharWidth) = 0;
      (pfa -> fsFontUse)
	= (FATTR_FONTUSE_OUTLINE | FATTR_FONTUSE_TRANSFORMABLE);
    }
  else
    {
      (pfa -> lMaxBaselineExt) = (pfm -> lMaxBaselineExt);
      (pfa -> lAveCharWidth) = (pfm -> lAveCharWidth);
      (pfa -> fsFontUse) = 0;
    }
}

static void
ps_set_font_size (ps_t * ps, LONG size)
{
  POINTL ptl [2];

  ((ptl[0]) . x) = 0;
  ((ptl[0]) . y) = 0;
  {
    LONG xres;
    ps_query_caps (ps, CAPS_HORIZONTAL_FONT_RES, 1, (&xres));
    ((ptl[1]) . x) = ((((xres * size) << 4) + 360) / 720);
  }
  {
    LONG yres;
    ps_query_caps (ps, CAPS_VERTICAL_FONT_RES, 1, (&yres));
    ((ptl[1]) . y) = ((((yres * size) << 4) + 360) / 720);
  }
  if (!GpiConvert ((PS_HANDLE (ps)), CVTC_DEVICE, CVTC_WORLD, 2, ptl))
    window_error (GpiConvert);
  {
    SIZEF s;
    (s . cx) = ((((ptl[1]) . x) - ((ptl[0]) . x)) << 12);
    (s . cy) = ((((ptl[1]) . y) - ((ptl[0]) . y)) << 12);
    if (!GpiSetCharBox ((PS_HANDLE (ps)), (&s)))
      window_error (GpiSetCharBox);
  }
}

static PLONG
ps_make_char_increments (LONG increment)
{
  PLONG increments = (OS_malloc ((sizeof (LONG)) * 512));
  unsigned int index;
  for (index = 0; (index < 512); index += 1)
    (increments[index]) = increment;
  return (increments);
}

static struct font_selection
{
  const char * name;
  unsigned int selector;
} font_selections [] =
{
  { ".bold", FATTR_SEL_BOLD },
  { ".italic", FATTR_SEL_ITALIC },
  { ".outline", FATTR_SEL_OUTLINE },
  { ".strikeout", FATTR_SEL_STRIKEOUT },
  { ".underscore", FATTR_SEL_UNDERSCORE },
  { 0, 0 }
};

static int
parse_font_spec (const char * spec,
		 PSZ * pname, LONG * psize, USHORT * pselection)
{
  const char * scan = spec;
  unsigned int size = 0;
  unsigned int selection = 0;
  while (('0' <= (*scan)) && ((*scan) <= '9'))
    size = ((size * 10) + ((*scan++) - '0'));
  if (size == 0)
    return (0);
  while (1)
    {
      struct font_selection * selections = font_selections;
      unsigned int name_length;
      while (1)
	{
	  if ((selections -> name) == 0)
	    goto no_more_selections;
	  name_length = (strlen (selections -> name));
	  if ((strncmp (scan, (selections -> name), name_length)) == 0)
	    {
	      selection |= (selections -> selector);
	      scan += name_length;
	      break;
	    }
	  selections += 1;
	}
    }
 no_more_selections:
  if ((*scan++) != '.')
    return (0);
  (*pname) = (OS_malloc ((strlen (scan)) + 1));
  strcpy ((*pname), scan);
  (*psize) = (size * 10);
  (*pselection) = selection;
  return (1);
}

static const char *
unparse_font_spec (PSZ name, LONG size, USHORT selection)
{
  char size_buffer [16];
  char selection_buffer [16];
  struct font_selection * selections = font_selections;
  char * result;

  sprintf (size_buffer, "%d", (size / 10));
  strcpy (selection_buffer, "");
  while (1)
    {
      if ((selections -> name) == 0)
	break;
      if ((selection & (selections -> selector)) != 0)
	strcat (selection_buffer, (selections -> name));
      selections += 1;
    }
  result
    = (OS_malloc ((strlen (size_buffer))
		  + (strlen (name))
		  + (strlen (selection_buffer))
		  + 2));
  strcpy (result, size_buffer);
  strcat (result, selection_buffer);
  strcat (result, ".");
  strcat (result, name);
  return (result);
}

/* Pointers and Icons */

static HPOINTER
query_system_pointer (qid_t qid, HWND desktop, LONG id, BOOL copyp)
{
  return (WinQuerySysPointer (desktop, id, copyp));
}

static BOOL
set_pointer (qid_t qid, HWND desktop, HPOINTER pointer)
{
  return (WinSetPointer (desktop, pointer));
}

static HPOINTER
window_load_pointer (qid_t qid, HWND desktop, HMODULE module, ULONG id)
{
  return (WinLoadPointer (desktop, module, id));
}

static BOOL
window_destroy_pointer (qid_t qid, HPOINTER pointer)
{
  return (WinDestroyPointer (pointer));
}

static BOOL
window_set_icon (window_t * window, HPOINTER icon)
{
  return (LONGFROMMR (WinSendMsg ((WINDOW_FRAME (window)), WM_SETICON,
				  (MPFROMLONG (icon)),
				  (MPFROMLONG (0)))));
}
