/* -*-C-*-

$Id: os2pm.c,v 1.5 1995/02/08 01:19:32 cph Exp $

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
#define INCL_GPI
#include "os2.h"

struct _ps_t;

typedef struct
{
  HWND frame;			/* frame window handle */
  HWND client;			/* client window handle */
  struct _ps_t * client_ps;	/* presentation space for client window */
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
  unsigned int cursor_shownp : 1; /* nonzero if cursor is visible */
  unsigned int minimizingp : 1; /* nonzero if window being minimized */
  unsigned int minimizedp : 1;	/* nonzero if window is minimized */
  unsigned int permanentp : 1;	/* nonzero means don't close on reload */
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
#define WINDOW_CURSOR_SHOWNP(window) ((window) -> cursor_shownp)
#define WINDOW_MINIMIZINGP(window) ((window) -> minimizingp)
#define WINDOW_MINIMIZEDP(window) ((window) -> minimizedp)
#define WINDOW_PERMANENTP(window) ((window) -> permanentp)

typedef struct _ps_t
{
  psid_t id;			/* psid for this ps */
  qid_t qid;			/* qid to send commands to */
  HPS handle;
  COLOR foreground_color;
  COLOR background_color;
  window_t * window;		/* if this is window PS, the window */
  HBITMAP bitmap;		/* if this is bitmap PS, the bitmap */
} ps_t;
#define PS_ID(ps) ((ps) -> id)
#define PS_QID(ps) ((ps) -> qid)
#define PS_HANDLE(ps) ((ps) -> handle)
#define PS_FOREGROUND_COLOR(ps) ((ps) -> foreground_color)
#define PS_BACKGROUND_COLOR(ps) ((ps) -> background_color)
#define PS_WINDOW(ps) ((ps) -> window)
#define PS_BITMAP(ps) ((ps) -> bitmap)

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

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  qid_t qid;
  qid_t event_qid;
  unsigned long style;
  char title [1];
} sm_open_request_t;
#define SM_OPEN_REQUEST_QID(m) (((sm_open_request_t *) (m)) -> qid)
#define SM_OPEN_REQUEST_EVENT_QID(m) (((sm_open_request_t *) (m)) -> event_qid)
#define SM_OPEN_REQUEST_STYLE(m) (((sm_open_request_t *) (m)) -> style)
#define SM_OPEN_REQUEST_TITLE(m) (((sm_open_request_t *) (m)) -> title)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  wid_t wid;
} sm_open_reply_t;
#define SM_OPEN_REPLY_WID(m) (((sm_open_reply_t *) (m)) -> wid)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
} sm_close_t;
#define SM_CLOSE_WINDOW(m) (((sm_close_t *) (m)) -> window)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  char showp;
} sm_show_t;
#define SM_SHOW_WINDOW(m) (((sm_show_t *) (m)) -> window)
#define SM_SHOW_SHOWP(m) (((sm_show_t *) (m)) -> showp)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  short x;
  short y;
} sm_move_cursor_t;
#define SM_MOVE_CURSOR_WINDOW(m) (((sm_move_cursor_t *) (m)) -> window)
#define SM_MOVE_CURSOR_X(m) (((sm_move_cursor_t *) (m)) -> x)
#define SM_MOVE_CURSOR_Y(m) (((sm_move_cursor_t *) (m)) -> y)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  unsigned short width;
  unsigned short height;
  unsigned short style;
} sm_shape_cursor_t;
#define SM_SHAPE_CURSOR_WINDOW(m) (((sm_shape_cursor_t *) (m)) -> window)
#define SM_SHAPE_CURSOR_WIDTH(m) (((sm_shape_cursor_t *) (m)) -> width)
#define SM_SHAPE_CURSOR_HEIGHT(m) (((sm_shape_cursor_t *) (m)) -> height)
#define SM_SHAPE_CURSOR_STYLE(m) (((sm_shape_cursor_t *) (m)) -> style)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  char showp;
} sm_show_cursor_t;
#define SM_SHOW_CURSOR_WINDOW(m) (((sm_show_cursor_t *) (m)) -> window)
#define SM_SHOW_CURSOR_SHOWP(m) (((sm_show_cursor_t *) (m)) -> showp)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  short xl;
  short xh;
  short yl;
  short yh;
  short x_delta;
  short y_delta;
} sm_scroll_t;
#define SM_SCROLL_WINDOW(m) (((sm_scroll_t *) (m)) -> window)
#define SM_SCROLL_XL(m) (((sm_scroll_t *) (m)) -> xl)
#define SM_SCROLL_XH(m) (((sm_scroll_t *) (m)) -> xh)
#define SM_SCROLL_YL(m) (((sm_scroll_t *) (m)) -> yl)
#define SM_SCROLL_YH(m) (((sm_scroll_t *) (m)) -> yh)
#define SM_SCROLL_X_DELTA(m) (((sm_scroll_t *) (m)) -> x_delta)
#define SM_SCROLL_Y_DELTA(m) (((sm_scroll_t *) (m)) -> y_delta)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  short xl;
  short xh;
  short yl;
  short yh;
} sm_invalidate_t;
#define SM_INVALIDATE_WINDOW(m) (((sm_invalidate_t *) (m)) -> window)
#define SM_INVALIDATE_XL(m) (((sm_invalidate_t *) (m)) -> xl)
#define SM_INVALIDATE_XH(m) (((sm_invalidate_t *) (m)) -> xh)
#define SM_INVALIDATE_YL(m) (((sm_invalidate_t *) (m)) -> yl)
#define SM_INVALIDATE_YH(m) (((sm_invalidate_t *) (m)) -> yh)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  unsigned short x;
  unsigned short y;
} sm_set_grid_t;
#define SM_SET_GRID_WINDOW(m) (((sm_set_grid_t *) (m)) -> window)
#define SM_SET_GRID_X(m) (((sm_set_grid_t *) (m)) -> x)
#define SM_SET_GRID_Y(m) (((sm_set_grid_t *) (m)) -> y)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
} sm_activate_t;
#define SM_ACTIVATE_WINDOW(m) (((sm_activate_t *) (m)) -> window)

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

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  short x;
  short y;
} sm_set_pos_t;
#define SM_SET_POS_WINDOW(m) (((sm_set_pos_t *) (m)) -> window)
#define SM_SET_POS_X(m) (((sm_set_pos_t *) (m)) -> x)
#define SM_SET_POS_Y(m) (((sm_set_pos_t *) (m)) -> y)

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

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  unsigned short width;
  unsigned short height;
} sm_set_size_t;
#define SM_SET_SIZE_WINDOW(m) (((sm_set_size_t *) (m)) -> window)
#define SM_SET_SIZE_WIDTH(m) (((sm_set_size_t *) (m)) -> width)
#define SM_SET_SIZE_HEIGHT(m) (((sm_set_size_t *) (m)) -> height)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
} sm_focusp_request_t;
#define SM_FOCUSP_REQUEST_WINDOW(m) (((sm_focusp_request_t *) (m)) -> window)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  char focusp;
} sm_focusp_reply_t;
#define SM_FOCUSP_REPLY_FOCUSP(m) (((sm_focusp_reply_t *) (m)) -> focusp)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  window_state_t state;
} sm_set_state_t;
#define SM_SET_STATE_WINDOW(m) (((sm_set_state_t *) (m)) -> window)
#define SM_SET_STATE_STATE(m) (((sm_set_state_t *) (m)) -> state)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  window_t * window;
  char title [1];
} sm_set_title_t;
#define SM_SET_TITLE_WINDOW(m) (((sm_set_title_t *) (m)) -> window)
#define SM_SET_TITLE_TITLE(m) (((sm_set_title_t *) (m)) -> title)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  qid_t qid;
  USHORT width;
  USHORT height;
} sm_bitmap_ps_open_request_t;
#define SM_BITMAP_PS_OPEN_REQUEST_QID(m)				\
  (((sm_bitmap_ps_open_request_t *) (m)) -> qid)
#define SM_BITMAP_PS_OPEN_REQUEST_WIDTH(m)				\
  (((sm_bitmap_ps_open_request_t *) (m)) -> width)
#define SM_BITMAP_PS_OPEN_REQUEST_HEIGHT(m)				\
  (((sm_bitmap_ps_open_request_t *) (m)) -> height)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  psid_t psid;
} sm_bitmap_ps_open_reply_t;
#define SM_BITMAP_PS_OPEN_REPLY_PSID(m)					\
  (((sm_bitmap_ps_open_reply_t *) (m)) -> psid)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
} sm_bitmap_ps_close_t;
#define SM_BITMAP_PS_CLOSE_PS(m) (((sm_bitmap_ps_close_t *) (m)) -> ps)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * target_ps;
  ps_t * source_ps;
  LONG npoints;
  POINTL points [4];
  LONG rop;
  ULONG options;
} sm_ps_bitblt_t;
#define SM_PS_BITBLT_TARGET_PS(m) (((sm_ps_bitblt_t *) (m)) -> target_ps)
#define SM_PS_BITBLT_SOURCE_PS(m) (((sm_ps_bitblt_t *) (m)) -> source_ps)
#define SM_PS_BITBLT_NPOINTS(m) (((sm_ps_bitblt_t *) (m)) -> npoints)
#define SM_PS_BITBLT_POINTS(m) (((sm_ps_bitblt_t *) (m)) -> points)
#define SM_PS_BITBLT_ROP(m) (((sm_ps_bitblt_t *) (m)) -> rop)
#define SM_PS_BITBLT_OPTIONS(m) (((sm_ps_bitblt_t *) (m)) -> options)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  short x;
  short y;
  unsigned short size;
  const char data [1];
} sm_write_t;
#define SM_WRITE_PS(m) (((sm_write_t *) (m)) -> ps)
#define SM_WRITE_X(m) (((sm_write_t *) (m)) -> x)
#define SM_WRITE_Y(m) (((sm_write_t *) (m)) -> y)
#define SM_WRITE_SIZE(m) (((sm_write_t *) (m)) -> size)
#define SM_WRITE_DATA(m) (((sm_write_t *) (m)) -> data)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  short xl;
  short xh;
  short yl;
  short yh;
} sm_clear_t;
#define SM_CLEAR_PS(m) (((sm_clear_t *) (m)) -> ps)
#define SM_CLEAR_XL(m) (((sm_clear_t *) (m)) -> xl)
#define SM_CLEAR_XH(m) (((sm_clear_t *) (m)) -> xh)
#define SM_CLEAR_YL(m) (((sm_clear_t *) (m)) -> yl)
#define SM_CLEAR_YH(m) (((sm_clear_t *) (m)) -> yh)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  unsigned short id;
  char spec [1];
} sm_set_font_request_t;
#define SM_SET_FONT_REQUEST_PS(m) (((sm_set_font_request_t *) (m)) -> ps)
#define SM_SET_FONT_REQUEST_ID(m) (((sm_set_font_request_t *) (m)) -> id)
#define SM_SET_FONT_REQUEST_SPEC(m) (((sm_set_font_request_t *) (m)) -> spec)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  font_metrics_t * metrics;
} sm_set_font_reply_t;
#define SM_SET_FONT_REPLY_METRICS(m) (((sm_set_font_reply_t *) (m)) -> metrics)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  COLOR foreground;
  COLOR background;
} sm_set_colors_t;
#define SM_SET_COLORS_PS(m) (((sm_set_colors_t *) (m)) -> ps)
#define SM_SET_COLORS_FOREGROUND(m) (((sm_set_colors_t *) (m)) -> foreground)
#define SM_SET_COLORS_BACKGROUND(m) (((sm_set_colors_t *) (m)) -> background)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  short x;
  short y;
} sm_move_gcursor_t;
#define SM_MOVE_GCURSOR_PS(m) (((sm_move_gcursor_t *) (m)) -> ps)
#define SM_MOVE_GCURSOR_X(m) (((sm_move_gcursor_t *) (m)) -> x)
#define SM_MOVE_GCURSOR_Y(m) (((sm_move_gcursor_t *) (m)) -> y)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  short x;
  short y;
} sm_line_t;
#define SM_LINE_PS(m) (((sm_line_t *) (m)) -> ps)
#define SM_LINE_X(m) (((sm_line_t *) (m)) -> x)
#define SM_LINE_Y(m) (((sm_line_t *) (m)) -> y)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  unsigned long npoints;
  PPOINTL points;
} sm_poly_line_t;
#define SM_POLY_LINE_PS(m) (((sm_poly_line_t *) (m)) -> ps)
#define SM_POLY_LINE_NPOINTS(m) (((sm_poly_line_t *) (m)) -> npoints)
#define SM_POLY_LINE_POINTS(m) (((sm_poly_line_t *) (m)) -> points)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  unsigned long npoints;
  PPOINTL points;
} sm_poly_line_disjoint_t;
#define SM_POLY_LINE_DISJOINT_PS(m)					\
  (((sm_poly_line_disjoint_t *) (m)) -> ps)
#define SM_POLY_LINE_DISJOINT_NPOINTS(m)				\
  (((sm_poly_line_disjoint_t *) (m)) -> npoints)
#define SM_POLY_LINE_DISJOINT_POINTS(m)					\
  (((sm_poly_line_disjoint_t *) (m)) -> points)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  LONG ltype;
} sm_set_line_type_t;
#define SM_SET_LINE_TYPE_PS(m) (((sm_set_line_type_t *) (m)) -> ps)
#define SM_SET_LINE_TYPE_TYPE(m) (((sm_set_line_type_t *) (m)) -> ltype)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ps_t * ps;
  LONG start;
  LONG count;
  PLONG values;
} sm_query_caps_t;
#define SM_QUERY_CAPS_PS(m) (((sm_query_caps_t *) (m)) -> ps)
#define SM_QUERY_CAPS_START(m) (((sm_query_caps_t *) (m)) -> start)
#define SM_QUERY_CAPS_COUNT(m) (((sm_query_caps_t *) (m)) -> count)
#define SM_QUERY_CAPS_VALUES(m) (((sm_query_caps_t *) (m)) -> values)

static void sync_transaction (qid_t, msg_t *);
static void sync_reply (qid_t);

static void pm_thread_procedure (void *);
static tqueue_t * make_pm_tqueue (HWND);

static void initialize_id_table (id_table_t *);
static unsigned int allocate_id (id_table_t *, void *);
static void deallocate_id (id_table_t *, unsigned int);
static void * id_to_pointer (id_table_t *, unsigned int);

static window_t * wid_to_window (wid_t);
static ps_t * psid_to_ps (psid_t);
static void close_all_windows (void);

static MRESULT EXPENTRY object_window_procedure (HWND, ULONG, MPARAM, MPARAM);
static MRESULT EXPENTRY frame_window_procedure (HWND, ULONG, MPARAM, MPARAM);
static MRESULT EXPENTRY window_procedure (HWND, ULONG, MPARAM, MPARAM);

static wid_t open_window (qid_t, qid_t, ULONG, PSZ);
static window_t * hwnd_to_window (HWND);
static void close_window (window_t *);
static void show_window (window_t *, int);
static void move_cursor (window_t *, short, short);
static void shape_cursor
  (window_t *, unsigned short, unsigned short, unsigned short);
static void show_cursor (window_t *, int);
static void recreate_cursor (window_t *);
static void activate_cursor (window_t *);
static void deactivate_cursor (window_t *);
static void scroll_rectangle (window_t *, short, short, PRECTL);
static void invalidate_rectangle (window_t *, PRECTL);
static void get_window_pos (window_t *, short *, short *);
static void set_window_pos (window_t *, short, short);
static void get_window_size (window_t *, unsigned short *, unsigned short *);
static void get_window_frame_size
  (window_t *, unsigned short *, unsigned short *);
static void set_window_size (window_t *, unsigned short, unsigned short);
static int window_focusp (window_t *);
static void set_window_state (window_t *, window_state_t);
static void set_window_title (window_t *, PSZ);

static psid_t open_bitmap_ps (qid_t, USHORT, USHORT);
static HDC get_ps_device (HPS);
static LONG get_device_capability (HDC, LONG);
static ps_t * make_ps (HDC, qid_t);
static void close_ps (ps_t *);
static void bitblt_ps (ps_t *, ps_t *, LONG, PPOINTL, LONG, ULONG);
static void write_ps (ps_t *, short, short, const char *, unsigned short);
static void maybe_activate_cursor (ps_t *);
static void maybe_deactivate_cursor (ps_t *);
static void clear_rectangle (ps_t *, PRECTL);
static void set_ps_colors (ps_t *, COLOR, COLOR);
static void move_gcursor (ps_t *, short, short);
static void draw_line (ps_t *, short, short);
static void poly_line (ps_t *, unsigned long, PPOINTL);
static void poly_line_disjoint (ps_t *, unsigned long, PPOINTL);
static void set_line_type (ps_t *, LONG);
static void query_caps (ps_t *, LONG, LONG, PLONG);
static font_metrics_t * set_font (ps_t *, unsigned short, const char *);

static msg_t * make_button_event
  (wid_t, unsigned char, unsigned char, unsigned short, unsigned short,
   unsigned short);
static msg_t * make_close_event (wid_t);
static msg_t * make_focus_event (wid_t, int);
static msg_t * make_key_event
  (wid_t, unsigned short, unsigned short, unsigned short);
static msg_t * make_paint_event
  (wid_t, unsigned short, unsigned short, unsigned short, unsigned short);
static msg_t * make_resize_event (wid_t, unsigned short, unsigned short);
static msg_t * make_visibility_event (wid_t, int);

#define ID_RESOURCES 1
#define ID_FRAME 1

#define UWM_ENCAPSULATION WM_USER

#define QWP_WINDOW QWL_USER

/* These should have been defined by PM header file.  */
#define MRVOID MRFROMP (0)
#define MRTRUE MRFROMLONG (TRUE)
#define MRFALSE MRFROMLONG (FALSE)

static id_table_t wid_table;
static id_table_t psid_table;
static qid_t pm_init_qid;
static TID pm_tid;
static HAB pm_hab;
static HMQ pm_hmq;
static HWND pm_object_window;
static tqueue_t * pm_tqueue;
static PFNWP original_frame_window_procedure;

static const char object_class [] = "mit-scheme.object";
static const char window_class [] = "mit-scheme.window";

#define SEND_EVENT(window, message)					\
{									\
  if ((WINDOW_EVENT_QID (window)) != QID_NONE)				\
    OS2_send_message ((WINDOW_EVENT_QID (window)), (message));		\
}

#define window_error(name) window_error_1 (#name, 1)
#define window_warning(name) window_error_1 (#name, 0)

static void
window_error_1 (const char * name, int fatalp)
{
  char buffer [1024];
  ERRORID code = (WinGetLastError (pm_hab));
  sprintf (buffer, "%s error %d occurred in the %s procedure.  \
This indicates a bug in the Scheme implementation.  \
Please report this information to a Scheme wizard.",
	   (fatalp ? "Fatal" : "Non-fatal"), code, name);
  if (fatalp)
    OS2_logic_error (buffer);
  else
    (void) WinMessageBox (HWND_DESKTOP,
			  NULLHANDLE,
			  buffer,
			  "Scheme Error",
			  0,
			  (MB_OK | MB_WARNING));
}

void
OS2_initialize_pm_thread (void)
{
  SET_MSG_TYPE_LENGTH (mt_window_open_request, sm_open_request_t);
  SET_MSG_TYPE_LENGTH (mt_window_open_reply, sm_open_reply_t);
  SET_MSG_TYPE_LENGTH (mt_window_close, sm_close_t);
  SET_MSG_TYPE_LENGTH (mt_window_show, sm_show_t);
  SET_MSG_TYPE_LENGTH (mt_window_move_cursor, sm_move_cursor_t);
  SET_MSG_TYPE_LENGTH (mt_window_shape_cursor, sm_shape_cursor_t);
  SET_MSG_TYPE_LENGTH (mt_window_show_cursor, sm_show_cursor_t);
  SET_MSG_TYPE_LENGTH (mt_window_scroll, sm_scroll_t);
  SET_MSG_TYPE_LENGTH (mt_window_invalidate, sm_invalidate_t);
  SET_MSG_TYPE_LENGTH (mt_window_set_grid, sm_set_grid_t);
  SET_MSG_TYPE_LENGTH (mt_window_activate, sm_activate_t);
  SET_MSG_TYPE_LENGTH (mt_window_pos_request, sm_pos_request_t);
  SET_MSG_TYPE_LENGTH (mt_window_pos_reply, sm_pos_reply_t);
  SET_MSG_TYPE_LENGTH (mt_window_set_pos, sm_set_pos_t);
  SET_MSG_TYPE_LENGTH (mt_window_size_request, sm_size_request_t);
  SET_MSG_TYPE_LENGTH (mt_window_size_reply, sm_size_reply_t);
  SET_MSG_TYPE_LENGTH (mt_window_frame_size_request, sm_frame_size_request_t);
  SET_MSG_TYPE_LENGTH (mt_window_frame_size_reply, sm_frame_size_reply_t);
  SET_MSG_TYPE_LENGTH (mt_window_frame_size_request, sm_frame_size_request_t);
  SET_MSG_TYPE_LENGTH (mt_window_frame_size_reply, sm_frame_size_reply_t);
  SET_MSG_TYPE_LENGTH (mt_window_set_size, sm_set_size_t);
  SET_MSG_TYPE_LENGTH (mt_window_focusp_request, sm_focusp_request_t);
  SET_MSG_TYPE_LENGTH (mt_window_focusp_reply, sm_focusp_reply_t);
  SET_MSG_TYPE_LENGTH (mt_window_set_state, sm_set_state_t);
  SET_MSG_TYPE_LENGTH (mt_window_set_title, sm_set_title_t);

  SET_MSG_TYPE_LENGTH (mt_bitmap_ps_open_request, sm_bitmap_ps_open_request_t);
  SET_MSG_TYPE_LENGTH (mt_bitmap_ps_open_reply, sm_bitmap_ps_open_reply_t);
  SET_MSG_TYPE_LENGTH (mt_bitmap_ps_close, sm_bitmap_ps_close_t);
  SET_MSG_TYPE_LENGTH (mt_ps_bitblt, sm_ps_bitblt_t);
  SET_MSG_TYPE_LENGTH (mt_ps_write, sm_write_t);
  SET_MSG_TYPE_LENGTH (mt_ps_clear, sm_clear_t);
  SET_MSG_TYPE_LENGTH (mt_ps_set_font_request, sm_set_font_request_t);
  SET_MSG_TYPE_LENGTH (mt_ps_set_font_reply, sm_set_font_reply_t);
  SET_MSG_TYPE_LENGTH (mt_ps_set_colors, sm_set_colors_t);
  SET_MSG_TYPE_LENGTH (mt_ps_move_gcursor, sm_move_gcursor_t);
  SET_MSG_TYPE_LENGTH (mt_ps_line, sm_line_t);
  SET_MSG_TYPE_LENGTH (mt_ps_poly_line, sm_poly_line_t);
  SET_MSG_TYPE_LENGTH (mt_ps_poly_line_disjoint, sm_poly_line_disjoint_t);
  SET_MSG_TYPE_LENGTH (mt_ps_set_line_type, sm_set_line_type_t);
  SET_MSG_TYPE_LENGTH (mt_ps_query_caps, sm_query_caps_t);

  SET_MSG_TYPE_LENGTH (mt_button_event, sm_button_event_t);
  SET_MSG_TYPE_LENGTH (mt_close_event, sm_close_event_t);
  SET_MSG_TYPE_LENGTH (mt_focus_event, sm_focus_event_t);
  SET_MSG_TYPE_LENGTH (mt_key_event, sm_key_event_t);
  SET_MSG_TYPE_LENGTH (mt_paint_event, sm_paint_event_t);
  SET_MSG_TYPE_LENGTH (mt_resize_event, sm_resize_event_t);
  SET_MSG_TYPE_LENGTH (mt_visibility_event, sm_visibility_event_t);

  initialize_id_table (& wid_table);
  initialize_id_table (& psid_table);
  original_frame_window_procedure = 0;
  {
    qid_t qid;
    OS2_make_qid_pair ((&pm_init_qid), (&qid));
    OS2_open_qid (qid, OS2_scheme_tqueue);
    pm_tid = (OS2_beginthread (pm_thread_procedure, 0, 0x4000));
    /* Wait for init message from PM thread.  This message tells us
       that the other end of the connection is established and that it
       is safe to send messages on the connection.  */
    OS2_destroy_message (OS2_wait_for_message (qid, mt_init));
    OS2_close_qid (qid);
  }
  add_reload_cleanup (close_all_windows);
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

static void
pm_thread_procedure (void * arg)
{
  QMSG qmsg;

  if ((OS2_thread_initialize (QID_NONE)) != 0)
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
  void ** scan = pointers;
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
  return ((id < (ID_TABLE_LENGTH (table)))
	  && (((ID_TABLE_POINTERS (table)) [id]) != 0));
}

static window_t *
wid_to_window (wid_t wid)
{
  return (id_to_pointer ((& wid_table), wid));
}

static ps_t *
psid_to_ps (psid_t psid)
{
  return (id_to_pointer ((& psid_table), psid));
}

int
OS2_wid_validp (wid_t wid)
{
  return (id_validp ((& wid_table), wid));
}

int
OS2_psid_validp (psid_t psid)
{
  return (id_validp ((& psid_table), psid));
}

psid_t
OS2_window_client_ps (wid_t wid)
{
  return (PS_ID (WINDOW_CLIENT_PS (wid_to_window (wid))));
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
	close_window (window);
    }
}

/* Implementation of the object window.  The object window handles
   encapsulated messages sent from the Scheme thread.  This defines
   the protocol used to communicate with the Scheme thread.  */

static void handle_window_open_request (msg_t *);
static void handle_window_close_request (msg_t *);
static void handle_window_show_request (msg_t *);
static void handle_window_move_cursor_request (msg_t *);
static void handle_window_shape_cursor_request (msg_t *);
static void handle_window_show_cursor_request (msg_t *);
static void handle_window_scroll_request (msg_t *);
static void handle_window_invalidate_request (msg_t *);
static void handle_window_set_grid_request (msg_t *);
static void handle_window_activate_request (msg_t *);
static void handle_window_pos_request (msg_t *);
static void handle_window_set_pos_request (msg_t *);
static void handle_window_size_request (msg_t *);
static void handle_window_frame_size_request (msg_t *);
static void handle_window_set_size_request (msg_t *);
static void handle_window_focusp_request (msg_t *);
static void handle_window_set_state_request (msg_t *);
static void handle_window_set_title_request (msg_t *);

static void handle_bitmap_ps_open_request (msg_t *);
static void handle_bitmap_ps_close_request (msg_t *);
static void handle_ps_bitblt_request (msg_t *);
static void handle_ps_write_request (msg_t *);
static void handle_ps_set_font_request (msg_t *);
static void handle_ps_clear_request (msg_t *);
static void handle_ps_set_colors_request (msg_t *);
static void handle_ps_move_gcursor_request (msg_t *);
static void handle_ps_line_request (msg_t *);
static void handle_ps_poly_line_request (msg_t *);
static void handle_ps_poly_line_disjoint_request (msg_t *);
static void handle_ps_set_line_type_request (msg_t *);
static void handle_ps_query_caps_request (msg_t *);

static MRESULT EXPENTRY
object_window_procedure (HWND window, ULONG msg, MPARAM mp1, MPARAM mp2)
{
  if (msg == UWM_ENCAPSULATION)
    {
      msg_t * message = (PVOIDFROMMP (mp1));
      switch (MSG_TYPE (message))
	{
	case mt_window_open_request:
	  handle_window_open_request (message);
	  break;
	case mt_window_close:
	  handle_window_close_request (message);
	  break;
	case mt_window_show:
	  handle_window_show_request (message);
	  break;
	case mt_window_move_cursor:
	  handle_window_move_cursor_request (message);
	  break;
	case mt_window_shape_cursor:
	  handle_window_shape_cursor_request (message);
	  break;
	case mt_window_show_cursor:
	  handle_window_show_cursor_request (message);
	  break;
	case mt_window_scroll:
	  handle_window_scroll_request (message);
	  break;
	case mt_window_invalidate:
	  handle_window_invalidate_request (message);
	  break;
	case mt_window_set_grid:
	  handle_window_set_grid_request (message);
	  break;
	case mt_window_activate:
	  handle_window_activate_request (message);
	  break;
	case mt_window_pos_request:
	  handle_window_pos_request (message);
	  break;
	case mt_window_set_pos:
	  handle_window_set_pos_request (message);
	  break;
	case mt_window_size_request:
	  handle_window_size_request (message);
	  break;
	case mt_window_frame_size_request:
	  handle_window_frame_size_request (message);
	  break;
	case mt_window_set_size:
	  handle_window_set_size_request (message);
	  break;
	case mt_window_focusp_request:
	  handle_window_focusp_request (message);
	  break;
	case mt_window_set_state:
	  handle_window_set_state_request (message);
	  break;
	case mt_window_set_title:
	  handle_window_set_title_request (message);
	  break;

	case mt_bitmap_ps_open_request:
	  handle_bitmap_ps_open_request (message);
	  break;
	case mt_bitmap_ps_close:
	  handle_bitmap_ps_close_request (message);
	  break;
	case mt_ps_bitblt:
	  handle_ps_bitblt_request (message);
	  break;
	case mt_ps_write:
	  handle_ps_write_request (message);
	  break;
	case mt_ps_set_font_request:
	  handle_ps_set_font_request (message);
	  break;
	case mt_ps_clear:
	  handle_ps_clear_request (message);
	  break;
	case mt_ps_set_colors:
	  handle_ps_set_colors_request (message);
	  break;
	case mt_ps_move_gcursor:
	  handle_ps_move_gcursor_request (message);
	  break;
	case mt_ps_line:
	  handle_ps_line_request (message);
	  break;
	case mt_ps_poly_line:
	  handle_ps_poly_line_request (message);
	  break;
	case mt_ps_poly_line_disjoint:
	  handle_ps_poly_line_disjoint_request (message);
	  break;
	case mt_ps_set_line_type:
	  handle_ps_set_line_type_request (message);
	  break;
	case mt_ps_query_caps:
	  handle_ps_query_caps_request (message);
	  break;

	default:
	  OS2_logic_error ("Unknown message type sent to PM thread.");
	  break;
	}
    }
  return (MRVOID);
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

wid_t
OS2_window_open (qid_t qid, qid_t event_qid, unsigned long style,
		 const char * title)
{
  msg_t * message
    = (OS2_create_message_1 (mt_window_open_request, (strlen (title))));
  wid_t wid;
  (SM_OPEN_REQUEST_QID (message)) = qid;
  (SM_OPEN_REQUEST_EVENT_QID (message)) = event_qid;
  (SM_OPEN_REQUEST_STYLE (message)) = style;
  strcpy ((SM_OPEN_REQUEST_TITLE (message)), title);
  message = (OS2_message_transaction (qid, message, mt_window_open_reply));
  wid = (SM_OPEN_REPLY_WID (message));
  OS2_destroy_message (message);
  return (wid);
}

static void
handle_window_open_request (msg_t * request)
{
  qid_t sender = (MSG_SENDER (request));
  msg_t * reply = (OS2_create_message (mt_window_open_reply));
  (SM_OPEN_REPLY_WID (reply))
    = (open_window ((SM_OPEN_REQUEST_QID (request)),
		    (SM_OPEN_REQUEST_EVENT_QID (request)),
		    (SM_OPEN_REQUEST_STYLE (request)),
		    (SM_OPEN_REQUEST_TITLE (request))));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

void
OS2_window_permanent (wid_t wid)
{
  (WINDOW_PERMANENTP (wid_to_window (wid))) = 1;
}

void
OS2_window_close (wid_t wid)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_close));
  (SM_CLOSE_WINDOW (message)) = window;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_close_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  close_window (SM_CLOSE_WINDOW (message));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_window_show (wid_t wid, int showp)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_show));
  (SM_SHOW_WINDOW (message)) = window;
  (SM_SHOW_SHOWP (message)) = showp;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_show_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  show_window ((SM_SHOW_WINDOW (message)), (SM_SHOW_SHOWP (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_window_move_cursor (wid_t wid, short x, short y)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_move_cursor));
  (SM_MOVE_CURSOR_WINDOW (message)) = window;
  (SM_MOVE_CURSOR_X (message)) = x;
  (SM_MOVE_CURSOR_Y (message)) = y;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_move_cursor_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  window_t * window = (SM_MOVE_CURSOR_WINDOW (message));
  (WINDOW_CURSOR_X (window)) = (SM_MOVE_CURSOR_X (message));
  (WINDOW_CURSOR_Y (window)) = (SM_MOVE_CURSOR_Y (message));
  OS2_destroy_message (message);
  move_cursor (window, (WINDOW_CURSOR_X (window)), (WINDOW_CURSOR_Y (window)));
  simple_reply (sender);
}

void
OS2_window_shape_cursor (wid_t wid,
			 unsigned short width, unsigned short height,
			 unsigned short style)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_shape_cursor));
  (SM_SHAPE_CURSOR_WINDOW (message)) = window;
  (SM_SHAPE_CURSOR_WIDTH (message)) = width;
  (SM_SHAPE_CURSOR_HEIGHT (message)) = height;
  (SM_SHAPE_CURSOR_STYLE (message)) = style;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_shape_cursor_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  shape_cursor ((SM_SHAPE_CURSOR_WINDOW (message)),
		(SM_SHAPE_CURSOR_WIDTH (message)),
		(SM_SHAPE_CURSOR_HEIGHT (message)),
		(SM_SHAPE_CURSOR_STYLE (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_window_show_cursor (wid_t wid, int showp)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_show_cursor));
  (SM_SHOW_CURSOR_WINDOW (message)) = window;
  (SM_SHOW_CURSOR_SHOWP (message)) = showp;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_show_cursor_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  show_cursor ((SM_SHOW_CURSOR_WINDOW (message)),
	       (SM_SHOW_CURSOR_SHOWP (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_window_scroll (wid_t wid, short xl, short xh, short yl, short yh,
		   short x_delta, short y_delta)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_scroll));
  (SM_SCROLL_WINDOW (message)) = window;
  (SM_SCROLL_XL (message)) = xl;
  (SM_SCROLL_XH (message)) = xh;
  (SM_SCROLL_YL (message)) = yl;
  (SM_SCROLL_YH (message)) = yh;
  (SM_SCROLL_X_DELTA (message)) = x_delta;
  (SM_SCROLL_Y_DELTA (message)) = y_delta;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_scroll_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  RECTL rectl;
  (rectl . xLeft)   = (SM_SCROLL_XL (message));
  (rectl . xRight)  = (SM_SCROLL_XH (message));
  (rectl . yBottom) = (SM_SCROLL_YL (message));
  (rectl . yTop)    = (SM_SCROLL_YH (message));
  scroll_rectangle ((SM_SCROLL_WINDOW (message)),
		    (SM_SCROLL_X_DELTA (message)),
		    (SM_SCROLL_Y_DELTA (message)),
		    (& rectl));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_window_invalidate (wid_t wid, short xl, short xh, short yl, short yh)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_invalidate));
  (SM_INVALIDATE_WINDOW (message)) = window;
  (SM_INVALIDATE_XL (message)) = xl;
  (SM_INVALIDATE_XH (message)) = xh;
  (SM_INVALIDATE_YL (message)) = yl;
  (SM_INVALIDATE_YH (message)) = yh;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_invalidate_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  RECTL rectl;
  (rectl . xLeft)   = (SM_INVALIDATE_XL (message));
  (rectl . xRight)  = (SM_INVALIDATE_XH (message));
  (rectl . yBottom) = (SM_INVALIDATE_YL (message));
  (rectl . yTop)    = (SM_INVALIDATE_YH (message));
  invalidate_rectangle ((SM_INVALIDATE_WINDOW (message)), (& rectl));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_window_set_grid (wid_t wid, unsigned short x, unsigned short y)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_set_grid));
  (SM_SET_GRID_WINDOW (message)) = window;
  (SM_SET_GRID_X (message)) = x;
  (SM_SET_GRID_Y (message)) = y;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_set_grid_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  window_t * window = (SM_SET_GRID_WINDOW (message));
  (WINDOW_GRID_X (window)) = (SM_SET_GRID_X (message));
  (WINDOW_GRID_Y (window)) = (SM_SET_GRID_Y (message));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_window_activate (wid_t wid)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_activate));
  (SM_ACTIVATE_WINDOW (message)) = window;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_activate_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  window_t * window = (SM_ACTIVATE_WINDOW (message));
  OS2_destroy_message (message);
  if (!WinSetActiveWindow (HWND_DESKTOP, (WINDOW_FRAME (window))))
    window_warning (WinSetActiveWindow);
  simple_reply (sender);
}

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
  get_window_pos ((SM_POS_REQUEST_WINDOW (request)),
		  (& (SM_POS_REPLY_X (reply))),
		  (& (SM_POS_REPLY_Y (reply))));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

void
OS2_window_set_pos (wid_t wid, short x, short y)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_set_pos));
  (SM_SET_POS_WINDOW (message)) = window;
  (SM_SET_POS_X (message)) = x;
  (SM_SET_POS_Y (message)) = y;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_set_pos_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  set_window_pos ((SM_SET_POS_WINDOW (message)),
		  (SM_SET_POS_X (message)),
		  (SM_SET_POS_Y (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
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
  get_window_size ((SM_SIZE_REQUEST_WINDOW (request)),
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
  get_window_frame_size ((SM_FRAME_SIZE_REQUEST_WINDOW (request)),
			 (& (SM_FRAME_SIZE_REPLY_WIDTH (reply))),
			 (& (SM_FRAME_SIZE_REPLY_HEIGHT (reply))));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

void
OS2_window_set_size (wid_t wid, unsigned short width, unsigned short height)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_set_size));
  (SM_SET_SIZE_WINDOW (message)) = window;
  (SM_SET_SIZE_WIDTH (message)) = width;
  (SM_SET_SIZE_HEIGHT (message)) = height;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_set_size_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  set_window_size ((SM_SET_SIZE_WINDOW (message)),
		   (SM_SET_SIZE_WIDTH (message)),
		   (SM_SET_SIZE_HEIGHT (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

int
OS2_window_focusp (wid_t wid)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_focusp_request));
  (SM_FOCUSP_REQUEST_WINDOW (message)) = window;
  message
    = (OS2_message_transaction ((WINDOW_QID (window)),
				message,
				mt_window_focusp_reply));
  {
    int result = (SM_FOCUSP_REPLY_FOCUSP (message));
    OS2_destroy_message (message);
    return (result);
  }
}

static void
handle_window_focusp_request (msg_t * request)
{
  qid_t sender = (MSG_SENDER (request));
  msg_t * reply = (OS2_create_message (mt_window_focusp_reply));
  (SM_FOCUSP_REPLY_FOCUSP (reply))
    = (window_focusp (SM_FOCUSP_REQUEST_WINDOW (request)));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

void
OS2_window_set_state (wid_t wid, window_state_t state)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message = (OS2_create_message (mt_window_set_state));
  (SM_SET_STATE_WINDOW (message)) = window;
  (SM_SET_STATE_STATE (message)) = state;
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_set_state_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  set_window_state ((SM_SET_STATE_WINDOW (message)),
		    (SM_SET_STATE_STATE (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_window_set_title (wid_t wid, const char * title)
{
  window_t * window = (wid_to_window (wid));
  msg_t * message
    = (OS2_create_message_1 (mt_window_set_title, (strlen (title))));
  (SM_SET_TITLE_WINDOW (message)) = window;
  strcpy ((SM_SET_TITLE_TITLE (message)), title);
  simple_transaction ((WINDOW_QID (window)), message);
}

static void
handle_window_set_title_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  set_window_title ((SM_SET_TITLE_WINDOW (message)),
		    (SM_SET_TITLE_TITLE (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

psid_t
OS2_open_bitmap_ps (qid_t qid, USHORT width, USHORT height)
{
  msg_t * message = (OS2_create_message (mt_bitmap_ps_open_request));
  psid_t psid;
  (SM_BITMAP_PS_OPEN_REQUEST_QID (message)) = qid;
  (SM_BITMAP_PS_OPEN_REQUEST_WIDTH (message)) = width;
  (SM_BITMAP_PS_OPEN_REQUEST_HEIGHT (message)) = height;
  message  = (OS2_message_transaction (qid, message, mt_bitmap_ps_open_reply));
  psid = (SM_BITMAP_PS_OPEN_REPLY_PSID (message));
  OS2_destroy_message (message);
  return (psid);
}

static void
handle_bitmap_ps_open_request (msg_t * request)
{
  qid_t sender = (MSG_SENDER (request));
  msg_t * reply = (OS2_create_message (mt_bitmap_ps_open_reply));
  (SM_BITMAP_PS_OPEN_REPLY_PSID (reply))
    = (open_bitmap_ps ((SM_BITMAP_PS_OPEN_REQUEST_QID (request)),
		       (SM_BITMAP_PS_OPEN_REQUEST_WIDTH (request)),
		       (SM_BITMAP_PS_OPEN_REQUEST_HEIGHT (request))));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

int
OS2_bitmap_ps_p (psid_t psid)
{
  return ((PS_BITMAP (psid_to_ps (psid))) != NULLHANDLE);
}

void
OS2_close_bitmap_ps (psid_t psid)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message (mt_bitmap_ps_close));
  (SM_BITMAP_PS_CLOSE_PS (message)) = ps;
  simple_transaction ((PS_QID (ps)), message);
}

static void
handle_bitmap_ps_close_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  close_ps (SM_BITMAP_PS_CLOSE_PS (message));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_ps_bitblt (psid_t target, psid_t source, LONG npoints, PPOINTL points,
	       LONG rop, ULONG options)
{
  ps_t * target_ps = (psid_to_ps (target));
  msg_t * message = (OS2_create_message (mt_ps_bitblt));
  (SM_PS_BITBLT_TARGET_PS (message)) = target_ps;
  (SM_PS_BITBLT_SOURCE_PS (message)) = (psid_to_ps (source));
  (SM_PS_BITBLT_NPOINTS (message)) = npoints;
  memcpy ((SM_PS_BITBLT_POINTS (message)),
	  points,
	  ((sizeof (POINTL)) * npoints));
  (SM_PS_BITBLT_ROP (message)) = rop;
  (SM_PS_BITBLT_OPTIONS (message)) = options;
  simple_transaction ((PS_QID (target_ps)), message);
}

static void
handle_ps_bitblt_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  bitblt_ps ((SM_PS_BITBLT_TARGET_PS (message)),
	     (SM_PS_BITBLT_SOURCE_PS (message)),
	     (SM_PS_BITBLT_NPOINTS (message)),
	     (& ((SM_PS_BITBLT_POINTS (message)) [0])),
	     (SM_PS_BITBLT_ROP (message)),
	     (SM_PS_BITBLT_OPTIONS (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_ps_write (psid_t psid, short x, short y,
	      const char * data, unsigned short size)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message_1 (mt_ps_write, (size - 1)));
  (SM_WRITE_PS (message)) = ps;
  (SM_WRITE_X (message)) = x;
  (SM_WRITE_Y (message)) = y;
  (SM_WRITE_SIZE (message)) = size;
  FASTCOPY (data, ((char *) (SM_WRITE_DATA (message))), size);
  simple_transaction ((PS_QID (ps)), message);
}

static void
handle_ps_write_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  write_ps ((SM_WRITE_PS (message)),
	    (SM_WRITE_X (message)),
	    (SM_WRITE_Y (message)),
	    (SM_WRITE_DATA (message)),
	    (SM_WRITE_SIZE (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

font_metrics_t *
OS2_ps_set_font (psid_t psid, unsigned short id, const char * name)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message
    = (OS2_create_message_1 (mt_ps_set_font_request, (strlen (name))));
  font_metrics_t * metrics;
  (SM_SET_FONT_REQUEST_PS (message)) = ps;
  (SM_SET_FONT_REQUEST_ID (message)) = id;
  strcpy ((SM_SET_FONT_REQUEST_SPEC (message)), name);
  message
    = (OS2_message_transaction ((PS_QID (ps)), message, mt_ps_set_font_reply));
  metrics = (SM_SET_FONT_REPLY_METRICS (message));
  OS2_destroy_message (message);
  return (metrics);
}

static void
handle_ps_set_font_request (msg_t * request)
{
  qid_t sender = (MSG_SENDER (request));
  msg_t * reply = (OS2_create_message (mt_ps_set_font_reply));
  (SM_SET_FONT_REPLY_METRICS (reply))
    = (set_font ((SM_SET_FONT_REQUEST_PS (request)),
		 (SM_SET_FONT_REQUEST_ID (request)),
		 (SM_SET_FONT_REQUEST_SPEC (request))));
  OS2_destroy_message (request);
  OS2_send_message (sender, reply);
}

void
OS2_ps_clear (psid_t psid, short xl, short xh, short yl, short yh)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message (mt_ps_clear));
  (SM_CLEAR_PS (message)) = ps;
  (SM_CLEAR_XL (message)) = xl;
  (SM_CLEAR_XH (message)) = xh;
  (SM_CLEAR_YL (message)) = yl;
  (SM_CLEAR_YH (message)) = yh;
  simple_transaction ((PS_QID (ps)), message);
}

static void
handle_ps_clear_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  RECTL rectl;
  (rectl . xLeft)   = (SM_CLEAR_XL (message));
  (rectl . xRight)  = (SM_CLEAR_XH (message));
  (rectl . yBottom) = (SM_CLEAR_YL (message));
  (rectl . yTop)    = (SM_CLEAR_YH (message));
  clear_rectangle ((SM_CLEAR_PS (message)), (& rectl));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_ps_set_colors (psid_t psid, COLOR foreground, COLOR background)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message (mt_ps_set_colors));
  (SM_SET_COLORS_PS (message)) = ps;
  (SM_SET_COLORS_FOREGROUND (message)) = foreground;
  (SM_SET_COLORS_BACKGROUND (message)) = background;
  simple_transaction ((PS_QID (ps)), message);
}

static void
handle_ps_set_colors_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  set_ps_colors ((SM_SET_COLORS_PS (message)),
		 (SM_SET_COLORS_FOREGROUND (message)),
		 (SM_SET_COLORS_BACKGROUND (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_ps_move_gcursor (psid_t psid, short x, short y)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message (mt_ps_move_gcursor));
  (SM_MOVE_GCURSOR_PS (message)) = ps;
  (SM_MOVE_GCURSOR_X (message)) = x;
  (SM_MOVE_GCURSOR_Y (message)) = y;
  simple_transaction ((PS_QID (ps)), message);
}

static void
handle_ps_move_gcursor_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  move_gcursor ((SM_MOVE_GCURSOR_PS (message)),
		(SM_MOVE_GCURSOR_X (message)),
		(SM_MOVE_GCURSOR_Y (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_ps_line (psid_t psid, short x, short y)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message (mt_ps_line));
  (SM_LINE_PS (message)) = ps;
  (SM_LINE_X (message)) = x;
  (SM_LINE_Y (message)) = y;
  simple_transaction ((PS_QID (ps)), message);
}

static void
handle_ps_line_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  draw_line ((SM_LINE_PS (message)),
	     (SM_LINE_X (message)),
	     (SM_LINE_Y (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_ps_poly_line (psid_t psid, unsigned long npoints, PPOINTL points)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message (mt_ps_poly_line));
  (SM_POLY_LINE_PS (message)) = ps;
  (SM_POLY_LINE_NPOINTS (message)) = npoints;
  (SM_POLY_LINE_POINTS (message)) = points;
  sync_transaction ((PS_QID (ps)), message);
}

static void
handle_ps_poly_line_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  poly_line ((SM_POLY_LINE_PS (message)),
	     (SM_POLY_LINE_NPOINTS (message)),
	     (SM_POLY_LINE_POINTS (message)));
  OS2_destroy_message (message);
  sync_reply (sender);
}

void
OS2_ps_poly_line_disjoint (psid_t psid, unsigned long npoints, PPOINTL points)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message (mt_ps_poly_line_disjoint));
  (SM_POLY_LINE_DISJOINT_PS (message)) = ps;
  (SM_POLY_LINE_DISJOINT_NPOINTS (message)) = npoints;
  (SM_POLY_LINE_DISJOINT_POINTS (message)) = points;
  sync_transaction ((PS_QID (ps)), message);
}

static void
handle_ps_poly_line_disjoint_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  poly_line_disjoint ((SM_POLY_LINE_DISJOINT_PS (message)),
		      (SM_POLY_LINE_DISJOINT_NPOINTS (message)),
		      (SM_POLY_LINE_DISJOINT_POINTS (message)));
  OS2_destroy_message (message);
  sync_reply (sender);
}

void
OS2_ps_set_line_type (psid_t psid, LONG type)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message (mt_ps_set_line_type));
  (SM_SET_LINE_TYPE_PS (message)) = ps;
  (SM_SET_LINE_TYPE_TYPE (message)) = type;
  simple_transaction ((PS_QID (ps)), message);
}

static void
handle_ps_set_line_type_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  set_line_type ((SM_SET_LINE_TYPE_PS (message)),
		 (SM_SET_LINE_TYPE_TYPE (message)));
  OS2_destroy_message (message);
  simple_reply (sender);
}

void
OS2_ps_query_caps (psid_t psid, LONG start, LONG count, PLONG values)
{
  ps_t * ps = (psid_to_ps (psid));
  msg_t * message = (OS2_create_message (mt_ps_query_caps));
  (SM_QUERY_CAPS_PS (message)) = ps;
  (SM_QUERY_CAPS_START (message)) = start;
  (SM_QUERY_CAPS_COUNT (message)) = count;
  (SM_QUERY_CAPS_VALUES (message)) = values;
  sync_transaction ((PS_QID (ps)), message);
}

static void
handle_ps_query_caps_request (msg_t * message)
{
  qid_t sender = (MSG_SENDER (message));
  query_caps ((SM_QUERY_CAPS_PS (message)),
	      (SM_QUERY_CAPS_START (message)),
	      (SM_QUERY_CAPS_COUNT (message)),
	      (SM_QUERY_CAPS_VALUES (message)));
  OS2_destroy_message (message);
  sync_reply (sender);
}

void
OS2_window_write (wid_t wid, short x, short y,
		  const char * data, unsigned short size)
{
  OS2_ps_write ((OS2_window_client_ps (wid)), x, y, data, size);
}

font_metrics_t *
OS2_window_set_font (wid_t wid, unsigned short id, const char * name)
{
  return (OS2_ps_set_font ((OS2_window_client_ps (wid)), id, name));
}

void
OS2_window_clear (wid_t wid, short xl, short xh, short yl, short yh)
{
  OS2_ps_clear ((OS2_window_client_ps (wid)), xl, xh, yl, yh);
}

static window_t * make_window (qid_t, qid_t);

static wid_t
open_window (qid_t qid, qid_t event_qid, ULONG style, PSZ title)
{
  window_t * window = (make_window (qid, event_qid));
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
			title,	/* title string */
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
  (WINDOW_ID (window)) = (allocate_id ((& wid_table), window));
  (WINDOW_CLIENT (window)) = NULLHANDLE;
  (WINDOW_CLIENT_PS (window)) = 0;
  (WINDOW_CURSOR_X (window)) = 0;
  (WINDOW_CURSOR_Y (window)) = 0;
  (WINDOW_CURSOR_WIDTH (window)) = 0;
  (WINDOW_CURSOR_HEIGHT (window)) = 0;
  (WINDOW_CURSOR_STYLE (window)) = (CURSOR_SOLID | CURSOR_FLASH);
  (WINDOW_CURSOR_SHOWNP (window)) = 0;
  (WINDOW_QID (window)) = qid;
  (WINDOW_EVENT_QID (window)) = event_qid;
  (WINDOW_MINIMIZINGP (window)) = 0;
  (WINDOW_MINIMIZEDP (window)) = 0;
  (WINDOW_PERMANENTP (window)) = 0;
  return (window);
}

static void
close_window (window_t * window)
{
  if (!WinDestroyWindow (WINDOW_FRAME (window)))
    window_warning (WinDestroyWindow);
  deallocate_id ((& wid_table), (WINDOW_ID (window)));
  OS_free (window);
}

static void
show_window (window_t * window, int showp)
{
  if (!WinShowWindow ((WINDOW_FRAME (window)), showp))
    window_warning (WinShowWindow);
}

static void
move_cursor (window_t * window, short x, short y)
{
  if (window_focusp (window))
    if (!WinCreateCursor ((WINDOW_CLIENT (window)),
			  x, y, 0, 0, CURSOR_SETPOS, 0))
      window_warning (WinCreateCursor);
}

static void
shape_cursor (window_t * window, unsigned short width, unsigned short height,
	      unsigned short style)
{
  (WINDOW_CURSOR_WIDTH (window)) = width;
  (WINDOW_CURSOR_HEIGHT (window)) = height;
  (WINDOW_CURSOR_STYLE (window)) = style;
  if (window_focusp (window))
    recreate_cursor (window);
}

static void
show_cursor (window_t * window, int showp)
{
  if (showp != 0)
    showp = 1;
  if ((window_focusp (window)) && (showp != (WINDOW_CURSOR_SHOWNP (window))))
    if (!WinShowCursor ((WINDOW_CLIENT (window)), showp))
      window_warning (WinShowCursor);
  (WINDOW_CURSOR_SHOWNP (window)) = showp;
}

static void
recreate_cursor (window_t * window)
{
  if (!WinCreateCursor ((WINDOW_CLIENT (window)),
			(WINDOW_CURSOR_X (window)),
			(WINDOW_CURSOR_Y (window)),
			(WINDOW_CURSOR_WIDTH (window)),
			(WINDOW_CURSOR_HEIGHT (window)),
			(WINDOW_CURSOR_STYLE (window)),
			0))
    window_warning (WinCreateCursor);
  if (WINDOW_CURSOR_SHOWNP (window))
    if (!WinShowCursor ((WINDOW_CLIENT (window)), TRUE))
      window_warning (WinShowCursor);
}

static void
activate_cursor (window_t * window)
{
  if ((WINDOW_CURSOR_SHOWNP (window)) && (window_focusp (window)))
    if (!WinShowCursor ((WINDOW_CLIENT (window)), TRUE))
      window_warning (WinShowCursor);
}

static void
deactivate_cursor (window_t * window)
{
  if ((WINDOW_CURSOR_SHOWNP (window)) && (window_focusp (window)))
    if (!WinShowCursor ((WINDOW_CLIENT (window)), FALSE))
      window_warning (WinShowCursor);
}

static void
scroll_rectangle (window_t * window, short x_delta, short y_delta,
		  PRECTL rectl)
{
  deactivate_cursor (window);
  if ((WinScrollWindow ((WINDOW_CLIENT (window)), x_delta, y_delta, rectl,
			0, NULLHANDLE, 0, 0))
      == RGN_ERROR)
    window_warning (WinScrollWindow);
  activate_cursor (window);
}

static void
invalidate_rectangle (window_t * window, PRECTL rectl)
{
  if (!WinInvalidateRect ((WINDOW_CLIENT (window)), rectl, FALSE))
    window_warning (WinInvalidateRect);
}

static void
get_window_pos (window_t * window, short * x, short * y)
{
  SWP swp;
  if (!WinQueryWindowPos ((WINDOW_FRAME (window)), (& swp)))
    window_error (WinQueryWindowPos);
  (* x) = (swp . x);
  (* y) = (swp . y);
}

static void
set_window_pos (window_t * window, short x, short y)
{
  if (!WinSetWindowPos ((WINDOW_FRAME (window)), NULLHANDLE, x, y,
			0, 0, SWP_MOVE))
    window_warning (WinSetWindowPos);
}

static void
get_window_size (window_t * window,
		 unsigned short * width, unsigned short * height)
{
  SWP swp;
  if (!WinQueryWindowPos ((WINDOW_CLIENT (window)), (& swp)))
    window_error (WinQueryWindowPos);
  (* width) = (swp . cx);
  (* height) = (swp . cy);
}

static void
get_window_frame_size (window_t * window,
		       unsigned short * width, unsigned short * height)
{
  SWP swp;
  if (!WinQueryWindowPos ((WINDOW_FRAME (window)), (& swp)))
    window_error (WinQueryWindowPos);
  (* width) = (swp . cx);
  (* height) = (swp . cy);
}

static void
set_window_size (window_t * window,
		 unsigned short width, unsigned short height)
{
  SWP swp;
  POINTL ptl;
  RECTL rcl;
  if (!WinQueryWindowPos ((WINDOW_CLIENT (window)), (& swp)))
    window_error (WinQueryWindowPos);
  (ptl . x) = (swp . x);
  (ptl . y) = (swp . y);
  if (!WinMapWindowPoints ((WINDOW_FRAME (window)), HWND_DESKTOP, (& ptl), 1))
    window_error (WinMapWindowPoints);
  (rcl . xLeft) = (ptl . x);
  (rcl . xRight) = ((ptl . x) + width);
  (rcl . yBottom) = (ptl . y);
  (rcl . yTop) = ((ptl . y) + height);
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
set_window_state (window_t * window, window_state_t state)
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
set_window_title (window_t * window, PSZ title)
{
  if (!WinSetWindowText ((WINDOW_FRAME (window)), title))
    window_warning (WinSetWindowText);
}

static HDC create_memory_device (void);
static HBITMAP create_bitmap (HPS, ULONG, ULONG);

static psid_t
open_bitmap_ps (qid_t qid, USHORT width, USHORT height)
{
  ps_t * ps = (make_ps ((create_memory_device ()), qid));
  HBITMAP hbm = (create_bitmap ((PS_HANDLE (ps)), width, height));
  if ((GpiSetBitmap ((PS_HANDLE (ps)), hbm)) == HBM_ERROR)
    window_error (GpiSetBitmap);
  (PS_BITMAP (ps)) = hbm;
  return (PS_ID (ps));
}

static HDC
create_memory_device (void)
{
  HDC hdc = (DevOpenDC (pm_hab, OD_MEMORY, "*", 0, 0, NULLHANDLE));
  if (hdc == DEV_ERROR)
    window_error (DevOpenDC);
  return (hdc);
}

static HBITMAP
create_bitmap (HPS hps, ULONG width, ULONG height)
{
  HDC hdc = (get_ps_device (hps));
  BITMAPINFOHEADER2 header;
  memset ((& header), 0, (sizeof (header)));
  (header . cbFix) = (sizeof (header));
  (header . cx) = width;
  (header . cy) = height;
  (header . cPlanes) = (get_device_capability (hdc, CAPS_COLOR_PLANES));
  (header . cBitCount) = (get_device_capability (hdc, CAPS_COLOR_BITCOUNT));
  {
    HBITMAP hbm = (GpiCreateBitmap (hps, (& header), 0, 0, 0));
    if (hbm == GPI_ERROR)
      window_error (GpiCreateBitmap);
    return (hbm);
  }
}

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
make_ps (HDC hdc, qid_t qid)
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
  if (!GpiSetBackMix (hps, BM_OVERPAINT))
    window_warning (GpiSetBackMix);
  /* Put color table in RGB mode so we can specify colors
     directly in RGB values rather than as indices.  */
  if (!GpiCreateLogColorTable (hps, LCOL_PURECOLOR, LCOLF_RGB, 0, 0, 0))
    window_warning (GpiCreateLogColorTable);
  (PS_HANDLE (ps)) = hps;
  (PS_ID (ps)) = (allocate_id ((& psid_table), ps));
  (PS_QID (ps)) = qid;
  (PS_FOREGROUND_COLOR (ps)) = (GpiQueryColor (hps));
  (PS_BACKGROUND_COLOR (ps)) = (GpiQueryBackColor (hps));
  (PS_WINDOW (ps)) = 0;
  (PS_BITMAP (ps)) = NULLHANDLE;
  return (ps);
}

static void
close_ps (ps_t * ps)
{
  HPS hps = (PS_HANDLE (ps));
  HDC hdc = (get_ps_device (hps));
  HBITMAP hbm = (PS_BITMAP (ps));
  if (!GpiDestroyPS (hps))
    window_warning (GpiDestroyPS);
  if (hbm != 0)
    {
      if ((hdc != NULLHANDLE) && ((DevCloseDC (hdc)) == DEV_ERROR))
	window_warning (DevCloseDC);
      if (!GpiDeleteBitmap (hbm))
	window_warning (GpiDeleteBitmap);
    }
  deallocate_id ((& psid_table), (PS_ID (ps)));
  OS_free (ps);
}

static void
bitblt_ps (ps_t * target, ps_t * source, LONG npoints, PPOINTL points,
	   LONG rop, ULONG options)
{
  if ((GpiBitBlt ((PS_HANDLE (target)), (PS_HANDLE (source)), npoints, points,
		  rop, options))
      == GPI_ERROR)
    window_warning (GpiBitBlt);
}

static void
write_ps (ps_t * ps, short x, short y, const char * data, unsigned short size)
{
  POINTL ptl;
  (ptl . x) = x;
  (ptl . y) = y;
  maybe_deactivate_cursor (ps);
  if (size <= 512)
    GpiCharStringAt ((PS_HANDLE (ps)), (& ptl), size, ((char *) data));
  else
    {
      const char * scan = data;
      GpiMove ((PS_HANDLE (ps)), (& ptl));
      while (size > 0)
	{
	  unsigned short n = ((size > 512) ? 512 : size);
	  GpiCharString ((PS_HANDLE (ps)), n, ((char *) scan));
	  size -= n;
	  scan += n;
	}
    }
  maybe_activate_cursor (ps);
}

static void
maybe_activate_cursor (ps_t * ps)
{
  window_t * window = (PS_WINDOW (ps));
  if (window != 0)
    activate_cursor (window);
}

static void
maybe_deactivate_cursor (ps_t * ps)
{
  window_t * window = (PS_WINDOW (ps));
  if (window != 0)
    deactivate_cursor (window);
}

static void
clear_rectangle (ps_t * ps, PRECTL rectl)
{
  maybe_deactivate_cursor (ps);
  if (!WinFillRect ((PS_HANDLE (ps)), rectl, (PS_BACKGROUND_COLOR (ps))))
    window_warning (WinFillRect);
  maybe_activate_cursor (ps);
}

static void
set_ps_colors (ps_t * ps, COLOR foreground, COLOR background)
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
move_gcursor (ps_t * ps, short x, short y)
{
  POINTL ptl;
  (ptl . x) = x;
  (ptl . y) = y;
  if (!GpiMove ((PS_HANDLE (ps)), (& ptl)))
    window_warning (GpiMove);
}

static void
draw_line (ps_t * ps, short x, short y)
{
  POINTL ptl;
  (ptl . x) = x;
  (ptl . y) = y;
  if ((GpiLine ((PS_HANDLE (ps)), (& ptl))) == GPI_ERROR)
    window_warning (GpiLine);
}

static void
poly_line (ps_t * ps, unsigned long npoints, PPOINTL points)
{
  if ((GpiPolyLine ((PS_HANDLE (ps)), npoints, points)) == GPI_ERROR)
    window_warning (GpiPolyLine);
}

static void
poly_line_disjoint (ps_t * ps, unsigned long npoints, PPOINTL points)
{
  if ((GpiPolyLineDisjoint ((PS_HANDLE (ps)), npoints, points))
      == GPI_ERROR)
    window_warning (GpiPolyLineDisjoint);
}

static void
set_line_type (ps_t * ps, LONG type)
{
  if (!GpiSetLineType ((PS_HANDLE (ps)), type))
    window_warning (GpiSetLineType);
}

static void
query_caps (ps_t * ps, LONG start, LONG count, PLONG values)
{
  HDC hdc = (get_ps_device (PS_HANDLE (ps)));
  if (hdc == NULLHANDLE)
    window_error (GpiQueryDevice);
  if (!DevQueryCaps (hdc, start, count, values))
    window_error (DevQueryCaps);
}

static int parse_font_spec (const char *, PSZ *, LONG *, USHORT *);
static int set_font_1 (HPS, PSZ, LONG, USHORT, LONG);

static font_metrics_t *
set_font (ps_t * ps, unsigned short id, const char * spec)
{
  PSZ name = 0;
  LONG size;
  USHORT selection;
  if (!parse_font_spec (spec, (& name), (& size), (& selection)))
    return (0);
  if (!set_font_1 ((PS_HANDLE (ps)), name, size, selection, id))
    {
      OS_free (name);
      return (0);
    }
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
}

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
      if ((strncmp (scan, ".bold", 5)) == 0)
	{
	  selection |= FATTR_SEL_BOLD;
	  scan += 5;
	}
      else if ((strncmp (scan, ".italic", 7)) == 0)
	{
	  selection |= FATTR_SEL_ITALIC;
	  scan += 7;
	}
      else
	break;
    }
  if ((*scan++) != '.')
    return (0);
  (*pname) = (OS_malloc ((strlen (scan)) + 1));
  strcpy ((*pname), scan);
  (*psize) = (size * 10);
  (*pselection) = selection;
  return (1);
}

static int create_font (HPS, LONG, PFONTMETRICS, USHORT);

static int
set_font_1 (HPS hps, PSZ name, LONG size, USHORT selection, LONG id)
{
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
  for (index = 0; (index < nfonts); index += 1)
    if (((((pfm [index]) . fsType) & FM_TYPE_FIXED) != 0)
	&& ((((pfm [index]) . fsDefn) & FM_DEFN_OUTLINE) == 0)
	&& (((pfm [index]) . sNominalPointSize) == size)
	&& (create_font (hps, id, (& (pfm [index])), selection)))
      {
	GpiSetCharSet (hps, id);
	OS_free (pfm);
	return (1);
      }
  OS_free (pfm);
  return (0);
}

static int
create_font (HPS hps, LONG font_id, PFONTMETRICS pfm, USHORT selection)
{
  FATTRS font_attrs;

  (font_attrs . usRecordLength) = (sizeof (font_attrs));
  (font_attrs . fsSelection) = selection;
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
  return ((GpiCreateLogFont (hps, 0, font_id, (& font_attrs))) == FONT_MATCH);
}

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

static int process_keychar
  (window_t *, unsigned short, unsigned char, unsigned char, unsigned short,
   unsigned short);
static int process_button (HWND, MPARAM, MPARAM, unsigned char, unsigned char);

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
	  = (make_ps ((WinOpenWindowDC (hwnd)), (WINDOW_QID (window))));
	(PS_WINDOW (WINDOW_CLIENT_PS (window))) = window;
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
	    if (!WinDestroyCursor (WINDOW_CLIENT (window)))
	      window_warning (WinDestroyCursor);
	  }
	SEND_EVENT (window,
		    (make_focus_event ((WINDOW_ID (window)),
				       (SHORT1FROMMP (mp2)))));
	return (MRVOID);
      }
    case WM_CHAR:
      return
	((process_keychar ((hwnd_to_window (hwnd)),
			   (SHORT1FROMMP (mp1)),
			   (CHAR3FROMMP (mp1)),
			   (CHAR4FROMMP (mp1)),
			   (SHORT1FROMMP (mp2)),
			   (SHORT2FROMMP (mp2))))
	 ? MRTRUE
	 : MRFALSE);

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
    case WM_CLOSE:
      {
	window_t * window = (hwnd_to_window (hwnd));
	SEND_EVENT (window, (make_close_event (WINDOW_ID (window))));
	return (MRVOID);
      }
    case WM_DESTROY:
      {
	window_t * window = (hwnd_to_window (hwnd));
	close_ps (WINDOW_CLIENT_PS (window));
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
	if (window_focusp (window))
	  {
	    if (!WinDestroyCursor (WINDOW_CLIENT (window)))
	      window_warning (WinDestroyCursor);
	    (WINDOW_CURSOR_X (window)) = 0;
	    (WINDOW_CURSOR_Y (window)) = 0;
	    recreate_cursor (window);
	  }
	SEND_EVENT (window,
		    (make_resize_event ((WINDOW_ID (window)),
					(SHORT1FROMMP (mp2)),
					(SHORT2FROMMP (mp2)))));
	return (MRVOID);
      }
    case WM_SHOW:
      {
	window_t * window = (hwnd_to_window (hwnd));
	SEND_EVENT (window,
		    (make_visibility_event ((WINDOW_ID (window)),
					    (SHORT1FROMMP (mp1)))));
	return (MRVOID);
      }
    case WM_BUTTON1DOWN:
      if (process_button (hwnd, mp1, mp2, 0, BUTTON_EVENT_DOWN))
	return (MRTRUE);
      break;
    case WM_BUTTON1UP:
      if (process_button (hwnd, mp1, mp2, 0, BUTTON_EVENT_UP))
	return (MRTRUE);
      break;
    case WM_BUTTON1CLICK:
      if (process_button (hwnd, mp1, mp2, 0, BUTTON_EVENT_CLICK))
	return (MRTRUE);
      break;
    case WM_BUTTON1DBLCLK:
      if (process_button (hwnd, mp1, mp2, 0, BUTTON_EVENT_DBLCLK))
	return (MRTRUE);
      break;
    case WM_BUTTON2DOWN:
      if (process_button (hwnd, mp1, mp2, 1, BUTTON_EVENT_DOWN))
	return (MRTRUE);
      break;
    case WM_BUTTON2UP:
      if (process_button (hwnd, mp1, mp2, 1, BUTTON_EVENT_UP))
	return (MRTRUE);
      break;
    case WM_BUTTON2CLICK:
      if (process_button (hwnd, mp1, mp2, 1, BUTTON_EVENT_CLICK))
	return (MRTRUE);
      break;
    case WM_BUTTON2DBLCLK:
      if (process_button (hwnd, mp1, mp2, 1, BUTTON_EVENT_DBLCLK))
	return (MRTRUE);
      break;
    case WM_BUTTON3DOWN:
      if (process_button (hwnd, mp1, mp2, 2, BUTTON_EVENT_DOWN))
	return (MRTRUE);
      break;
    case WM_BUTTON3UP:
      if (process_button (hwnd, mp1, mp2, 2, BUTTON_EVENT_UP))
	return (MRTRUE);
      break;
    case WM_BUTTON3CLICK:
      if (process_button (hwnd, mp1, mp2, 2, BUTTON_EVENT_CLICK))
	return (MRTRUE);
      break;
    case WM_BUTTON3DBLCLK:
      if (process_button (hwnd, mp1, mp2, 2, BUTTON_EVENT_DBLCLK))
	return (MRTRUE);
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

static int
process_keychar (window_t * window, unsigned short flags,
		 unsigned char repeat, unsigned char scan_code,
		 unsigned short char_code, unsigned short virtual_key)
{
  unsigned short code;
  /* Ignore compound keys for now.  */
  if ((flags & (KC_DEADKEY | KC_COMPOSITE | KC_INVALIDCOMP | KC_KEYUP)) != 0)
    return (0);
  else if ((flags & KC_VIRTUALKEY) != 0)
    code = virtual_key;
  else if ((flags & (KC_CHAR | KC_CTRL | KC_ALT)) != 0)
    code = char_code;
  else
    return (0);
  SEND_EVENT
    (window,
     (make_key_event ((WINDOW_ID (window)), code, flags, repeat)));
}

static int
process_button (HWND hwnd, MPARAM mp1, MPARAM mp2,
		unsigned char number, unsigned char type)
{
  window_t * window = (hwnd_to_window (hwnd));
  SEND_EVENT (window,
	      (make_button_event ((WINDOW_ID (window)),
				  number,
				  type,
				  (SHORT1FROMMP (mp1)),
				  (SHORT2FROMMP (mp1)),
				  ((SHORT2FROMMP (mp2))
				   & (KC_SHIFT | KC_CTRL | KC_ALT)))));
  return (1);
}

static msg_t *
make_button_event (wid_t wid, unsigned char number, unsigned char type,
		   unsigned short x, unsigned short y, unsigned short flags)
{
  msg_t * message = (OS2_create_message (mt_button_event));
  (SM_BUTTON_EVENT_WID (message)) = wid;
  (SM_BUTTON_EVENT_TYPE (message)) = (number | (type << 4));
  (SM_BUTTON_EVENT_X (message)) = x;
  (SM_BUTTON_EVENT_Y (message)) = y;
  (SM_BUTTON_EVENT_FLAGS (message)) = flags;
  return (message);
}

static msg_t *
make_close_event (wid_t wid)
{
  msg_t * message = (OS2_create_message (mt_close_event));
  (SM_CLOSE_EVENT_WID (message)) = wid;
  return (message);
}

static msg_t *
make_focus_event (wid_t wid, int gainedp)
{
  msg_t * message = (OS2_create_message (mt_focus_event));
  (SM_FOCUS_EVENT_WID (message)) = wid;
  (SM_FOCUS_EVENT_GAINEDP (message)) = gainedp;
  return (message);
}

static msg_t *
make_key_event (wid_t wid, unsigned short code,
		unsigned short flags, unsigned short repeat)
{
  msg_t * message = (OS2_create_message (mt_key_event));
  (SM_KEY_EVENT_WID (message)) = wid;
  (SM_KEY_EVENT_CODE (message)) = code;
  (SM_KEY_EVENT_FLAGS (message)) = flags;
  (SM_KEY_EVENT_REPEAT (message)) = repeat;
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

static msg_t *
make_resize_event (wid_t wid, unsigned short width, unsigned short height)
{
  msg_t * message = (OS2_create_message (mt_resize_event));
  (SM_RESIZE_EVENT_WID (message)) = wid;
  (SM_RESIZE_EVENT_WIDTH (message)) = width;
  (SM_RESIZE_EVENT_HEIGHT (message)) = height;
  return (message);
}

static msg_t *
make_visibility_event (wid_t wid, int shownp)
{
  msg_t * message = (OS2_create_message (mt_visibility_event));
  (SM_VISIBILITY_EVENT_WID (message)) = wid;
  (SM_VISIBILITY_EVENT_SHOWNP (message)) = shownp;
  return (message);
}
