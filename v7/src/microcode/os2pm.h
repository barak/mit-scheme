/* -*-C-*-

$Id: os2pm.h,v 1.3 1995/01/06 00:00:27 cph Exp $

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

#ifndef SCM_OS2PM_H
#define SCM_OS2PM_H

typedef unsigned short wid_t;

typedef struct
{
  unsigned short width;
  unsigned short height;
  unsigned short descender;
} font_metrics_t;
#define FONT_METRICS_WIDTH(m) ((m) -> width)
#define FONT_METRICS_HEIGHT(m) ((m) -> height)
#define FONT_METRICS_DESCENDER(m) ((m) -> descender)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  wid_t wid;
  unsigned char btype;
  unsigned short x;
  unsigned short y;
  unsigned short flags;
} sm_button_event_t;
#define SM_BUTTON_EVENT_WID(m) (((sm_button_event_t *) (m)) -> wid)
#define SM_BUTTON_EVENT_TYPE(m) (((sm_button_event_t *) (m)) -> btype)
#define SM_BUTTON_EVENT_X(m) (((sm_button_event_t *) (m)) -> x)
#define SM_BUTTON_EVENT_Y(m) (((sm_button_event_t *) (m)) -> y)
#define SM_BUTTON_EVENT_FLAGS(m) (((sm_button_event_t *) (m)) -> flags)

#define BUTTON_EVENT_DOWN 0
#define BUTTON_EVENT_UP 1
#define BUTTON_EVENT_CLICK 2
#define BUTTON_EVENT_DBLCLK 3

#define BUTTON_TYPE_NUMBER(type) ((type) & 0xf)
#define BUTTON_TYPE_EVENT(type) (((type) >> 4) & 0xf)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  wid_t wid;
} sm_close_event_t;
#define SM_CLOSE_EVENT_WID(m) (((sm_close_event_t *) (m)) -> wid)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  wid_t wid;
  char gainedp;
} sm_focus_event_t;
#define SM_FOCUS_EVENT_WID(m) (((sm_focus_event_t *) (m)) -> wid)
#define SM_FOCUS_EVENT_GAINEDP(m) (((sm_focus_event_t *) (m)) -> gainedp)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  wid_t wid;
  unsigned short code;
  unsigned short flags;
  unsigned short repeat;
} sm_key_event_t;
#define SM_KEY_EVENT_WID(m) (((sm_key_event_t *) (m)) -> wid)
#define SM_KEY_EVENT_CODE(m) (((sm_key_event_t *) (m)) -> code)
#define SM_KEY_EVENT_FLAGS(m) (((sm_key_event_t *) (m)) -> flags)
#define SM_KEY_EVENT_REPEAT(m) (((sm_key_event_t *) (m)) -> repeat)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  wid_t wid;
  unsigned short xl;
  unsigned short xh;
  unsigned short yl;
  unsigned short yh;
} sm_paint_event_t;
#define SM_PAINT_EVENT_WID(m) (((sm_paint_event_t *) (m)) -> wid)
#define SM_PAINT_EVENT_XL(m) (((sm_paint_event_t *) (m)) -> xl)
#define SM_PAINT_EVENT_XH(m) (((sm_paint_event_t *) (m)) -> xh)
#define SM_PAINT_EVENT_YL(m) (((sm_paint_event_t *) (m)) -> yl)
#define SM_PAINT_EVENT_YH(m) (((sm_paint_event_t *) (m)) -> yh)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  wid_t wid;
  unsigned short width;
  unsigned short height;
} sm_resize_event_t;
#define SM_RESIZE_EVENT_WID(m) (((sm_resize_event_t *) (m)) -> wid)
#define SM_RESIZE_EVENT_WIDTH(m) (((sm_resize_event_t *) (m)) -> width)
#define SM_RESIZE_EVENT_HEIGHT(m) (((sm_resize_event_t *) (m)) -> height)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  wid_t wid;
  char shownp;
} sm_visibility_event_t;
#define SM_VISIBILITY_EVENT_WID(m) (((sm_visibility_event_t *) (m)) -> wid)
#define SM_VISIBILITY_EVENT_SHOWNP(m)					\
  (((sm_visibility_event_t *) (m)) -> shownp)

typedef enum
{
  state_top,
  state_bottom,
  state_show,
  state_hide,
  state_activate,
  state_deactivate,
  state_minimize,
  state_maximize,
  state_restore,
  state_supremum
} window_state_t;

extern msg_t * OS2_read_pm_tqueue (tqueue_t *, int);
extern void OS2_write_pm_tqueue (tqueue_t *, msg_t *);

extern int OS2_wid_validp (wid_t);
extern qid_t OS2_create_pm_qid (tqueue_t *);
extern wid_t OS2_window_open (qid_t, qid_t, unsigned long, const char *);
extern void OS2_window_permanent (wid_t);
extern void OS2_window_close (wid_t);
extern void OS2_window_show (wid_t, int);
extern void OS2_window_write
  (wid_t, short, short, const char *, unsigned short);
extern void OS2_window_move_cursor (wid_t, short, short);
extern void OS2_window_shape_cursor
  (wid_t, unsigned short, unsigned short, unsigned short);
extern void OS2_window_show_cursor (wid_t, int);
extern void OS2_window_clear (wid_t, short, short, short, short);
extern void OS2_window_scroll
  (wid_t, short, short, short, short, short, short);
extern void OS2_window_invalidate (wid_t, short, short, short, short);
extern font_metrics_t * OS2_window_set_font
  (wid_t, unsigned short, const char *);
extern void OS2_window_set_grid (wid_t, unsigned short, unsigned short);
extern void OS2_window_activate (wid_t);
extern void OS2_window_pos (wid_t, short *, short *);
extern void OS2_window_set_pos (wid_t, short, short);
extern void OS2_window_size (wid_t, unsigned short *, unsigned short *);
extern void OS2_window_set_size (wid_t, unsigned short, unsigned short);
extern int OS2_window_focusp (wid_t);
extern void OS2_window_set_state (wid_t, window_state_t);
extern void OS2_window_set_colors (wid_t, COLOR, COLOR);
extern void OS2_window_move_gcursor (wid_t, short, short);
extern void OS2_window_line (wid_t, short, short);
extern void OS2_window_poly_line (wid_t, unsigned long, PPOINTL);
extern void OS2_window_poly_line_disjoint (wid_t, unsigned long, PPOINTL);
extern void OS2_window_set_line_type (wid_t, LONG);
extern void OS2_window_query_caps (wid_t, LONG, LONG, PLONG);
extern void OS2_window_set_title (wid_t, const char *);

#endif /* SCM_OS2PM_H */
