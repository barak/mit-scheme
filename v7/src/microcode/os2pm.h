/* -*-C-*-

$Id: os2pm.h,v 1.1 1994/12/02 20:44:35 cph Exp $

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

#ifndef SCM_OS2PM_H
#define SCM_OS2PM_H

typedef unsigned short twid_t;

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  qid_t event_qid;
  const char * title;
} sm_twindow_open_request_t;
#define SM_TWINDOW_OPEN_REQUEST_EVENT_QID(m)				\
  (((sm_twindow_open_request_t *) (m)) -> event_qid)
#define SM_TWINDOW_OPEN_REQUEST_TITLE(m)				\
  (((sm_twindow_open_request_t *) (m)) -> title)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
} sm_twindow_open_reply_t;
#define SM_TWINDOW_OPEN_REPLY_TWID(m)					\
  (((sm_twindow_open_reply_t *) (m)) -> twid)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
} sm_twindow_close_request_t;
#define SM_TWINDOW_CLOSE_REQUEST_TWID(m)				\
  (((sm_twindow_close_request_t *) (m)) -> twid)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
  unsigned short x;
  unsigned short y;
  unsigned short size;
  const char * data;
} sm_twindow_write_request_t;
#define SM_TWINDOW_WRITE_REQUEST_TWID(m)				\
  (((sm_twindow_write_request_t *) (m)) -> twid)
#define SM_TWINDOW_WRITE_REQUEST_X(m)					\
  (((sm_twindow_write_request_t *) (m)) -> x)
#define SM_TWINDOW_WRITE_REQUEST_Y(m)					\
  (((sm_twindow_write_request_t *) (m)) -> y)
#define SM_TWINDOW_WRITE_REQUEST_SIZE(m)				\
  (((sm_twindow_write_request_t *) (m)) -> size)
#define SM_TWINDOW_WRITE_REQUEST_DATA(m)				\
  (((sm_twindow_write_request_t *) (m)) -> data)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
  unsigned short x;
  unsigned short y;
} sm_twindow_move_cursor_request_t;
#define SM_TWINDOW_MOVE_CURSOR_REQUEST_TWID(m)				\
  (((sm_twindow_move_cursor_request_t *) (m)) -> twid)
#define SM_TWINDOW_MOVE_CURSOR_REQUEST_X(m)				\
  (((sm_twindow_move_cursor_request_t *) (m)) -> x)
#define SM_TWINDOW_MOVE_CURSOR_REQUEST_Y(m)				\
  (((sm_twindow_move_cursor_request_t *) (m)) -> y)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
} sm_twindow_clear_request_t;
#define SM_TWINDOW_CLEAR_REQUEST_TWID(m)				\
  (((sm_twindow_clear_request_t *) (m)) -> twid)
#define SM_TWINDOW_CLEAR_REQUEST_X(m)					\
  (((sm_twindow_clear_request_t *) (m)) -> x)
#define SM_TWINDOW_CLEAR_REQUEST_Y(m)					\
  (((sm_twindow_clear_request_t *) (m)) -> y)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
  unsigned short x;
  unsigned short y;
} sm_twindow_clear_eol_request_t;
#define SM_TWINDOW_CLEAR_EOL_REQUEST_TWID(m)				\
  (((sm_twindow_clear_eol_request_t *) (m)) -> twid)
#define SM_TWINDOW_CLEAR_EOL_REQUEST_X(m)				\
  (((sm_twindow_clear_eol_request_t *) (m)) -> x)
#define SM_TWINDOW_CLEAR_EOL_REQUEST_Y(m)				\
  (((sm_twindow_clear_eol_request_t *) (m)) -> y)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
  unsigned short x_start;
  unsigned short x_end;
  unsigned short y_start;
  unsigned short y_end;
  short x_delta;
  short y_delta;
} sm_twindow_scroll_request_t;
#define SM_TWINDOW_SCROLL_REQUEST_TWID(m)				\
  (((sm_twindow_scroll_request_t *) (m)) -> twid)
#define SM_TWINDOW_SCROLL_REQUEST_X_START(m)				\
  (((sm_twindow_scroll_request_t *) (m)) -> x_start)
#define SM_TWINDOW_SCROLL_REQUEST_X_END(m)				\
  (((sm_twindow_scroll_request_t *) (m)) -> x_end)
#define SM_TWINDOW_SCROLL_REQUEST_Y_START(m)				\
  (((sm_twindow_scroll_request_t *) (m)) -> y_start)
#define SM_TWINDOW_SCROLL_REQUEST_Y_END(m)				\
  (((sm_twindow_scroll_request_t *) (m)) -> y_end)
#define SM_TWINDOW_SCROLL_REQUEST_X_DELTA(m)				\
  (((sm_twindow_scroll_request_t *) (m)) -> x_delta)
#define SM_TWINDOW_SCROLL_REQUEST_Y_DELTA(m)				\
  (((sm_twindow_scroll_request_t *) (m)) -> y_delta)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
  unsigned short code;
  unsigned short flags;
  unsigned short repeat;
} sm_key_event_t;
#define SM_KEY_EVENT_TWID(m) (((sm_key_event_t *) (m)) -> twid)
#define SM_KEY_EVENT_CODE(m) (((sm_key_event_t *) (m)) -> code)
#define SM_KEY_EVENT_FLAGS(m) (((sm_key_event_t *) (m)) -> flags)
#define SM_KEY_EVENT_REPEAT(m) (((sm_key_event_t *) (m)) -> repeat)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
  
} sm_button_event_t;
#define SM_BUTTON_EVENT_TWID(m) (((sm_button_event_t *) (m)) -> twid)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
} sm_close_event_t;
#define SM_CLOSE_EVENT_TWID(m) (((sm_close_event_t *) (m)) -> twid)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
  unsigned char state;
} sm_visibility_event_t;
#define SM_VISIBILITY_EVENT_TWID(m) (((sm_visibility_event_t *) (m)) -> twid)
#define SM_VISIBILITY_EVENT_STATE(m) (((sm_visibility_event_t *) (m)) -> state)

#define VISIBILITY_OBSCURED 0
#define VISIBILITY_PARTIALLY_OBSCURED 1
#define VISIBILITY_UNOBSCURED 2

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  twid_t twid;
  unsigned short width;
  unsigned short height;
} sm_resize_event_t;
#define SM_RESIZE_EVENT_TWID(m) (((sm_resize_event_t *) (m)) -> twid)
#define SM_RESIZE_EVENT_WIDTH(m) (((sm_resize_event_t *) (m)) -> width)
#define SM_RESIZE_EVENT_HEIGHT(m) (((sm_resize_event_t *) (m)) -> height)

extern int OS2_read_pm_tqueue (tqueue_t *, int);
extern void OS2_write_pm_tqueue (tqueue_t *, msg_t *);

extern twid_t OS2_twindow_open (qid_t, const char *);
extern void OS2_twindow_close (twid_t);
extern void OS2_twindow_write
  (twid_t, unsigned short, unsigned short, const char *, unsigned short);
extern void OS2_twindow_move_cursor (twid_t, unsigned short, unsigned short);
extern void OS2_twindow_clear (twid_t);
extern void OS2_twindow_clear_eol (twid_t, unsigned short, unsigned short);
extern void OS2_twindow_scroll
  (twid_t, unsigned short, unsigned short, unsigned short, unsigned short,
   short, short);

#endif /* SCM_OS2PM_H */
