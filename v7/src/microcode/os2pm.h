/* -*-C-*-

$Id: os2pm.h,v 1.12 1995/11/03 01:29:32 cph Exp $

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

typedef unsigned short psid_t;
#define PSID_NONE 0

typedef unsigned short wid_t;
#define WID_NONE 0

typedef unsigned short bid_t;
#define BID_NONE 0

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
  ULONG msg;
  MPARAM mp1;
  MPARAM mp2;
} sm_pm_event_t;
#define SM_PM_EVENT_WID(m) (((sm_pm_event_t *) (m)) -> wid)
#define SM_PM_EVENT_MSG(m) (((sm_pm_event_t *) (m)) -> msg)
#define SM_PM_EVENT_MP1(m) (((sm_pm_event_t *) (m)) -> mp1)
#define SM_PM_EVENT_MP2(m) (((sm_pm_event_t *) (m)) -> mp2)

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

/* This machine-generated file contains most of the procedure prototypes.  */
#include "os2pm-ed.h"

extern int OS2_psid_validp (psid_t);
extern int OS2_wid_validp (wid_t);
extern int OS2_bid_validp (bid_t);
extern psid_t OS2_window_client_ps (wid_t);
extern qid_t OS2_create_pm_qid (tqueue_t *);
extern void OS2_window_permanent (wid_t);
extern void OS2_window_mousetrack (wid_t, int);
extern HWND OS2_window_frame_handle (wid_t);
extern HWND OS2_window_client_handle (wid_t);
extern int OS2_memory_ps_p (psid_t);
extern bid_t OS2_ps_get_bitmap (psid_t);

extern void OS2_window_pos (wid_t, short *, short *);
extern void OS2_window_size (wid_t, unsigned short *, unsigned short *);
extern void OS2_window_frame_size (wid_t, unsigned short *, unsigned short *);
extern bid_t OS2_ps_set_bitmap (psid_t, bid_t);
extern font_metrics_t * OS2_ps_set_font (psid_t, unsigned short, const char *);

extern int OS2_translate_wm_char
  (MPARAM, MPARAM, unsigned short *, unsigned short *, unsigned char *);

#endif /* SCM_OS2PM_H */
