/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

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
