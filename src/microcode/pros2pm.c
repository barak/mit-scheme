/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

#include "scheme.h"
#include "prims.h"
#define INCL_WIN
#define INCL_GPI
#include "os2.h"

static PPOINTL coordinate_vector_point_args
  (unsigned int, unsigned int, unsigned long *);

static qid_t pm_qid;

static qid_t
qid_argument (unsigned int arg_number)
{
  unsigned int qid = (arg_index_integer (arg_number, (QID_MAX + 1)));
  if (! ((OS2_qid_openp (qid)) && ((OS2_qid_twin (qid)) != QID_NONE)))
    error_bad_range_arg (arg_number);
  return (qid);
}

static psid_t
psid_argument (unsigned int arg_number)
{
  unsigned long result = (arg_ulong_integer (arg_number));
  if (!OS2_psid_validp (result))
    error_bad_range_arg (arg_number);
  return (result);
}

static psid_t
memory_psid_argument (unsigned int arg_number)
{
  psid_t psid = (psid_argument (arg_number));
  if (!OS2_memory_ps_p (psid))
    error_bad_range_arg (arg_number);
  return (psid);
}

static wid_t
wid_argument (unsigned int arg_number)
{
  unsigned long result = (arg_ulong_integer (arg_number));
  if (!OS2_wid_validp (result))
    error_bad_range_arg (arg_number);
  return (result);
}

static bid_t
bid_argument (unsigned int arg_number)
{
  unsigned long result = (arg_ulong_integer (arg_number));
  if (!OS2_bid_validp (result))
    error_bad_range_arg (arg_number);
  return (result);
}

static short
short_arg (unsigned int arg_number)
{
  long result = (arg_integer (arg_number));
  if (! ((-32768 <= result) && (result < 32768)))
    error_bad_range_arg (arg_number);
  return (result);
}

#define SSHORT_ARG short_arg
#define USHORT_ARG(n) arg_index_integer ((n), 0x10000)

static unsigned short
dimension_arg (unsigned int arg_number)
{
  unsigned short result = (USHORT_ARG (arg_number));
  if (result == 0)
    error_bad_range_arg (arg_number);
  return (result);
}

#define COORDINATE_ARG SSHORT_ARG
#define DIMENSION_ARG dimension_arg
#define HWND_ARG(n) ((HWND) (arg_ulong_integer (n)))

void
OS2_initialize_window_primitives (void)
{
  pm_qid = (OS2_create_pm_qid (OS2_scheme_tqueue));
}

DEFINE_PRIMITIVE ("OS2WIN-ALARM", Prim_OS2_window_alarm, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (WinAlarm (HWND_DESKTOP, (arg_ulong_integer (1)))));
}

DEFINE_PRIMITIVE ("OS2WIN-BEEP", Prim_OS2_window_beep, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  DosBeep ((arg_ulong_integer (1)), (arg_ulong_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PM-SYNCHRONIZE", Prim_OS2_pm_synchronize, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  OS2_pm_synchronize (pm_qid);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-OPEN", Prim_OS2_window_open, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_window_open (pm_qid,
					(OS2_qid_twin (qid_argument (1))),
					(FCF_TITLEBAR | FCF_SYSMENU
					 | FCF_SHELLPOSITION | FCF_SIZEBORDER
					 | FCF_MINMAX | FCF_TASKLIST
					 | FCF_NOBYTEALIGN),
					NULLHANDLE,
					1,
					0,
					(STRING_ARG (2)))));
}

DEFINE_PRIMITIVE ("OS2WIN-CLOSE", Prim_OS2_window_close, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS2_window_close (wid_argument (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-SHOW", Prim_OS2_window_show, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS2_window_show ((wid_argument (1)), (BOOLEAN_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-MOVE-CURSOR", Prim_OS2_window_move_cursor, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS2_window_move_cursor ((wid_argument (1)),
			  (COORDINATE_ARG (2)),
			  (COORDINATE_ARG (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-SHAPE-CURSOR", Prim_OS2_window_shape_cursor, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  OS2_window_shape_cursor ((wid_argument (1)),
			   (DIMENSION_ARG (2)),
			   (DIMENSION_ARG (3)),
			   (USHORT_ARG (4)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-SHOW-CURSOR", Prim_OS2_window_show_cursor, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS2_window_show_cursor ((wid_argument (1)), (BOOLEAN_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-SCROLL", Prim_OS2_window_scroll, 7, 7, 0)
{
  PRIMITIVE_HEADER (7);
  OS2_window_scroll ((wid_argument (1)),
		     (COORDINATE_ARG (2)),
		     (COORDINATE_ARG (3)),
		     (COORDINATE_ARG (4)),
		     (COORDINATE_ARG (5)),
		     (SSHORT_ARG (6)),
		     (SSHORT_ARG (7)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-INVALIDATE", Prim_OS2_window_invalidate, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  OS2_window_invalidate ((wid_argument (1)),
			 (COORDINATE_ARG (2)),
			 (COORDINATE_ARG (3)),
			 (COORDINATE_ARG (4)),
			 (COORDINATE_ARG (5)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-SET-GRID", Prim_OS2_window_set_grid, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS2_window_set_grid ((wid_argument (1)),
		       (DIMENSION_ARG (2)),
		       (DIMENSION_ARG (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-ACTIVATE", Prim_OS2_window_activate, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS2_window_activate (wid_argument (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-GET-POS", Prim_OS2_window_get_pos, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT p = (cons (SHARP_F, SHARP_F));
    short x;
    short y;
    OS2_window_pos ((wid_argument (1)), (& x), (& y));
    SET_PAIR_CAR (p, (LONG_TO_FIXNUM (x)));
    SET_PAIR_CDR (p, (LONG_TO_FIXNUM (y)));
    PRIMITIVE_RETURN (p);
  }
}

DEFINE_PRIMITIVE ("OS2WIN-SET-POS", Prim_OS2_window_set_pos, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS2_window_set_pos ((wid_argument (1)), (SSHORT_ARG (2)), (SSHORT_ARG (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-GET-SIZE", Prim_OS2_window_get_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT p = (cons (SHARP_F, SHARP_F));
    unsigned short width;
    unsigned short height;
    OS2_window_size ((wid_argument (1)), (& width), (& height));
    SET_PAIR_CAR (p, (LONG_TO_UNSIGNED_FIXNUM (width)));
    SET_PAIR_CDR (p, (LONG_TO_UNSIGNED_FIXNUM (height)));
    PRIMITIVE_RETURN (p);
  }
}

DEFINE_PRIMITIVE ("OS2WIN-GET-FRAME-SIZE", Prim_OS2_window_get_frame_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT p = (cons (SHARP_F, SHARP_F));
    unsigned short width;
    unsigned short height;
    OS2_window_frame_size ((wid_argument (1)), (& width), (& height));
    SET_PAIR_CAR (p, (LONG_TO_UNSIGNED_FIXNUM (width)));
    SET_PAIR_CDR (p, (LONG_TO_UNSIGNED_FIXNUM (height)));
    PRIMITIVE_RETURN (p);
  }
}

DEFINE_PRIMITIVE ("OS2WIN-SET-SIZE", Prim_OS2_window_set_size, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS2_window_set_size ((wid_argument (1)), (USHORT_ARG (2)), (USHORT_ARG (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-FOCUS?", Prim_OS2_window_focusp, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (OS2_window_focusp (wid_argument (1))));
}

DEFINE_PRIMITIVE ("OS2WIN-SET-STATE", Prim_OS2_window_set_state, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS2_window_set_state
    ((wid_argument (1)),
     ((window_state_t) (arg_index_integer (2, ((long) state_supremum)))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-SET-TITLE", Prim_OS2_window_set_title, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS2_window_set_title ((wid_argument (1)), (STRING_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-TRACK-MOUSE", Prim_OS2_window_track_mouse, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS2_window_mousetrack ((wid_argument (1)), (BOOLEAN_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-FRAME-HANDLE", Prim_OS2_window_frame_handle, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_window_frame_handle (wid_argument (1))));
}

DEFINE_PRIMITIVE ("OS2WIN-CLIENT-HANDLE", Prim_OS2_window_client_handle, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_window_client_handle (wid_argument (1))));
}

DEFINE_PRIMITIVE ("OS2WIN-UPDATE-FRAME", Prim_OS2_window_update_frame, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS2_window_update_frame ((wid_argument (1)), (USHORT_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2-WINDOW-HANDLE-FROM-ID", Prim_OS2_window_handle_from_id, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_window_handle_from_id (pm_qid,
						  (arg_ulong_integer (1)),
						  (arg_ulong_integer (2)))));
}

DEFINE_PRIMITIVE ("OS2WIN-QUERY-SYS-VALUE", Prim_OS2_window_query_sys_value, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_window_query_sys_value (pm_qid,
						   (HWND_ARG (1)),
						   (arg_integer (2)))));
}

DEFINE_PRIMITIVE ("OS2-MAP-WINDOW-POINT", Prim_OS2_map_window_point, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT scheme_point;
    POINTL point;
    BOOL rc;

    CHECK_ARG (3, PAIR_P);
    scheme_point = (ARG_REF (3));
    if ((!INTEGER_P (PAIR_CAR (scheme_point)))
	|| (!INTEGER_P (PAIR_CDR (scheme_point))))
      error_wrong_type_arg (3);
    if ((!integer_to_long_p (PAIR_CAR (scheme_point)))
	|| (!integer_to_long_p (PAIR_CDR (scheme_point))))
      error_bad_range_arg (3);
    (point . x) = (integer_to_long (PAIR_CAR (scheme_point)));
    (point . y) = (integer_to_long (PAIR_CDR (scheme_point)));
    rc = (WinMapWindowPoints ((HWND_ARG (1)), (HWND_ARG (2)), (&point), 1));
    if (rc)
      {
	SET_PAIR_CAR (scheme_point, (long_to_integer (point . x)));
	SET_PAIR_CDR (scheme_point, (long_to_integer (point . y)));
      }
    PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (rc));
  }
}

DEFINE_PRIMITIVE ("OS2WIN-SET-CAPTURE", PRIM_OS2_WINDOW_SET_CAPTURE, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT
     (OS2_window_set_capture ((wid_argument (1)), (BOOLEAN_ARG (2)))));
}

DEFINE_PRIMITIVE ("OS2WIN-PS", Prim_OS2_window_ps, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_window_client_ps (wid_argument (1))));
}

DEFINE_PRIMITIVE ("OS2PS-CREATE-MEMORY-PS", Prim_OS2_create_memory_ps, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (ulong_to_integer (OS2_create_memory_ps (pm_qid)));
}

DEFINE_PRIMITIVE ("OS2PS-DESTROY-MEMORY-PS", Prim_OS2_destroy_memory_ps, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS2_destroy_memory_ps (memory_psid_argument (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-CREATE-BITMAP", Prim_OS2_create_bitmap, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_create_bitmap ((psid_argument (1)),
					  (USHORT_ARG (2)),
					  (USHORT_ARG (3)))));
}

DEFINE_PRIMITIVE ("OS2PS-DESTROY-BITMAP", Prim_OS2_destroy_bitmap, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS2_destroy_bitmap (bid_argument (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-GET-BITMAP", Prim_OS2_ps_get_bitmap, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    bid_t bid = (OS2_ps_get_bitmap ((memory_psid_argument (1))));
    PRIMITIVE_RETURN ((bid == BID_NONE) ? SHARP_F : (ulong_to_integer (bid)));
  }
}

DEFINE_PRIMITIVE ("OS2PS-SET-BITMAP", Prim_OS2_ps_set_bitmap, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    bid_t bid
      = (OS2_ps_set_bitmap
	 ((memory_psid_argument (1)),
	  (((ARG_REF (2)) == SHARP_F) ? BID_NONE : (bid_argument (2)))));
    PRIMITIVE_RETURN ((bid == BID_NONE) ? SHARP_F : (ulong_to_integer (bid)));
  }
}

DEFINE_PRIMITIVE ("OS2PS-BITBLT", Prim_OS2_ps_bitblt, 6, 6, 0)
{
  PRIMITIVE_HEADER (6);
  {
    void * position = dstack_position;
    psid_t target = (psid_argument (1));
    psid_t source = (psid_argument (2));
    unsigned long npoints;
    PPOINTL points = (coordinate_vector_point_args (3, 4, (& npoints)));
    LONG rop = (arg_index_integer (5, 0x100));
    ULONG options = (arg_ulong_integer (6));
    if (! ((npoints == 3) || (npoints == 4)))
      error_bad_range_arg (3);
    OS2_ps_bitblt (target, source, npoints, points, rop, options);
    dstack_set_position (position);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-WRITE", Prim_OS2_ps_write, 6, 6, 0)
{
  PRIMITIVE_HEADER (6);
  CHECK_ARG (4, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (4));
    unsigned long start = (arg_ulong_integer (5));
    unsigned long end = (arg_ulong_integer (6));
    if (end > (STRING_LENGTH (string)))
      error_bad_range_arg (6);
    if (start > end)
      error_bad_range_arg (5);
    OS2_ps_draw_text ((psid_argument (1)),
		      (COORDINATE_ARG (2)),
		      (COORDINATE_ARG (3)),
		      (STRING_LOC (string, start)),
		      (end - start));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-TEXT-WIDTH", Prim_OS2_ps_text_width, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (2));
    unsigned long start = (arg_ulong_integer (3));
    unsigned long end = (arg_ulong_integer (4));
    if (end > (STRING_LENGTH (string)))
      error_bad_range_arg (4);
    if (start > end)
      error_bad_range_arg (3);
    PRIMITIVE_RETURN
      (ulong_to_integer
       (OS2_ps_text_width ((psid_argument (1)),
			   (STRING_LOC (string, start)),
			   (end - start))));
  }
}

static SCHEME_OBJECT
convert_font_metrics (font_metrics_t * m)
{
  if (m == 0)
    return (SHARP_F);
  else
    {
      SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (v, 0, (ulong_to_integer (FONT_METRICS_WIDTH (m))));
      VECTOR_SET (v, 1, (ulong_to_integer (FONT_METRICS_HEIGHT (m))));
      VECTOR_SET (v, 2, (ulong_to_integer (FONT_METRICS_DESCENDER (m))));
      OS_free (m);
      return (v);
    }
}

DEFINE_PRIMITIVE ("OS2PS-GET-FONT-METRICS", Prim_OS2_ps_get_font_metrics, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (convert_font_metrics (OS2_ps_get_font_metrics (psid_argument (1))));
}

DEFINE_PRIMITIVE ("OS2PS-SET-FONT", Prim_OS2_ps_set_font, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (convert_font_metrics (OS2_ps_set_font ((psid_argument (1)),
					    (USHORT_ARG (2)),
					    (STRING_ARG (3)))));
}

DEFINE_PRIMITIVE ("OS2PS-CLEAR", Prim_OS2_ps_clear, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  OS2_ps_clear ((psid_argument (1)),
		(COORDINATE_ARG (2)),
		(COORDINATE_ARG (3)),
		(COORDINATE_ARG (4)),
		(COORDINATE_ARG (5)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-SET-COLORS", Prim_OS2_ps_set_colors, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS2_ps_set_colors ((psid_argument (1)),
		     (arg_index_integer (2, 0x1000000)),
		     (arg_index_integer (3, 0x1000000)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-MOVE-GRAPHICS-CURSOR", Prim_OS2_ps_move_gcursor, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS2_ps_move_gcursor ((psid_argument (1)),
		       (COORDINATE_ARG (2)),
		       (COORDINATE_ARG (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-LINE", Prim_OS2_ps_line, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS2_ps_draw_line ((psid_argument (1)),
		    (COORDINATE_ARG (2)),
		    (COORDINATE_ARG (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-DRAW-POINT", Prim_OS2_ps_draw_point, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  OS2_ps_draw_point ((psid_argument (1)),
		     (COORDINATE_ARG (2)),
		     (COORDINATE_ARG (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-POLY-LINE", Prim_OS2_ps_poly_line, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    void * position = dstack_position;
    unsigned long npoints;
    PPOINTL points = (coordinate_vector_point_args (2, 3, (& npoints)));
    OS2_ps_poly_line ((psid_argument (1)),
		      npoints,
		      points);
    dstack_set_position (position);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-POLY-LINE-DISJOINT", Prim_OS2_ps_poly_line_disjoint, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    void * position = dstack_position;
    unsigned long npoints;
    PPOINTL points = (coordinate_vector_point_args (2, 3, (& npoints)));
    OS2_ps_poly_line_disjoint ((psid_argument (1)),
			       npoints,
			       points);
    dstack_set_position (position);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static PPOINTL
coordinate_vector_point_args (unsigned int x_no, unsigned int y_no,
			      unsigned long * npoints)
{
  SCHEME_OBJECT x_vector = (ARG_REF (x_no));
  SCHEME_OBJECT y_vector = (ARG_REF (y_no));
  if (!VECTOR_P (x_vector))
    error_wrong_type_arg (x_no);
  if (!VECTOR_P (y_vector))
    error_wrong_type_arg (y_no);
  {
    unsigned long length = (VECTOR_LENGTH (x_vector));
    if (length != (VECTOR_LENGTH (y_vector)))
      error_bad_range_arg (x_no);
    {
      SCHEME_OBJECT * scan_x = (VECTOR_LOC (x_vector, 0));
      SCHEME_OBJECT * end_x = (VECTOR_LOC (x_vector, length));
      SCHEME_OBJECT * scan_y = (VECTOR_LOC (y_vector, 0));
      PPOINTL points = (dstack_alloc (length * (sizeof (POINTL))));
      PPOINTL scan_points = points;
      while (scan_x < end_x)
	{
	  SCHEME_OBJECT x = (*scan_x++);
	  SCHEME_OBJECT y = (*scan_y++);
	  if (!FIXNUM_P (x))
	    error_bad_range_arg (x_no);
	  if (!FIXNUM_P (y))
	    error_bad_range_arg (y_no);
	  (scan_points -> x) = (FIXNUM_TO_LONG (x));
	  (scan_points -> y) = (FIXNUM_TO_LONG (y));
	  scan_points += 1;
	}
      (* npoints) = length;
      return (points);
    }
  }
}

DEFINE_PRIMITIVE ("OS2PS-SET-LINE-TYPE", Prim_OS2_ps_set_line_type, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS2_ps_set_line_type ((psid_argument (1)), (arg_index_integer (2, 10)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-SET-MIX", Prim_OS2_ps_set_mix, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS2_ps_set_mix ((psid_argument (1)), (arg_index_integer (2, 18)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-QUERY-CAPABILITIES", Prim_OS2_ps_query_caps, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    LONG count = (arg_nonnegative_integer (3));
    PLONG values = (OS_malloc (count * (sizeof (LONG))));
    OS2_ps_query_caps ((psid_argument (1)),
		       (arg_nonnegative_integer (2)),
		       count,
		       values);
    {
      SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, count, 1));
      LONG index = 0;
      while (index < count)
	{
	  VECTOR_SET (v, index, (long_to_integer (values [index])));
	  index += 1;
	}
      OS_free (values);
      PRIMITIVE_RETURN (v);
    }
  }
}

DEFINE_PRIMITIVE ("OS2PS-QUERY-CAPABILITY", Prim_OS2_ps_query_cap, 2, 2, 0)
{
  LONG values [1];
  PRIMITIVE_HEADER (2);
  OS2_ps_query_caps ((psid_argument (1)),
		     (arg_nonnegative_integer (2)),
		     1,
		     values);
  PRIMITIVE_RETURN (long_to_integer (values [0]));
}

DEFINE_PRIMITIVE ("OS2PS-RESET-CLIP-RECTANGLE", Prim_OS2_ps_reset_clip_rectangle, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS2_ps_reset_clip_rectangle (psid_argument (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-SET-CLIP-RECTANGLE", Prim_OS2_ps_set_clip_rectangle, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  OS2_ps_set_clip_rectangle ((psid_argument (1)),
			     (COORDINATE_ARG (2)),
			     (COORDINATE_ARG (3)),
			     (COORDINATE_ARG (4)),
			     (COORDINATE_ARG (5)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-GET-BITMAP-PARAMETERS", Prim_OS2_ps_get_bitmap_parameters, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT s = (allocate_string (sizeof (BITMAPINFOHEADER)));
    PBITMAPINFOHEADER params = ((PBITMAPINFOHEADER) (STRING_POINTER (s)));
    (params -> cbFix) = (sizeof (BITMAPINFOHEADER));
    OS2_get_bitmap_parameters ((bid_argument (1)), params);
    PRIMITIVE_RETURN (s);
  }
}

DEFINE_PRIMITIVE ("OS2PS-GET-BITMAP-BITS", Prim_OS2_ps_get_bitmap_bits, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  PRIMITIVE_RETURN
    (ulong_to_integer
     (OS2_ps_get_bitmap_bits ((memory_psid_argument (1)),
			      (arg_ulong_integer (2)),
			      (arg_ulong_integer (3)),
			      (STRING_ARG (4)),
			      ((void *) (STRING_ARG (5))))));
}

DEFINE_PRIMITIVE ("OS2PS-SET-BITMAP-BITS", Prim_OS2_ps_set_bitmap_bits, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  PRIMITIVE_RETURN
    (ulong_to_integer
     (OS2_ps_set_bitmap_bits ((memory_psid_argument (1)),
			      (arg_ulong_integer (2)),
			      (arg_ulong_integer (3)),
			      (STRING_ARG (4)),
			      ((void *) (STRING_ARG (5))))));
}

DEFINE_PRIMITIVE ("OS2-CLIPBOARD-WRITE-TEXT", Prim_OS2_clipboard_write_text, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS2_clipboard_write_text (pm_qid, (STRING_ARG (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2-CLIPBOARD-READ-TEXT", Prim_OS2_clipboard_read_text, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  {
    const char * text = (OS2_clipboard_read_text (pm_qid));
    SCHEME_OBJECT result;
    if (text == 0)
      result = SHARP_F;
    else
      {
	result = (char_pointer_to_string (text));
	OS_free ((void *) text);
      }
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("OS2MENU-CREATE", Prim_OS2_menu_create, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_menu_create (pm_qid,
					(HWND_ARG (1)),
					(USHORT_ARG (2)),
					(USHORT_ARG (3)))));
}

DEFINE_PRIMITIVE ("OS2MENU-DESTROY", Prim_OS2_menu_destroy, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS2_menu_destroy (pm_qid, (HWND_ARG (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2MENU-INSERT-ITEM", Prim_OS2_menu_insert_item, 7, 7, 0)
{
  PRIMITIVE_HEADER (7);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_menu_insert_item (pm_qid,
					     (HWND_ARG (1)),
					     (USHORT_ARG (2)),
					     (USHORT_ARG (3)),
					     (USHORT_ARG (4)),
					     (USHORT_ARG (5)),
					     (HWND_ARG (6)),
					     (STRING_ARG (7)))));
}

DEFINE_PRIMITIVE ("OS2MENU-REMOVE-ITEM", Prim_OS2_menu_remove_item, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_menu_remove_item (pm_qid,
					     (HWND_ARG (1)),
					     (USHORT_ARG (2)),
					     (BOOLEAN_ARG (3)),
					     (BOOLEAN_ARG (4)))));
}

DEFINE_PRIMITIVE ("OS2MENU-GET-ITEM", Prim_OS2_menu_get_item, 3, 3, 0)
{
  PMENUITEM item;
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (3);

  item = (OS2_menu_get_item (pm_qid,
			     (HWND_ARG (1)),
			     (USHORT_ARG (2)),
			     (BOOLEAN_ARG (3))));
  if (item == 0)
    PRIMITIVE_RETURN (SHARP_F);
  result = (allocate_marked_vector (TC_VECTOR, 6, 1));
  VECTOR_SET (result, 0, (long_to_integer (item -> iPosition)));
  VECTOR_SET (result, 1, (ulong_to_integer (item -> afStyle)));
  VECTOR_SET (result, 2, (ulong_to_integer (item -> afAttribute)));
  VECTOR_SET (result, 3, (ulong_to_integer (item -> id)));
  VECTOR_SET (result, 4, (ulong_to_integer (item -> hwndSubMenu)));
  VECTOR_SET (result, 5, (ulong_to_integer (item -> hItem)));
  OS_free (item);
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("OS2MENU-N-ITEMS", Prim_OS2_menu_n_items, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_menu_n_items (pm_qid, (HWND_ARG (1)))));
}

DEFINE_PRIMITIVE ("OS2MENU-NTH-ITEM-ID", Prim_OS2_menu_nth_item_id, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_menu_nth_item_id (pm_qid,
					     (HWND_ARG (1)),
					     (USHORT_ARG (2)))));
}

DEFINE_PRIMITIVE ("OS2MENU-GET-ITEM-ATTRIBUTES", Prim_OS2_menu_get_item_attributes, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_menu_get_item_attributes (pm_qid,
						     (HWND_ARG (1)),
						     (USHORT_ARG (2)),
						     (BOOLEAN_ARG (3)),
						     (USHORT_ARG (4)))));
}

DEFINE_PRIMITIVE ("OS2MENU-SET-ITEM-ATTRIBUTES", Prim_OS2_menu_set_item_attributes, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (OS2_menu_set_item_attributes (pm_qid,
						      (HWND_ARG (1)),
						      (USHORT_ARG (2)),
						      (BOOLEAN_ARG (3)),
						      (USHORT_ARG (4)),
						      (USHORT_ARG (5)))));
}

DEFINE_PRIMITIVE ("OS2WIN-LOAD-MENU", Prim_OS2_window_load_menu, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_window_load_menu ((wid_argument (1)),
					     (arg_ulong_integer (2)),
					     (arg_ulong_integer (3)))));
}

DEFINE_PRIMITIVE ("OS2WIN-POPUP-MENU", Prim_OS2_window_popup_menu, 7, 7, 0)
{
  PRIMITIVE_HEADER (7);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT
     (OS2_window_popup_menu (pm_qid,
			     (HWND_ARG (1)),
			     (HWND_ARG (2)),
			     (HWND_ARG (3)),
			     (arg_integer (4)),
			     (arg_integer (5)),
			     (arg_integer (6)),
			     (arg_ulong_integer (7)))));
}

DEFINE_PRIMITIVE ("OS2WIN-FONT-DIALOG", Prim_OS2_window_font_dialog, 2, 2, 0)
{
  const char * spec;
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (2);

  spec = (OS2_window_font_dialog ((wid_argument (1)),
				  (((ARG_REF (2)) == SHARP_F)
				   ? 0
				   : (STRING_ARG (2)))));
  if (spec == 0)
    PRIMITIVE_RETURN (SHARP_F);
  result = (char_pointer_to_string (spec));
  OS_free ((void *) spec);
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("OS2-QUERY-SYSTEM-POINTER", Prim_OS2_query_system_pointer, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_query_system_pointer (pm_qid,
						 (HWND_ARG (1)),
						 (arg_integer (2)),
						 (BOOLEAN_ARG (3)))));
}

DEFINE_PRIMITIVE ("OS2-SET-POINTER", Prim_OS2_set_pointer, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (OS2_set_pointer (pm_qid,
					 (HWND_ARG (1)),
					 (arg_ulong_integer (2)))));
}

DEFINE_PRIMITIVE ("OS2WIN-LOAD-POINTER", Prim_OS2_window_load_pointer, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (ulong_to_integer (OS2_window_load_pointer (pm_qid,
						(HWND_ARG (1)),
						(arg_ulong_integer (2)),
						(arg_ulong_integer (3)))));
}

DEFINE_PRIMITIVE ("OS2WIN-DESTROY-POINTER", Prim_OS2_window_destroy_pointer, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (OS2_window_destroy_pointer (pm_qid,
						    (arg_ulong_integer (1)))));
}

DEFINE_PRIMITIVE ("OS2WIN-SET-ICON", Prim_OS2_window_set_icon, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT
     (OS2_window_set_icon ((wid_argument (1)), (arg_ulong_integer (2)))));
}

DEFINE_PRIMITIVE ("OS2WIN-OPEN-EVENT-QID", Prim_OS2_window_open_event_qid, 0, 0, 0)
{
  qid_t local;
  qid_t remote;
  PRIMITIVE_HEADER (0);
  OS2_make_qid_pair ((&local), (&remote));
  OS2_open_qid (local, OS2_scheme_tqueue);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (local));
}

DEFINE_PRIMITIVE ("OS2WIN-CLOSE-EVENT-QID", Prim_OS2_window_close_event_qid, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS2_close_qid_pair (qid_argument (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define ET_BUTTON	0
#define ET_CLOSE	1
#define ET_FOCUS	2
#define ET_KEY		3
#define ET_PAINT	4
#define ET_RESIZE	5
#define ET_VISIBILITY	6
#define ET_COMMAND	7
#define ET_HELP		8
#define ET_MOUSEMOVE	9

#define CVT_USHORT(n, v)						\
  VECTOR_SET (result, n, (LONG_TO_UNSIGNED_FIXNUM (v)))
#define CVT_SHORT(n, v)							\
  VECTOR_SET (result, n, (LONG_TO_FIXNUM (v)))
#define CVT_BOOLEAN(n, v)						\
  VECTOR_SET (result, n, (BOOLEAN_TO_OBJECT (v)))

static SCHEME_OBJECT make_button_event
  (wid_t, MPARAM, MPARAM, unsigned short, unsigned short);

DEFINE_PRIMITIVE ("OS2WIN-GET-EVENT", Prim_OS2_window_get_event, 2, 2, 0)
{
  qid_t qid;
  int blockp;
  PRIMITIVE_HEADER (2);

  qid = (qid_argument (1));
  blockp = (BOOLEAN_ARG (2));
  Primitive_GC_If_Needed (8);
  while (1)
    {
      msg_t * message = (OS2_receive_message (qid, blockp, 1));
      SCHEME_OBJECT result = SHARP_F;
      if (message == 0)
	PRIMITIVE_RETURN (result);
      switch (MSG_TYPE (message))
	{
	case mt_pm_event:
	  {
	    wid_t wid = (SM_PM_EVENT_WID (message));
	    ULONG msg = (SM_PM_EVENT_MSG (message));
	    MPARAM mp1 = (SM_PM_EVENT_MP1 (message));
	    MPARAM mp2 = (SM_PM_EVENT_MP2 (message));
	    OS2_destroy_message (message);
	    switch (msg)
	      {
	      case WM_SETFOCUS:
		{
		  result = (allocate_marked_vector (TC_VECTOR, 3, 0));
		  CVT_USHORT (0, ET_FOCUS);
		  CVT_USHORT (1, wid);
		  CVT_BOOLEAN (2, (SHORT1FROMMP (mp2)));
		  break;
		}
	      case WM_SIZE:
		{
		  result = (allocate_marked_vector (TC_VECTOR, 4, 0));
		  CVT_USHORT (0, ET_RESIZE);
		  CVT_USHORT (1, wid);
		  CVT_USHORT (2, (SHORT1FROMMP (mp2)));
		  CVT_USHORT (3, (SHORT2FROMMP (mp2)));
		  break;
		}
	      case WM_CLOSE:
		{
		  result = (allocate_marked_vector (TC_VECTOR, 2, 0));
		  CVT_USHORT (0, ET_CLOSE);
		  CVT_USHORT (1, wid);
		  break;
		}
	      case WM_COMMAND:
	      case WM_HELP:
		{
		  result = (allocate_marked_vector (TC_VECTOR, 5, 0));
		  CVT_USHORT (0,
				((msg == WM_HELP) ? ET_HELP : ET_COMMAND));
		  CVT_USHORT (1, wid);
		  CVT_USHORT (2, (SHORT1FROMMP (mp1)));
		  CVT_USHORT (3, (SHORT1FROMMP (mp2)));
		  CVT_BOOLEAN (4, (SHORT2FROMMP (mp2)));
		  break;
		}
	      case WM_SHOW:
		{
		  result = (allocate_marked_vector (TC_VECTOR, 3, 0));
		  CVT_USHORT (0, ET_VISIBILITY);
		  CVT_USHORT (1, wid);
		  CVT_BOOLEAN (2, (SHORT1FROMMP (mp1)));
		  break;
		}
	      case WM_CHAR:
		{
		  unsigned short code;
		  unsigned short flags;
		  unsigned char repeat;
		  if (OS2_translate_wm_char (mp1, mp2,
					     (&code), (&flags), (&repeat)))
		    {
		      result = (allocate_marked_vector (TC_VECTOR, 5, 0));
		      CVT_USHORT (0, ET_KEY);
		      CVT_USHORT (1, wid);
		      CVT_USHORT (2, code);
		      CVT_USHORT (3, flags);
		      CVT_USHORT (4, repeat);
		    }
		  break;
		}
	      case WM_BUTTON1DOWN:
		result = (make_button_event (wid, mp1, mp2, 0, 0));
		break;
	      case WM_BUTTON1UP:
		result = (make_button_event (wid, mp1, mp2, 0, 1));
		break;
	      case WM_BUTTON1CLICK:
		result = (make_button_event (wid, mp1, mp2, 0, 2));
		break;
	      case WM_BUTTON1DBLCLK:
		result = (make_button_event (wid, mp1, mp2, 0, 3));
		break;
	      case WM_BUTTON2DOWN:
		result = (make_button_event (wid, mp1, mp2, 1, 0));
		break;
	      case WM_BUTTON2UP:
		result = (make_button_event (wid, mp1, mp2, 1, 1));
		break;
	      case WM_BUTTON2CLICK:
		result = (make_button_event (wid, mp1, mp2, 1, 2));
		break;
	      case WM_BUTTON2DBLCLK:
		result = (make_button_event (wid, mp1, mp2, 1, 3));
		break;
	      case WM_BUTTON3DOWN:
		result = (make_button_event (wid, mp1, mp2, 2, 0));
		break;
	      case WM_BUTTON3UP:
		result = (make_button_event (wid, mp1, mp2, 2, 1));
		break;
	      case WM_BUTTON3CLICK:
		result = (make_button_event (wid, mp1, mp2, 2, 2));
		break;
	      case WM_BUTTON3DBLCLK:
		result = (make_button_event (wid, mp1, mp2, 2, 3));
		break;
	      case WM_MOUSEMOVE:
		result = (allocate_marked_vector (TC_VECTOR, 6, 0));
		CVT_USHORT (0, ET_MOUSEMOVE);
		CVT_USHORT (1, wid);
		CVT_SHORT (2, (SHORT1FROMMP (mp1)));
		CVT_SHORT (3, (SHORT2FROMMP (mp1)));
		CVT_USHORT (4, (SHORT1FROMMP (mp2)));
		CVT_USHORT (5, (SHORT2FROMMP (mp2)));
		break;
	      default:
		break;
	      }
	    break;
	  }
	case mt_paint_event:
	  {
	    result = (allocate_marked_vector (TC_VECTOR, 6, 0));
	    CVT_USHORT (0, ET_PAINT);
	    CVT_USHORT (1, (SM_PAINT_EVENT_WID (message)));
	    CVT_USHORT (2, (SM_PAINT_EVENT_XL (message)));
	    CVT_USHORT (3, (SM_PAINT_EVENT_XH (message)));
	    CVT_USHORT (4, (SM_PAINT_EVENT_YL (message)));
	    CVT_USHORT (5, (SM_PAINT_EVENT_YH (message)));
	    OS2_destroy_message (message);
	    break;
	  }
	default:
	  OS2_destroy_message (message);
	  OS2_error_anonymous ();
	  break;
	}
      if (result != SHARP_F)
	PRIMITIVE_RETURN (result);
    }
}

static SCHEME_OBJECT
make_button_event (wid_t wid, MPARAM mp1, MPARAM mp2,
		   unsigned short number, unsigned short type)
{
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 7, 0));
  CVT_USHORT (0, ET_BUTTON);
  CVT_USHORT (1, wid);
  CVT_USHORT (2, number);
  CVT_USHORT (3, type);
  CVT_SHORT (4, (SHORT1FROMMP (mp1)));
  CVT_SHORT (5, (SHORT2FROMMP (mp1)));
  CVT_USHORT (6, ((SHORT2FROMMP (mp2)) & (KC_SHIFT | KC_CTRL | KC_ALT)));
  return (result);
}

DEFINE_PRIMITIVE ("OS2WIN-EVENT-READY?", Prim_OS2_window_event_ready, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  switch (OS2_message_availablep ((qid_argument (1)), (BOOLEAN_ARG (2))))
    {
    case mat_available:
      PRIMITIVE_RETURN (SHARP_T);
    case mat_not_available:
      PRIMITIVE_RETURN (SHARP_F);
    case mat_interrupt:
      PRIMITIVE_RETURN (FIXNUM_ZERO);
    }
}

DEFINE_PRIMITIVE ("OS2WIN-CONSOLE-WID", Prim_OS2_window_console_wid, 0, 0, 0)
{
  extern wid_t OS2_console_wid (void);
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (ulong_to_integer (OS2_console_wid ()));
}

DEFINE_PRIMITIVE ("OS2WIN-DESKTOP-WIDTH", Prim_OS2_window_desktop_width, 0, 0, 0)
{
  SWP swp;
  PRIMITIVE_HEADER (0);
  WinQueryWindowPos (HWND_DESKTOP, (& swp));
  PRIMITIVE_RETURN (long_to_integer (swp . cx));
}

DEFINE_PRIMITIVE ("OS2WIN-DESKTOP-HEIGHT", Prim_OS2_window_desktop_height, 0, 0, 0)
{
  SWP swp;
  PRIMITIVE_HEADER (0);
  WinQueryWindowPos (HWND_DESKTOP, (& swp));
  PRIMITIVE_RETURN (long_to_integer (swp . cy));
}
