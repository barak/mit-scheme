/* -*-C-*-

$Id: pros2pm.c,v 1.4 1995/02/14 00:25:34 cph Exp $

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
  unsigned long result = (arg_nonnegative_integer (arg_number));
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
  unsigned long result = (arg_nonnegative_integer (arg_number));
  if (!OS2_wid_validp (result))
    error_bad_range_arg (arg_number);
  return (result);
}

static bid_t
bid_argument (unsigned int arg_number)
{
  unsigned long result = (arg_nonnegative_integer (arg_number));
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

void
OS2_initialize_window_primitives (void)
{
  pm_qid = (OS2_create_pm_qid (OS2_scheme_tqueue));
}

DEFINE_PRIMITIVE ("OS2WIN-BEEP", Prim_OS2_window_beep, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  DosBeep ((arg_nonnegative_integer (1)), (arg_nonnegative_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2WIN-OPEN", Prim_OS2_window_open, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (long_to_integer (OS2_window_open (pm_qid,
				       (OS2_qid_twin (qid_argument (1))),
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
			   (arg_nonnegative_integer (4)));
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

DEFINE_PRIMITIVE ("OS2WIN-PS", Prim_OS2_window_ps, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (OS2_window_client_ps (wid_argument (1))));
}

DEFINE_PRIMITIVE ("OS2PS-CREATE-MEMORY-PS", Prim_OS2_create_memory_ps, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (OS2_create_memory_ps (pm_qid)));
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
    (long_to_integer (OS2_create_bitmap ((psid_argument (1)),
					 (USHORT_ARG (2)),
					 (USHORT_ARG (3)))));
}

DEFINE_PRIMITIVE ("OS2PS-DESTROY-BITMAP", Prim_OS2_destroy_bitmap, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS2_destroy_bitmap (bid_argument (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-SET-BITMAP", Prim_OS2_ps_set_bitmap, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    bid_t bid
      = (OS2_ps_set_bitmap
	 ((memory_psid_argument (1)),
	  (((ARG_REF (2)) == SHARP_F) ? BID_NONE : (bid_argument (2)))));
    PRIMITIVE_RETURN ((bid == BID_NONE) ? SHARP_F : (long_to_integer (bid)));
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
    ULONG options = (arg_nonnegative_integer (6));
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
    unsigned long start = (arg_nonnegative_integer (5));
    unsigned long end = (arg_nonnegative_integer (6));
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
    unsigned long start = (arg_nonnegative_integer (3));
    unsigned long end = (arg_nonnegative_integer (4));
    if (end > (STRING_LENGTH (string)))
      error_bad_range_arg (4);
    if (start > end)
      error_bad_range_arg (3);
    PRIMITIVE_RETURN
      (long_to_integer
       (OS2_ps_text_width ((psid_argument (1)),
			   (STRING_LOC (string, start)),
			   (end - start))));
  }
}

DEFINE_PRIMITIVE ("OS2PS-SET-FONT", Prim_OS2_ps_set_font, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 3, 1));
    font_metrics_t * m
      = (OS2_ps_set_font ((psid_argument (1)),
			  (USHORT_ARG (2)),
			  (STRING_ARG (3))));
    if (m == 0)
      PRIMITIVE_RETURN (SHARP_F);
    VECTOR_SET (result, 0, (long_to_integer (FONT_METRICS_WIDTH (m))));
    VECTOR_SET (result, 1, (long_to_integer (FONT_METRICS_HEIGHT (m))));
    VECTOR_SET (result, 2, (long_to_integer (FONT_METRICS_DESCENDER (m))));
    OS_free (m);
    PRIMITIVE_RETURN (result);
  }
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
  OS2_ps_set_clip_rectangle ((psid_argument (1)), 0);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-SET-CLIP-RECTANGLE", Prim_OS2_ps_set_clip_rectangle, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  {
    RECTL rectl;
    (rectl . xLeft) = (COORDINATE_ARG (2));
    (rectl . xRight) = (COORDINATE_ARG (3));
    (rectl . yBottom) = (COORDINATE_ARG (4));
    (rectl . yTop) = (COORDINATE_ARG (5));
    OS2_ps_set_clip_rectangle ((psid_argument (1)), (& rectl));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2PS-GET-BITMAP-PARAMETERS", Prim_OS2_ps_get_bitmap_parameters, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT s = (allocate_string (sizeof (BITMAPINFOHEADER2)));
    PBITMAPINFOHEADER2 params = ((PBITMAPINFOHEADER2) (STRING_LOC (s, 0)));
    (params -> cbFix) = (sizeof (BITMAPINFOHEADER2));
    OS2_get_bitmap_parameters ((bid_argument (1)), params);
    PRIMITIVE_RETURN (s);
  }
}

DEFINE_PRIMITIVE ("OS2PS-GET-BITMAP-BITS", Prim_OS2_ps_get_bitmap_bits, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  PRIMITIVE_RETURN
    (long_to_integer
     (OS2_ps_get_bitmap_bits ((memory_psid_argument (1)),
			      (arg_nonnegative_integer (2)),
			      (arg_nonnegative_integer (3)),
			      (STRING_ARG (4)),
			      (STRING_ARG (5)))));
}

DEFINE_PRIMITIVE ("OS2PS-SET-BITMAP-BITS", Prim_OS2_ps_set_bitmap_bits, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  PRIMITIVE_RETURN
    (long_to_integer
     (OS2_ps_set_bitmap_bits ((memory_psid_argument (1)),
			      (arg_nonnegative_integer (2)),
			      (arg_nonnegative_integer (3)),
			      (STRING_ARG (4)),
			      (STRING_ARG (5)))));
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

#define CVT_UNSIGNED(n, v)						\
  VECTOR_SET (result, n, (LONG_TO_UNSIGNED_FIXNUM (v)))
#define CVT_BOOLEAN(n, v)						\
  VECTOR_SET (result, n, (BOOLEAN_TO_OBJECT (v)))

DEFINE_PRIMITIVE ("OS2WIN-GET-EVENT", Prim_OS2_window_get_event, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Primitive_GC_If_Needed (8);
  {
    msg_t * message
      = (OS2_receive_message ((qid_argument (1)), (BOOLEAN_ARG (2)), 1));
    SCHEME_OBJECT result = SHARP_F;
    if (message != 0)
      switch (MSG_TYPE (message))
	{
	case mt_button_event:
	  {
	    unsigned short type = (SM_BUTTON_EVENT_TYPE (message));
	    result = (allocate_marked_vector (TC_VECTOR, 7, 0));
	    CVT_UNSIGNED (0, ET_BUTTON);
	    CVT_UNSIGNED (1, (SM_BUTTON_EVENT_WID (message)));
	    CVT_UNSIGNED (2, (BUTTON_TYPE_NUMBER (type)));
	    CVT_UNSIGNED (3, (BUTTON_TYPE_EVENT (type)));
	    CVT_UNSIGNED (4, (SM_BUTTON_EVENT_X (message)));
	    CVT_UNSIGNED (5, (SM_BUTTON_EVENT_Y (message)));
	    CVT_UNSIGNED (6, (SM_BUTTON_EVENT_FLAGS (message)));
	    break;
	  }
	case mt_close_event:
	  {
	    result = (allocate_marked_vector (TC_VECTOR, 2, 0));
	    CVT_UNSIGNED (0, ET_CLOSE);
	    CVT_UNSIGNED (1, (SM_CLOSE_EVENT_WID (message)));
	    break;
	  }
	case mt_focus_event:
	  {
	    result = (allocate_marked_vector (TC_VECTOR, 3, 0));
	    CVT_UNSIGNED (0, ET_FOCUS);
	    CVT_UNSIGNED (1, (SM_FOCUS_EVENT_WID (message)));
	    CVT_BOOLEAN  (2, (SM_FOCUS_EVENT_GAINEDP (message)));
	    break;
	  }
	case mt_key_event:
	  {
	    result = (allocate_marked_vector (TC_VECTOR, 5, 0));
	    CVT_UNSIGNED (0, ET_KEY);
	    CVT_UNSIGNED (1, (SM_KEY_EVENT_WID (message)));
	    CVT_UNSIGNED (2, (SM_KEY_EVENT_CODE (message)));
	    CVT_UNSIGNED (3, (SM_KEY_EVENT_FLAGS (message)));
	    CVT_UNSIGNED (4, (SM_KEY_EVENT_REPEAT (message)));
	    break;
	  }
	case mt_paint_event:
	  {
	    result = (allocate_marked_vector (TC_VECTOR, 6, 0));
	    CVT_UNSIGNED (0, ET_PAINT);
	    CVT_UNSIGNED (1, (SM_PAINT_EVENT_WID (message)));
	    CVT_UNSIGNED (2, (SM_PAINT_EVENT_XL (message)));
	    CVT_UNSIGNED (3, (SM_PAINT_EVENT_XH (message)));
	    CVT_UNSIGNED (4, (SM_PAINT_EVENT_YL (message)));
	    CVT_UNSIGNED (5, (SM_PAINT_EVENT_YH (message)));
	    break;
	  }
	case mt_resize_event:
	  {
	    result = (allocate_marked_vector (TC_VECTOR, 4, 0));
	    CVT_UNSIGNED (0, ET_RESIZE);
	    CVT_UNSIGNED (1, (SM_RESIZE_EVENT_WID (message)));
	    CVT_UNSIGNED (2, (SM_RESIZE_EVENT_WIDTH (message)));
	    CVT_UNSIGNED (3, (SM_RESIZE_EVENT_HEIGHT (message)));
	    break;
	  }
	case mt_visibility_event:
	  {
	    result = (allocate_marked_vector (TC_VECTOR, 3, 0));
	    CVT_UNSIGNED (0, ET_VISIBILITY);
	    CVT_UNSIGNED (1, (SM_VISIBILITY_EVENT_WID (message)));
	    CVT_BOOLEAN  (2, (SM_VISIBILITY_EVENT_SHOWNP (message)));
	    break;
	  }
	default:
	  OS2_destroy_message (message);
	  OS2_error_anonymous ();
	  break;
	}
    OS2_destroy_message (message);
    PRIMITIVE_RETURN (result);
  }
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
  PRIMITIVE_RETURN (long_to_integer (OS2_console_wid ()));
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
