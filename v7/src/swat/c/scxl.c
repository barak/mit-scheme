/* X11 support similar to that in Joel Bartlett's Scheme-To-C xlib (scxl) */

#include "scheme.h"
#include "prims.h"
#include "ux.h"
#include "uxselect.h"

/* Changed 7/95 by Nick in an attempt to fix problem Hal was having with SWAT over PPP (i.e. slow connections) */
/* commented out 'cause x11.h includes em all
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include "ansidecl.h" */

#include "x11.h"

extern void EXFUN (block_signals, (void));
extern void EXFUN (unblock_signals, (void));

/* end nick's changes - but see below for more */


/* Operations */

DEFINE_PRIMITIVE ("%XAllocNamedColor", Prim_scxl_allocated_named_color,
		  5, 5, 0)
{ /* (%XAllocNamedColor display colormap color-string
                        return-alloc return-exact)
  */
  PRIMITIVE_HEADER(5);
  CHECK_ARG(4, STRING_P);
  CHECK_ARG(5, STRING_P);
  if (STRING_LENGTH(ARG_REF(4)) < sizeof (XColor))
    error_bad_range_arg(4);
  if (STRING_LENGTH(ARG_REF(5)) < sizeof (XColor))
    error_bad_range_arg(5);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) XAllocNamedColor((Display *) arg_integer(1),
			      (Colormap) arg_integer(2),
			      STRING_ARG(3),
			      (XColor *) STRING_ARG(4),
			      (XColor *) STRING_ARG(5))));
}

DEFINE_PRIMITIVE ("%XChangeWindowAttributes", Prim_scxl_change_wind_attr,
		  4, 4, 0)
{ /* (%XChangeWindowAttributes display window mask attributes) */
  /* ATTRIBUTES is a string */
  PRIMITIVE_HEADER(4);
  CHECK_ARG(4, STRING_P);
  if (STRING_LENGTH(ARG_REF(4)) < sizeof (XSetWindowAttributes))
    error_bad_range_arg(4);
  XChangeWindowAttributes((Display *) arg_integer(1),
			  (Window) arg_integer(2),
			  (unsigned long) arg_integer(3),
			  (XSetWindowAttributes *) STRING_ARG(4));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XCheckMaskEvent", Prim_scxl_check_mask_event, 3, 3, 0)
{ /* (%XCheckMaskEvent display event-mask return-event) */
  PRIMITIVE_HEADER (3);
  CHECK_ARG(3, STRING_P);
  if (STRING_LENGTH(ARG_REF(3)) < sizeof(XEvent))
    error_bad_range_arg(3);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT
		    (XCheckMaskEvent ((Display *) arg_integer(1),
				      (long) arg_integer(2),
				      (XEvent *) STRING_ARG(3))));
}

DEFINE_PRIMITIVE ("%XClearArea", Prim_scxl_clear_area, 7, 7, 0)
{ /* (%XClearArea display window x y width height) */
  PRIMITIVE_HEADER (7);
  XClearArea ((Display *) arg_integer(1),
	      (Drawable) arg_integer(2),
	      (int) arg_integer(3),
	      (int) arg_integer(4),
	      (unsigned int) arg_integer(5),
	      (unsigned int) arg_integer(6),
	      (Bool) BOOLEAN_ARG(7));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XClearWindow", Prim_scxl_clear_window, 2, 2, 0)
{ /* (%XClearWindow display window) */
  PRIMITIVE_HEADER (2);
  XClearWindow ((Display *) arg_integer(1),
		(Drawable) arg_integer(2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XCloseDisplay", Prim_scxl_close, 1, 1, 0)
{ /* (%XCloseDisplay display) */
  PRIMITIVE_HEADER (1);
  XCloseDisplay((Display *) arg_integer(1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XConnectionNumber", Prim_scxl_connection_number, 1, 1, 0)
{ /* (%XConnectionNumber display) */
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer
		    (XConnectionNumber((Display *) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%XCreateGC", Prim_scxl_create_gc, 4, 4, 0)
{ /* (%XCreateGC display window mask values) */
  PRIMITIVE_HEADER(4);
  CHECK_ARG(4, STRING_P);
  if (STRING_LENGTH(ARG_REF(4)) < sizeof(XGCValues))
    error_bad_range_arg(4);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) XCreateGC((Display *) arg_integer(1),
		       (Drawable) arg_integer(2),
		       (unsigned long) arg_integer(3),
		       (XGCValues *) STRING_ARG(4))));
}

DEFINE_PRIMITIVE ("%XCreateRegion", Prim_scxl_create_region, 0, 0, 0)
{ /* (%XCreateRegion) */
  Region Result;
  PRIMITIVE_HEADER(0);
  Result = XCreateRegion();
  PRIMITIVE_RETURN (long_to_integer ((long) Result));
}

DEFINE_PRIMITIVE ("%XCreateSimpleWindow", Prim_scxl_create_simple_window,
		  9, 9, 0)
{ /* (%XCreateSimpleWindow display parent-window x y width height
                           border-width border-color background-color)
  */
  PRIMITIVE_HEADER(9);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) XCreateSimpleWindow
      ((Display *) arg_integer(1),
       (Window) arg_integer(2),
       (int) arg_integer(3),
       (int) arg_integer(4),
       (unsigned int) arg_integer(5),
       (unsigned int) arg_integer(6),
       (unsigned int) arg_integer(7),
       (unsigned long) arg_integer(8),
       (unsigned long) arg_integer(9))));
}

DEFINE_PRIMITIVE ("%XDecodeButtonEvent", prim_scxl_decode_button, 2, 2, 0)
{ /* (%XDecodeButtonEvent event vector) */
  SCHEME_OBJECT Result = ARG_REF(2);
  SCHEME_OBJECT *Next;
  XButtonEvent *Input;

  PRIMITIVE_HEADER (2);
  CHECK_ARG(1, STRING_P);
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(XButtonEvent))
    error_bad_range_arg(1);
  CHECK_ARG(2, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 15)
   error_bad_range_arg(2);
  Input = (XButtonEvent *) STRING_ARG(1);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = long_to_integer ((long) (Input->type));   /* 0 */
  *Next++ = long_to_integer ((long) (Input->serial)); /* 1 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->send_event);     /* 2 */
  *Next++ = long_to_integer ((long) (Input->display)); /* 3 */
  *Next++ = long_to_integer ((long) (Input->window)); /* 4 */
  *Next++ = long_to_integer ((long) (Input->root));   /* 5 */
  *Next++ = long_to_integer ((long) (Input->subwindow)); /* 6 */
  *Next++ = long_to_integer ((long) (Input->time));   /* 7 */
  *Next++ = long_to_integer ((long) (Input->x));      /* 8 */
  *Next++ = long_to_integer ((long) (Input->y));      /* 9 */
  *Next++ = long_to_integer ((long) (Input->x_root)); /* 10 */
  *Next++ = long_to_integer ((long) (Input->y_root)); /* 11 */
  *Next++ = long_to_integer ((long) (Input->state));  /* 12 */
  *Next++ = long_to_integer ((long) (Input->button));  /* 13 */
  *Next = BOOLEAN_TO_OBJECT(Input->same_screen);      /* 14 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}
  
DEFINE_PRIMITIVE ("%XDecodeConfigureEvent",
		  prim_scxl_decode_config, 2, 2, 0)
{ /* (%XDecodeConfigureEvent event vector) */
  SCHEME_OBJECT Result = ARG_REF(2);
  SCHEME_OBJECT *Next;
  XConfigureEvent *Input;

  PRIMITIVE_HEADER (2);
  CHECK_ARG(1, STRING_P);
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(XConfigureEvent))
    error_bad_range_arg(1);
  CHECK_ARG(2, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 13)
   error_bad_range_arg(2);
  Input = (XConfigureEvent *) STRING_ARG(1);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = long_to_integer ((long) (Input->type));   /* 0 */
  *Next++ = long_to_integer ((long) (Input->serial)); /* 1 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->send_event);     /* 2 */
  *Next++ = long_to_integer ((long) (Input->display)); /* 3 */
  *Next++ = long_to_integer ((long) (Input->event));  /* 4 */
  *Next++ = long_to_integer ((long) (Input->window)); /* 5 */
  *Next++ = long_to_integer ((long) (Input->x));      /* 6 */
  *Next++ = long_to_integer ((long) (Input->y));      /* 7 */
  *Next++ = long_to_integer ((long) (Input->width));  /* 8 */
  *Next++ = long_to_integer ((long) (Input->height)); /* 9 */
  *Next++ = long_to_integer ((long) (Input->border_width)); /* 10 */
  *Next++ = long_to_integer ((long) (Input->above));  /* 11 */
  *Next = BOOLEAN_TO_OBJECT(Input->override_redirect); /* 12 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}
  
DEFINE_PRIMITIVE ("%XDecodeCrossingEvent", prim_scxl_decode_crossing, 2, 2, 0)
{ /* (%XDecodeCrossingEvent event vector) */
  SCHEME_OBJECT Result = ARG_REF(2);
  SCHEME_OBJECT *Next;
  XCrossingEvent *Input;

  PRIMITIVE_HEADER (2);
  CHECK_ARG(1, STRING_P);
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(XCrossingEvent))
    error_bad_range_arg(1);
  CHECK_ARG(2, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 17)
   error_bad_range_arg(2);
  Input = (XCrossingEvent *) STRING_ARG(1);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = long_to_integer ((long) (Input->type));   /* 0 */
  *Next++ = long_to_integer ((long) (Input->serial)); /* 1 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->send_event);     /* 2 */
  *Next++ = long_to_integer ((long) (Input->display)); /* 3 */
  *Next++ = long_to_integer ((long) (Input->window)); /* 4 */
  *Next++ = long_to_integer ((long) (Input->root));   /* 5 */
  *Next++ = long_to_integer ((long) (Input->subwindow)); /* 6 */
  *Next++ = long_to_integer ((long) (Input->time));   /* 7 */
  *Next++ = long_to_integer ((long) (Input->x));      /* 8 */
  *Next++ = long_to_integer ((long) (Input->y));      /* 9 */
  *Next++ = long_to_integer ((long) (Input->x_root)); /* 10 */
  *Next++ = long_to_integer ((long) (Input->y_root)); /* 11 */
  *Next++ = long_to_integer ((long) (Input->mode));   /* 12 */
  *Next++ = long_to_integer ((long) (Input->detail)); /* 13 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->same_screen);    /* 14 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->focus);	      /* 15 */
  *Next = long_to_integer ((long) (Input->state));    /* 16 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}
  
DEFINE_PRIMITIVE ("%XDecodeExposeEvent", prim_scxl_decode_expose, 2, 2, 0)
{ /* (%XDecodeExposeEvent event vector) */
  SCHEME_OBJECT Result = ARG_REF(2);
  SCHEME_OBJECT *Next;
  XExposeEvent *Input;

  PRIMITIVE_HEADER (2);
  CHECK_ARG(1, STRING_P);
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(XExposeEvent))
    error_bad_range_arg(1);
  CHECK_ARG(2, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 10)
   error_bad_range_arg(2);
  Input = (XExposeEvent *) STRING_ARG(1);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = long_to_integer ((long) (Input->type));   /* 0 */
  *Next++ = long_to_integer ((long) (Input->serial)); /* 1 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->send_event);     /* 2 */
  *Next++ = long_to_integer ((long) (Input->display)); /* 3 */
  *Next++ = long_to_integer ((long) (Input->window)); /* 4 */
  *Next++ = long_to_integer ((long) (Input->x));      /* 5 */
  *Next++ = long_to_integer ((long) (Input->y));      /* 6 */
  *Next++ = long_to_integer ((long) (Input->width));  /* 7 */
  *Next++ = long_to_integer ((long) (Input->height)); /* 8 */
  *Next = long_to_integer ((long) (Input->count));    /* 9 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XDecodeKeyEvent", prim_scxl_decode_key, 2, 2, 0)
{ /* (%XDecodeKeyEvent event vector) */
  SCHEME_OBJECT Result = ARG_REF(2);
  SCHEME_OBJECT *Next;
  XKeyEvent *Input;

  PRIMITIVE_HEADER (2);
  CHECK_ARG(1, STRING_P);
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(XKeyEvent))
    error_bad_range_arg(1);
  CHECK_ARG(2, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 15)
   error_bad_range_arg(2);
  Input = (XKeyEvent *) STRING_ARG(1);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = long_to_integer ((long) (Input->type));   /* 0 */
  *Next++ = long_to_integer ((long) (Input->serial)); /* 1 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->send_event);     /* 2 */
  *Next++ = long_to_integer ((long) (Input->display)); /* 3 */
  *Next++ = long_to_integer ((long) (Input->window)); /* 4 */
  *Next++ = long_to_integer ((long) (Input->root));   /* 5 */
  *Next++ = long_to_integer ((long) (Input->subwindow)); /* 6 */
  *Next++ = long_to_integer ((long) (Input->time));   /* 7 */
  *Next++ = long_to_integer ((long) (Input->x));      /* 8 */
  *Next++ = long_to_integer ((long) (Input->y));      /* 9 */
  *Next++ = long_to_integer ((long) (Input->x_root)); /* 10 */
  *Next++ = long_to_integer ((long) (Input->y_root)); /* 11 */
  *Next++ = long_to_integer ((long) (Input->state));  /* 12 */
  *Next++ = long_to_integer ((long) (Input->keycode)); /* 13 */
  *Next = BOOLEAN_TO_OBJECT(Input->same_screen);      /* 14 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XDecodeMotionEvent", prim_scxl_decode_motion, 2, 2, 0)
{ /* (%XDecodeMotionEvent event vector) */
  SCHEME_OBJECT Result = ARG_REF(2);
  SCHEME_OBJECT *Next;
  XMotionEvent *Input;

  PRIMITIVE_HEADER (2);
  CHECK_ARG(1, STRING_P);
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(XMotionEvent))
    error_bad_range_arg(1);
  CHECK_ARG(2, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 15)
   error_bad_range_arg(2);
  Input = (XMotionEvent *) STRING_ARG(1);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = long_to_integer ((long) (Input->type));   /* 0 */
  *Next++ = long_to_integer ((long) (Input->serial)); /* 1 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->send_event);     /* 2 */
  *Next++ = long_to_integer ((long) (Input->display)); /* 3 */
  *Next++ = long_to_integer ((long) (Input->window)); /* 4 */
  *Next++ = long_to_integer ((long) (Input->root));   /* 5 */
  *Next++ = long_to_integer ((long) (Input->subwindow)); /* 6 */
  *Next++ = long_to_integer ((long) (Input->time));   /* 7 */
  *Next++ = long_to_integer ((long) (Input->x));      /* 8 */
  *Next++ = long_to_integer ((long) (Input->y));      /* 9 */
  *Next++ = long_to_integer ((long) (Input->x_root)); /* 10 */
  *Next++ = long_to_integer ((long) (Input->y_root)); /* 11 */
  *Next++ = long_to_integer ((long) (Input->state));  /* 12 */
  *Next++ = long_to_integer ((long) (Input->is_hint)); /* 13 */
  *Next = BOOLEAN_TO_OBJECT(Input->same_screen);      /* 14 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}
  
DEFINE_PRIMITIVE ("%XDecodeUnknownEvent", Prim_scxl_decode_unknown, 2, 2, 0)
{ /* (%XDecodeUnknownEvent event vector) */
  SCHEME_OBJECT Result = ARG_REF(2);
  SCHEME_OBJECT *Next;
  XAnyEvent *Input;

  PRIMITIVE_HEADER (2);
  CHECK_ARG(1, STRING_P);
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(XAnyEvent))
    error_bad_range_arg(1);
  CHECK_ARG(2, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 5)
   error_bad_range_arg(2);
  Input = (XAnyEvent *) STRING_ARG(1);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = long_to_integer ((long) (Input->type));   /* 0 */
  *Next++ = long_to_integer ((long) (Input->serial)); /* 1 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->send_event);     /* 2 */
  *Next++ = long_to_integer ((long) (Input->display)); /* 3 */
  *Next = long_to_integer ((long) (Input->window));   /* 4 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XDecodeWindowAttributes", Prim_scxl_decode_wind_attr, 2, 2, 0)
{ /* (%XDecodeWindowAttributes attributes vector) */
  SCHEME_OBJECT Result = ARG_REF(2);
  SCHEME_OBJECT *Next;
  XWindowAttributes *Input;

  PRIMITIVE_HEADER (2);
  CHECK_ARG(1, STRING_P);
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(XWindowAttributes))
    error_bad_range_arg(1);
  CHECK_ARG(2, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 23)
   error_bad_range_arg(2);
  Input = (XWindowAttributes *) STRING_ARG(1);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = long_to_integer ((long) (Input->x));      /* 0 */
  *Next++ = long_to_integer ((long) (Input->y));      /* 1 */
  *Next++ = long_to_integer ((long) (Input->width));  /* 2 */
  *Next++ = long_to_integer ((long) (Input->height)); /* 3 */
  *Next++ = long_to_integer ((long) (Input->border_width)); /* 4 */
  *Next++ = long_to_integer ((long) (Input->depth));  /* 5 */
  *Next++ = long_to_integer ((long) (Input->visual)); /* 6 */
  *Next++ = long_to_integer ((long) (Input->root));   /* 7 */
  *Next++ = long_to_integer ((long) (Input->class));  /* 8 */
  *Next++ = long_to_integer ((long) (Input->bit_gravity)); /* 9 */
  *Next++ = long_to_integer ((long) (Input->win_gravity)); /* 10 */
  *Next++ = long_to_integer ((long) (Input->backing_store)); /* 11 */
  *Next++ = long_to_integer ((long) (Input->backing_planes)); /* 12 */
  *Next++ = long_to_integer ((long) (Input->backing_pixel)); /* 13 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->save_under);     /* 14 */
  *Next++ = long_to_integer ((long) (Input->colormap));	/* 15 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->map_installed);  /* 16 */
  *Next++ = long_to_integer ((long) (Input->map_state)); /* 17 */
  *Next++ = long_to_integer ((long) (Input->all_event_masks)); /* 18 */
  *Next++ = long_to_integer ((long) (Input->your_event_mask)); /* 19 */
  *Next++ = long_to_integer ((long) (Input->do_not_propagate_mask)); /* 20 */
  *Next++ = BOOLEAN_TO_OBJECT(Input->override_redirect); /* 21 */
  *Next = long_to_integer ((long) (Input->screen));   /* 22 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XDecodeXColor", Prim_scxl_decode_xcolor, 2, 2, 0)
{ /* (%XDecodeXColor xcolor vector) */
  SCHEME_OBJECT Result = ARG_REF(2);
  SCHEME_OBJECT *Next;
  XColor *Input;

  PRIMITIVE_HEADER (2);
  CHECK_ARG(1, STRING_P);
  if (STRING_LENGTH(ARG_REF(1)) != sizeof(XColor))
    error_bad_range_arg(1);
  CHECK_ARG(2, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 5)
   error_bad_range_arg(2);
  Input = (XColor *) STRING_ARG(1);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = long_to_integer ((long) (Input->pixel));  /* 0 */
  *Next++ = long_to_integer ((long) (Input->red));    /* 1 */
  *Next++ = long_to_integer ((long) (Input->green));  /* 2 */
  *Next++ = long_to_integer ((long) (Input->blue));   /* 3 */
  *Next = long_to_integer ((long) (Input->flags));    /* 4 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XDefaultColormap", Prim_scxl_default_colormap, 2, 2, 0)
{ /* (%XDefaultColormap display screen) */
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) XDefaultColormap((Display *) arg_integer(1),
			      arg_integer(2))));
}

DEFINE_PRIMITIVE ("%XDefaultRootWindow", Prim_scxl_default_root_window,
		  1, 1, 0)
{ /* (%XDefaultRootWindow display) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) XDefaultRootWindow ((Display *) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%XDefaultScreen", Prim_scxl_default_screen, 1, 1, 0)
{ /* (%XDefaultScreen display) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long) XDefaultScreen((Display *) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%XDestroyRegion", Prim_scxl_destroy_region, 1, 1, 0)
{ /* (%XDestroyRegion region) */
  PRIMITIVE_HEADER (1);
  XDestroyRegion ((Region) arg_integer(1));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XDestroyWindow", Prim_scxl_destroy_window, 2, 2, 0)
{ /* (%XDestroyWindow display window) */
  PRIMITIVE_HEADER (2);
  XDestroyWindow((Display *) arg_integer(1),
		 (Window) arg_integer(2));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XDrawArc", Prim_scxl_draw_arc, 9, 9, 0)
{ /* (%XDrawArc display window context
                      x y width height angle1 angle2) */
  PRIMITIVE_HEADER (9);
  XDrawArc((Display *) arg_integer(1),
	   (Drawable) arg_integer(2),
	   (GC) arg_integer(3),
	   (int) arg_integer(4),
	   (int) arg_integer(5),
	   (unsigned int) arg_integer(6),
	   (unsigned int) arg_integer(7),
	   (unsigned int) arg_integer(8),
	   (unsigned int) arg_integer(9));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XDrawLine", Prim_scxl_draw_line, 7, 7, 0)
{ /* (%XDrawLine display window context x1 y1 x2 y2) */
  PRIMITIVE_HEADER (7);
  XDrawLine((Display *) arg_integer(1),
	    (Drawable) arg_integer(2),
	    (GC) arg_integer(3),
	    (int) arg_integer(4),
	    (int) arg_integer(5),
	    (int) arg_integer(6),
	    (int) arg_integer(7));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XDrawRectangle", Prim_scxl_draw_rectangle, 7, 7, 0)
{ /* (%XDrawRectangle display window context x y width height) */
  PRIMITIVE_HEADER (7);
  XDrawRectangle((Display *) arg_integer(1),
		 (Drawable) arg_integer(2),
		 (GC) arg_integer(3),
		 (int) arg_integer(4),
		 (int) arg_integer(5),
		 (unsigned int) arg_integer(6),
		 (unsigned int) arg_integer(7));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XFillArc", Prim_scxl_fill_arc, 9, 9, 0)
{ /* (%XFillArc display window context
                      x y width height angle1 angle2) */
  PRIMITIVE_HEADER (9);
  XFillArc((Display *) arg_integer(1),
	   (Drawable) arg_integer(2),
	   (GC) arg_integer(3),
	   (int) arg_integer(4),
	   (int) arg_integer(5),
	   (unsigned int) arg_integer(6),
	   (unsigned int) arg_integer(7),
	   (unsigned int) arg_integer(8),
	   (unsigned int) arg_integer(9));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XFillRectangle", Prim_scxl_fill_rectangle, 7, 7, 0)
{ /* (%XFillRectangle display window context x y width height) */
  PRIMITIVE_HEADER (7);
  XFillRectangle((Display *) arg_integer(1),
		 (Drawable) arg_integer(2),
		 (GC) arg_integer(3),
		 (int) arg_integer(4),
		 (int) arg_integer(5),
		 (unsigned int) arg_integer(6),
		 (unsigned int) arg_integer(7));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XFlush", Prim_scxl_flush, 1, 1, 0)
{ /* (%XFlush display) */
  PRIMITIVE_HEADER (1);
  XFlush((Display *) arg_integer(1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XFreeColormap", Prim_scxl_free_colormap, 2, 2, 0)
{ /* (%XFreeColormap display colormap) */
  PRIMITIVE_HEADER(2);
  XFreeColormap((Display *) arg_integer(1), (Colormap) arg_integer(2));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XFreeGC", Prim_scxl_free_gc, 2, 2, 0)
{ /* (%XFreeGC display graphic-context) */
  PRIMITIVE_HEADER(2);
  XFreeGC((Display *) arg_integer(1), (GC) arg_integer(2));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XGetDefault", Prim_scxl_get_default, 3, 3, 0)
{ /* (%XGetDefault display program option) */
  PRIMITIVE_HEADER(3);
  PRIMITIVE_RETURN
    (char_pointer_to_string
     ((unsigned char *) XGetDefault((Display *) arg_integer(1),
				    STRING_ARG(2),
				    STRING_ARG(3))));
}

DEFINE_PRIMITIVE ("%XGetWindowAttributes", Prim_scxl_get_wind_attr, 3, 3, 0)
{ /* (%XGetWindowAttributes display window attributes-to-fill) */
  PRIMITIVE_HEADER(3);
  CHECK_ARG(3, STRING_P);
  if (STRING_LENGTH(ARG_REF(3)) < sizeof(XWindowAttributes))
    error_bad_range_arg(3);
  PRIMITIVE_RETURN
    (long_to_integer
     ((long)
      XGetWindowAttributes((Display *) arg_integer(1),
			   (Window) arg_integer(2),
			   (XWindowAttributes *) STRING_ARG(3))));
}

DEFINE_PRIMITIVE ("%XIntersectRegion", Prim_scxl_intersect_reg, 3, 3, 0)
{ /* (%XIntersectRegion source1 source2 dest) */
  PRIMITIVE_HEADER (3);
  XIntersectRegion((Region) arg_integer(1),
		   (Region) arg_integer(2),
		   (Region) arg_integer(3));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XLoadFont", Prim_scxl_load_font, 2, 2, 0)
{ /* (%XLoadFont display name-string) */
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (long_to_integer ((long) XLoadFont((Display *) arg_integer(1),
				       STRING_ARG(2))));
}

DEFINE_PRIMITIVE ("%XMapWindow", Prim_scxl_map_window, 2, 2, 0)
{ /* (%XMapWindow display window) */
  PRIMITIVE_HEADER(2);
  XMapWindow((Display *) arg_integer(1),
	     (Window) arg_integer(2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XNextEvent", Prim_scxl_next_event, 2, 2, 0)
{ /* (%XNextEvent display returned-event) */
  PRIMITIVE_HEADER (2);
  CHECK_ARG(2, STRING_P);
  if (STRING_LENGTH(ARG_REF(2)) < sizeof(XEvent))
    error_bad_range_arg(2);
  XNextEvent((Display *) arg_integer(1),
	     (XEvent *) STRING_ARG(2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
     
DEFINE_PRIMITIVE ("%XOpenDisplay", Prim_scxl_open_display, 1, 1, 0)
{ /* (%XOpenDisplay string) */
  PRIMITIVE_HEADER (1);
  {
    /* Changed 7/95 by Nick in an attempt to fix problem Hal was having with SWAT over PPP (i.e. slow connections) */
    Display * display;
    block_signals ();
    display = XOpenDisplay(STRING_ARG(1));
    unblock_signals ();
    PRIMITIVE_RETURN (long_to_integer((long) display));
  }
}

DEFINE_PRIMITIVE ("%XPending", Prim_scxl_pending, 1, 1, 0)
{ /* (%XPending display) */
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer(XPending ((Display *) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%XPutBackEvent", Prim_scxl_put_back_event, 2, 2, 0)
{ /* (%XPutBackEvent display event) */
  PRIMITIVE_HEADER (2);
  CHECK_ARG(2, STRING_P);
  if (STRING_LENGTH(ARG_REF(2)) < sizeof(XEvent))
    error_bad_range_arg(2);
  XPutBackEvent ((Display *) arg_integer(1),
		 (XEvent *) STRING_ARG(2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XQueryPointer", Prim_scxl_query_pointer, 3, 3, 0)
{ /* (%XQueryPointer display window result-vector) */
  SCHEME_OBJECT Result = ARG_REF(3);
  SCHEME_OBJECT *Next;
  Window Root=0, Child=0;
  int Root_X=0, Root_Y=0, Win_X=0, Win_Y=0;
  unsigned int Keys_Buttons=0;
  Bool result_status;

  PRIMITIVE_HEADER (3);
  CHECK_ARG(3, VECTOR_P);
  if (VECTOR_LENGTH(Result) < 8) error_bad_range_arg(3);
  result_status = XQueryPointer((Display *) arg_integer(1),
				(Window) arg_integer(2),
				&Root, &Child, &Root_X, &Root_Y,
				&Win_X, &Win_Y, &Keys_Buttons);
  Next = VECTOR_LOC(Result, 0);
  *Next++ = BOOLEAN_TO_OBJECT(result_status);	      /* 0 */
  *Next++ = long_to_integer ((long) Root);	      /* 1 */
  *Next++ = long_to_integer ((long) Child);	      /* 2 */
  *Next++ = long_to_integer ((long) Root_X);	      /* 3 */
  *Next++ = long_to_integer ((long) Root_Y);	      /* 4 */
  *Next++ = long_to_integer ((long) Win_X);	      /* 5 */
  *Next++ = long_to_integer ((long) Win_Y);	      /* 6 */
  *Next++ = long_to_integer ((long) Keys_Buttons);    /* 7 */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XQueryTree", Prim_query_tree, 2, 2, 0)
{ /* (%XQueryTree display window)
     returns a vector of #(root parent . kids)
  */
  SCHEME_OBJECT Kid_Return;
  Window Root, Parent, *Kids;
  unsigned int NKids, i;

  PRIMITIVE_HEADER (2);
  if (XQueryTree((Display *) arg_integer(1), (Window) arg_integer(2),
		 &Root, &Parent, &Kids, &NKids)==0)
  { error_external_return();
  }
  Kid_Return = allocate_marked_vector(TC_VECTOR, NKids+2, true);
  VECTOR_SET(Kid_Return, 0, long_to_integer((long) Root));
  VECTOR_SET(Kid_Return, 1, long_to_integer((long) Parent));
  for (i=0; i < NKids; i++)
    VECTOR_SET(Kid_Return, i+2, long_to_integer((long) Kids[i]));
  XFree(Kids);
  PRIMITIVE_RETURN (Kid_Return);
}

DEFINE_PRIMITIVE ("%XScreenCount", Prim_scxl_screencount, 1, 1, 0)
{ /* (%XScreenCount display) */
  PRIMITIVE_HEADER(1);
  PRIMITIVE_RETURN (long_to_integer
		    (XScreenCount((Display *) arg_integer(1))));
}

DEFINE_PRIMITIVE ("%XSetForeground", Prim_scxl_set_foreground, 3, 3, 0)
{ /* (%XSetForeground display context pixel) */
  PRIMITIVE_HEADER(3);
  XSetForeground((Display *) arg_integer(1),
		 (GC) arg_integer(2),
		 arg_integer(3));
  PRIMITIVE_RETURN (UNSPECIFIC);
}


DEFINE_PRIMITIVE ("%XSetFunction", Prim_scxl_set_function, 3, 3, 0)
{ /* (%XSetFunction display context function_number) */
  PRIMITIVE_HEADER(3);
  XSetFunction((Display *) arg_integer(1),
		 (GC) arg_integer(2),
		 arg_integer(3));
  PRIMITIVE_RETURN (UNSPECIFIC);
}


DEFINE_PRIMITIVE ("%XSetRegion", Prim_scxl_set_region, 3, 3, 0)
{ /* (%XSetForeground display gc region) */
  PRIMITIVE_HEADER(3);
  XSetRegion((Display *) arg_integer(1),
	     (GC) arg_integer(2),
	     (Region) arg_integer(3));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XStoreName", Prim_scxl_store_name, 3, 3, 0)
{ /* (%XStoreName display window title-string */
  PRIMITIVE_HEADER (3);
  XStoreName((Display *) arg_integer(1),
	     (Window) arg_integer(2),
	     STRING_ARG(3));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XSubtractRegion", Prim_scxl_subtract_reg, 3, 3, 0)
{ /* (%XSubtractRegion source1 source2 dest) */
  PRIMITIVE_HEADER (3);
  XSubtractRegion((Region) arg_integer(1),
		  (Region) arg_integer(2),
		  (Region) arg_integer(3));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XTranslateCoordinates", Prim_scxl_translate_coords,
		  6, 6, 0)
{ /* (%XTranslateCoordinates display old-window new-window x y vector)
  */
  int X, Y;
  Window W;
  SCHEME_OBJECT Vect;
  Boolean status;
  PRIMITIVE_HEADER (6);
  Vect = VECTOR_ARG(6);
  if (VECTOR_LENGTH(Vect) < 4) error_bad_range_arg(6);
  status = XTranslateCoordinates((Display *) arg_integer(1),
				 (Window) arg_integer(2),
				 (Window) arg_integer(3),
				 (int) arg_integer(4),
				 (int) arg_integer(5),
				 &X, &Y, &W);
  VECTOR_SET(Vect, 0, BOOLEAN_TO_OBJECT(status));
  VECTOR_SET(Vect, 1, long_to_integer((long) X));
  VECTOR_SET(Vect, 2, long_to_integer((long) Y));
  VECTOR_SET(Vect, 3, long_to_integer((long) W));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XUnionRegion", Prim_scxl_union_reg, 3, 3, 0)
{ /* (%XUnionRegion source1 source2 dest) */
  PRIMITIVE_HEADER (3);
  XUnionRegion((Region) arg_integer(1),
	       (Region) arg_integer(2),
	       (Region) arg_integer(3));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XUnionRectSpecsWithRegion!", Prim_scxl_union_rectspecs, 6, 6, 0)
{ /* (%XUnionRectSpecsWithRegion! x y width height inregion outregion) */
  XRectangle Rect;
  PRIMITIVE_HEADER (6);
  Rect.x = arg_integer(1);
  Rect.y = arg_integer(2);
  Rect.width = arg_integer(3);
  Rect.height = arg_integer(4);
  XUnionRectWithRegion(&Rect,
		       (Region) (arg_integer (5)),
		       (Region) (arg_integer (6)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%XUnloadFont", Prim_scxl_unload_font, 2, 2, 0)
{ /* (%XUnloadFont display font) */
  PRIMITIVE_HEADER(2);
  XUnloadFont((Display *) arg_integer(1), (Font) arg_integer(2));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

/* Data structure constructors.  These are represented as strings to */
/* circumvent  garbage collection */

DEFINE_PRIMITIVE ("%XMake-Color", Prim_scxl_make_color, 0, 0, 0)
{ /* (%XMake-Color) */
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN(allocate_string(sizeof(XColor)));
}

DEFINE_PRIMITIVE ("%XMake-Event", Prim_scxl_make_event, 0, 0, 0)
{ /* (%XMake-Event) */
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN(allocate_string(sizeof(XEvent)));
}

DEFINE_PRIMITIVE ("%XMake-GCValues", Prim_scxl_make_gc_values, 0, 0, 0)
{ /* (%XMake-GCValues) */
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN(allocate_string(sizeof(XGCValues)));
}

DEFINE_PRIMITIVE ("%XMake-GetWindowAttributes", Prim_scxl_make_get_wind_attr,
		  0, 0, 0)
{ /* (%XMake-GetWindowAttributes) */
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN(allocate_string(sizeof(XWindowAttributes)));
}

DEFINE_PRIMITIVE ("%XMake-SetWindowAttributes", Prim_scxl_make_set_wind_attr,
		  0, 0, 0)
{ /* (%XMake-SetWindowAttributes) */
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN(allocate_string(sizeof(XSetWindowAttributes)));
}

/* Mutators */

#define Mutator(StructType, Field, FieldType, Converter)	\
{								\
  PRIMITIVE_HEADER(2);						\
  CHECK_ARG(1, STRING_P);					\
  if (STRING_LENGTH(ARG_REF(1)) < sizeof(StructType))		\
    error_bad_range_arg(1);					\
  ((StructType *) (STRING_ARG(1)))->Field =			\
    ((FieldType) Converter(2));					\
  PRIMITIVE_RETURN (UNSPECIFIC);				\
}

DEFINE_PRIMITIVE ("%XSetWindowAttributes-Event_Mask!",
		  Prim_scxl_XSetWindowAttributes_Event_Mask_bang,
		  2, 2, 0)
  Mutator(XSetWindowAttributes, event_mask, long, arg_integer)

static int
DEFUN (x_io_error_handler, (display),
       Display * display)
{
  fprintf (stderr, "\nX IO Error on display 0x%x\n", display);
  error_external_return ();
}

void DEFUN (Scheme_x_error_handler, (display, error_event),
	    Display * display AND
	    XErrorEvent * error_event)
{
  char buffer [2048];
  XGetErrorText (display, (error_event -> error_code),
		 buffer, (sizeof (buffer)));
  fprintf (stderr, "\nX Error: %s\n", buffer);
  fprintf (stderr, "         Request code: %d\n",
	   (error_event -> request_code));
  fprintf (stderr, "         Error serial: 0x%x\n",
	   (error_event -> serial));
  fprintf (stderr, "         Display: %d (0x%x)\n",
	   error_event->display, error_event->display);
  fprintf (stderr, "         Resource ID: %d (0x%x)\n",
	   error_event->resourceid, error_event->resourceid);
  fprintf (stderr, "         Minor code: %d (0x%x)\n",
	   error_event->minor_code, error_event->minor_code);
  fflush (stderr);
}

static int
DEFUN (Scheme_low_x_error_handler, (display, error_event),
       Display * display AND
       XErrorEvent * error_event)
{ Scheme_x_error_handler(display, error_event);
  error_external_return ();
}

DEFINE_PRIMITIVE("%XInitSCXL!", Prim_scxl_init, 0, 0, 0)
{ extern int _XDefaultError();
  PRIMITIVE_HEADER (0);
  XSetErrorHandler (Scheme_low_x_error_handler);
  XSetIOErrorHandler (x_io_error_handler);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE("%XSync", Prim_scxl_sync, 2, 2, 0)
{ PRIMITIVE_HEADER (2);
  XSync((Display *) arg_integer(1), BOOLEAN_ARG(2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE("%XSynchronize", Prim_scxl_synchronize, 2, 2, 0)
{ PRIMITIVE_HEADER (2);
  XSynchronize((Display *) arg_integer(1), BOOLEAN_ARG(2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

SCHEME_OBJECT Debug_State_Flag;

DEFINE_PRIMITIVE("%SetDebugState!", Prim_scxl_state, 1, 1, 0)
{ PRIMITIVE_HEADER(1);
  Debug_State_Flag = ARG_REF(1);
  PRIMITIVE_RETURN(UNSPECIFIC);
}

extern char *EXFUN (dload_initialize_file, (void));

char *
  DEFUN_VOID (dload_initialize_file)
{ declare_primitive("%XAllocNamedColor", Prim_scxl_allocated_named_color,
		  5, 5, 0);
  declare_primitive("%XChangeWindowAttributes", Prim_scxl_change_wind_attr,
		  4, 4, 0);
  declare_primitive("%XCheckMaskEvent", Prim_scxl_check_mask_event, 3, 3, 0);
  declare_primitive("%XClearArea", Prim_scxl_clear_area, 7, 7, 0);
  declare_primitive("%XClearWindow", Prim_scxl_clear_window, 2, 2, 0);
  declare_primitive("%XCloseDisplay", Prim_scxl_close, 1, 1, 0);
  declare_primitive("%XConnectionNumber", Prim_scxl_connection_number, 1, 1, 0);
  declare_primitive("%XCreateGC", Prim_scxl_create_gc, 4, 4, 0);
  declare_primitive("%XCreateRegion", Prim_scxl_create_region, 0, 0, 0);
  declare_primitive("%XCreateSimpleWindow", Prim_scxl_create_simple_window,
		  9, 9, 0);
  declare_primitive("%XDecodeButtonEvent", prim_scxl_decode_button, 2, 2, 0);
  declare_primitive("%XDecodeConfigureEvent",
		  prim_scxl_decode_config, 2, 2, 0);
  declare_primitive("%XDecodeCrossingEvent", prim_scxl_decode_crossing, 2, 2, 0);
  declare_primitive("%XDecodeExposeEvent", prim_scxl_decode_expose, 2, 2, 0);
  declare_primitive("%XDecodeKeyEvent", prim_scxl_decode_key, 2, 2, 0);
  declare_primitive("%XDecodeMotionEvent", prim_scxl_decode_motion, 2, 2, 0);
  declare_primitive("%XDecodeUnknownEvent", Prim_scxl_decode_unknown, 2, 2, 0);
  declare_primitive("%XDecodeWindowAttributes", Prim_scxl_decode_wind_attr, 2, 2, 0);
  declare_primitive("%XDecodeXColor", Prim_scxl_decode_xcolor, 2, 2, 0);
  declare_primitive("%XDefaultColormap", Prim_scxl_default_colormap, 2, 2, 0);
  declare_primitive("%XDefaultRootWindow", Prim_scxl_default_root_window,
		  1, 1, 0);
  declare_primitive("%XDefaultScreen", Prim_scxl_default_screen, 1, 1, 0);
  declare_primitive("%XDestroyRegion", Prim_scxl_destroy_region, 1, 1, 0);
  declare_primitive("%XDestroyWindow", Prim_scxl_destroy_window, 2, 2, 0);
  declare_primitive("%XDrawArc", Prim_scxl_draw_arc, 9, 9, 0);
  declare_primitive("%XDrawLine", Prim_scxl_draw_line, 7, 7, 0);
  declare_primitive("%XDrawRectangle", Prim_scxl_draw_rectangle, 7, 7, 0);
  declare_primitive("%XFillArc", Prim_scxl_fill_arc, 9, 9, 0);
  declare_primitive("%XFillRectangle", Prim_scxl_fill_rectangle, 7, 7, 0);
  declare_primitive("%XFlush", Prim_scxl_flush, 1, 1, 0);
  declare_primitive("%XFreeColormap", Prim_scxl_free_colormap, 2, 2, 0);
  declare_primitive("%XFreeGC", Prim_scxl_free_gc, 2, 2, 0);
  declare_primitive("%XGetDefault", Prim_scxl_get_default, 3, 3, 0);
  declare_primitive("%XGetWindowAttributes", Prim_scxl_get_wind_attr, 3, 3, 0);
  declare_primitive("%XIntersectRegion", Prim_scxl_intersect_reg, 3, 3, 0);
  declare_primitive("%XLoadFont", Prim_scxl_load_font, 2, 2, 0);
  declare_primitive("%XMapWindow", Prim_scxl_map_window, 2, 2, 0);
  declare_primitive("%XNextEvent", Prim_scxl_next_event, 2, 2, 0);
  declare_primitive("%XOpenDisplay", Prim_scxl_open_display, 1, 1, 0);
  declare_primitive("%XPending", Prim_scxl_pending, 1, 1, 0);
  declare_primitive("%XPutBackEvent", Prim_scxl_put_back_event, 2, 2, 0);
  declare_primitive("%XQueryPointer", Prim_scxl_query_pointer, 3, 3, 0);
  declare_primitive("%XQueryTree", Prim_query_tree, 2, 2, 0);
  declare_primitive("%XScreenCount", Prim_scxl_screencount, 1, 1, 0);
  declare_primitive("%XSetForeground", Prim_scxl_set_foreground, 3, 3, 0);
  declare_primitive("%XSetFunction", Prim_scxl_set_function, 3, 3, 0);
  declare_primitive("%XSetRegion", Prim_scxl_set_region, 3, 3, 0);
  declare_primitive("%XStoreName", Prim_scxl_store_name, 3, 3, 0);
  declare_primitive("%XSubtractRegion", Prim_scxl_subtract_reg, 3, 3, 0);
  declare_primitive("%XTranslateCoordinates", Prim_scxl_translate_coords,
		  6, 6, 0);
  declare_primitive("%XUnionRegion", Prim_scxl_union_reg, 3, 3, 0);
  declare_primitive("%XUnionRectSpecsWithRegion!", Prim_scxl_union_rectspecs, 6, 6, 0);
  declare_primitive("%XUnloadFont", Prim_scxl_unload_font, 2, 2, 0);
  declare_primitive("%XMake-Color", Prim_scxl_make_color, 0, 0, 0);
  declare_primitive("%XMake-Event", Prim_scxl_make_event, 0, 0, 0);
  declare_primitive("%XMake-GCValues", Prim_scxl_make_gc_values, 0, 0, 0);
  declare_primitive("%XMake-GetWindowAttributes", Prim_scxl_make_get_wind_attr,
		  0, 0, 0);
  declare_primitive("%XMake-SetWindowAttributes", Prim_scxl_make_set_wind_attr,
		  0, 0, 0);
  declare_primitive("%XSetWindowAttributes-Event_Mask!",
		  Prim_scxl_XSetWindowAttributes_Event_Mask_bang,
		  2, 2, 0);
  declare_primitive("%XInitSCXL!", Prim_scxl_init, 0, 0, 0);
  declare_primitive("%XSync", Prim_scxl_sync, 2, 2, 0);
  declare_primitive("%XSynchronize", Prim_scxl_synchronize, 2, 2, 0);
  declare_primitive("%SetDebugState!", Prim_scxl_state, 1, 1, 0);
  return "#SCXL";
}
