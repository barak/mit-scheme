/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

/* Primitives for dealing with colors and color maps */

#include "scheme.h"
#include "prims.h"
#include "x11.h"

DEFINE_PRIMITIVE ("X-GET-WINDOW-ATTRIBUTES", Prim_x_get_window_attributes, 1, 1, 0)
{
  PRIMITIVE_HEADER(1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XWindowAttributes a;
    if (! (XGetWindowAttributes ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&a))))
      error_external_return ();
    {
      SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 23, true));
      VECTOR_SET (result, 0, (long_to_integer (a . x)));
      VECTOR_SET (result, 1, (long_to_integer (a . y)));
      VECTOR_SET (result, 2, (long_to_integer (a . width)));
      VECTOR_SET (result, 3, (long_to_integer (a . height)));
      VECTOR_SET (result, 4, (long_to_integer (a . border_width)));
      VECTOR_SET (result, 5, (long_to_integer (a . depth)));
      VECTOR_SET (result, 6, (X_VISUAL_TO_OBJECT (a . visual)));
      VECTOR_SET (result, 7, (long_to_integer (a . root)));
      VECTOR_SET (result, 8, (long_to_integer (a . class)));
      VECTOR_SET (result, 9, (long_to_integer (a . bit_gravity)));
      VECTOR_SET (result, 10, (long_to_integer (a . win_gravity)));
      VECTOR_SET (result, 11, (long_to_integer (a . backing_store)));
      VECTOR_SET (result, 12, (long_to_integer (a . backing_planes)));
      VECTOR_SET (result, 13, (long_to_integer (a . backing_pixel)));
      VECTOR_SET (result, 14, (BOOLEAN_TO_OBJECT (a . save_under)));
      VECTOR_SET (result, 15,
		  (X_COLORMAP_TO_OBJECT ((a . colormap), (XW_XD (xw)))));
      VECTOR_SET (result, 16, (BOOLEAN_TO_OBJECT (a . map_installed)));
      VECTOR_SET (result, 17, (long_to_integer (a . map_state)));
      VECTOR_SET (result, 18, (long_to_integer (a . all_event_masks)));
      VECTOR_SET (result, 19, (long_to_integer (a . your_event_mask)));
      VECTOR_SET (result, 20, (long_to_integer (a . do_not_propagate_mask)));
      VECTOR_SET (result, 21, (BOOLEAN_TO_OBJECT (a . override_redirect)));
      VECTOR_SET (result, 22,
		  (long_to_integer (XScreenNumberOfScreen (a . screen))));
      PRIMITIVE_RETURN (result);
    }
  }
}

/* Visuals */

DEFINE_PRIMITIVE ("X-GET-DEFAULT-VISUAL", Prim_x_get_default_visual, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (X_VISUAL_TO_OBJECT
     (XDefaultVisual ((XD_DISPLAY (x_display_arg (1))), (arg_integer (2)))));
}

DEFINE_PRIMITIVE ("X-WINDOW-VISUAL", Prim_x_window_visual, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XWindowAttributes a;
    if (! (XGetWindowAttributes ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&a))))
      error_external_return ();
    PRIMITIVE_RETURN (X_VISUAL_TO_OBJECT (a . visual));
  }
}

DEFINE_PRIMITIVE ("X-VISUAL-DEALLOCATE", Prim_x_visual_deallocate, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  deallocate_x_visual (x_visual_arg (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE("X-GET-VISUAL-INFO", Prim_x_get_visual_info, 10, 10, 0)
/* Inputs: Scheme window or display
           (the remaining are either #F or a valid value)
           Visual-ID
	   Screen number (or #F is window supplied)
	   Depth
	   Class
	   Red-mask (integer)
	   Green-mask (integer)
	   Blue-mask (integer)
	   Colormap size
	   Bits per RGB

  Returns a vector of vectors, each of which has the following format:
           Visual (Scheme format, for use in later calls)
           Visual-ID
	   Screen number
	   Depth
	   Class
	   Red-mask (integer)
	   Green-mask (integer)
	   Blue-mask (integer)
	   Colormap size
	   Bits per RGB
*/
#define LOAD_IF(argno, type, field, mask_bit)		\
  if (ARG_REF(argno) != SHARP_F)			\
  { VI.field = type arg_integer(argno);			\
    VIMask |= mask_bit;					\
  }
{ PRIMITIVE_HEADER (10);
  { Display *dpy;
    long ScreenNumber;
    XVisualInfo VI, *VIList, *ThisVI;
    long VIMask = VisualNoMask;
    long AnswerSize, i;
    int AnswerCount;
    SCHEME_OBJECT Result, This_Vector;

    if (ARG_REF(3) == SHARP_F)
    { struct xwindow * xw = x_window_arg (1);
      XWindowAttributes attrs;

      dpy = XW_DISPLAY(xw);
      XGetWindowAttributes(dpy, XW_WINDOW(xw), &attrs);
      ScreenNumber = XScreenNumberOfScreen(attrs.screen);
    }
    else
    { struct xdisplay * xd = x_display_arg (1);
      ScreenNumber = arg_integer(3);
      dpy = XD_DISPLAY(xd);
    }
    VI.screen = ScreenNumber;
    LOAD_IF(2, (VisualID), visualid, VisualIDMask);
    LOAD_IF(4, (unsigned int), depth, VisualDepthMask);
    LOAD_IF(5, (int), class, VisualClassMask);
    LOAD_IF(6, (unsigned long), red_mask, VisualRedMaskMask);
    LOAD_IF(7, (unsigned long), green_mask, VisualGreenMaskMask);
    LOAD_IF(8, (unsigned long), blue_mask, VisualBlueMaskMask);
    LOAD_IF(9, (int), colormap_size, VisualColormapSizeMask);
    LOAD_IF(10, (int), bits_per_rgb, VisualBitsPerRGBMask);
    VIList = XGetVisualInfo(dpy, VIMask, &VI, &AnswerCount);
    AnswerSize = (AnswerCount + 1) + (11 * AnswerCount);
    if (GC_NEEDED_P (AnswerSize))
    { XFree((void *) VIList);
      Primitive_GC (AnswerSize);
    }
    Result = allocate_marked_vector (TC_VECTOR, AnswerCount, false);
    for (i=0, ThisVI=VIList; i < AnswerCount; i++, ThisVI++)
    { This_Vector = allocate_marked_vector(TC_VECTOR, 10, false);
      VECTOR_SET(This_Vector, 0, (X_VISUAL_TO_OBJECT (ThisVI->visual)));
      VECTOR_SET(This_Vector, 1, long_to_integer((long) ThisVI->visualid));
      VECTOR_SET(This_Vector, 2, long_to_integer(ThisVI->screen));
      VECTOR_SET(This_Vector, 3, long_to_integer(ThisVI->depth));
      VECTOR_SET(This_Vector, 4, long_to_integer(ThisVI->class));
      VECTOR_SET(This_Vector, 5, long_to_integer(ThisVI->red_mask));
      VECTOR_SET(This_Vector, 6, long_to_integer(ThisVI->green_mask));
      VECTOR_SET(This_Vector, 7, long_to_integer(ThisVI->blue_mask));
      VECTOR_SET(This_Vector, 8, long_to_integer(ThisVI->colormap_size));
      VECTOR_SET(This_Vector, 9, long_to_integer(ThisVI->bits_per_rgb));
      VECTOR_SET(Result, i, This_Vector);
    }
    XFree((void *) VIList);
    PRIMITIVE_RETURN(Result);
  }
}

/* Colormaps */

DEFINE_PRIMITIVE ("X-GET-DEFAULT-COLORMAP", Prim_x_get_default_colormap, 2, 2,
  "Given DISPLAY and SCREEN-NUMBER, return default colormap for screen.")
{
  PRIMITIVE_HEADER (2);
  {
    struct xdisplay * xd = (x_display_arg (1));
    PRIMITIVE_RETURN
      (X_COLORMAP_TO_OBJECT
       ((XDefaultColormap ((XD_DISPLAY (xd)), (arg_integer (2)))), xd));
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-COLORMAP", Prim_x_window_colormap, 1, 1,
  "Return WINDOW's colormap.")
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XWindowAttributes a;
    if (! (XGetWindowAttributes ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&a))))
      error_external_return ();
    PRIMITIVE_RETURN (X_COLORMAP_TO_OBJECT ((a . colormap), (XW_XD (xw))));
  }
}

DEFINE_PRIMITIVE ("X-SET-WINDOW-COLORMAP", Prim_x_set_window_colormap, 2, 2,
  "Set WINDOW's colormap to COLORMAP.")
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    XSetWindowColormap ((XW_DISPLAY (xw)), (XW_WINDOW (xw)),
			(XCM_COLORMAP (x_colormap_arg (2))));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-CREATE-COLORMAP", Prim_x_create_colormap, 3, 3,
  "Given WINDOW, and VISUAL, create and return a colormap.\n\
If third arg WRITEABLE is true, returned colormap may be modified.")
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    PRIMITIVE_RETURN
      (X_COLORMAP_TO_OBJECT
       ((XCreateColormap ((XW_DISPLAY (xw)), (XW_WINDOW (xw)),
			  (XV_VISUAL (x_visual_arg (2))), (BOOLEAN_ARG (3)))),
	(XW_XD (xw))));
  }
}

DEFINE_PRIMITIVE ("X-COPY-COLORMAP-AND-FREE", Prim_x_copy_colormap_and_free,
		  1, 1,
  "Return a new copy of COLORMAP.")
{
  PRIMITIVE_HEADER (1);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    PRIMITIVE_RETURN
      (X_COLORMAP_TO_OBJECT
       ((XCopyColormapAndFree ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)))),
	(XCM_XD (xcm))));
  }
}

DEFINE_PRIMITIVE ("X-FREE-COLORMAP", Prim_x_free_colormap, 1, 1,
  "Deallocate COLORMAP.")
{
  PRIMITIVE_HEADER (1);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    XFreeColormap ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)));
    deallocate_x_colormap (xcm);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define ARG_RGB_VALUE(argno) (arg_index_integer ((argno), 65536))

DEFINE_PRIMITIVE ("X-ALLOCATE-COLOR", Prim_x_allocate_color, 4, 4, 0)
{
  /* Input: colormap, red, green, blue
     Returns: pixel, or #F if unable to allocate color cell.  */
  PRIMITIVE_HEADER (4);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    XColor c;
    (c . red) = (ARG_RGB_VALUE (2));
    (c . green) = (ARG_RGB_VALUE (3));
    (c . blue) = (ARG_RGB_VALUE (4));
    PRIMITIVE_RETURN
      ((XAllocColor ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)), (&c)))
       ? (long_to_integer (c . pixel))
       : SHARP_F);
  }
}

DEFINE_PRIMITIVE ("X-STORE-COLOR", Prim_x_store_color, 5, 5,
  "Input: colormap, pixel, r, g, b (r/g/b may be #f).")
{
  PRIMITIVE_HEADER (5);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    XColor c;
    (c . pixel) = (arg_nonnegative_integer (2));
    (c . flags) = 0;
    if ((ARG_REF (3)) != SHARP_F)
      {
	(c . red) = (arg_index_integer (3, 65536));
	(c . flags) |= DoRed;
      }
    if ((ARG_REF (4)) != SHARP_F)
      {
	(c . green) = (arg_index_integer (4, 65536));
	(c . flags) |= DoGreen;
      }
    if ((ARG_REF (5)) != SHARP_F)
      {
	(c . blue) = (arg_index_integer (5, 65536));
	(c . flags) |= DoBlue;
      }
    XStoreColor ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)), (&c));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define CONVERT_COLOR_OBJECT(index, color, flag)			\
{									\
  SCHEME_OBJECT object = (VECTOR_REF (color_object, (index)));		\
  if (object != SHARP_F)						\
    {									\
      if (! ((INTEGER_P (object)) && (integer_to_long_p (object))))	\
	goto losing_color_object;					\
      {									\
	long value = (integer_to_long (object));			\
	if ((value < 0) || (value > 65535))				\
	  goto losing_color_object;					\
	(colors_scan -> color) = value;					\
	(colors_scan -> flags) |= (flag);				\
      }									\
    }									\
}

DEFINE_PRIMITIVE ("X-STORE-COLORS", Prim_x_store_colors, 2, 2,
  "Input: colormap, vector of vectors, each of\n\
which contains pixel, r, g, b (where r/g/b can be #f or integer).")
{
  PRIMITIVE_HEADER (2);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    SCHEME_OBJECT color_vector = (VECTOR_ARG (2));
    unsigned long n_colors = (VECTOR_LENGTH (color_vector));
    XColor * colors = (dstack_alloc ((sizeof (XColor)) * n_colors));
    {
      SCHEME_OBJECT * vector_scan = (VECTOR_LOC (color_vector, 0));
      SCHEME_OBJECT * vector_end = (vector_scan + n_colors);
      XColor * colors_scan = colors;
      while (vector_scan < vector_end)
	{
	  SCHEME_OBJECT color_object = (*vector_scan++);
	  if (! ((VECTOR_P (color_object))
		 && ((VECTOR_LENGTH (color_object)) == 4)))
	    {
	    losing_color_object:
	      error_wrong_type_arg (3);
	    }
	  {
	    SCHEME_OBJECT pixel_object = (VECTOR_REF (color_object, 0));
	    if (! ((INTEGER_P (pixel_object))
		   && (integer_to_long_p (pixel_object))))
	      goto losing_color_object;
	    (colors_scan -> pixel) = (integer_to_long (pixel_object));
	  }
	  (colors_scan -> flags) = 0;
	  CONVERT_COLOR_OBJECT (1, red, DoRed);
	  CONVERT_COLOR_OBJECT (2, green, DoGreen);
	  CONVERT_COLOR_OBJECT (3, blue, DoBlue);
	  colors_scan += 1;
	}
    }
    XStoreColors ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)), colors, n_colors);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-FREE-COLORS", Prim_x_free_colors, 1, -1, 0)
{
  /* Input: colormap, pixel ... */
  PRIMITIVE_HEADER (LEXPR);
  if (GET_LEXPR_ACTUALS < 1)
    signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    unsigned int n_pixels = (GET_LEXPR_ACTUALS - 1);
    unsigned long * pixels =
      (dstack_alloc ((sizeof (unsigned long)) * n_pixels));
    unsigned int i;
    for (i = 0; (i < n_pixels); i += 1)
      (pixels[i]) = (arg_integer (i + 2));
    XFreeColors ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)),
		 pixels, n_pixels, 0);
  }
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-QUERY-COLOR", Prim_x_query_color, 2, 2, 0)
{
  /* Input: colormap, pixel
     Output: vector of red, green, blue */
  PRIMITIVE_HEADER (2);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 3, true));
    XColor c;
    c . pixel = (arg_integer (2));
    XQueryColor ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)), (&c));
    VECTOR_SET (result, 0, (long_to_integer (c . red)));
    VECTOR_SET (result, 1, (long_to_integer (c . green)));
    VECTOR_SET (result, 2, (long_to_integer (c . blue)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("X-QUERY-COLORS", Prim_x_query_colors, 1, -1, 0)
{
  /* Input: colormap, pixel ...
     Output: a vector of vectors, each with #(red, green, blue)  */
  PRIMITIVE_HEADER (LEXPR);
  if (GET_LEXPR_ACTUALS < 1)
    signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    unsigned int n_colors = (GET_LEXPR_ACTUALS - 1);
    XColor * colors = (dstack_alloc ((sizeof (XColor)) * n_colors));
    unsigned int i;
    for (i = 0; (i < n_colors); i += 1)
      ((colors[i]) . pixel) = (arg_integer (i + 2));
    XQueryColors ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)), colors, n_colors);
    {
      SCHEME_OBJECT result =
	(allocate_marked_vector (TC_VECTOR, n_colors, true));
      for (i = 0; (i < n_colors); i += 1)
	{
	  SCHEME_OBJECT cv = (allocate_marked_vector (TC_VECTOR, 3, true));
	  VECTOR_SET (cv, 0, (long_to_integer ((colors[i]) . red)));
	  VECTOR_SET (cv, 1, (long_to_integer ((colors[i]) . green)));
	  VECTOR_SET (cv, 2, (long_to_integer ((colors[i]) . blue)));
	  VECTOR_SET (result, i, cv);
	}
      PRIMITIVE_RETURN (result);
    }
  }
}

/* Named colors */

DEFINE_PRIMITIVE ("X-PARSE-COLOR", Prim_x_parse_color, 2, 2, 0)
{ /* Input: colormap, string
     Output: vector of pixel, red, green, blue
  */
  PRIMITIVE_HEADER (2);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    XColor TheColor;
    if (! (XParseColor ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)),
			(STRING_ARG (2)), (&TheColor))))
      PRIMITIVE_RETURN (SHARP_F);
    {
      SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 4, true));
      VECTOR_SET(result, 0, long_to_integer(TheColor.pixel));
      VECTOR_SET(result, 1, long_to_integer(TheColor.red));
      VECTOR_SET(result, 2, long_to_integer(TheColor.green));
      VECTOR_SET(result, 3, long_to_integer(TheColor.blue));
      PRIMITIVE_RETURN (result);
    }
  }
}

DEFINE_PRIMITIVE ("X-ALLOCATE-NAMED-COLOR", Prim_x_allocate_named_color, 2, 2, 0)
{ /* Input: colormap, name
     Returns: vector of closest pixel, red, green, blue
                        exact   pixel, red, green, blue
  */

  SCHEME_OBJECT Result;
  XColor Exact, Closest;
  struct xcolormap * xcm;
  PRIMITIVE_HEADER (2);

  xcm = (x_colormap_arg (1));
  XAllocNamedColor
    ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)),
     (STRING_ARG (2)), &Exact, &Closest);
  Result = allocate_marked_vector(TC_VECTOR, 8, true);
  VECTOR_SET(Result, 0, long_to_integer(Closest.pixel));
  VECTOR_SET(Result, 1, long_to_integer(Closest.red));
  VECTOR_SET(Result, 2, long_to_integer(Closest.green));
  VECTOR_SET(Result, 3, long_to_integer(Closest.blue));
  VECTOR_SET(Result, 4, long_to_integer(Exact.pixel));
  VECTOR_SET(Result, 5, long_to_integer(Exact.red));
  VECTOR_SET(Result, 6, long_to_integer(Exact.green));
  VECTOR_SET(Result, 7, long_to_integer(Exact.blue));
  PRIMITIVE_RETURN(Result);
}

DEFINE_PRIMITIVE("X-STORE-NAMED-COLOR", Prim_x_store_named_color, 6, 6, 0)
{
  /* Input: colormap, color name, pixel, DoRed, DoGreen, DoBlue */
  PRIMITIVE_HEADER(6);
  {
    struct xcolormap * xcm = (x_colormap_arg (1));
    XStoreNamedColor ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)),
		      (STRING_ARG (2)), (arg_integer (4)),
		      (((BOOLEAN_ARG (4)) ? DoRed : 0)
		       | ((BOOLEAN_ARG (5)) ? DoGreen : 0)
		       | ((BOOLEAN_ARG (6)) ? DoBlue : 0)));
  }
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE("X-LOOKUP-COLOR", Prim_x_lookup_color, 2, 2, 0)
{
  /* Input: colormap, name
     Returns: vector of closest pixel, red, green, blue
     exact   pixel, red, green, blue
     */

  SCHEME_OBJECT Result;
  XColor Exact, Closest;
  struct xcolormap * xcm;
  PRIMITIVE_HEADER (2);

  xcm = (x_colormap_arg (1));
  if (! (XAllocNamedColor
	 ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)),
	  (STRING_ARG (2)), &Exact, &Closest)))
    PRIMITIVE_RETURN (SHARP_F);
  Result = allocate_marked_vector(TC_VECTOR, 8, true);
  VECTOR_SET(Result, 0, long_to_integer(Closest.pixel));
  VECTOR_SET(Result, 1, long_to_integer(Closest.red));
  VECTOR_SET(Result, 2, long_to_integer(Closest.green));
  VECTOR_SET(Result, 3, long_to_integer(Closest.blue));
  VECTOR_SET(Result, 4, long_to_integer(Exact.pixel));
  VECTOR_SET(Result, 5, long_to_integer(Exact.red));
  VECTOR_SET(Result, 6, long_to_integer(Exact.green));
  VECTOR_SET(Result, 7, long_to_integer(Exact.blue));
  PRIMITIVE_RETURN(Result);
}
