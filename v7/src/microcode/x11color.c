/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11color.c,v 1.2 1991/07/11 03:57:36 cph Exp $

Copyright (c) 1991 Massachusetts Institute of Technology

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

/* Primitives for dealing with colors and color maps */

#include "scheme.h"
#include "prims.h"
#include "x11.h"

extern unsigned int allocate_x_visual ();

DEFINE_PRIMITIVE("X-GET-WINDOW-ATTRIBUTES", Prim_x_get_window_attributes, 1, 1, 0)
{ PRIMITIVE_HEADER(1);
  { XWindowAttributes attrs;
    struct xwindow * xw = x_window_arg(1);

    if (XGetWindowAttributes(XW_DISPLAY(xw), XW_WINDOW(xw), &attrs) == 0)
    { PRIMITIVE_RETURN (SHARP_F);
    }
    else
    { SCHEME_OBJECT Result = allocate_marked_vector(TC_VECTOR, 23, true);
      VECTOR_SET(Result, 0, long_to_integer(attrs.x));
      VECTOR_SET(Result, 1, long_to_integer(attrs.y));
      VECTOR_SET(Result, 2, long_to_integer(attrs.width));
      VECTOR_SET(Result, 3, long_to_integer(attrs.height));
      VECTOR_SET(Result, 4, long_to_integer(attrs.border_width));
      VECTOR_SET(Result, 5, long_to_integer(attrs.depth));
      VECTOR_SET(Result, 6, XV_TO_OBJECT(allocate_x_visual(attrs.visual)));
      VECTOR_SET(Result, 7, long_to_integer(attrs.root));
      VECTOR_SET(Result, 8, long_to_integer(attrs.class));
      VECTOR_SET(Result, 9, long_to_integer(attrs.bit_gravity));
      VECTOR_SET(Result, 10, long_to_integer(attrs.win_gravity));
      VECTOR_SET(Result, 11, long_to_integer(attrs.backing_store));
      VECTOR_SET(Result, 12, long_to_integer(attrs.backing_planes));
      VECTOR_SET(Result, 13, long_to_integer(attrs.backing_pixel));
      VECTOR_SET(Result, 14, BOOLEAN_TO_OBJECT(attrs.save_under));
      VECTOR_SET(Result, 15, long_to_integer(attrs.colormap));
      VECTOR_SET(Result, 16, BOOLEAN_TO_OBJECT(attrs.map_installed));
      VECTOR_SET(Result, 17, long_to_integer(attrs.map_state));
      VECTOR_SET(Result, 18, long_to_integer(attrs.all_event_masks));
      VECTOR_SET(Result, 19, long_to_integer(attrs.your_event_mask));
      VECTOR_SET(Result, 20, long_to_integer(attrs.do_not_propagate_mask));
      VECTOR_SET(Result, 21, BOOLEAN_TO_OBJECT(attrs.override_redirect));
      VECTOR_SET(Result, 22,
		 long_to_integer(XScreenNumberOfScreen(attrs.screen)));
      PRIMITIVE_RETURN(Result);
    }
  }
}

DEFINE_PRIMITIVE("X-GET-DEFAULT-VISUAL", Prim_x_get_default_visual, 2, 2, 0)
/* Inputs: (Scheme window and #F) or (Scheme display and screen number)
   Returns: Scheme visual
*/
{ PRIMITIVE_HEADER(2);
  { Display *dpy;
    long ScreenNumber;
    Visual *answer;

    if (ARG_REF(2) == SHARP_F)
    { struct xwindow * xw = x_window_arg (1);
      XWindowAttributes attrs;
      
      dpy = XW_DISPLAY(xw);
      XGetWindowAttributes(dpy, XW_WINDOW(xw), &attrs);
      ScreenNumber = XScreenNumberOfScreen(attrs.screen);
    }
    else
    { struct xdisplay * xd = x_display_arg (1);
      ScreenNumber = arg_integer(2);
      dpy = XD_DISPLAY(xd);
    }
    answer = XDefaultVisual(dpy, ScreenNumber);
    PRIMITIVE_RETURN(XV_TO_OBJECT(allocate_x_visual(answer)));
  }
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
    if (GC_Check (AnswerSize))
    { XFree((PTR) VIList);
      Primitive_GC (AnswerSize);
    }
    Result = allocate_marked_vector (TC_VECTOR, AnswerCount, false);
    for (i=0, ThisVI=VIList; i < AnswerCount; i++, ThisVI++)
    { This_Vector = allocate_marked_vector(TC_VECTOR, 10, false);
      VECTOR_SET(This_Vector, 0, XV_TO_OBJECT(allocate_x_visual(ThisVI->visual)));
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
    XFree((PTR) VIList);
    PRIMITIVE_RETURN(Result);
  }
}

DEFINE_PRIMITIVE("X-GET-DEFAULT-COLORMAP", Prim_x_get_default_colormap, 2, 2, 0)
{ /* Input: (Scheme) display, screen number */
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN(
    long_to_integer(
      XDefaultColormap(XD_DISPLAY(x_display_arg(1)), arg_integer(2))));
}

DEFINE_PRIMITIVE("X-PARSE-COLOR", Prim_x_parse_color, 3, 3, 0)
{ /* Input: (Scheme) display, colormap, string
     Output: vector of pixel, red, green, blue
  */
  PRIMITIVE_HEADER (3);
  { XColor TheColor;
    if (XParseColor(XD_DISPLAY(x_display_arg(1)),
		    arg_integer(2), STRING_ARG(3), &TheColor) == 0)
    { PRIMITIVE_RETURN(SHARP_F);
    }
    else
    { SCHEME_OBJECT Result;

      Result = allocate_marked_vector(TC_VECTOR, 4, true);
      VECTOR_SET(Result, 0, long_to_integer(TheColor.pixel));
      VECTOR_SET(Result, 1, long_to_integer(TheColor.red));
      VECTOR_SET(Result, 2, long_to_integer(TheColor.green));
      VECTOR_SET(Result, 3, long_to_integer(TheColor.blue));
      PRIMITIVE_RETURN(Result);
    }
  }
}

DEFINE_PRIMITIVE("X-CREATE-COLORMAP", Prim_x_create_colormap, 3, 3, 0)
{ /* Input: (Scheme) window, (Scheme) Visual, Allocate? */

  PRIMITIVE_HEADER(3);
  {  struct xwindow * xw = x_window_arg (1);
     Display * dpy = XW_DISPLAY(xw);
     Visual * v = x_visual_arg (2);
     SCHEME_OBJECT Allocate = BOOLEAN_ARG (3);
     
     PRIMITIVE_RETURN(
       long_to_integer(XCreateColormap(dpy, XW_WINDOW(xw), v, Allocate)));
   }
}

DEFINE_PRIMITIVE("X-COPY-COLORMAP-AND-FREE", Prim_x_copy_colormap_and_free, 2, 2, 0)
{ /* Input: (Scheme) display, colormap */
  PRIMITIVE_HEADER(2);
  PRIMITIVE_RETURN(
    long_to_integer(
      XCopyColormapAndFree(XD_DISPLAY(x_display_arg(1)), arg_integer(2))));
}

DEFINE_PRIMITIVE("X-SET-WINDOW-COLORMAP", Prim_x_set_window_colormap, 2, 2, 0)
{ /* Input: (Scheme) window, colormap */
  struct xwindow * xw = x_window_arg (1);

  PRIMITIVE_HEADER(2);
  XSetWindowColormap(XW_DISPLAY(xw), XW_WINDOW(xw), arg_integer(2));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE("X-FREE-COLORMAP", Prim_x_free_colormap, 2, 2, 0)
{ /* Input: (Scheme) display, colormap */
  PRIMITIVE_HEADER (2);
  XFreeColormap(XD_DISPLAY(x_display_arg(1)), arg_integer(2));
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE("X-ALLOCATE-COLOR", Prim_x_allocate_color, 5, 5, 0)
{ /* Input: (Scheme) display, colormap, red, green, blue
     Returns: vector with pixel, red, green, blue
  */
  XColor ColorObj;
  SCHEME_OBJECT Result;
  
  PRIMITIVE_HEADER(5);
  ColorObj.red = arg_integer(3);
  ColorObj.green = arg_integer(4);
  ColorObj.blue = arg_integer(5);
  XAllocColor(XD_DISPLAY(x_display_arg(1)), arg_integer(2), &ColorObj);
  Result = allocate_marked_vector(TC_VECTOR, 4, true);
  VECTOR_SET(Result, 0, long_to_integer(ColorObj.pixel));
  VECTOR_SET(Result, 1, long_to_integer(ColorObj.red));
  VECTOR_SET(Result, 2, long_to_integer(ColorObj.green));
  VECTOR_SET(Result, 3, long_to_integer(ColorObj.blue));
  PRIMITIVE_RETURN(Result);
}  

DEFINE_PRIMITIVE("X-ALLOCATE-NAMED-COLOR", Prim_x_allocate_named_color, 3, 3, 0)
{ /* Input: (Scheme) display, colormap, name
     Returns: vector of closest pixel, red, green, blue
                        exact   pixel, red, green, blue
  */

  SCHEME_OBJECT Result;
  XColor Exact, Closest;

  PRIMITIVE_HEADER(3);
  XAllocNamedColor(XD_DISPLAY(x_display_arg (1)),
		   arg_integer(2), STRING_ARG(3), &Exact, &Closest);
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

DEFINE_PRIMITIVE("X-LOOKUP-COLOR", Prim_x_lookup_color, 3, 3, 0)
{ /* Input: (Scheme) display, colormap, name
     Returns: vector of closest pixel, red, green, blue
                        exact   pixel, red, green, blue
  */

  SCHEME_OBJECT Result;
  XColor Exact, Closest;
  long Stat;

  PRIMITIVE_HEADER(3);
  Stat = XAllocNamedColor(XD_DISPLAY(x_display_arg (1)),
			  arg_integer(2), STRING_ARG(3), &Exact, &Closest);
  if (Stat == 0)
  { PRIMITIVE_RETURN (SHARP_F);
  }
  else
  { Result = allocate_marked_vector(TC_VECTOR, 8, true);
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
}

DEFINE_PRIMITIVE ("X-STORE-COLOR", Prim_x_store_color, 6, 6,
  "Input: (Scheme) display, colormap, pixel, r, g, b (r/g/b may be #f).")
{
  XColor c;
  PRIMITIVE_HEADER (6);

  (c . pixel) = (arg_nonnegative_integer (3));
  (c . flags) = 0;
  if ((ARG_REF (4)) != SHARP_F)
    {
      (c . red) = (arg_index_integer (4, 65536));
      (c . flags) |= DoRed;
    }
  if ((ARG_REF (5)) != SHARP_F)
    {
      (c . green) = (arg_index_integer (5, 65536));
      (c . flags) |= DoGreen;
    }
  if ((ARG_REF (6)) != SHARP_F)
    {
      (c . blue) = (arg_index_integer (6, 65536));
      (c . flags) |= DoBlue;
    }
  XStoreColor ((XD_DISPLAY (x_display_arg (1))), (arg_integer (2)), (&c));
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

DEFINE_PRIMITIVE ("X-STORE-COLORS", Prim_x_store_colors, 3, 3,
  "Input: (Scheme) display, colormap, vector of vectors, each of\n\
which contains pixel, r, g, b (where r/g/b can be #f or integer).")
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT color_vector = (VECTOR_ARG (3));
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
    XStoreColors
      ((XD_DISPLAY (x_display_arg (1))),
       (arg_integer (2)),
       colors,
       n_colors);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE("X-STORE-NAMED-COLOR", Prim_x_store_named_color, 7, 7, 0)
{ /* Input: (Scheme) display, colormap, color name, pixel, DoRed, DoGreen,
            DoBlue */
  PRIMITIVE_HEADER(7);
  { long flags = 0;
    if (BOOLEAN_ARG(5))  flags |= DoRed;
    if (BOOLEAN_ARG(6))  flags |= DoGreen;
    if (BOOLEAN_ARG(7))  flags |= DoBlue;
    XStoreNamedColor(XD_DISPLAY(x_display_arg(1)), arg_integer(2),
		     STRING_ARG(3), arg_integer(4), flags);
  }
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE("X-FREE-COLORS", Prim_x_free_colors, 2, -1, 0)
{ /* Input: (Scheme) display, colormap, pixel ... */
  PRIMITIVE_HEADER(LEXPR);
  { long npixels = (LEXPR_N_ARGUMENTS()) - 2;
    long * First_Pixel = (long *) Free;
    long i, *This_Pixel;

    Primitive_GC_If_Needed (npixels);
    for (i=0, This_Pixel=First_Pixel; i < npixels; i++)
    { *This_Pixel++ = integer_to_long(ARG_REF(i+3));
    }
    XFreeColors(XD_DISPLAY(x_display_arg(1)), arg_integer(2),
		((unsigned long *) First_Pixel), npixels, 0);
  }
  PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE("X-QUERY-COLOR", Prim_x_query_color, 3, 3, 0)
{ /* Input: (Scheme) display, colormap, pixel
     Output: vector of red, green, blue
  */
  PRIMITIVE_HEADER(3);
  { XColor ThisColor;
    SCHEME_OBJECT Result = allocate_marked_vector(TC_VECTOR, 3, true);
    
    ThisColor.pixel = arg_integer(3);
    XQueryColor(XD_DISPLAY(x_display_arg(1)), arg_integer(2), &ThisColor);
    VECTOR_SET(Result, 0, long_to_integer(ThisColor.red));
    VECTOR_SET(Result, 1, long_to_integer(ThisColor.green));
    VECTOR_SET(Result, 2, long_to_integer(ThisColor.blue));
    PRIMITIVE_RETURN(Result);
  }
}
  
DEFINE_PRIMITIVE("X-QUERY-COLORS", Prim_x_query_colors, 2, -1, 0)
{ /* Input: (Scheme) display, colormap, pixel ...
     Output: a vector of vectors, each with (red, green, blue)
  */

  PRIMITIVE_HEADER(LEXPR);
  { long npixels = (LEXPR_N_ARGUMENTS()) - 2;
    XColor * First_Color = (XColor *) Free;
    long i;
    XColor *This_Color;
    SCHEME_OBJECT Result, *Next_Result;

    Primitive_GC_If_Needed(npixels * (BYTES_TO_WORDS(sizeof(XColor))));
    for (i=0, This_Color=First_Color; i < npixels; i++, This_Color++)
    { This_Color->pixel = integer_to_long(ARG_REF(i+3));
    }
    Free = (SCHEME_OBJECT *) This_Color;
    XQueryColors(XD_DISPLAY(x_display_arg(1)), arg_integer(2),
		 First_Color, npixels);
    Result = allocate_marked_vector(TC_VECTOR, npixels, true);
    for (i=0, This_Color=First_Color, Next_Result=VECTOR_LOC(Result, 0);
	 i < npixels; i++, This_Color++)
    { SCHEME_OBJECT This_Vector = allocate_marked_vector(TC_VECTOR, 3, true);
      *Next_Result++ = This_Vector;
      VECTOR_SET(This_Vector, 0, long_to_integer(This_Color->red));
      VECTOR_SET(This_Vector, 1, long_to_integer(This_Color->green));
      VECTOR_SET(This_Vector, 2, long_to_integer(This_Color->blue));
    }
    PRIMITIVE_RETURN(Result);
  }
}
