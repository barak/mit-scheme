/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of an x11 plugin for MIT/GNU Scheme.

This plugin is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This plugin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this plugin; if not, write to the Free Software Foundation,
Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

*/

/* Functions for dealing with colors and color maps */

#include "x11.h"

/* Visuals */

struct xvisual *
x_window_visual (struct xwindow * xw)
{
  XWindowAttributes a;
  if (! (XGetWindowAttributes ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&a))))
    return (NULL);
  return (allocate_x_visual (a . visual));
}

void
x_get_visual_info (struct xdisplay * xd,
		   long mask,
		   XVisualInfo * info,
		   XVisualInfo * * items_return,
		   int * nitems_return)
{
  Display * dpy = (XD_DISPLAY(xd));
  *items_return = XGetVisualInfo(dpy, mask, info, nitems_return);
}

/* Colormaps */

struct xcolormap *
x_window_colormap (struct xwindow * xw)
{
  XWindowAttributes a;
  if (! (XGetWindowAttributes ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&a))))
    return (NULL);
  return (allocate_x_colormap ((a . colormap), (XW_XD (xw))));
}

void
x_set_window_colormap (struct xwindow * xw, struct xcolormap * xcm)
{
  XSetWindowColormap ((XW_DISPLAY (xw)), (XW_WINDOW (xw)),
		      (XCM_COLORMAP (xcm)));
}

struct xcolormap *
x_create_colormap (struct xwindow * xw, struct xvisual * visual,
		   int writable_p)
{
  return (allocate_x_colormap
	  ((XCreateColormap ((XW_DISPLAY (xw)), (XW_WINDOW (xw)),
			     (XV_VISUAL (visual)), writable_p)),
	   (XW_XD (xw))));
}

void
x_free_colormap (struct xcolormap * xcm)
{
  XFreeColormap ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)));
  deallocate_x_colormap (xcm);
}

long
x_allocate_color (struct xcolormap * xcm,
		  unsigned int red, unsigned int green, unsigned int blue)
{
    XColor c;
    if ((red >= 65536)
	|| (green >= 65536)
	|| (blue >= 65536))
      return (-1);
    (c . red) = red;
    (c . green) = green;
    (c . blue) = blue;
    return ((XAllocColor ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)), (&c)))
	    ? (c . pixel)
	    : -1);
}

void
x_store_color (struct xcolormap * xcm,
	       int pixel, int red, int green, int blue)
{
  XColor c;
  (c . pixel) = pixel;
  (c . flags) = 0;
  if (red != -1)
    {
      (c . red) = red;
      (c . flags) |= DoRed;
    }
  if (green != -1)
    {
      (c . green) = green;
      (c . flags) |= DoGreen;
    }
  if (blue != -1)
    {
      (c . blue) = blue;
      (c . flags) |= DoBlue;
    }
  XStoreColor ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)), (&c));
}

void
x_store_colors (struct xcolormap * xcm,
		unsigned int * color_vector,
		unsigned int n_colors)
{
  XColor * colors = malloc (n_colors * (sizeof (XColor)));
  unsigned int * vector_scan = color_vector;
  XColor * colors_scan = colors;
  XColor * colors_end = (colors + n_colors);
  while (colors_scan < colors_end)
    {
      (colors_scan -> pixel) = (*vector_scan++);
      (colors_scan -> flags) = 0;
      {
	int red = *vector_scan++;
	int green = *vector_scan++;
	int blue = *vector_scan++;
	if (red != -1)
	  {
	    (colors_scan -> red) = red;
	    (colors_scan -> flags) |= DoRed;
	  }
	if (green != -1)
	  {
	    (colors_scan -> green) = green;
	    (colors_scan -> flags) |= DoGreen;
	  }
	if (blue != -1)
	  {
	    (colors_scan -> blue) = blue;
	    (colors_scan -> flags) |= DoBlue;
	  }
	colors_scan += 1;
      }
    }
  XStoreColors ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)), colors, n_colors);
  free (colors);
}

void
x_query_color (struct xcolormap * xcm,
	       unsigned long pixel,
	       unsigned int * results)
{
  XColor c;
  c . pixel = pixel;
  XQueryColor ((XCM_DISPLAY (xcm)), (XCM_COLORMAP (xcm)), (&c));
  results[0] = (c . red);
  results[1] = (c . green);
  results[2] = (c . blue);
}
