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

/* Header for x11-shim.c, x11-const.c and x11base.c et al. */

#include "x11.h"

/* x11base.c */

extern void x_close_display (struct xdisplay * xd);
extern void x_close_all_displays (void);
extern int x_window_set_input_hint (struct xwindow * xw, int input_hint);
extern int x_window_set_name (struct xwindow * xw, const char * name);
extern int x_window_set_icon_name (struct xwindow * xw, const char * name);
extern int x_event_delete_window_p (struct xwindow * xw, XEvent * event);
extern int x_event_take_focus_p (struct xwindow * xw, XEvent * event);
extern Time x_event_take_focus_time (XEvent * event);
extern int x_lookup_string (XKeyEvent * event,
			    char *buffer_return, int bytes_buffer,
			    KeySym * keysym_return);
extern unsigned long x_modifier_mask_to_bucky_bits (unsigned int mask,
						    struct xwindow * xw);
extern struct xdisplay * x_open_display (char * display_name);
extern void x_display_get_size (struct xdisplay * xd, long screen,
				int * results);
extern int x_set_default_font (struct xdisplay * xd, const char * name);
extern int x_display_descriptor (struct xdisplay * xd);
extern long x_max_request_size (struct xdisplay * xd);
extern int x_display_process_events (struct xdisplay * xd,
				     XEvent * event,
				     struct xwindow ** xw_ret);
extern void x_select_input (struct xdisplay * xd, Window window, long mask);
extern long x_window_event_mask (struct xwindow * xw);
extern int x_window_set_event_mask (struct xwindow * xw, long mask);
extern void x_window_or_event_mask (struct xwindow * xw, long mask);
extern void x_window_andc_event_mask (struct xwindow * xw, long mask);
extern struct xdisplay * x_window_display (struct xwindow * xw);
extern long x_window_screen_number (struct xwindow * xw);
extern int x_window_x_size (struct xwindow * xw);
extern int x_window_y_size (struct xwindow * xw);
extern void x_window_beep (struct xwindow * xw);
extern void x_window_clear (struct xwindow * xw);
extern void x_display_flush (struct xdisplay * xd);
extern void x_window_flush (struct xwindow * xw);
extern void x_display_sync (struct xdisplay * xd, Bool discard);
extern char * x_display_get_default (struct xdisplay * xd,
				     char * resource_name,
				     char * class_name);
extern int x_window_query_pointer (struct xwindow * xw, int * result);
extern unsigned long x_window_id (struct xwindow * xw);
extern void x_window_set_foreground_color_pixel (struct xwindow * xw,
						 unsigned long pixel);
extern void x_window_set_foreground_color_name (struct xwindow * xw,
						char * color);
extern int x_window_set_background_color_pixel (struct xwindow * xw,
						unsigned long pixel);
extern void x_window_set_background_color_name (struct xwindow * xw,
						char * color);
extern void x_window_set_border_color_pixel (struct xwindow * xw,
					     unsigned long pixel);
extern void x_window_set_border_color_name (struct xwindow * xw, char * color);
extern void x_window_set_cursor_color_pixel (struct xwindow * xw,
					     unsigned long pixel);
extern void x_window_set_cursor_color_name (struct xwindow * xw, char * color);
extern int x_window_set_mouse_color_pixel (struct xwindow * xw,
					   unsigned long pixel);
extern void x_window_set_mouse_color_name (struct xwindow * xw, char * color);
extern int x_window_set_mouse_shape (struct xwindow * xw, int shape);
extern int x_window_set_font (struct xwindow * xw, char * font_name);
extern void x_window_set_border_width (struct xwindow * xw, uint border_width);
extern void x_window_set_internal_border_width (struct xwindow * xw,
						uint internal_border_width);
extern int x_window_set_input_focus (struct xwindow * xw, Time time);
extern void x_window_map (struct xwindow * xw);
extern void x_window_iconify (struct xwindow * xw);
extern void x_window_withdraw (struct xwindow * xw);
extern void x_window_set_size (struct xwindow * xw, int width, int height);
extern void x_window_raise (struct xwindow * xw);
extern void x_window_lower (struct xwindow * xw);
extern void x_window_get_size (struct xwindow * xw, int * dimens);
extern void x_window_get_position (struct xwindow * xw, int * coord_return);
extern void x_window_set_position (struct xwindow * xw, int x, int y);
extern XFontStruct * x_font_structure_by_name (struct xdisplay * xd,
					       const char * font_name);
extern XFontStruct * x_font_structure_by_id (struct xdisplay * xd, XID id);
extern void x_free_font (struct xdisplay * xd, XFontStruct *font);
extern char * * x_list_fonts (struct xdisplay * xd,
			      char * pattern, long limit, int * actual_count);
extern Atom x_intern_atom (struct xdisplay * xd, const char * name, int soft_p);
extern int x_get_atom_name (struct xdisplay * xd, Atom atom,
			    char * * name_return);
extern int x_get_window_property (struct xdisplay * xd,
				  Window window, Atom property,
				  long long_offset, long long_length,
				  Bool delete, Atom req_type,
				  Atom * actual_type_return,
				  int * actual_format_return,
				  unsigned long * nitems_return,
				  unsigned long * bytes_after_return,
				  unsigned char * * prop_return);
extern int x_change_property (struct xdisplay * wd,
			      Window window, Atom property,
			      Atom type, int format, int mode,
			      unsigned char * data, unsigned long dlen);
extern void x_delete_property (struct xdisplay * xd,
			       Window window, Atom property);
extern void x_set_selection_owner (struct xdisplay * xd,
				   Atom selection, Window owner, Time time);
extern Window x_get_selection_owner (struct xdisplay * xd, Atom selection);
extern void x_convert_selection (struct xdisplay * xd,
				 Atom selection, Atom target,
				 Atom property, Window requestor, Time time);
extern void x_send_selection_notify (struct xdisplay * xd,
				     Window requestor,
				     Atom selection, Atom target,
				     Atom property, Time time);

/* x11color.c */

extern struct xvisual * x_window_visual (struct xwindow * xw);
extern void x_get_visual_info (struct xdisplay * xd,
			       long mask, XVisualInfo * info,
			       XVisualInfo * * items_return,
			       int * nitems_return);
extern struct xcolormap * x_window_colormap (struct xwindow * xw);
extern void x_set_window_colormap (struct xwindow * xw, struct xcolormap * xcm);
extern struct xcolormap * x_create_colormap (struct xwindow * xw,
					     struct xvisual * visual,
					     int writable_p);
extern void x_free_colormap (struct xcolormap * xcm);
extern long x_allocate_color (struct xcolormap * xcm, unsigned int red,
			      unsigned int green, unsigned int blue);
extern void x_store_color (struct xcolormap * xcm,
			   int pixel, int red, int green, int blue);
extern void x_store_colors (struct xcolormap * xcm, int * color_vector,
			    unsigned long n_colors);
extern void x_query_color (struct xcolormap * xcm,
			   unsigned long pixel,
			   unsigned int * results);

/* x11graph.c */

extern void x_graphics_set_vdc_extent (struct xwindow * xw,
				       float x_left, float y_bottom,
				       float x_right, float y_top);
extern void x_graphics_vdc_extent (struct xwindow * xw, float * results);
extern void x_graphics_reset_clip_rectangle (struct xwindow * xw);
extern void x_graphics_set_clip_rectangle (struct xwindow * xw,
					   float x_left, float y_bottom,
					   float x_right, float y_top);
extern void x_graphics_reconfigure (struct xwindow * xw,
				    unsigned int width, unsigned int height);
extern struct xwindow * x_graphics_open_window (struct xdisplay * xd,
						char * geometry,
						const char * resource_name,
						const char * resource_class,
						int map_p);
extern void x_graphics_draw_line (struct xwindow * xw,
				  float x_start, float y_start,
				  float x_end, float y_end);
extern void x_graphics_move_cursor (struct xwindow * xw, float x, float y);
extern void x_graphics_drag_cursor (struct xwindow * xw, float x, float y);
extern void x_graphics_draw_point (struct xwindow * xw, float x, float y);
extern void x_graphics_draw_arc (struct xwindow * xw,
				 float virtual_device_x, float virtual_device_y,
				 float radius_x, float radius_y,
				 float angle_start, float angle_sweep,
				 int fill_p);
extern void x_graphics_draw_string (struct xwindow * xw,
				    float x, float y, char * string);
extern void x_graphics_draw_image_string (struct xwindow * xw,
					  float x, float y, char * string);
extern int x_graphics_set_function (struct xwindow * xw, unsigned int function);
extern void x_graphics_draw_points (struct xwindow * xw,
				    double * x_vector, double * y_vector,
				    unsigned int n_points, XPoint * points);
extern void x_graphics_draw_lines (struct xwindow * xw,
				   double * x_vector, double * y_vector,
				   unsigned int n_points, XPoint * points);
extern int x_graphics_set_fill_style (struct xwindow * xw,
				      unsigned int fill_style);
extern int x_graphics_set_line_style (struct xwindow * xw, unsigned int style);
extern int x_graphics_set_dashes (struct xwindow * xw, int dash_offset,
				  char * dash_list, int dash_list_length);
extern int x_graphics_copy_area (struct xwindow * source_xw,
				 struct xwindow * destination_xw,
				 int source_x, int source_y,
				 int width, int height,
				 int dest_x, int dest_y);
extern void x_graphics_fill_polygon (struct xwindow * xw,
				     double * vector, unsigned int length,
				     XPoint * points);
extern struct ximage * x_create_image (struct xwindow * xw,
				       uint width, uint height);
extern int x_bytes_into_image (unsigned char * vector, int length,
			       struct ximage * ximage);
extern long x_get_pixel_from_image (struct ximage * xi, int x, int y);
extern int x_set_pixel_in_image (struct ximage * xi,
				 int x, int y, unsigned long pixel);
extern void x_destroy_image (struct ximage * xi);
extern int x_display_image (struct ximage * xi,
			    unsigned int x_offset, unsigned int y_offset,
			    struct xwindow * xw,
			    unsigned int window_xoff, unsigned int window_yoff,
			    unsigned int width, unsigned int height);
extern void x_read_image (struct ximage * xi,
			  long XImageOffset, long YImageOffset,
			  struct xwindow * xw,
			  long XWindowOffset, long YWindowOffset,
			  long Width, long Height);
extern int x_window_depth (struct xwindow * xw);
extern float x_graphics_map_x_coordinate (struct xwindow * xw, int signed_xp);
extern float x_graphics_map_y_coordinate (struct xwindow * xw, int signed_yp);

/* x11term.c */

extern void xterm_erase_cursor (struct xwindow * xw);
extern void xterm_draw_cursor (struct xwindow * xw);
extern void xterm_dump_rectangle (struct xwindow * xw,
				  int signed_x, int signed_y,
				  unsigned int width, unsigned int height);
extern void xterm_reconfigure (struct xwindow * xw,
			       unsigned int x_csize, unsigned int y_csize);
extern long xterm_map_x_coordinate (struct xwindow * xw, int signed_xp);
extern long xterm_map_y_coordinate (struct xwindow * xw, int signed_yp);
extern unsigned long xterm_map_x_size (struct xwindow * xw,
				       unsigned int width);
extern unsigned long xterm_map_y_size (struct xwindow * xw,
				       unsigned int height);
extern struct xwindow * xterm_open_window (struct xdisplay * xd,
					   char * geometry,
					   const char * resource_name,
					   const char * resource_class,
					   int map_p);
extern unsigned int xterm_x_size (struct xwindow * xw);
extern unsigned int xterm_y_size (struct xwindow * xw);
extern void xterm_set_size (struct xwindow * xw,
			    unsigned int width, unsigned int height);
extern void xterm_enable_cursor (struct xwindow * xw, int enable_p);
extern int xterm_write_cursor (struct xwindow * xw,
			       unsigned int x, unsigned int y);
extern int xterm_write_char (struct xwindow * xw,
			     unsigned int x, unsigned int y,
			     int c, unsigned int hl);
extern int xterm_write_substring (struct xwindow * xw,
				  unsigned int x, unsigned int y,
				  unsigned char * string, unsigned int start,
				  unsigned int end, unsigned int hl);
extern int xterm_clear_rectangle (struct xwindow * xw,
				  unsigned int x_start, unsigned int x_end,
				  unsigned int y_start, unsigned int y_end,
				  unsigned int hl);
extern int xterm_scroll_lines_up (struct xwindow * xw,
				  unsigned int x_start,
				  unsigned int x_end,
				  unsigned int y_start,
				  unsigned int y_end,
				  unsigned int lines);
extern int xterm_scroll_lines_down (struct xwindow * xw,
				    unsigned int x_start,
				    unsigned int x_end,
				    unsigned int y_start,
				    unsigned int y_end,
				    unsigned int lines);
extern int xterm_save_contents (struct xwindow * xw,
				unsigned int x_start,
				unsigned int x_end,
				unsigned int y_start,
				unsigned int y_end,
				char * contents);
extern int xterm_restore_contents (struct xwindow * xw,
				   unsigned int x_start,
				   unsigned int x_end,
				   unsigned int y_start,
				   unsigned int y_end,
				   char * contents);
