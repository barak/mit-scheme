/* -*-C-*-

$Id: sgraph.h,v 1.8 2002/11/20 19:46:14 cph Exp $

Copyright (c) 1988, 1989, 1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

#include <starbase.c.h>

/* Bobcat graphics primitives. Interface to the Starbase package*/

#define SINGLE_ECHO             0
#define NO_ECHO                 0
#define SMALL_TRACKING_CROSS    3
#define RUBBER_BAND_LINE        4
#define RUBBER_BAND_RECTANGLE   5
#define MAX_NUMBER_OF_CORNERS   512
#define TWICE_MAX_NUMBER_OF_CORNERS  (2 * MAX_NUMBER_OF_CORNERS)

extern int screen_handle;
extern long replacement_rule;
extern float xposition;
extern float yposition;

extern char * sb_device;
extern char * sb_driver;
extern float sb_xmin;
extern float sb_xmax;
extern float sb_ymin;
extern float sb_ymax;
extern float sb_zmin;
extern float sb_zmax;

extern void sb_close_device ();
