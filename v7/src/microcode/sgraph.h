/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/sgraph.h,v 1.5 1989/09/20 23:05:07 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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
