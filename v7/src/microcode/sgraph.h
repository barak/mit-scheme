/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/sgraph.h,v 1.2 1988/03/11 02:54:59 arthur Rel $ */

#include <starbase.c.h>

/* Defaults (300h color display) can be overriden in the Makefile. */

#ifndef STARBASE_DRIVER_NAME

#define STARBASE_DRIVER_NAME "hp300h"

#define STARBASE_COLOR_TABLE_START 		0
#define STARBASE_COLOR_TABLE_SIZE 		16

/* Screen Boundaries */

#define STARBASE_XMIN		-512.0
#define STARBASE_YMIN		-384.0
#define STARBASE_ZMIN		   0.0
#define STARBASE_XMAX		 511.0
#define STARBASE_YMAX		 383.0
#define STARBASE_ZMAX		   0.0

#endif

/* Bobcat graphics primitives. Interface to the Starbase package*/

#define SINGLE_ECHO             0
#define NO_ECHO                 0
#define SMALL_TRACKING_CROSS    3
#define RUBBER_BAND_LINE        4
#define RUBBER_BAND_RECTANGLE   5
#define MAX_NUMBER_OF_CORNERS   512
#define TWICE_MAX_NUMBER_OF_CORNERS  (2 * MAX_NUMBER_OF_CORNERS)

#define DEFAULT_REPLACEMENT_RULE 3

extern int screen_handle;
extern int locator_handle;     /* mouse, ignored if not present */
extern long replacement_rule;
extern float xposition, yposition;
extern float Color_Table[STARBASE_COLOR_TABLE_SIZE][3];

/* Generic dispatch of coordinates. No BIGNUM support yet. */

#define Make_Flonum( pointer, flonum, integer, error)		\
{								\
  switch( Type_Code( pointer))					\
    {								\
    case TC_FIXNUM:						\
      Sign_Extend( pointer, integer);				\
      flonum = ((float) integer);				\
      break;							\
    case TC_BIG_FLONUM:						\
      flonum = (float) Get_Float( pointer);			\
      break;							\
    default:							\
      Primitive_Error( error);					\
    }								\
}
