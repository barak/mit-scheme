/* -*-C-*-

Copyright (c) 1990 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/gpio.c,v 1.7 1990/10/02 21:51:25 jinx Rel $ */

/* Scheme primitives for GPIO */

#include "scheme.h"
#include "prims.h"
#include "ux.h"
#include "uxio.h"

#include <stdio.h>
#include <fcntl.h> 
#include <dvio.h>

DEFINE_PRIMITIVE ("GPIO-OPEN", Prim_gpio_open, 1, 1, 0)
{
  int gpio_channel;

  PRIMITIVE_HEADER (1);

  gpio_channel = (open (STRING_ARG (1), (O_RDWR | O_NDELAY)));
  if (gpio_channel == -1)
  {
    error_external_return();
  }
  if (!(LONG_TO_FIXNUM_P( gpio_channel)))
  {
    /* This is a crock, but guarantees that we can assume fixnum
       in all the other primitives.
     */
    close (gpio_channel);
    error_external_return();
  }

  /* Reset interface */

  io_reset (gpio_channel);

  /* Timeout in 5 sec. */
  io_timeout_ctl (gpio_channel, 5000000);

  /* Guarantee exclusive access. */
  io_lock (gpio_channel);

#if 1
  /* Map into address space. */
  io_burst (gpio_channel, 1);
#endif

  /* Set data width to 16 bits. */
  io_width_ctl (gpio_channel, 16);
  
  PRIMITIVE_RETURN( LONG_TO_FIXNUM (gpio_channel));
}


DEFINE_PRIMITIVE ("GPIO-CLOSE", Prim_gpio_close, 1, 1, 0)
{
  int gpio_channel;

  PRIMITIVE_HEADER (1);

  gpio_channel = (UNSIGNED_FIXNUM_ARG (1));

#if 1
  io_burst (gpio_channel, 0);
#endif

  io_unlock (gpio_channel);
  close (gpio_channel);
  
  PRIMITIVE_RETURN( long_to_integer( gpio_channel ));
}


DEFINE_PRIMITIVE ("GPIO-READ-STATUS", Prim_gpio_read_status, 1, 1, 0)
{
  int gpio_channel;

  PRIMITIVE_HEADER (1);

  gpio_channel = (UNSIGNED_FIXNUM_ARG (1));
  
  PRIMITIVE_RETURN( LONG_TO_FIXNUM( gpio_get_status( gpio_channel)));
}


DEFINE_PRIMITIVE ("GPIO-WRITE-CONTROL", Prim_gpio_write_control, 2, 2, 0)
{
  int gpio_channel, control_value;

  PRIMITIVE_HEADER (2);

  gpio_channel = (UNSIGNED_FIXNUM_ARG (1));
  control_value = (UNSIGNED_FIXNUM_ARG (2));
  
  PRIMITIVE_RETURN( LONG_TO_FIXNUM( gpio_set_ctl( gpio_channel, control_value)));
}

/* Both of the following return the number of bytes transferred. */


DEFINE_PRIMITIVE ("GPIO-READ-STRING!", Prim_gpio_read_string, 4, 4, 0)
{
  int gpio_channel, count;
  char *data;

  PRIMITIVE_HEADER (4);

  gpio_channel = (UNSIGNED_FIXNUM_ARG (1));
  data = ((char *) (STRING_LOC ((ARG_REF (2)), (UNSIGNED_FIXNUM_ARG (3)))));
  count = (UNSIGNED_FIXNUM_ARG (4));

  while (1)
  {
    long scr;
    INTERRUPTABLE_EXTENT
      (scr, (read (gpio_channel, data, count)));
    if (scr < 0)
    {
      UX_prim_check_errno ("read");
      continue;
    }
    if (scr > count)
      error_external_return ();
    PRIMITIVE_RETURN( LONG_TO_FIXNUM (scr));
  }
}


DEFINE_PRIMITIVE ("GPIO-WRITE-STRING", Prim_gpio_write_string, 4, 4, 0)
{
  int gpio_channel, count;
  char *data;

  PRIMITIVE_HEADER (4);

  gpio_channel = (UNSIGNED_FIXNUM_ARG (1));
  data = ((char *) (STRING_LOC ((ARG_REF (2)), (UNSIGNED_FIXNUM_ARG (3)))));
  count = (UNSIGNED_FIXNUM_ARG (4));

  while (1)
  {
    long scr;
    INTERRUPTABLE_EXTENT
      (scr, (write (gpio_channel, data, count)));
    if (scr < 0)
    {
      UX_prim_check_errno ("write");
      continue;
    }
    if (scr > count)
      error_external_return ();
    PRIMITIVE_RETURN( LONG_TO_FIXNUM (scr));
  }
}
