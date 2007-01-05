/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* $Id: gpio.c,v 1.15 2007/01/05 21:19:25 cph Exp $ */

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
    long scr = (read (gpio_channel, data, count));
    if (scr < 0)
    {
      UX_prim_check_errno (syscall_read);
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
    long scr = (write (gpio_channel, data, count));
    if (scr < 0)
    {
      UX_prim_check_errno (syscall_write);
      continue;
    }
    if (scr > count)
      error_external_return ();
    PRIMITIVE_RETURN( LONG_TO_FIXNUM (scr));
  }
}
