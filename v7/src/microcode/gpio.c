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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/gpio.c,v 1.2 1990/06/13 20:49:58 jinx Exp $ */

/* Scheme primitives for GPIO */

#include "scheme.h"
#include "prims.h"

#include <stdio.h>
#include <fcntl.h> 
#include <dvio.h>

/* "Old style" primitives.
   Should be flushed.
 */

DEFINE_PRIMITIVE ("OPEN-GPIO", Prim_open_gpio, 1, 1, 0)
{
  int gpio_channel;

  PRIMITIVE_HEADER (1);

  gpio_channel = open (STRING_ARG (1), O_RDWR);
  if (gpio_channel == -1) error_external_return();    

  io_reset (gpio_channel);
  io_width_ctl (gpio_channel, 16);
  
  PRIMITIVE_RETURN( long_to_integer (gpio_channel));
}


DEFINE_PRIMITIVE ("CLOSE-GPIO", Prim_close_gpio, 1, 1, 0)
{
  int gpio_channel;

  PRIMITIVE_HEADER (1);

  gpio_channel = (arg_integer (1));
  close ( gpio_channel );
  
  PRIMITIVE_RETURN( long_to_integer( gpio_channel ));
}


DEFINE_PRIMITIVE ("READ-GPIO", Prim_read_gpio, 1, 1, 0)
{
  int gpio_channel;
  unsigned long result;
  unsigned char buffer[4];
  int xfer;

  PRIMITIVE_HEADER (1);

  gpio_channel = arg_integer(1);

  buffer[0] = ((unsigned char) 0);
  buffer[1] = ((unsigned char) 0);

  xfer = read (gpio_channel, &buffer[0], 2);

  if (xfer != 2) error_external_return();

  result = ((unsigned long) buffer[1]);
  result = (result + (((unsigned long) buffer[0]) << 8));

  PRIMITIVE_RETURN( long_to_integer (result));
}


DEFINE_PRIMITIVE ("WRITE-GPIO", Prim_write_gpio, 2, 2, 0)
{
  int gpio_channel;
  unsigned long output;
  unsigned char buffer[2];
  int xfer;

  PRIMITIVE_HEADER (2);

  gpio_channel = (arg_integer (1));
  output = (arg_integer (2));

  buffer[1] = (output & 0xff);
  buffer[0] = ((output >> 8) & 0xff);

  xfer = write (gpio_channel, &buffer[0], 2);

  /* xfer is 2 if successfull */
 
  PRIMITIVE_RETURN( long_to_integer( xfer ));
}

/* "New style" primitives. */


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
    error_external_return;
  }

  io_lock (gpio_channel);
  io_reset (gpio_channel);
  io_width_ctl (gpio_channel, 16);
  
  PRIMITIVE_RETURN( LONG_TO_FIXNUM (gpio_channel));
}


DEFINE_PRIMITIVE ("GPIO-CLOSE", Prim_gpio_close, 1, 1, 0)
{
  int gpio_channel;

  PRIMITIVE_HEADER (1);

  gpio_channel = (UNSIGNED_FIXNUM_ARG (1));

  io_unlock (gpio_channel);
  close ( gpio_channel );
  
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


DEFINE_PRIMITIVE ("GPIO-READ-STRING!", Prim_gpio_read_string, 3, 3, 0)
{
  int gpio_channel, count, result;
  char *data;

  PRIMITIVE_HEADER (3);

  gpio_channel = (UNSIGNED_FIXNUM_ARG (1));
  data = (STRING_ARG (2));
  count = (UNSIGNED_FIXNUM_ARG (3));

  result = (read (gpio_channel, &data[0], count));

  PRIMITIVE_RETURN( LONG_TO_FIXNUM (result));
}


DEFINE_PRIMITIVE ("GPIO-WRITE-STRING", Prim_gpio_write_string, 3, 3, 0)
{
  int gpio_channel, count, result;
  char *data;

  PRIMITIVE_HEADER (3);

  gpio_channel = (UNSIGNED_FIXNUM_ARG (1));
  data = (STRING_ARG (2));
  count = (UNSIGNED_FIXNUM_ARG (3));

  result = (write (gpio_channel, &data[0], count));

  PRIMITIVE_RETURN( LONG_TO_FIXNUM ( result ));
}

