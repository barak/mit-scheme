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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/gpio.c,v 1.1 1990/06/12 16:35:28 jinx Exp $ */

/* Scheme primitives for GPIO */

#include "scheme.h"
#include "prims.h"

#include <stdio.h>
#include <fcntl.h> 
#include <dvio.h>


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
  unsigned char buffer[4];
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

DEFINE_PRIMITIVE ("CLOSE-GPIO", Prim_close_gpio, 1, 1, 0)
{
  int gpio_channel;

  PRIMITIVE_HEADER (1);

  gpio_channel = (arg_integer (1));
  close ( gpio_channel );
  
  PRIMITIVE_RETURN( long_to_integer( gpio_channel ));
}
