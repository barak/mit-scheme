/* -*-C-*-

$Id: dosio.h,v 1.5 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef SCM_DOSIO_H
#define SCM_DOSIO_H

#include "osio.h"

struct channel
{
  int descriptor;
  enum channel_type type;
  unsigned int internal : 1;
  unsigned int nonblocking : 1;
  unsigned int buffered : 1;
  unsigned int cooked : 1;
};

#define MARK_CHANNEL_CLOSED(channel) ((CHANNEL_DESCRIPTOR (channel)) = (-1))
#define CHANNEL_CLOSED_P(channel) ((CHANNEL_DESCRIPTOR (channel)) < 0)
#define CHANNEL_OPEN_P(channel) ((CHANNEL_DESCRIPTOR (channel)) >= 0)
#define CHANNEL_DESCRIPTOR(channel) ((channel_table [(channel)]) . descriptor)
#define CHANNEL_TYPE(channel) ((channel_table [(channel)]) . type)
#define CHANNEL_INTERNAL(channel) ((channel_table [(channel)]) . internal)
#define CHANNEL_NONBLOCKING(channel)					\
  ((channel_table [(channel)]) . nonblocking)
#define CHANNEL_BLOCKING_P(channel)					\
  (!CHANNEL_NONBLOCKING(channel))
#define CHANNEL_BUFFERED(channel) ((channel_table [(channel)]) . buffered)
#define CHANNEL_COOKED(channel) ((channel_table [(channel)]) . cooked)

#define MAKE_CHANNEL(descriptor, type, receiver)			\
{									\
  Tchannel MAKE_CHANNEL_temp = (channel_allocate ());			\
  (CHANNEL_DESCRIPTOR (MAKE_CHANNEL_temp)) = (descriptor);		\
  (CHANNEL_TYPE (MAKE_CHANNEL_temp)) = (type);				\
  (CHANNEL_INTERNAL (MAKE_CHANNEL_temp)) = 0;				\
  (CHANNEL_NONBLOCKING (MAKE_CHANNEL_temp)) = 0;			\
  (CHANNEL_BUFFERED (MAKE_CHANNEL_temp)) = 1;				\
  (CHANNEL_COOKED (MAKE_CHANNEL_temp)) = 0;				\
  receiver (MAKE_CHANNEL_temp);						\
}

extern struct channel * channel_table;
extern Tchannel EXFUN (channel_allocate, (void));

#define BACKSPACE		'\b'
#define SPACE			' '
#define CARRIAGE_RETURN		'\r'
#define LINEFEED		'\n'
#define CNTRL_Z			'\032'
#define DELETE			'\177'

#endif /* SCM_DOSIO_H */
