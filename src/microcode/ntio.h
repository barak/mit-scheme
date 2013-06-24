/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

#ifndef SCM_NTIO_H
#define SCM_NTIO_H

#include "osio.h"

typedef long channel_op_read (Tchannel, void *, unsigned long);
typedef long channel_op_write (Tchannel, const void *, unsigned long);
typedef void channel_op_close (Tchannel, int);
typedef long channel_op_n_read (Tchannel);

typedef struct _channel_class_t
{
  enum channel_type type;
  channel_op_read * op_read;
  channel_op_write * op_write;
  channel_op_close * op_close;
  channel_op_n_read * op_n_read;
} channel_class_t;

#define CHANNEL_CLASS_TYPE(class) ((class) -> type)
#define CHANNEL_CLASS_OP_READ(class) ((class) -> op_read)
#define CHANNEL_CLASS_OP_WRITE(class) ((class) -> op_write)
#define CHANNEL_CLASS_OP_CLOSE(class) ((class) -> op_close)
#define CHANNEL_CLASS_OP_N_READ(class) ((class) -> op_n_read)

#define CHANNEL_N_READ_UNKNOWN (-2)
#define CHANNEL_N_READ_WOULD_BLOCK (-1)

struct channel
{
  channel_class_t * class;
  HANDLE handle;
  unsigned int internal : 1;
  unsigned int nonblocking : 1;
  unsigned int buffered : 1;
  unsigned int cooked : 1;
};

#define CHANNEL_CLASS(c) ((NT_channel_table[c]) . class)
#define CHANNEL_HANDLE(c) ((NT_channel_table[c]) . handle)
#define CHANNEL_INTERNAL(c) ((NT_channel_table[c]) . internal)
#define CHANNEL_NONBLOCKING(c) ((NT_channel_table[c]) . nonblocking)
#define CHANNEL_BUFFERED(c) ((NT_channel_table[c]) . buffered)
#define CHANNEL_COOKED(c) ((NT_channel_table[c]) . cooked)

#define CHANNEL_TYPE(channel) (CHANNEL_CLASS_TYPE (CHANNEL_CLASS (channel)))
#define MARK_CHANNEL_CLOSED(channel)					\
  ((CHANNEL_HANDLE (channel)) = INVALID_HANDLE_VALUE)
#define CHANNEL_CLOSED_P(channel)					\
  ((CHANNEL_HANDLE (channel)) == INVALID_HANDLE_VALUE)
#define CHANNEL_OPEN_P(channel)						\
  ((CHANNEL_HANDLE (channel)) != INVALID_HANDLE_VALUE)
#define CHANNEL_BLOCKING_P(channel) (!CHANNEL_NONBLOCKING (channel))

extern channel_class_t * NT_channel_class_generic;
extern channel_class_t * NT_channel_class_file;
extern channel_class_t * NT_channel_class_screen;
extern channel_class_t * NT_channel_class_console;
extern channel_class_t * NT_channel_class_anonymous_pipe;
extern channel_class_t * NT_channel_class_named_pipe;
extern struct channel * NT_channel_table;

extern Tchannel NT_make_channel (HANDLE, channel_class_t *);
extern channel_class_t * NT_handle_channel_class (HANDLE);
extern Tchannel NT_open_handle (HANDLE);
extern void NT_handle_close_on_abort (HANDLE);
extern long NT_channel_n_read (Tchannel);

#define BACKSPACE		'\b'
#define SPACE			' '
#define CARRIAGE_RETURN		'\r'
#define LINEFEED		'\n'
#define CNTRL_Z			'\032'
#define ASCII_DELETE		'\177'

extern BOOL Screen_IsScreenHandle (HANDLE);

#ifndef GUI
#  define CONSOLE_HANDLE (STDIN_HANDLE)
#  define IsConsoleHandle(h)  ((h) == CONSOLE_HANDLE)
#else
#  define IsConsoleHandle(h)  (0 == 1)
#endif

#endif /* SCM_NTIO_H */
