/* -*-C-*-

$Id: ntio.h,v 1.10 1997/10/25 07:40:26 cph Exp $

Copyright (c) 1992-97 Massachusetts Institute of Technology

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

extern BOOL EXFUN (Screen_IsScreenHandle, (HANDLE));

#ifndef GUI
#  define CONSOLE_HANDLE (STDIN_HANDLE)
#  define IsConsoleHandle(h)  ((h) == CONSOLE_HANDLE)
#else
#  define IsConsoleHandle(h)  (0 == 1)
#endif

#endif /* SCM_NTIO_H */
