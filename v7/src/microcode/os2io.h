/* -*-C-*-

$Id: os2io.h,v 1.6 2003/02/14 18:28:22 cph Exp $

Copyright (c) 1994-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#ifndef SCM_OS2IO_H
#define SCM_OS2IO_H

#include "osio.h"

typedef enum
{
  chop_close,
  chop_read,
  chop_write,
  chop_input_buffered,
  chop_input_flush,
  chop_output_cooked,
  chop_output_flush,
  chop_output_drain
} chop_t;

typedef void * choparg_t;
typedef void (* channel_op_t)
     (Tchannel, chop_t, choparg_t, choparg_t, choparg_t);

struct channel
{
  LHANDLE handle;
  channel_op_t operator;
  void * operator_context;
  enum channel_type type;
  unsigned int open : 1;
  unsigned int internal : 1;
  unsigned int nonblocking : 1;
  unsigned int inputp : 1;
  unsigned int outputp : 1;
};

#define _CHANNEL(c) (OS2_channel_table [(c)])
#define CHANNEL_HANDLE(c) ((_CHANNEL (c)) . handle)
#define CHANNEL_OPERATOR(c) ((_CHANNEL (c)) . operator)
#define CHANNEL_OPERATOR_CONTEXT(c) ((_CHANNEL (c)) . operator_context)
#define CHANNEL_TYPE(c) ((_CHANNEL (c)) . type)
#define CHANNEL_OPEN(c) ((_CHANNEL (c)) . open)
#define CHANNEL_INTERNAL(c) ((_CHANNEL (c)) . internal)
#define CHANNEL_NONBLOCKING(c) ((_CHANNEL (c)) . nonblocking)
#define CHANNEL_INPUTP(c) ((_CHANNEL (c)) . inputp)
#define CHANNEL_OUTPUTP(c) ((_CHANNEL (c)) . outputp)

#define CHANNEL_ABSTRACT_P(c) ((CHANNEL_OPERATOR (c)) != 0)

#define channel_type_console channel_type_os2_console
#define channel_type_unnamed_pipe channel_type_os2_unnamed_pipe
#define channel_type_named_pipe channel_type_os2_named_pipe

/* Channel modes: */
#define CHANNEL_READ	1
#define CHANNEL_WRITE	2

extern struct channel * OS2_channel_table;
extern Tchannel * OS2_channel_pointer_table;
extern Tchannel OS2_make_channel (LHANDLE, unsigned int);
extern void OS2_initialize_channel
  (Tchannel, LHANDLE, unsigned int, enum channel_type);
extern Tchannel OS2_allocate_channel (void);
extern void OS2_channel_close_all_noerror (void);
extern void OS_channel_close_on_abort (Tchannel);
extern void OS2_handle_close_on_abort (LHANDLE);
extern void OS2_channel_operation
  (Tchannel, chop_t, choparg_t, choparg_t, choparg_t);

#endif /* SCM_OS2IO_H */
