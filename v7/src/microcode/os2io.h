/* -*-C-*-

$Id: os2io.h,v 1.3 1996/05/09 20:21:48 cph Exp $

Copyright (c) 1994-96 Massachusetts Institute of Technology

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
