/* -*-C-*-

$Id: os2cthrd.h,v 1.5 1996/05/09 20:21:30 cph Exp $

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

#ifndef SCM_OS2CTHRD_H
#define SCM_OS2CTHRD_H

#ifndef SM_READAHEAD_MAX
#define SM_READAHEAD_MAX 4096
#endif

typedef struct
{
  TID tid;
  qid_t reader_qid;
  qid_t writer_qid;
  unsigned int eofp : 1;
  unsigned int first_read_p : 1;
} channel_context_t;
#define CHANNEL_CONTEXT_TID(c) ((c) -> tid)
#define CHANNEL_CONTEXT_READER_QID(c) ((c) -> reader_qid)
#define CHANNEL_CONTEXT_WRITER_QID(c) ((c) -> writer_qid)
#define CHANNEL_CONTEXT_EOFP(c) ((c) -> eofp)
#define CHANNEL_CONTEXT_FIRST_READ_P(c) ((c) -> first_read_p)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  unsigned short size;
  unsigned short index;
  char data [SM_READAHEAD_MAX];
} sm_readahead_t;
#define SM_READAHEAD_SIZE(m) (((sm_readahead_t *) (m)) -> size)
#define SM_READAHEAD_INDEX(m) (((sm_readahead_t *) (m)) -> index)
#define SM_READAHEAD_DATA(m) (((sm_readahead_t *) (m)) -> data)

enum readahead_ack_action { raa_read, raa_close };

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  enum readahead_ack_action action;
} sm_readahead_ack_t;
#define SM_READAHEAD_ACK_ACTION(m) (((sm_readahead_ack_t *) (m)) -> action)

typedef msg_t * (* channel_reader_t) (LHANDLE, qid_t, msg_t *, int *);

extern void OS2_start_channel_thread
  (Tchannel, channel_reader_t, channel_op_t);
extern void OS2_channel_thread_read_op
  (Tchannel, choparg_t, choparg_t, choparg_t);

extern channel_context_t * OS2_make_channel_context (void);
extern long OS2_channel_thread_read (Tchannel, char *, size_t);
extern enum readahead_ack_action OS2_wait_for_readahead_ack (qid_t);
extern void OS2_channel_thread_close (Tchannel);

typedef struct
{
  msg_list_t * head;
  msg_list_t * tail;
} readahead_buffer_t;

extern readahead_buffer_t * OS2_make_readahead_buffer (void);
extern int OS2_readahead_buffer_emptyp (readahead_buffer_t *);
extern void OS2_readahead_buffer_insert (readahead_buffer_t *, char);
extern char OS2_readahead_buffer_rubout (readahead_buffer_t *);
extern msg_t * OS2_make_readahead (void);
extern msg_t * OS2_readahead_buffer_read (readahead_buffer_t *);
extern msg_list_t * OS2_readahead_buffer_read_all (readahead_buffer_t *);

#endif /* SCM_OS2CTHRD_H */
