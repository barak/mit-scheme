/* -*-C-*-

$Id: os2cthrd.h,v 1.1 1994/11/28 03:42:55 cph Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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
  qid_t reader_qid;
  qid_t writer_qid;
  msg_t * readahead;
  unsigned int readahead_index;
  char eofp;
} channel_context_t;
#define CHANNEL_CONTEXT_READER_QID(c) ((c) -> reader_qid)
#define CHANNEL_CONTEXT_WRITER_QID(c) ((c) -> writer_qid)
#define CHANNEL_CONTEXT_READAHEAD(c) ((c) -> readahead)
#define CHANNEL_CONTEXT_READAHEAD_INDEX(c) ((c) -> readahead_index)
#define CHANNEL_CONTEXT_EOFP(c) ((c) -> eofp)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  ULONG size;
  char data [SM_READAHEAD_MAX];
} sm_readahead_t;
#define SM_READAHEAD_SIZE(m) (((sm_readahead_t *) (m)) -> size)
#define SM_READAHEAD_DATA(m) (((sm_readahead_t *) (m)) -> data)

#define OS2_make_readahead() OS2_create_message (mt_readahead)
#define OS2_make_readahead_ack() OS2_create_message (mt_readahead_ack)

typedef msg_t sm_readahead_ack_t;

extern channel_context_t * OS2_make_channel_context (void);
extern long channel_thread_read (Tchannel, char *, size_t);
extern void OS2_wait_for_readahead_ack (qid_t);
extern void channel_thread_close (Tchannel);

#endif /* SCM_OS2CTHRD_H */
