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

#define OS2_make_readahead_buffer OS2_create_msg_fifo
#define OS2_readahead_buffer_emptyp OS2_msg_fifo_emptyp

extern void OS2_readahead_buffer_insert (void *, char);
extern char OS2_readahead_buffer_rubout (void *);
extern msg_t * OS2_make_readahead (void);
extern msg_t * OS2_readahead_buffer_read (void *);

#define OS2_readahead_buffer_read_all(b)				\
  ((msg_t **) (OS2_msg_fifo_remove_all (b)))

#endif /* SCM_OS2CTHRD_H */
