/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosinsn.h,v 1.1 1992/05/05 06:55:13 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

#ifndef _DOSINSN_H_
#  define _DOSINSN_H_

extern unsigned short getCS (void);
extern unsigned short getDS (void);
extern unsigned short getSS (void);
extern void farcpy (unsigned dst_off, unsigned dst_sel,
		    unsigned src_off, unsigned src_sel,
		    unsigned size);

#define TRAMP_SIZE(npush)	(4 * ((3 + (7 + (5 * npush))) / 4))

#define INSN_DECLS()					\
  unsigned short getCS (void);				\
  unsigned char * startptr;				\
  unsigned char * byteptr;				\
  unsigned short * wordptr;				\
  unsigned long * dwordptr

#define INIT_INSNS(store)				\
do {							\
  startptr = ((unsigned char *) (store));		\
  byteptr = startptr;					\
} while (0)

#define PUSH_INSN(value)				\
do {							\
  *byteptr++ = 0x68;					\
  dwordptr = ((unsigned long *) byteptr);		\
  *dwordptr++ = ((unsigned long) (value));		\
  byteptr = ((unsigned char *) dwordptr);		\
} while (0)

#define JMP_INSN(value)					\
do {							\
  *byteptr++ = 0xea;					\
  dwordptr = ((unsigned long *) byteptr);		\
  *dwordptr++ = ((unsigned long) (value));		\
  wordptr = ((unsigned short *) dwordptr);		\
  *wordptr++ = (getCS ());				\
  byteptr = ((unsigned char *) wordptr);		\
} while (0)

#define FRET_INSN()					\
do {							\
  *byteptr++ = 0xcb;					\
} while (0)

/* pad with HLT to end (on dword bdry.) */

#define HLT_INSNS(npush)				\
do {							\
  unsigned char * endptr =				\
    (startptr + (TRAMP_SIZE (npush)));			\
  while (byteptr < endptr)				\
    *byteptr++ = 0xf4;					\
} while (0)

#endif /* _DOSINSN_H_ */
