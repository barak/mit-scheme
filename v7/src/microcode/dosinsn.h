/* -*-C-*-

$Id: dosinsn.h,v 1.3 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

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

#ifndef _DOSINSN_H_
#  define _DOSINSN_H_

#ifdef getDS
#undef getDS
#endif

#ifdef getCS
#undef getCS
#endif

#ifdef getSS
#undef getSS
#endif

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
