/* -*-C-*-

$Id: hppacach.h,v 1.7 2002/11/20 19:46:09 cph Exp $

Copyright (c) 1990-2000 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

#ifndef HPPACACHE_H		/* Prevent multiple inclusion */
#define HPPACACHE_H

#define I_CACHE		1
#define D_CACHE		2

#include <fcntl.h>

#ifdef __HPUX__
#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/param.h>
#include <machine/cpu.h>
#include <machine/pdc_rqsts.h>
#endif /* __HPUX__ */

/* PDC_CACHE (processor dependent code cache information call)
   return data destructuring.

   This is the same information as in machine/pdc_rqsts.h
   pdc_cache_rtn_block, but with fields as defined in the PDC_CACHE
   section of the I/O Architecture Manual.

   The main difference is the cache configuration field.
   Which is correct?  */

union cache_conf
{
  unsigned long word;
  struct
  {
    unsigned block:	8;
    unsigned line:	3;
    unsigned res1:	2;
    unsigned wt:	1;
    unsigned fsel:	2;
    unsigned cst:	3;
    unsigned res2:	11;
    unsigned hv:	2;
  } bits;
};

struct cache_info
{
  unsigned long size;		/* in bytes */
  union cache_conf conf;	/* format description */
  unsigned long base;		/* start address */
  unsigned long stride;		/* in bytes */
  unsigned long count;		/* number of entries */
  unsigned long loop;		/* set associativity */
};

union tlb_conf
{
  unsigned long word;
  struct
  {
    unsigned res1:	12;
    unsigned psel:	2;
    unsigned hv1:	1;
    unsigned res2:	1;
    unsigned cst:	3;
    unsigned res3:	11;
    unsigned hv2:	2;
  } bits;
};

struct tlb_info
{
  unsigned long size;		/* number of entries */
  union tlb_conf conf;		/* format description */
  unsigned long sp_base;	/* space parameters */
  unsigned long sp_stride;
  unsigned long sp_count;
  unsigned long off_base;	/* offset parameters */
  unsigned long off_stride;
  unsigned long off_count;
  unsigned long loop;		/* set associativity */
};

struct pdc_cache_result
{
  struct cache_info I_info;
  struct cache_info D_info;
  struct tlb_info IT_info;
  struct tlb_info DT_info;
};

#ifdef __HPUX__

#  define HARDWARE_SIZE sizeof (utsname.machine)

#else /* not __HPUX__ */
/* Presumably BSD */

#  define HARDWARE_SIZE 9

struct pdc_cache_rtn_block
{
  struct pdc_cache_result goodies;
  int filler[2];
};

#endif /* __HPUX__ */

struct pdc_cache_dump
{
  char hardware[HARDWARE_SIZE];
  struct pdc_cache_rtn_block cache_format;
};

#endif /* HPPACACHE_H */
