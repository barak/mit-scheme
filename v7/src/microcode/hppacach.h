/* -*-C-*-

$Id: hppacach.h,v 1.4 1993/02/06 05:34:46 gjr Exp $

Copyright (c) 1990-1993 Massachusetts Institute of Technology

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

#ifndef HPPACACHE_H		/* Prevent multiple inclusion */
#define HPPACACHE_H

#define I_CACHE		1
#define D_CACHE		2

#include <fcntl.h>

#ifdef _HPUX
#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/param.h>
#include <machine/cpu.h>
#include <machine/pdc_rqsts.h>
#endif /* _HPUX */

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

#ifdef _HPUX

#  define HARDWARE_SIZE sizeof (utsname.machine)

#else /* not _HPUX */
/* Presumably BSD */

#  define HARDWARE_SIZE 9

struct pdc_cache_rtn_block
{
  struct pdc_cache_result goodies;
  int filler[2];
};

#endif /* _HPUX */

struct pdc_cache_dump
{
  char hardware[HARDWARE_SIZE];
  struct pdc_cache_rtn_block cache_format;
};

#endif /* HPPACACHE_H */
