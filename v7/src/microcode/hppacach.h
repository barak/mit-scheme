/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/hppacach.h,v 1.1 1990/08/08 20:20:56 jinx Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

#ifdef C_SCHEME
#include "paths.h"
#define CACHE_FILENAME_PATH SCHEME_SOURCES_PATH
#define MODELS_FILENAME "HPPAmodels"
#endif

/*
   Format strings used to determine the name of the file
   where to store (and examine) the cache description.

   The strings are used as in
   printf (CACHE_FILENAME, CACHE_FILENAME_PATH, hostname_string, model_string);

   The default value (below) would generate the file name
   /tmp/altdorf.cache
   when running on a machine named altdorf.

   To ignore a string parameter, use the %.0s format.
 */

#ifndef CACHE_FILENAME_PATH
#define CACHE_FILENAME_PATH "/tmp/"
#endif

#ifndef CACHE_FILENAME
#define CACHE_FILENAME "%s%s.cache"
#endif /* CACHE_FILENAME */

#ifndef MODELS_FILENAME
#define MODELS_FILENAME "%shppamodels"
#endif

#define I_CACHE		1
#define D_CACHE		2

#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/param.h>
#include <machine/cpu.h>
#include <machine/pdc_rqsts.h>
#include <fcntl.h>

/* PDC_CACHE (processor dependent code cache information call)
   return data destructuring.

   This is the same information as in machine/pdc_rqsts.h
   pdc_cache_rtn_block, but with fields as defined in the PDC_CACHE
   section of the I/O Architecture Manual.

   The main difference is the cache configuration field.
   Which is correct?
 */

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

struct pdc_cache_dump
{
  char hardware[sizeof (utsname.machine)];
  struct pdc_cache_rtn_block cache_format;
};

#endif /* HPPACACHE_H */
