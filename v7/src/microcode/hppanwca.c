/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/hppanwca.c,v 1.1 1992/01/07 16:53:10 jinx Exp $

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

/* Program to convert ascii-format cache descriptions into the binary
   form used by Scheme.

   To use, replace the structure labeled "written_data" below with the
   new one (if not present in the data base), then recompile this program:
     cc -Aa -D_HPUX_SOURCE -O -o hppanewcache hppanewcache.c
   and then type
     ./hppanewcache >>HPPAmodels
 */

#include <stdio.h>
#include "hppacache.h"

struct pdc_cache_written
{
  char hardware[sizeof (utsname.machine)];
  struct pdc_cache_result cache_format;
};

static struct pdc_cache_written written_data =
{
  /* Cache description for amertume, an HP PA 9000/750 processor. */

  "9000/750",

  {
    /*
      I-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a read-only cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      D-cache information:
	size		262144 bytes (256 K).
	conf		0x01402000
	base		0x0
	stride		32 bytes.
	count		8192 entries.
	loop		1 association per entry.
	block size	1 line.
	line size	2 (16-byte units).
      It is a write-to cache.
      It issues coherent operations.
      Both FDC and FIC must be used to flush.
    */
    { 262144, 0x01402000, 0x0, 32, 8192, 1 },
    /*
      I-TLB information:
	size		96 entries (0 K).
	conf		0x00012000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00012000, 0x0, 0, 1, 0x0, 0, 1, 1 },
    /*
      D-TLB information:
	size		96 entries (0 K).
	conf		0x00002000
	sp_base		0x0
	sp_stride	0
	sp_count	1
	off_base	0x0
	off_stride	0
	off_count	1
	loop		1 association per entry.
      It issues coherent operations.
      Both PDTLB and PITLB must be used to purge.
    */
    { 96, 0x00002000, 0x0, 0, 1, 0x0, 0, 1, 1 }
  }};

#define INTMIN(x,y) (((y) > (x)) ? (y) : (x))

main ()
{
  struct pdc_cache_dump data_to_dump;

  memcpy (&data_to_dump.hardware, &written_data.hardware,
	  (INTMIN ((sizeof (data_to_dump.hardware)),
		   (sizeof (written_data.hardware)))));
  memcpy (&data_to_dump.cache_format, &written_data.cache_format,
	  (INTMIN ((sizeof (data_to_dump.cache_format)),
		   (sizeof (written_data.cache_format)))));
  fprintf (stderr, "Writing %d bytes...\n", (sizeof (data_to_dump)));
  fflush (stderr);
  write ((fileno (stdout)), &data_to_dump, (sizeof (data_to_dump)));
  exit (0);
}
