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

/* Program to convert ascii-format cache descriptions into the binary
   form used by Scheme.

   To use, replace the structure labeled "written_data" below with the
   new one (if not present in the data base), then recompile this program:
     cc -Ae -O -o hppanewcache hppanewcache.c
   and then type
     ./hppanewcache >>HPPAmodels
 */

#include <stdio.h>
#define __HPUX__
#include "hppacach.h"

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
