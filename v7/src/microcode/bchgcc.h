/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchgcc.h,v 9.26 1987/02/12 01:17:47 jinx Exp $ */

#include "gccode.h"

/* All of these are in objects (Pointer), not bytes. */

#define GC_EXTRA_BUFFER_SIZE	512
#define GC_DISK_BUFFER_SIZE	4096
#define GC_BUFFER_SPACE		(GC_DISK_BUFFER_SIZE + GC_EXTRA_BUFFER_SIZE)
#define GC_BUFFER_BYTES		(GC_DISK_BUFFER_SIZE * sizeof(Pointer))

#define GC_FILE_MASK		0644	/* Everyone reads, owner writes */
#define GC_DEFAULT_FILE_NAME	"/tmp/GCXXXXXX"

extern Pointer *scan_buffer_top;
extern Pointer *free_buffer_top;
extern Pointer *dump_and_reload_scan_buffer();
extern Pointer *dump_and_reset_free_buffer();
extern void    dump_free_directly();

extern Pointer *GCLoop();
