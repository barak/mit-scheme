/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/osfile.h,v 1.1 1990/06/20 19:36:20 cph Rel $

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

#ifndef SCM_OSFILE_H
#define SCM_OSFILE_H

#include "os.h"

extern Tchannel EXFUN (OS_open_input_file, (CONST char * filename));
extern Tchannel EXFUN (OS_open_output_file, (CONST char * filename));
extern Tchannel EXFUN (OS_open_io_file, (CONST char * filename));
extern Tchannel EXFUN (OS_open_append_file, (CONST char * filename));
extern Tchannel EXFUN (OS_open_load_file, (CONST char * filename));
extern Tchannel EXFUN (OS_open_dump_file, (CONST char * filename));
extern off_t EXFUN (OS_file_length, (Tchannel channel));
extern off_t EXFUN (OS_file_position, (Tchannel channel));
extern void EXFUN (OS_file_set_position, (Tchannel channel, off_t position));

#endif /* SCM_OSFILE_H */
