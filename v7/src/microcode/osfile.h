/* -*-C-*-

$Id: osfile.h,v 1.4 2002/11/20 19:46:12 cph Exp $

Copyright (c) 1990, 1999 Massachusetts Institute of Technology

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
