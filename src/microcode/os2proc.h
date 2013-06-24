/* -*-C-*-

$Id: os2proc.h,v 1.2 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1997, 1999 Massachusetts Institute of Technology

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

#ifndef SCM_OS2PROC_H
#define SCM_OS2PROC_H

#include "osproc.h"

extern Tprocess OS2_make_subprocess
 (const char *, const char *, const char *, const char *,
  enum process_channel_type, Tchannel,
  enum process_channel_type, Tchannel,
  enum process_channel_type, Tchannel);

#endif /* SCM_OS2PROC_H */
