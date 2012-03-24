/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

#ifndef SCM_UXTERM_H
#define SCM_UXTERM_H

#include "osterm.h"

extern int terminal_state_buffered_p (Ttty_state * s);
extern void terminal_state_buffered (Ttty_state * s, Tchannel channel);
extern void terminal_state_nonbuffered (Ttty_state * s, int fd, int polling);
extern void terminal_state_raw (Ttty_state * s, int fd);
extern void get_terminal_state (Tchannel channel, Ttty_state * s);
extern void set_terminal_state (Tchannel channel, Ttty_state * s);

#endif /* SCM_UXTERM_H */
