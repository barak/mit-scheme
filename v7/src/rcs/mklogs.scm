#| -*-Scheme-*-

$Id: mklogs.scm,v 1.10 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
|#

;;;; Update the RCS log files in the standard Scheme directories.

(for-each rcs-directory-log
	  '("/scheme/7.4/src/compiler"
	    "/scheme/7.4/src/microcode"
	    "/scheme/7.4/src/runtime"
	    "/scheme/7.4/src/sf"

	    "/scheme/8.0/src/6001"
	    "/scheme/8.0/src/bench"
	    "/scheme/8.0/src/compiler"
	    "/scheme/8.0/src/cref"
	    "/scheme/8.0/src/edwin"
	    "/scheme/8.0/src/microcode"
	    "/scheme/8.0/src/pcsample"
	    "/scheme/8.0/src/rcs"
	    "/scheme/8.0/src/runtime"
	    "/scheme/8.0/src/sf"
	    "/scheme/8.0/src/sos"
	    "/scheme/8.0/src/swat"
	    "/scheme/8.0/src/win32"))