#| -*-Scheme-*-

$Id: mklogs.scm,v 1.11 1999/01/02 06:35:54 cph Exp $

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
	  '("/scheme/v7/src/compiler"
	    "/scheme/v7/src/microcode"
	    "/scheme/v7/src/runtime"
	    "/scheme/v7/src/sf"

	    "/scheme/v8/src/6001"
	    "/scheme/v8/src/bench"
	    "/scheme/v8/src/compiler"
	    "/scheme/v8/src/cref"
	    "/scheme/v8/src/edwin"
	    "/scheme/v8/src/microcode"
	    "/scheme/v8/src/pcsample"
	    "/scheme/v8/src/rcs"
	    "/scheme/v8/src/runtime"
	    "/scheme/v8/src/sf"
	    "/scheme/v8/src/sos"
	    "/scheme/v8/src/swat"
	    "/scheme/v8/src/win32"))