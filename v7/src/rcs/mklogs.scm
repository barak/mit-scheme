#| -*-Scheme-*-

$Id: mklogs.scm,v 1.13 1999/12/15 01:29:17 cph Exp $

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
	  '("/scheme/v7/src/6001"
	    "/scheme/v7/src/compiler"
	    "/scheme/v7/src/cref"
	    "/scheme/v7/src/edwin"
	    "/scheme/v7/src/microcode"
	    "/scheme/v7/src/pcsample"
	    "/scheme/v7/src/rcs"
	    "/scheme/v7/src/runtime"
	    "/scheme/v7/src/sf"
	    "/scheme/v7/src/sos"
	    "/scheme/v7/src/win32"

	    "/scheme/v8/src/bench"
	    "/scheme/v8/src/compiler"
	    "/scheme/v8/src/microcode"
	    "/scheme/v8/src/runtime"
	    "/scheme/v8/src/sf"
	    "/scheme/v8/src/swat"

	    "/scheme/etc"
	    "/scheme/documentation/ref-manual"
	    "/scheme/documentation/user-manual"
	    "/scheme/documentation/sos"))