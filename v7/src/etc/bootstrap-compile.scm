#| -*-Scheme-*-

$Id: bootstrap-compile.scm,v 1.1 2000/10/16 18:17:49 cph Exp $

Copyright (c) 2000 Massachusetts Institute of Technology

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

(sf "microcode/utabmd")

(for-each (lambda (name)
	    (with-working-directory-pathname name
	      (lambda ()
		(load (pathname-new-type name "sf"))
		(load (pathname-new-type name "cbf")))))
	  '("cref" "runtime" "sf" "compiler" "edwin" "6001"))

(for-each (lambda (name)
	    (load (merge-pathnames "compile" (pathname-as-directory name))))
	  '("sos" "imail"))