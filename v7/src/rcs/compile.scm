#| -*-Scheme-*-

$Id: compile.scm,v 1.2 2000/03/31 14:19:37 cph Exp $

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

(load-option 'CREF)

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (for-each (lambda (filename)
		(compile-file filename '() system-global-syntax-table))
	      '("object" "format" "nparse" "logmer"))
    (cref/generate-constructors "rcs")
    (sf "rcs.con")
    (sf "rcs.ldr")))