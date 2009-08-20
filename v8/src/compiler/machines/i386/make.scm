#| -*-Scheme-*-

$Id$

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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

;;;; Compiler: System Construction

(declare (usual-integrations))

(let ((loader
       (lambda ()
	 (load-option 'SF)
	 (let* ((value ((load "base/make") "Intel i386"))
		(env (->environment '(compiler))))
	   (set! (access compiler:generate-stack-checks? env) false)
	   (set! (access compiler:compress-top-level? env) true)
	   value)
	 (load "midend/load" #F))))
  (loader))
