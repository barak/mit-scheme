#| -*-Scheme-*-

$Id: b62fa5e22fc872e64940a2a122fac35fe1c475f4 $

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

;;;; Compiler: System Construction

(declare (usual-integrations))

(let ((loader
       (lambda ()
	 (load-option 'SF)
	 (let ((value ((load "base/make")
		       (lambda ()
			 (string-append
			  "HP PA  untagged fixnums and entries, "
			  (number->string
			   ((access rtlgen/number-of-argument-registers
				    (->environment '(compiler midend)))))
			  " arg regs")))))
	   (set! (access compiler:compress-top-level?
			 (->environment '(compiler)))
		 true)
	   value)
	 (load "midend/load" #F))))

  (if #F
      ;; This temporary monkey-business stops uncompiled code from being
      ;; purified so that TRACE & BREAK dont take so long
      (let ((old-purify purify))
	(fluid-let ((purify (lambda (thing)
			      (if (not (comment? thing))
				  (old-purify thing)))))
	  (loader)))
      (loader)))
