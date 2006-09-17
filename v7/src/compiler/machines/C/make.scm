#| -*-Scheme-*-

$Id: make.scm,v 1.7 2006/09/17 12:10:04 gjr Exp $

Copyright (c) 1992, 1999, 2006 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Compiler: System Construction

(declare (usual-integrations))

(load-option 'synchronous-subprocess)

(begin
  (declare-shared-library "sf+compiler" (lambda () true))
  (let ((value ((load "base/make")
		(string-append "C/" microcode-id/machine-type))))
    (set! (access compiler:compress-top-level? (->environment '(compiler)))
	  true)
    (set! (access compiler:compile-data-files-as-expressions?
		  (->environment '(compiler top-level)))
	  false)
    (set! (access compiler:fggen-unmap-reference-traps-early?
		  (->environment '(compiler fg-generator)))
	  false)
    value))