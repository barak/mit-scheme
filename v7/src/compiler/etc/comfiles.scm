#| -*-Scheme-*-

$Id: comfiles.scm,v 1.7 2002/11/20 19:45:48 cph Exp $

Copyright (c) 1989-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Stage recompilation checks

(declare (usual-integrations))

(define compiler-directories
  `("back" "base" "fggen" "fgopt" "rtlbase" "rtlgen" "rtlopt"
	   ,(if (equal? microcode-id/operating-system-name "unix")
		"machine"
		"machines/i386")))	   

(define runtime-directories
  '("runtime" "sf" "cref"))

(define (->string name)
  (cond ((string? name) name)
	((symbol? name) (symbol->string name))
	(else (error "->string: Can't coerce" name))))

(define (for-each-file proc directories)
  (for-each (lambda (dname)
	      (for-each proc
			(directory-read
			 (string-append (->string dname)
					"/*.scm"))))
	    directories))

;; This assumes that the working directory contains the copy of the compiler
;; to check.

(define (check-stage directories #!optional stage)
  (let ((stage
	 (if (default-object? stage)
	     "STAGE2"
	     (->string stage))))
    (for-each-file
     (lambda (name)
       (let* ((path0 (->pathname name))
	      (path1 (pathname-new-type (->pathname path0) "com"))
	      (path2 (pathname-new-directory
		      path1
		      (append (pathname-directory path1)
			      `(,stage)))))
	 (cond ((not (file-exists? path1))
		(if (file-exists? path2)
		    (warn "Directory mismatch"
			  `(,path2 exists ,path1 does not))
		    (warn "Missing compiled files for" path0)))
	       ((not (file-exists? path2))
		(warn "Directory mismatch"
		      `(,path1 exists ,path2 does not)))
	       (else
		(show-differences path1 path2)))))
     directories)))

(define (check-compiler #!optional stage)
  (check-stage compiler-directories
	       (if (default-object? stage) "STAGE2" stage)))