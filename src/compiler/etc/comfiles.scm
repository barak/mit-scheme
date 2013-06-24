#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

|#

;;;; Stage recompilation checks

(declare (usual-integrations))

(define compiler-directories
  `("back" "base" "fggen" "fgopt" "rtlbase" "rtlgen" "rtlopt"
	   ,(if (eq? 'UNIX microcode-id/operating-system)
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

(define (compare-trees root1 root2)
  (for-each (lambda (d)
	      (compare-directory
	       (pathname-as-directory (merge-pathnames d root1))
	       (pathname-as-directory (merge-pathnames d root2))))
	    (append runtime-directories
		    (map (lambda (d) (string-append "compiler/" d))
			 compiler-directories))))

(define (compare-directory d1 d2)
  (for-each (lambda (p1)
	      (let ((p2 (merge-pathnames (file-pathname p1) d2)))
		(if (file-exists? p2)
		    (show-differences p1 p2)
		    (warn "Directory mismatch" `(,p1 exists ,p2 does not)))))
	    (directory-read (merge-pathnames "*.com" d1))))
  