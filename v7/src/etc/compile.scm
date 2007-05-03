#| -*-Scheme-*-

$Id: compile.scm,v 1.14 2007/05/03 03:45:52 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Program to compile MIT/GNU Scheme.

;;; This compiles the part of the system written in Scheme.
;;; The part written in C is compiled using "make".

(define boot-dirs
  '("cref" "runtime" "sf" "compiler" "star-parser"))

(define non-boot-dirs
  '("win32" "sos" "xml" "edwin" "imail" "6001" "ssp" "xdoc"))

(define (compile-dir name)
  (with-working-directory-pathname name
    (lambda ()
      (if (file-exists? (pathname-new-type name "sf"))
	  (begin
	    (load (pathname-new-type name "sf"))
	    (load (pathname-new-type name "cbf")))
	  (load "compile")))))

(define (load-dir name)
  (with-working-directory-pathname name
    (lambda ()
      (cond ((and (string=? name "compiler")
		  (eq? microcode-id/compiled-code-type 'C))
	     (load "machines/C/make"))
	    ((file-exists? (pathname-new-type name "sf"))
	     (load "make"))
	    (else
	     (load "load"))))))

(define (compile-everything)
  (compile-dir "cref")
  (if (not (name->package '(cross-reference)))
      (load-dir "cref"))
  (for-each compile-dir boot-dirs)
  (for-each compile-dir non-boot-dirs))

(define (compile-bootstrap-1)
  (compile-dir "cref")
  (if (not (name->package '(cross-reference)))
      (load-dir "cref"))
  (compile-dir "sf"))

(define (compile-bootstrap-2)
  (load-dir "cref")
  (load-dir "sf")
  (with-working-directory-pathname "compiler"
    (lambda ()
      (load "compiler.sf"))))

(define (compile-bootstrap-3)
  (with-working-directory-pathname "compiler"
    (lambda ()
      (load "compiler.cbf"))))

(define (compile-bootstrap-4)
  (load-dir "cref")
  (load-dir "sf")
  (load-dir "compiler"))

(define (c-compile-dir name)
  (compile-dir name)
  (liarc-compile-pkgs name))

(define (c-compile-pkgs name)
  (with-working-directory-pathname name
    (lambda ()
      (cbf (string-append name "-unx.pkd"))
      (if (not (string=? name "compiler")) ;kludge
	  (begin
	    (cbf (string-append name "-w32.pkd"))
	    (cbf (string-append name "-os2.pkd")))))))

(define (c-prepare)
  (fluid-let ((compiler:invoke-c-compiler? #f))
    (for-each liarc-compile-dir boot-dirs)))

(define (c-compile)
  (fluid-let ((compiler:invoke-c-compiler? #f))
    (for-each liarc-compile-dir boot-dirs)
    (for-each liarc-compile-dir non-boot-dirs)))