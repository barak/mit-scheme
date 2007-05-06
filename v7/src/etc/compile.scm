#| -*-Scheme-*-

$Id: compile.scm,v 1.18 2007/05/06 14:17:04 cph Exp $

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

(define (compile-everything)
  (compile-all-dirs compile-dir))

(define (compile-all-dirs compile-dir)
  (compile-boot-dirs compile-dir)
  (for-each compile-dir
	    '("win32" "sos" "xml" "edwin" "imail" "6001" "ssp" "xdoc")))

(define (compile-boot-dirs compile-dir)
  (compile-cref compile-dir)
  (for-each compile-dir '("runtime" "cref" "sf" "compiler" "star-parser")))

(define (compile-cref compile-dir)
  (compile-dir "cref")
  (if (not (name->package '(cross-reference)))
      (load-dir "cref")))

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

(define (compile-bootstrap-1)
  (load-option 'SF)
  (with-working-directory-pathname "compiler"
    (lambda ()
      (load "compiler.sf"))))

(define (compile-bootstrap-2)
  (if (eq? microcode-id/compiled-code-type 'C)
      (fluid-let ((compiler:invoke-c-compiler? #f))
	(with-working-directory-pathname "compiler"
	  (lambda ()
	    (load "compiler.cbf")))
	(c-compile-pkgs "compiler"))
      (with-working-directory-pathname "compiler"
	(lambda ()
	  (load "compiler.cbf")))))

(define (compile-bootstrap-3)
  (load-option 'SF)
  (load-dir "compiler"))

(define (c-prepare)
  (fluid-let ((compiler:invoke-c-compiler? #f))
    (compile-boot-dirs c-compile-dir)
    (cf "microcode/utabmd")))

(define (c-compile)
  (fluid-let ((compiler:invoke-c-compiler? #f))
    (compile-all-dirs c-compile-dir)))

(define (c-compile-dir name)
  (compile-dir name)
  (c-compile-pkgs name))

(define (c-compile-pkgs name)
  (let ((root
	 (if (string=? name "star-parser")
	     "parser"
	     name)))
    (with-working-directory-pathname name
      (lambda ()
	(let ((compile-pkg
	       (lambda (os)
		 (let ((name (string-append root "-" os ".pkd")))
		   (if (file-exists? name)
		       (cbf name))))))
	  (compile-pkg "unx")
	  (compile-pkg "w32")
	  (compile-pkg "os2"))))))