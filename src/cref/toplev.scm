#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Package Model: Top Level

(declare (usual-integrations))

(define cref/source-root #!default)
(define cref/object-root #!default)

(define (cref/source-pathname pathname)
  ;; This assumes we're sitting in the source directory.
  (merge-pathnames pathname))

(define (cref/object-pathname pathname)
  ;; This assumes we're sitting in the source directory.
  (merge-pathnames (enough-pathname pathname cref/source-root)
		   cref/object-root))

(define (generate/common kernel)
  (lambda (filename #!optional os-type)
    (for-each-os-type os-type
      (let ((pathname (cref/source-pathname filename)))
	(lambda (os-type)
	  (let ((pmodel (read-package-model pathname os-type)))
	    (let ((changes? (read-file-analyses! pmodel os-type)))
	      (resolve-references! pmodel)
	      (kernel pathname pmodel changes? os-type))))))))

(define (cref/generate-trivial-constructor filename #!optional os-type)
  (for-each-os-type os-type
    (let ((pathname (cref/source-pathname filename)))
      (lambda (os-type)
	(write-external-descriptions pathname
				     (read-package-model pathname os-type)
				     #f
				     os-type)))))

(define (for-each-os-type os-type procedure)
  (cond ((default-object? os-type) (procedure microcode-id/operating-system))
	((eq? os-type 'all) (for-each procedure os-types))
	((memq os-type os-types) (procedure os-type))
	(else (error:bad-range-argument os-type #f))))

(define (cref/package-files filename os-type)
  (append-map package/files
	      (pmodel/packages
	       (bind-condition-handler (list condition-type:warning)
		   (lambda (condition)
		     condition
		     (muffle-warning))
		 (lambda ()
		   (read-package-model filename os-type))))))

(define os-types
  '(nt unix))

(define cref/generate-cref
  (generate/common
   (lambda (pathname pmodel changes? os-type)
     (write-cref pathname pmodel changes? os-type))))

(define cref/generate-cref-unusual
  (generate/common
   (lambda (pathname pmodel changes? os-type)
     (write-cref-unusual pathname pmodel changes? os-type))))

(define cref/generate-constructors
  (generate/common
   (lambda (pathname pmodel changes? os-type)
     (write-cref-unusual pathname pmodel changes? os-type)
     (write-external-descriptions pathname pmodel changes? os-type))))

(define cref/generate-all
  (generate/common
   (lambda (pathname pmodel changes? os-type)
     (write-cref pathname pmodel changes? os-type)
     (write-external-descriptions pathname pmodel changes? os-type))))

(define (write-external-descriptions pathname pmodel changes? os-type)
  (let* ((object-pathname (cref/object-pathname pathname))
	 (package-set (package-set-pathname object-pathname os-type)))
    (if (or changes?
	    (file-modification-time<? package-set
				      (pathname-default-type pathname "pkg")))
	(fasdump (construct-external-descriptions pmodel) package-set))))

(define (write-cref pathname pmodel changes? os-type)
  (let* ((object-pathname (cref/object-pathname pathname))
	 (cref-pathname
	  (pathname-new-type (package-set-pathname object-pathname os-type)
			     "crf")))
    (if (or changes?
	    (file-modification-time<? cref-pathname
				      (pathname-default-type pathname "pkg")))
	(call-with-output-file cref-pathname
	  (lambda (port)
	    (format-packages pmodel port))))))

(define (write-cref-unusual pathname pmodel changes? os-type)
  (let* ((object-pathname (cref/object-pathname pathname))
	 (cref-pathname
	  (pathname-new-type (package-set-pathname object-pathname os-type)
			     "crf")))
    (if (or changes?
	    (file-modification-time<? cref-pathname
				      (pathname-default-type pathname "pkg")))
	(call-with-output-file cref-pathname
	  (lambda (port)
	    (format-packages-unusual pmodel port))))))