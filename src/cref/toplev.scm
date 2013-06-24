#| -*-Scheme-*-

$Id: toplev.scm,v 1.21 2003/09/05 20:51:44 cph Exp $

Copyright 1988,1989,1991,1993,1995,1996 Massachusetts Institute of Technology
Copyright 1998,2000,2001,2002,2003 Massachusetts Institute of Technology

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

;;;; Package Model: Top Level

(declare (usual-integrations))

(define (generate/common kernel)
  (lambda (filename #!optional os-type)
    (let ((pathname (merge-pathnames filename))
	  (os-type
	   (if (or (default-object? os-type) (not os-type))
	       microcode-id/operating-system
	       os-type)))
      (let ((pmodel (read-package-model pathname os-type)))
	(let ((changes? (read-file-analyses! pmodel os-type)))
	  (resolve-references! pmodel)
	  (kernel pathname pmodel changes? os-type))))))

(define (cref/generate-trivial-constructor filename #!optional os-type)
  (let ((pathname (merge-pathnames filename))
	(os-type
	 (if (or (default-object? os-type)
		 (not os-type))
	     microcode-id/operating-system
	     os-type)))
    (write-external-descriptions pathname
				 (read-package-model pathname os-type)
				 #f
				 os-type)))

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
  (let ((package-set (package-set-pathname pathname os-type)))
    (if (or changes?
	    (file-modification-time<? package-set
				      (pathname-default-type pathname "pkg")))
	(fasdump (construct-external-descriptions pmodel) package-set))))

(define (write-cref pathname pmodel changes? os-type)
  (let ((cref-pathname
	 (pathname-new-type (package-set-pathname pathname os-type) "crf")))
    (if (or changes?
	    (file-modification-time<? cref-pathname
				      (pathname-default-type pathname "pkg")))
	(with-output-to-file cref-pathname
	  (lambda ()
	    (format-packages pmodel))))))

(define (write-cref-unusual pathname pmodel changes? os-type)
  (let ((cref-pathname
	 (pathname-new-type (package-set-pathname pathname os-type) "crf")))
    (if (or changes?
	    (file-modification-time<? cref-pathname
				      (pathname-default-type pathname "pkg")))
	(with-output-to-file cref-pathname
	  (lambda ()
	    (format-packages-unusual pmodel))))))