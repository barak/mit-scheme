#| -*-Scheme-*-

$Id: toplev.scm,v 1.19 2002/11/20 19:45:57 cph Exp $

Copyright (c) 1988-2002 Massachusetts Institute of Technology

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
	    (not (file-modification-time<?
		  (pathname-default-type pathname "pkg")
		  package-set)))
	(fasdump (construct-external-descriptions pmodel) package-set))))

(define (write-cref pathname pmodel changes? os-type)
  (let ((cref-pathname
	 (pathname-new-type (package-set-pathname pathname os-type) "crf")))
    (if (or changes?
	    (not (file-modification-time<?
		  (pathname-default-type pathname "pkg")
		  cref-pathname)))
	(with-output-to-file cref-pathname
	  (lambda ()
	    (format-packages pmodel))))))

(define (write-cref-unusual pathname pmodel changes? os-type)
  (let ((cref-pathname
	 (pathname-new-type (package-set-pathname pathname os-type) "crf")))
    (if (or changes?
	    (not (file-modification-time<?
		  (pathname-default-type pathname "pkg")
		  cref-pathname)))
	(with-output-to-file cref-pathname
	  (lambda ()
	    (format-packages-unusual pmodel))))))