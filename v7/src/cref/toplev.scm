#| -*-Scheme-*-

$Id: toplev.scm,v 1.12 1999/01/02 06:11:34 cph Exp $

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

;;;; Package Model: Top Level

(declare (usual-integrations))

(define (generate/common kernel)
  (lambda (filename)
    (let ((pathname (merge-pathnames filename)))
      (let ((pmodel (read-package-model pathname)))
	(let ((changes? (read-file-analyses! pmodel)))
	  (resolve-references! pmodel)
	  (kernel pathname pmodel changes?))))))

(define (cref/generate-trivial-constructor filename)
  (let ((pathname (merge-pathnames filename)))
    (write-constructor pathname (read-package-model pathname) #f)))

(define cref/generate-cref
  (generate/common
   (lambda (pathname pmodel changes?)
     (write-cref pathname pmodel changes?))))

(define cref/generate-cref-unusual
  (generate/common
   (lambda (pathname pmodel changes?)
     (write-cref-unusual pathname pmodel changes?))))

(define cref/generate-constructors
  (generate/common
   (lambda (pathname pmodel changes?)
     (write-cref-unusual pathname pmodel changes?)
     (write-globals pathname pmodel changes?)
     (write-constructor pathname pmodel changes?)
     (write-loader pathname pmodel changes?))))

(define cref/generate-all
  (generate/common
   (lambda (pathname pmodel changes?)
     (write-cref pathname pmodel changes?)
     (write-globals pathname pmodel changes?)
     (write-constructor pathname pmodel changes?)
     (write-loader pathname pmodel changes?))))

(define (write-constructor pathname pmodel changes?)
  (if (or changes? (not (file-processed? pathname "pkg" "con")))
      (let ((constructor (construct-constructor pmodel)))
	(with-output-to-file (pathname-new-type pathname "con")
	  (lambda ()
	    (fluid-let ((*unparser-list-breadth-limit* #F)
			(*unparser-list-depth-limit*   #F))
	      (write-string ";;; -*-Scheme-*-")
	      (newline)
	      (write-string ";;; program to make package structure")
	      (newline)
	      (write '(DECLARE (USUAL-INTEGRATIONS)))
	      (for-each (lambda (expression)
			  (pp expression (current-output-port) true))
		constructor)))))))

(define (write-loader pathname pmodel changes?)
  changes?
  (if (not (file-processed? pathname "pkg" "ldr"))
      (let ((loader (construct-loader pmodel)))
	(with-output-to-file (pathname-new-type pathname "ldr")
	  (lambda ()
	    (fluid-let ((*unparser-list-breadth-limit* #F)
			(*unparser-list-depth-limit*   #F))
	      (write-string ";;; -*-Scheme-*-")
	      (newline)
	      (write-string ";;; program to load package contents")
	      (newline)
	      (write '(DECLARE (USUAL-INTEGRATIONS)))
	      (for-each (lambda (expression)
			  (pp expression (current-output-port) true))
		loader)))))))

(define (write-cref pathname pmodel changes?)
  (if (or changes? (not (file-processed? pathname "pkg" "crf")))
      (with-output-to-file (pathname-new-type pathname "crf")
	(lambda ()
	  (format-packages pmodel)))))

(define (write-cref-unusual pathname pmodel changes?)
  (if (or changes? (not (file-processed? pathname "pkg" "crf")))
      (with-output-to-file (pathname-new-type pathname "crf")
	(lambda ()
	  (format-packages-unusual pmodel)))))

(define (write-globals pathname pmodel changes?)
  (if (or changes? (not (file-processed? pathname "pkg" "glo")))
      (let ((package-bindings
	     (map (lambda (package)
		    (cons package
			  (list-transform-positive
			      (package/sorted-bindings package)
			    binding/source-binding)))
		  (pmodel/packages pmodel)))
	    (exports '()))
	(for-each (lambda (entry)
		    (for-each (lambda (binding)
				(for-each (lambda (link)
					    (set! exports
						  (cons (link/destination link)
							exports))
					    unspecific)
					  (binding/links binding)))
			      (cdr entry)))
		  package-bindings)
	(for-each (lambda (binding)
		    (let ((package (binding/package binding)))
		      (let ((entry (assq package package-bindings)))
			(if entry
			    (set-cdr! entry (cons binding (cdr entry)))
			    (begin
			      (set! package-bindings
				    (cons (list package binding)
					  package-bindings))
			      unspecific)))))
		  exports)
	(fasdump (map (lambda (entry)
			(cons (package/name (car entry))
			      (map binding/name (cdr entry))))
		      package-bindings)
		 (pathname-new-type pathname "glo")))))