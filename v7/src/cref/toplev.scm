#| -*-Scheme-*-

$Id: toplev.scm,v 1.10 1996/04/23 21:16:46 cph Exp $

Copyright (c) 1988-96 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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