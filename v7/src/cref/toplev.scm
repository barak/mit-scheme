#| -*-Scheme-*-

$Id: toplev.scm,v 1.14 2001/08/15 02:59:58 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
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
    (write-external-descriptions pathname (read-package-model pathname) #f)))

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
     (write-external-descriptions pathname pmodel changes?))))

(define cref/generate-all
  (generate/common
   (lambda (pathname pmodel changes?)
     (write-cref pathname pmodel changes?)
     (write-globals pathname pmodel changes?)
     (write-external-descriptions pathname pmodel changes?))))

(define (write-external-descriptions pathname pmodel changes?)
  (if (or changes? (not (file-processed? pathname "pkg" "pkd")))
      (fasdump (construct-external-descriptions pmodel)
	       (pathname-new-type pathname "pkd"))))

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
	(fasdump (cons '(VERSION . 2)
		       (map (lambda (entry)
			      (vector (package/name (car entry))
				      (let loop ((package (car entry)))
					(let ((parent
					       (package/parent package)))
					  (if parent
					      (cons (package/name parent)
						    (loop parent))
					      '())))
				      (map binding/name (cdr entry))))
			    package-bindings))
		 (pathname-new-type pathname "glo")))))