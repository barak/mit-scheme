#| -*-Scheme-*-

$Id: sfile.scm,v 14.20 1999/01/02 06:19:10 cph Exp $

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

;;;; Simple File Operations
;;; package: ()

(declare (usual-integrations))

(define (file-exists? filename)
  ((ucode-primitive file-exists? 1) (->namestring (merge-pathnames filename))))

(define (rename-file from to)
  ((ucode-primitive file-rename) (->namestring (merge-pathnames from))
				 (->namestring (merge-pathnames to))))

(define (delete-file filename)
  ((ucode-primitive file-remove) (->namestring (merge-pathnames filename))))

(define (delete-file-no-errors filename)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler (list condition-type:file-error
				   condition-type:port-error)
	 (lambda (condition)
	   condition
	   (k #f))
       (lambda ()
	 (delete-file filename)
	 #t)))))

(define (file-eq? x y)
  ((ucode-primitive file-eq?) (->namestring (merge-pathnames x))
			      (->namestring (merge-pathnames y))))

(define (call-with-temporary-filename receiver)
  (call-with-temporary-file-pathname
   (lambda (pathname)
     (receiver (->namestring pathname)))))

(define (call-with-temporary-file-pathname receiver)
  (let ((pathname (temporary-file-pathname)))
    (dynamic-wind
     (lambda () unspecific)
     (lambda () (receiver pathname))
     (lambda () (deallocate-temporary-file pathname)))))

(define (allocate-temporary-file pathname)
  (and (not (file-exists? pathname))
       (let ((objects (get-fixed-objects-vector))
	     (slot (fixed-objects-vector-slot 'FILES-TO-DELETE))
	     (filename (->namestring pathname)))
	 (without-interrupts
	  (lambda ()
	    (and (file-touch pathname)
		 (begin
		   (vector-set! objects slot
				(cons filename (vector-ref objects slot)))
		   ((ucode-primitive set-fixed-objects-vector! 1) objects)
		   #t)))))))

(define (deallocate-temporary-file pathname)
  (delete-file-no-errors pathname)
  (let ((objects (get-fixed-objects-vector))
	(slot (fixed-objects-vector-slot 'FILES-TO-DELETE))
	(filename (->namestring pathname)))
    (without-interrupts
     (lambda ()
       (vector-set! objects slot
		    (delete! filename (vector-ref objects slot)))
       ((ucode-primitive set-fixed-objects-vector! 1) objects)))))

(define (current-file-time)
  (call-with-temporary-file-pathname file-modification-time))

(define (guarantee-init-file-specifier object procedure)
  (if (not (init-file-specifier? object))
      (error:wrong-type-argument object "init-file specifier" procedure)))

(define (init-file-specifier? object)
  (and (list? object)
       (for-all? object
	 (lambda (object)
	   (and (string? object)
		(not (string-null? object)))))))

(define (guarantee-init-file-directory pathname)
  (let ((directory (user-homedir-pathname)))
    (if (not (file-directory? directory))
	(error "Home directory doesn't exist:" directory)))
  (let loop ((pathname pathname))
    (let ((directory (directory-pathname pathname)))
      (if (not (file-directory? directory))
	  (begin
	    (loop (directory-pathname-as-file directory))
	    (make-directory directory))))))

(define (open-input-init-file specifier)
  (open-input-file (init-file-specifier->pathname specifier)))

(define (open-output-init-file specifier #!optional append?)
  (let ((pathname (init-file-specifier->pathname specifier)))
    (guarantee-init-file-directory pathname)
    (open-output-file pathname (if (default-object? append?) #f append?))))