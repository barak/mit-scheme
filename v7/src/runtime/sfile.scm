#| -*-Scheme-*-

$Id: sfile.scm,v 14.19 1996/04/24 03:39:30 cph Exp $

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