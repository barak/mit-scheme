#| -*-Scheme-*-

$Id: sfile.scm,v 14.31 2001/12/17 17:40:59 cph Exp $

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

;;;; Simple File Operations
;;; package: ()

(declare (usual-integrations))

(define (file-exists-direct? filename)
  (let ((result
	 ((ucode-primitive file-exists-direct? 1)
	  (->namestring (merge-pathnames filename)))))
    (if (eq? 0 result)
	#t
	result)))

(define (file-exists-indirect? filename)
  (let ((result
	 ((ucode-primitive file-exists? 1)
	  (->namestring (merge-pathnames filename)))))
    (if (eq? 0 result)
	#f
	result)))

(define file-exists? file-exists-indirect?)

(define file-type-direct)
(define file-type-indirect)
(let ((make-file-type
       (lambda (procedure)
	 (lambda (filename)
	   (let ((n (procedure (->namestring (merge-pathnames filename)))))
	     (and n
		  (let ((types
			 '#(REGULAR
			    DIRECTORY
			    UNIX-SYMBOLIC-LINK
			    UNIX-CHARACTER-DEVICE
			    UNIX-BLOCK-DEVICE
			    UNIX-NAMED-PIPE
			    UNIX-SOCKET
			    OS2-NAMED-PIPE
			    WIN32-NAMED-PIPE)))
		    (if (fix:< n (vector-length types))
			(vector-ref types n)
			'UNKNOWN))))))))
  (set! file-type-direct
	(make-file-type (ucode-primitive file-type-direct 1)))
  (set! file-type-indirect
	(make-file-type (ucode-primitive file-type-indirect 1))))

(define (file-regular? filename)
  (eq? 'REGULAR (file-type-indirect filename)))

(define (file-directory? filename)
  (eq? 'DIRECTORY (file-type-indirect filename)))

(define (file-symbolic-link? filename)
  ((ucode-primitive file-symlink? 1)
   (->namestring (merge-pathnames filename))))
(define file-soft-link? file-symbolic-link?)

(define (file-access filename amode)
  ((ucode-primitive file-access 2)
   (->namestring (merge-pathnames filename))
   amode))

(define (file-readable? filename)
  (file-access filename 4))

(define (file-writeable? filename)
  ((ucode-primitive file-access 2)
   (let ((pathname (merge-pathnames filename)))
     (let ((filename (->namestring pathname)))
       (if ((ucode-primitive file-exists? 1) filename)
	   filename
	   (directory-namestring pathname))))
   2))
(define file-writable? file-writeable?) ;upwards compatability

(define (file-executable? filename)
  (file-access filename 1))

(define (file-touch filename)
  ((ucode-primitive file-touch 1) (->namestring (merge-pathnames filename))))

(define (make-directory name)
  ((ucode-primitive directory-make 1)
   (->namestring (directory-pathname-as-file (merge-pathnames name)))))

(define (delete-directory name)
  ((ucode-primitive directory-delete 1)
   (->namestring (directory-pathname-as-file (merge-pathnames name)))))

(define (rename-file from to)
  ((ucode-primitive file-rename) (->namestring (merge-pathnames from))
				 (->namestring (merge-pathnames to))))

(define (delete-file filename)
  ((ucode-primitive file-remove) (->namestring (merge-pathnames filename))))

(define (hard-link-file from to)
  ((ucode-primitive file-link-hard 2) (->namestring (merge-pathnames from))
				      (->namestring (merge-pathnames to))))

(define (soft-link-file from to)
  ((ucode-primitive file-link-soft 2) (->namestring from)
				      (->namestring (merge-pathnames to))))

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

(define (current-file-time)
  (call-with-temporary-file-pathname file-modification-time))

(define (directory-file-names directory #!optional include-dots?)
  (let ((channel
	 (directory-channel-open
	  (->namestring (pathname-as-directory directory))))
	(include-dots?
	 (if (default-object? include-dots?) #f include-dots?)))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop
	     (if (and (not include-dots?)
		      (or (string=? "." name)
			  (string=? ".." name)))
		 result
		 (cons name result)))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (file-processed? filename input-type output-type)
  (file-modification-time<?
   (pathname-default-type filename input-type)
   (pathname-new-type filename output-type)))

(define (file-modification-time<? source target)
  (let ((source (file-modification-time-indirect source)))
    (and source
	 (let ((target (file-modification-time-indirect target)))
	   (and target
		(<= source target))))))

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