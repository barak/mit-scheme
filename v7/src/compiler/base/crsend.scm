#| -*-Scheme-*-

$Id: crsend.scm,v 1.18 2007/06/14 17:39:26 cph Exp $

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

;;;; Finish cross-compilation process

;;; This program takes the output of the cross compiler (.moc files)
;;; and converts it into its final form.  It must be run on the target
;;; machine.  It can be loaded and run without the rest of the
;;; compiler.

(declare (usual-integrations))

(define (finish-cross-compilation:directory directory #!optional force?)
  (let ((force? (if (default-object? force?) #f force?)))
    (let loop ((directory directory))
      (for-each (lambda (pathname)
		  (cond ((file-directory? pathname)
			 (if (not (let ((ns (file-namestring pathname)))
				    (or (string=? ns ".")
					(string=? ns ".."))))
			     (loop pathname)))
			((let ((t (pathname-type pathname)))
			   (and (string? t)
				(string=? t "moc")))
			 (finish-cross-compilation:file pathname force?))))
		(directory-read (pathname-as-directory directory))))))

(define (finish-cross-compilation:file input-file #!optional force?)
  (let* ((input-file (pathname-default-type input-file "moc"))
	 (output-file (pathname-new-type input-file "com")))
    (if (or (if (default-object? force?) #t force?)
	    (file-modification-time<? output-file input-file))
	(with-notification
	    (lambda (port)
	      (write-string "Compiling file: " port)
	      (write (enough-namestring input-file) port)
	      (write-string " => " port)
	      (write (enough-namestring output-file) port))
	  (lambda ()
	    (fasdump (finish-cross-compilation:scode (fasload input-file #t))
		     output-file
		     #t))))))

(define (finish-cross-compilation:scode cross-compilation)
  (let ((compile-by-procedures? (vector-ref cross-compilation 0))
	(expression (cross-link-end (vector-ref cross-compilation 1)))
	(others (map cross-link-end (vector-ref cross-compilation 2))))
    (if (null? others)
	expression
	(scode/make-comment
	 (make-dbg-info-vector
	  (let ((all-blocks
		 (list->vector
		  (cons
		   (compiled-code-address->block expression)
		   others))))
	    (if compile-by-procedures?
		(list 'COMPILED-BY-PROCEDURES
		      all-blocks
		      (list->vector others))
		all-blocks)))
	 expression))))

(define (cross-link-end object)
  (let ((code-vector (cc-vector/code-vector object)))
    (cross-link/process-code-vector
     (if (compiled-code-block? code-vector)
	 code-vector
	 (begin
	   (guarantee-vector code-vector #f)
	   (let ((new-code-vector
		  (cross-link/finish-assembly
		   (cc-code-block/bit-string code-vector)
		   (cc-code-block/objects code-vector)
		   (cc-code-block/object-width code-vector))))
	     (set-compiled-code-block/debugging-info!
	      new-code-vector
	      (cc-code-block/debugging-info code-vector))
	     new-code-vector)))
     object)))

(define (cross-link/process-code-vector code-vector cc-vector)
  (let ((bindings
	 (let ((label-bindings (cc-vector/label-bindings cc-vector)))
	   (map (lambda (label)
		  (cons
		   label
		   (with-absolutely-no-interrupts
		     (lambda ()
		       ((ucode-primitive primitive-object-set-type)
			(ucode-type compiled-entry)
			(make-non-pointer-object
			 (+ (cdr (or (assq label label-bindings)
				     (error "Missing entry point" label)))
			    (object-datum code-vector))))))))
		(cc-vector/entry-points cc-vector)))))
    (let ((label->expression
	   (lambda (label)
	     (cdr (or (assq label bindings)
		      (error "Label not defined as entry point:" label))))))
      (let ((expression (label->expression (cc-vector/entry-label cc-vector))))
	(for-each (lambda (entry)
		    (set-lambda-body! (car entry)
				      (label->expression (cdr entry))))
		  (cc-vector/ic-procedure-headers cc-vector))
	expression))))

(define (cross-link/finish-assembly code-block objects scheme-object-width)
  (let* ((bl (quotient (bit-string-length code-block) scheme-object-width))
	 (non-pointer-length ((ucode-primitive make-non-pointer-object) bl))
	 (output-block (make-vector (+ (length objects) bl 1))))
    (with-absolutely-no-interrupts
      (lambda ()
	(vector-set! output-block 0
		     ((ucode-primitive primitive-object-set-type)
		      (ucode-type manifest-nm-vector)
		      non-pointer-length))))
    ;; After header just inserted.
    (write-bits! output-block (* scheme-object-width 2) code-block)
    (insert-objects! output-block objects (+ bl 1))
    (object-new-type (ucode-type compiled-code-block) output-block)))

(define (insert-objects! v objects where)
  (let ((end (vector-length v)))
    (do ((objects objects (cdr objects))
	 (index where (fix:+ index 1)))
	((not (fix:< index end)) unspecific)
      (vector-set! v index (cadar objects)))))

(define-structure (cc-code-block (type vector)
				 (conc-name cc-code-block/))
  (debugging-info #f read-only #f)
  (bit-string #f read-only #t)
  (objects #f read-only #t)
  (object-width #f read-only #t))

(define-structure (cc-vector (type vector)
			     (constructor cc-vector/make)
			     (conc-name cc-vector/))
  (code-vector #f read-only #t)
  (entry-label #f read-only #t)
  (entry-points #f read-only #t)
  (label-bindings #f read-only #t)
  (ic-procedure-headers #f read-only #t))

(define-syntax ucode-primitive
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (apply make-primitive-procedure (cdr form)))))

(define-syntax ucode-type
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (apply microcode-type (cdr form)))))