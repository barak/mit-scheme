;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/io.scm,v 13.41 1987/01/23 00:15:03 jinx Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Input/output utilities

(declare (usual-integrations)
	 (compilable-primitive-functions &make-object))

(define close-all-open-files)

(define primitive-io
  (make-package primitive-io
		((open-files-slot (fixed-objects-vector-slot 'OPEN-FILES))
		 (header-size 2)
		 (counter-slot 0)
		 (file-vector-slot 1)
		 (default-size 10)
		 (buffer-size 10)
		 (closed-direction 0)

		 (make-physical-channel (make-primitive-procedure 'HUNK3-CONS))
		 (channel-number system-hunk3-cxr0)
		 (channel-name system-hunk3-cxr1)
		 (channel-direction system-hunk3-cxr2)
		 (set-channel-direction! system-hunk3-set-cxr2!)
		 (non-marked-vector-cons
		  (make-primitive-procedure 'NON-MARKED-VECTOR-CONS))
		 (insert-non-marked-vector!
		  (make-primitive-procedure 'INSERT-NON-MARKED-VECTOR!))
		 )

(declare (compilable-primitive-functions
	  (make-physical-channel hunk3-cons)
	  (channel-number system-hunk3-cxr0)
	  (channel-name system-hunk3-cxr1)
	  (channel-direction system-hunk3-cxr2)
	  (set-channel-direction! system-hunk3-set-cxr2!)
	  non-marked-vector-cons
	  insert-non-marked-vector!))

;;;; Open/Close Files

;;;  Direction is one of the following:
;;;     - true:   output channel
;;;	- false:  input channel
;;;	- 0:	  closed channel

(define open-channel-wrapper
  (let ((open-channel (make-primitive-procedure 'FILE-OPEN-CHANNEL)))
    (named-lambda ((open-channel-wrapper direction) filename)
      (let ((open-files-vector
	     (vector-ref (get-fixed-objects-vector) open-files-slot))
	    (file-info
	     (make-physical-channel (open-channel filename direction)
				    filename
				    direction)))
	(add-file! file-info
		   (if (= (vector-ref open-files-vector counter-slot)
			  (- (vector-length open-files-vector) header-size))
		       (grow-files-vector! open-files-vector)
		       open-files-vector))
	file-info))))

(define open-input-channel (open-channel-wrapper #!FALSE))
(define open-output-channel (open-channel-wrapper #!TRUE))

(define close-physical-channel
  (let ((primitive (make-primitive-procedure 'FILE-CLOSE-CHANNEL)))
    (named-lambda (close-physical-channel channel)
      (if (eq? closed-direction
	       (set-channel-direction! channel closed-direction))
	  #!TRUE					;Already closed!
	  (begin (primitive channel)
		 (remove-from-files-vector! channel)
		 (channel-name channel))))))

(define physical-channel-eof?
  (let ((primitive (make-primitive-procedure 'FILE-EOF?)))
    (named-lambda (physical-channel-eof? channel)
      (or (eq? (channel-direction channel) closed-direction)
	  (primitive (primitive (channel-number channel)))))))

(set! close-all-open-files
(named-lambda (close-all-open-files)
  (without-interrupts
   (lambda ()
     (for-each close-physical-channel (all-open-channels))))))

;;; This is a crock -- it will have to be redesigned if we ever have
;;; more than one terminal connected to this system.  Right now if one
;;; just opens these channels (using "CONSOLE:" and "KEYBOARD:" on the
;;; 9836), a regular file channel is opened which is both slower and
;;; will not work when restoring the band.

(define console-output-channel (make-physical-channel 0 "CONSOLE:" #!TRUE))
(define console-input-channel (make-physical-channel 0 "KEYBOARD:" #!FALSE))
(define (get-console-output-channel) console-output-channel)
(define (get-console-input-channel) console-input-channel)

(define (console-channel? channel)
  (zero? (channel-number channel)))

;;;; Files Vector Operations

(define (grow-files-vector! old)
  (without-interrupts
   (lambda ()
     (let ((new (vector-cons (+ buffer-size (vector-length old)) '()))
	   (nm (non-marked-vector-cons
		(+ buffer-size (- (vector-length old) header-size)))))
       (lock-vector! old)
       (let ((num (+ header-size (vector-ref old counter-slot))))
	 (define (loop current)
	   (if (= current num)
	       (begin (clear-vector! new current
				     (+ buffer-size (vector-length old)))
		      (vector-set! (get-fixed-objects-vector) open-files-slot
				   new)
		      (unlock-vector! old)
		      (unlock-vector! new))	;Must be done when installed!
	       (begin (vector-set! new current (vector-ref old current))
		      (loop (1+ current)))))
	 (vector-set! new counter-slot (vector-ref old counter-slot))
	 (insert-non-marked-vector! new file-vector-slot nm)
	 (lock-vector! new)		;If GC occurs it will be alright
	 (loop header-size)
	 new)))))

(define (add-file! file open-files)
  (without-interrupts
   (lambda ()
     (lock-vector! open-files)
     (vector-set! open-files
		  (+ header-size
		     (vector-set! open-files
				  counter-slot
				  (1+ (vector-ref open-files counter-slot))))
		  file)
     (unlock-vector! open-files))))
      
(define (remove-from-files-vector! file)
  (without-interrupts
   (lambda ()
     (let ((open-files (vector-ref (get-fixed-objects-vector)
				   open-files-slot)))
       (lock-vector! open-files)
       (let ((max (+ header-size (vector-ref open-files counter-slot))))
	 (define (loop count)
	      (cond ((= count max)
		     (unlock-vector! open-files)
		     (error "Not an i/o channel" 'CLOSE-CHANNEL file))
		    ((eq? file (vector-ref open-files count))
		     (let inner ((count (1+ count)))
			  (if (= count max)
			      (begin
			       (vector-set! open-files
					    counter-slot
					    (-1+
					     (vector-ref open-files
							 counter-slot)))
			       (vector-set! open-files (-1+ count) '()))
			      (begin
			       (vector-set! open-files
					    (-1+ count)
					    (vector-ref open-files count))
			       (inner (1+ count))))))
		    (else (loop (1+ count)))))
	 (loop header-size)
	 (unlock-vector! open-files))))))

(define (clear-vector! v start end)
  (without-interrupts
   (lambda ()
     (subvector-fill! v start end '()))))

(define (all-open-channels)
  (let ((files-vector (vector-ref (get-fixed-objects-vector) open-files-slot)))
    (without-interrupts
     (lambda ()
       (lock-vector! files-vector)
       (let ((result
	      (subvector->list files-vector
			       header-size
			       (+ header-size
				  (vector-ref files-vector counter-slot)))))
	 (unlock-vector! files-vector)
	 result)))))
  
(define ((locker flag) v)
  (with-interrupts-reduced INTERRUPT-MASK-NONE
   (lambda (old-mask)
     (vector-set! v
		  file-vector-slot
		  (&make-object flag
				(vector-ref v file-vector-slot)))
     #!TRUE)))				; Guarantee a good value returned

(define lock-vector!
  (locker (microcode-type 'NULL)))

(define unlock-vector!
  (locker (microcode-type 'MANIFEST-SPECIAL-NM-VECTOR)))

(define (setup-files-vector)
  (let ((base-vector (vector-cons (+ default-size header-size) '())))
    (vector-set! base-vector counter-slot 0)
    (insert-non-marked-vector! base-vector file-vector-slot
			       (non-marked-vector-cons default-size))
;   (lock-vector! base-vector)
    (clear-vector! base-vector header-size (+ default-size header-size))
    (vector-set! (get-fixed-objects-vector) open-files-slot base-vector)
    (unlock-vector! base-vector)))

;;; end PRIMITIVE-IO package.
))

((access setup-files-vector primitive-io))
(add-gc-daemon! (make-primitive-procedure 'CLOSE-LOST-OPEN-FILES))

(add-gc-daemon! (access close-lost-open-files-daemon primitive-io))