;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/io.scm,v 13.45 1987/04/13 18:43:17 cph Rel $
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
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Input/output utilities

(declare (usual-integrations))

(define close-all-open-files)

(define primitive-io
  (let ((open-file-list-tag '*ALL-THE-OPEN-FILES*)

	(weak-cons-type (microcode-type 'WEAK-CONS))

	(make-physical-channel (make-primitive-procedure 'HUNK3-CONS))
	(channel-descriptor system-hunk3-cxr0)
	(set-channel-descriptor! system-hunk3-set-cxr0!)
	(channel-name system-hunk3-cxr1)
	(channel-direction system-hunk3-cxr2)
	(set-channel-direction! system-hunk3-set-cxr2!)

	(closed-direction 0)
	(closed-descriptor false))

    (make-environment
    
(declare (integrate-primitive-procedures
	  (make-physical-channel hunk3-cons)
	  (channel-descriptor system-hunk3-cxr0)
	  (set-channel-descriptor! system-hunk3-set-cxr0!)
	  (channel-name system-hunk3-cxr1)
	  (channel-direction system-hunk3-cxr2)
	  (set-channel-direction! system-hunk3-set-cxr2!)))

(define open-files-list)
(define traversing?)
    
(define (initialize)
  (set! open-files-list (list open-file-list-tag))
  (set! traversing? false)
  true)

;;;; Open/Close Files

;;;  Direction is one of the following:
;;;     - true:   output channel
;;;	- false:  input channel
;;;	- 0:	  closed channel

(define open-channel-wrapper
  (let ((open-channel (make-primitive-procedure 'FILE-OPEN-CHANNEL)))
    (named-lambda ((open-channel-wrapper direction) filename)
      (without-interrupts
       (lambda ()
	 (let ((channel
		(make-physical-channel (open-channel filename direction)
				       filename
				       direction)))
	   (with-interrupt-mask interrupt-mask-none ; Disallow gc
	    (lambda (ie)
	      (set-cdr! open-files-list
			(cons (system-pair-cons weak-cons-type
						channel
						(channel-descriptor channel))
			      (cdr open-files-list)))))
	   channel))))))

(define open-input-channel (open-channel-wrapper false))
(define open-output-channel (open-channel-wrapper true))

;; This is locked from interrupts, but GC can occur since the
;; procedure itself hangs on to the channel until the last moment,
;; when it returns the channel's name.  The list will not be spliced
;; by the daemon behind its back because of the traversing? flag.

(define close-physical-channel
  (let ((primitive (make-primitive-procedure 'FILE-CLOSE-CHANNEL)))
    (named-lambda (close-physical-channel channel)
      (fluid-let ((traversing? true))
	(without-interrupts
	 (lambda ()
	   (if (eq? closed-direction
		    (set-channel-direction! channel closed-direction))
	       true			;Already closed!
	       (begin
		 (primitive (set-channel-descriptor! channel
						     closed-descriptor))
		 (let loop
		     ((l1 open-files-list)
		      (l2 (cdr open-files-list)))
		   (cond ((null? l2)
			  (set! traversing? false)
			  (error "CLOSE-PHYSICAL-CHANNEL: lost channel"
				 channel))
			 ((eq? channel (system-pair-car (car l2)))
			  (set-cdr! l1 (cdr l2))
			  (channel-name channel))
			 (else
			  (loop l2 (cdr l2)))))))))))))

;;;; Finalization and daemon.

(define (close-files action)
  (lambda ()
    (fluid-let ((traversing? true))
      (without-interrupts
       (lambda ()
	 (let loop ((l (cdr open-files-list)))
	   (cond ((null? l) true)
		 (else
		  (let ((channel (system-pair-car (car l))))
		    (if (not (eq? channel false))
			(begin
			  (set-channel-descriptor! channel
						   closed-descriptor)
			  (set-channel-direction! channel
						  closed-direction)))
		    (action (system-pair-cdr (car l)))
		    (set-cdr! open-files-list (cdr l)))
		  (loop (cdr open-files-list))))))))))

;;; This is invoked before disk-restoring.  It "cleans" the microcode.

(set! close-all-open-files
  (close-files (make-primitive-procedure 'FILE-CLOSE-CHANNEL)))

;;; This is invoked after disk-restoring.  It "cleans" the new runtime system.

(define reset!
  (close-files (lambda (ignore) true)))

;; This is the daemon which closes files which no one points to.
;; Runs with GC, and lower priority interrupts, disabled.
;; It is unsafe because of the (unnecessary) consing by the
;; interpreter while it executes the loop.

;; Replaced by a primitive installed below.

#|

(define close-lost-open-files-daemon
  (let ((primitive (make-primitive-procedure 'FILE-CLOSE-CHANNEL)))
    (named-lambda (close-lost-open-files-daemon)
      (if (not traversing?)
	  (let loop
	      ((l1 open-files-list)
	       (l2 (cdr open-files-list)))
	    (cond ((null? l2)
		   true)
		  ((null? (system-pair-car (car l2)))
		   (primitive (system-pair-cdr (car l2)))
		   (set-cdr! l1 (cdr l2))
		   (loop l1 (cdr l1)))
		  (else
		   (loop l2 (cdr l2)))))))))

|#

(define close-lost-open-files-daemon
  (let ((primitive (make-primitive-procedure 'CLOSE-LOST-OPEN-FILES)))
    (named-lambda (close-lost-open-files-daemon)
      (if (not traversing?)
	  (primitive open-files-list)))))

;;; End of PRIMITIVE-IO package.
)))

((access initialize primitive-io))
(add-gc-daemon! (access close-lost-open-files-daemon primitive-io))