#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/io.scm,v 14.5 1990/04/10 20:05:13 cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; Input/output utilities
;;; package: (runtime primitive-io)

(declare (usual-integrations))

(define (initialize-package!)
  (set! close-all-open-files (close-files file-close-channel))
  (set! primitive-io/reset! (close-files (lambda (ignore) ignore)))
  (set! open-files-list (list 'OPEN-FILES-LIST))
  (set! traversing? false)
  (add-gc-daemon! close-lost-open-files-daemon)
  (add-event-receiver! event:after-restore primitive-io/reset!)
  (add-event-receiver! event:before-exit close-all-open-files))

(define-integrable (make-physical-channel descriptor channel direction)
  (hunk3-cons descriptor channel direction))

(define-integrable (channel-descriptor channel)
  (system-hunk3-cxr0 channel))

(define-integrable (set-channel-descriptor! channel descriptor)
  (system-hunk3-set-cxr0! channel descriptor))

(define-integrable (channel-name channel)
  (system-hunk3-cxr1 channel))

(define-integrable (channel-direction channel)
  (system-hunk3-cxr2 channel))

(define-integrable (set-channel-direction! channel direction)
  (system-hunk3-set-cxr2! channel direction))

(define-primitives
  file-open-channel
  file-close-channel
  close-lost-open-files)

(define-integrable closed-direction 0)
(define-integrable closed-descriptor false)

(define open-files-list)
(define traversing?)

;;;; Open/Close Files

;;;  Direction is one of the following:
;;;	- #f:		input channel
;;;     - #t:		output channel
;;;     - 'append:	append output channel
;;;	- 0:		closed channel

(define (open-channel filename-or-process direction)
  (without-interrupts
   (lambda ()
     (let ((channel
	    (case direction
	      ((#F)
	       (make-physical-channel
		(if (process? filename-or-process)
		    (process-get-input-channel filename-or-process)
		    (file-open-channel filename-or-process direction))
		filename-or-process
		direction))
	      ((#T)
	       (make-physical-channel
		(if (process? filename-or-process)
		    (process-get-output-channel filename-or-process)
		    (file-open-channel filename-or-process direction))
		filename-or-process
		direction))
	      (else
	       (if (process? filename-or-process)
		   (error "Can't open process channel for append"
			  filename-or-process))
	       (make-physical-channel
		(file-open-channel filename-or-process 'APPEND)
		filename-or-process
		#T)))))
       (with-absolutely-no-interrupts
	(lambda ()
	  (set-cdr! open-files-list
		    (cons (system-pair-cons (ucode-type weak-cons)
					    channel
					    (channel-descriptor channel))
			  (cdr open-files-list)))))
       channel))))

(define (open-input-channel filename-or-process)
  (open-channel filename-or-process false))

(define (open-output-channel filename-or-process)
  (open-channel filename-or-process true))

(define (open-append-channel filename)
  (open-channel filename 'APPEND))

;;; This is locked from interrupts, but GC can occur since the
;;; procedure itself hangs on to the channel until the last moment,
;;; when it returns the channel's name.  The list will not be spliced
;;; by the daemon behind its back because of the traversing? flag.

(define (close-physical-channel channel)
  (fluid-let ((traversing? true))
    (without-interrupts
     (lambda ()
       (if (eq? closed-direction (channel-direction channel))
	   true				;Already closed!
	   (begin
	     (file-close-channel (channel-descriptor channel))
	     (set-channel-direction! channel closed-direction)
	     (set-channel-descriptor! channel closed-descriptor)
	     (let loop
		 ((l1 open-files-list)
		  (l2 (cdr open-files-list)))
	       (cond ((null? l2)
		      (set! traversing? false)
		      (error "CLOSE-PHYSICAL-CHANNEL: lost channel" channel))
		     ((eq? channel (system-pair-car (car l2)))
		      (set-cdr! l1 (cdr l2))
		      (channel-name channel))
		     (else
		      (loop l2 (cdr l2)))))))))))

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
(define close-all-open-files)

;;; This is invoked after disk-restoring.  It "cleans" the new runtime system.
(define primitive-io/reset!)

;;; This is the daemon which closes files which no one points to.
;;; Runs with GC, and lower priority interrupts, disabled.
;;; It is unsafe because of the (unnecessary) consing by the
;;; interpreter while it executes the loop.

;;; Replaced by a primitive installed below.
#|
(define (close-lost-open-files-daemon)
  (if (not traversing?)
      (let loop ((l1 open-files-list) (l2 (cdr open-files-list)))
	(cond ((null? l2)
	       true)
	      ((null? (system-pair-car (car l2)))
	       (file-close-channel (system-pair-cdr (car l2)))
	       (set-cdr! l1 (cdr l2))
	       (loop l1 (cdr l1)))
	      (else
	       (loop l2 (cdr l2)))))))
|#
(define (close-lost-open-files-daemon)
  (if (not traversing?)
      (close-lost-open-files open-files-list)))