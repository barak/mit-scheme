#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/savres.scm,v 14.9 1989/06/09 16:51:40 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Save/Restore World
;;; package: (runtime save/restore)

(declare (usual-integrations))

;;; (DISK-SAVE  filename #!optional identify)
;;; (DUMP-WORLD filename #!optional identify)
;;; Saves a world image in FILENAME.  IDENTIFY has the following meaning:
;;;
;;;    [] Not supplied => ^G on restore (normal for saving band).
;;;    [] String => New world ID message, and ^G on restore.
;;;    [] #F => Returns normally on restore; value is true iff restored.
;;;    [] Otherwise => Returns normally, running `event:after-restart'.
;;;
;;; The image saved by DISK-SAVE does not include the "microcode", the
;;; one saved by DUMP-WORLD does, and is an executable file.

(define (initialize-package!)
  (set! disk-save (setup-image disk-save/kernel))
  (set! dump-world (setup-image dump-world/kernel)))

(define disk-save)
(define dump-world)

(define (setup-image save-image)
  (lambda (filename #!optional identify)
    (let ((identify
	   (if (default-object? identify) world-identification identify))
	  (time (get-decoded-time)))
      (discard-debugging-info!)
      (gc-flip)
      (trigger-secondary-gc-daemons!)
      (save-image filename
		  (lambda ()
		    (set! time-world-saved time)
		    (if (string? identify)
			unspecific
			false))
		  (lambda ()
		    (set! time-world-saved time)
		    (event-distributor/invoke! event:after-restore)
		    (if (string? identify)
			(begin
			  (set! world-identification identify)
			  (clear console-output-port)
			  (abort->top-level
			   (lambda (cmdl)
			     (identify-world (cmdl/output-port cmdl))
			     (event-distributor/invoke! event:after-restart))))
			(begin
			  (event-distributor/invoke! event:after-restart)
			  true)))))))

(define (disk-save/kernel filename after-suspend after-restore)
  ((without-interrupts
    (lambda ()
      (call-with-current-continuation
       (lambda (continuation)
	 (let ((fixed-objects (get-fixed-objects-vector))
	       (dynamic-state (current-dynamic-state)))
	   (fluid-let ()
	     ((ucode-primitive call-with-current-continuation)
	      (lambda (restart)
		(gc-flip)
		(let loop ()
		  (if (not ((ucode-primitive dump-band)
			    restart
			    (canonicalize-output-filename filename)))
		      (begin
			(error "Disk save failed: (PROCEED 0) to retry")
			(loop))))
		(continuation after-suspend)))
	     ((ucode-primitive set-fixed-objects-vector!) fixed-objects)
	     (set-current-dynamic-state! dynamic-state)
	     ;; This instruction is a noop, so I flushed it -- cph.
	     ;; (enable-interrupts! interrupt-mask/none)
	     (read-microcode-tables!)
	     after-restore))))))))

(define (dump-world/kernel filename after-suspend after-restore)
  ((with-absolutely-no-interrupts
    (lambda ()
      (if ((ucode-primitive dump-world 1) filename)
	  after-restore
	  after-suspend)))))

(define (disk-restore #!optional filename)
  (if (default-object? filename)
      (set! filename
	    (or ((ucode-primitive reload-band-name))
		(error "DISK-RESTORE: No default band name available"))))
  (event-distributor/invoke! event:before-exit)
  ((ucode-primitive load-band) (canonicalize-input-filename filename)))
(define world-identification "Scheme")
(define time-world-saved)

(define (identify-world #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port))))
    (newline port)
    (write-string world-identification port)
    (if time-world-saved
	(begin
	  (write-string " saved on " port)
	  (write-string (decoded-time/date-string time-world-saved) port)
	  (write-string " at " port)
	  (write-string (decoded-time/time-string time-world-saved) port)))
    (newline port)
    (write-string "  Release " port)
    (write-string microcode-id/release-string port)
    (for-each-system!
     (lambda (system)
       (newline port)
       (write-string "  " port)
       (write-string (system/identification-string system) port)))))