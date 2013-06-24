#| -*-Scheme-*-

$Id: savres.scm,v 14.34 2001/10/05 19:11:15 cph Exp $

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

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
  (set! dump-world (setup-image dump-world/kernel))
  unspecific)

(define disk-save)
(define dump-world)
(define *within-restore-window?* #f)

(define (setup-image save-image)
  (lambda (filename #!optional identify)
    (let ((identify
	   (if (default-object? identify) world-identification identify))
	  (time (local-decoded-time)))
      (gc-clean)
      (save-image
       filename
       (lambda ()
	 (set! time-world-saved time)
	 (if (string? identify) unspecific #f))
       (lambda ()
	 (set! time-world-saved time)
	 (fluid-let ((*within-restore-window?* #t))
	   (event-distributor/invoke! event:after-restore))
	 (start-thread-timer)
	 (cond ((string? identify)
		(set! world-identification identify)
		(abort->top-level
		 (lambda (cmdl)
		   (identify-world (cmdl/port cmdl))
		   (event-distributor/invoke! event:after-restart))))
	       ((not identify)
		#t)
	       (else
		(event-distributor/invoke! event:after-restart)
		#t)))))))

(define (disk-save/kernel filename after-suspend after-restore)
  (let ((filename (->namestring (merge-pathnames filename))))
    ((without-interrupts
      (lambda ()
	(call-with-current-continuation
	 (lambda (continuation)
	   ;; GC cannot be allowed before the fixed-objects-vector
	   ;; is reset after restoring.
	   (with-absolutely-no-interrupts
	       (lambda ()
		 (let ((fixed-objects (get-fixed-objects-vector)))
		   ((ucode-primitive call-with-current-continuation)
		    (lambda (restart)
		      (without-interrupts
		       (lambda ()
			 (gc-flip)
			 (do ()
			     (((ucode-primitive dump-band) restart filename))
			   (with-simple-restart 'RETRY "Try again."
			     (lambda ()
			       (error "Disk save failed:" filename))))
			 (continuation after-suspend)))))
		   ((ucode-primitive set-fixed-objects-vector!)
		    fixed-objects))))
	   (re-read-microcode-tables!)
	   after-restore)))))))

(define (dump-world/kernel filename after-suspend after-restore)
  (gc-flip)
  ((with-absolutely-no-interrupts
    (lambda ()
      (if ((ucode-primitive dump-world 1) filename)
	  after-restore
	  after-suspend)))))

(define (disk-restore #!optional filename)
  ;; Force order of events -- no need to run event:before-exit if
  ;; there's an error here.
  (let ((filename
	 (->namestring
	  (if (default-object? filename)
	      (merge-pathnames
	       (let ((filename ((ucode-primitive reload-band-name))))
		 (if (not filename)
		     (error "no default band name available"))
		 filename))
	      (let ((pathname (->pathname filename))
		    (try
		     (lambda (pathname)
		       (let ((pathname (merge-pathnames pathname)))
			 (and (file-exists? pathname)
			      pathname)))))
		(or (try pathname)
		    (if (pathname-type pathname)
			(system-library-pathname pathname)
			(let ((pathname (pathname-new-type pathname "com")))
			  (or (try pathname)
			      (system-library-pathname pathname))))))))))
    (event-distributor/invoke! event:before-exit)
    ((ucode-primitive load-band) filename)))

(define world-identification "Scheme")
(define time-world-saved #f)

(define (identify-world #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port))))
    (write-string world-identification port)
    (if time-world-saved
	(begin
	  (write-string " saved on " port)
	  (write-string (decoded-time/date-string time-world-saved) port)
	  (write-string " at " port)
	  (write-string (decoded-time/time-string time-world-saved) port)))
    (newline port)
    (for-each (lambda (name)
		(write-string "  " port)
		(write-string (get-subsystem-identification-string name) port)
		(newline port))
	      (get-subsystem-names))))