#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Save/Restore World
;;; package: (runtime save/restore)

(declare (usual-integrations))

;;; (DISK-SAVE  filename #!optional identify)
;;; Saves a world image in FILENAME.  IDENTIFY has the following meaning:
;;;
;;;    [] Not supplied => ^G on restore (normal for saving band).
;;;    [] String => New world ID message, and ^G on restore.
;;;    [] #F => Returns normally on restore; value is true iff restored.
;;;    [] Otherwise => Returns normally, running `event:after-restart'.
;;;
;;; The image saved by DISK-SAVE does not include the "microcode".

(define world-id "Image")
(define time-world-saved #f)
(define *within-restore-window?* #f)

(define (disk-save filename #!optional id)
  (let ((filename (->namestring (merge-pathnames filename)))
	(id (if (default-object? id) world-id id))
	(time (local-decoded-time)))
    (gc-clean)
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
		    (with-interrupt-mask interrupt-mask/gc-ok
		      (lambda (interrupt-mask)
			interrupt-mask
			(gc-flip)
			(do ()
			    (((ucode-primitive dump-band) restart filename))
			  (with-simple-restart 'RETRY "Try again."
			    (lambda ()
			      (error "Disk save failed:" filename))))
			(continuation
			 (lambda ()
			   (set! time-world-saved time)
			   (if (string? id) unspecific #f)))))))
		 ((ucode-primitive set-fixed-objects-vector!) fixed-objects))))
	   (re-read-microcode-tables!)
	   (lambda ()
	     (set! time-world-saved time)
	     (fluid-let ((*within-restore-window?* #t))
	       (event-distributor/invoke! event:after-restore))
	     (start-thread-timer)
	     (cond ((string? id)
		    (set! world-id id)
		    (abort->top-level
		     (lambda (cmdl)
		       (if (not (cmdl/batch-mode? cmdl))
			   (identify-world (cmdl/port cmdl)))
		       (event-distributor/invoke! event:after-restart))))
		   ((not id)
		    #t)
		   (else
		    (event-distributor/invoke! event:after-restart)
		    #t))))))))))

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

(define (identify-world #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'IDENTIFY-WORLD))))
    (write-mit-scheme-copyright port)
    (newline port)
    (write-mit-scheme-license port "" #t)
    (newline port)
    (newline port)
    (if time-world-saved
	(begin
	  (write-string world-id port)
	  (write-string " saved on " port)
	  (write-string (decoded-time/date-string time-world-saved) port)
	  (write-string " at " port)
	  (write-string (decoded-time/time-string time-world-saved) port)
	  (newline port)))
    (write-strings-in-columns (map get-subsystem-identification-string
				   (get-subsystem-names))
			      port
			      #t
			      1
			      "  "
			      " || "
			      "")))