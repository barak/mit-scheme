;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/system.scm,v 13.46 1987/04/27 17:33:22 cph Exp $
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

;;;; Systems

(declare (usual-integrations))

;;; (DISK-SAVE  filename #!optional identify)
;;; (DUMP-WORLD filename #!optional identify)
;;; Saves a world image in FILENAME.  IDENTIFY has the following meaning:
;;;
;;;    [] Not supplied => ^G on restore (normal for saving band).
;;;    [] String => New world ID message, and ^G on restore.
;;;    [] Otherwise => Returns normally (very useful for saving bugs!).
;;;
;;; The image saved by DISK-SAVE does not include the "microcode", the
;;; one saved by DUMP-WORLD does, and is an executable file.

(define disk-save)
(define dump-world)
(define event:after-restore)
(define event:after-restart)
(define full-quit)
(define identify-world)
(define identify-system)
(define add-system!)
(define add-secondary-gc-daemon!)
(let ()

(define world-identification "Scheme")
(define known-systems '())
(define secondary-gc-daemons '())
(define date-world-saved)
(define time-world-saved)

(define (restart-world)
  (screen-clear)
  (abort->top-level
   (lambda ()
     (identify-world)
     (event:after-restart))))

(define (setup-image save-image)
  (lambda (filename #!optional identify)
    (let ((d (date)) (t (time)))
      (gc-flip)
      ((access trigger-daemons garbage-collector-package) secondary-gc-daemons)
      (save-image filename
		  (lambda (ie)
		    (set-interrupt-enables! ie)
		    (set! date-world-saved d)
		    (set! time-world-saved t)
		    *the-non-printing-object*)
		  (lambda (ie)
		    (set-interrupt-enables! ie)
		    (set! date-world-saved d)
		    (set! time-world-saved t)
		    (event:after-restore)
		    (cond ((unassigned? identify)
			   (restart-world))
			  ((string? identify)
			   (set! world-identification identify)
			   (restart-world))
			  (else
			   *the-non-printing-object*)))))))

(set! disk-save
      (setup-image save-world))

(set! dump-world
      (setup-image
       (let ((primitive (make-primitive-procedure 'DUMP-WORLD true)))
	 (lambda (filename after-dumping after-restoring)
	   (let ((ie (set-interrupt-enables! interrupt-mask-none)))
	     ((if (primitive filename)
		  after-restoring
		  after-dumping)
	      ie))))))

(set! event:after-restore (make-event-distributor))
(set! event:after-restart (make-event-distributor))

(add-event-receiver! event:after-restart
 (lambda ()
   (if (not (unassigned? init-file-pathname))
       (let ((file
	      (or (pathname->input-truename
		   (merge-pathnames init-file-pathname
				    (working-directory-pathname)))
		  (pathname->input-truename
		   (merge-pathnames init-file-pathname
				    (home-directory-pathname))))))
	 (if (not (null? file))
	     (load file user-initial-environment))))))

;; This is not the right place for this, but I don't know what is.

(add-event-receiver!
 event:after-restore
 (lambda ()
   ((access reset! continuation-package))))

(set! full-quit
(named-lambda (full-quit)
  (quit)
  (restart-world)))

(set! identify-world
(named-lambda (identify-world)
  (newline)
  (write-string world-identification)
  (write-string " saved on ")
  (write-string (apply date->string date-world-saved))
  (write-string " at ")
  (write-string (apply time->string time-world-saved))
  (newline)
  (write-string "  Release ")
  (write-string (access :release microcode-system))
  (for-each identify-system known-systems)))

(set! identify-system
(named-lambda (identify-system system)
  (newline)
  (write-string "  ")
  (write-string (access :name system))
  (write-string " ")
  (write (access :version system))
  (let ((mod (access :modification system)))
    (if mod
	(begin (write-string ".")
	       (write mod))))))

(set! add-system!
(named-lambda (add-system! system)
  (set! known-systems (append! known-systems (list system)))))

(set! add-secondary-gc-daemon!
(named-lambda (add-secondary-gc-daemon! daemon)
  (if (not (memq daemon secondary-gc-daemons))
      (set! secondary-gc-daemons (cons daemon secondary-gc-daemons)))))

)

;;; Load the given system, which must have the following variables
;;; defined:
;;;
;;; :FILES which will be assigned the list of filenames actually
;;; loaded.
;;;
;;; :FILES-LISTS which should contain a list of pairs, the car of each
;;; pair being an environment, and the cdr a list of filenames.  The
;;; files are loaded in the order specified, into the environments
;;; specified.  COMPILED?, if false, means change all of the file
;;; types to "BIN".

(define load-system!)
(let ()

(set! load-system!
  (named-lambda (load-system! system #!optional compiled?)
    (if (unassigned? compiled?) (set! compiled? (query "Load compiled")))
    (define (loop files)
      (if (null? files)
	  '()
	  (split-list files 20
	    (lambda (head tail)
	      (let ((scode (map fasload head)))
		(newline)
		(write-string "Purify")
		(purify (list->vector scode) true)
		(append! scode (loop tail)))))))
    (let ((files (format-files-list (access :files-lists system) compiled?)))
      (set! (access :files system)
	    (map (lambda (file) (pathname->string (car file))) files))
      (for-each (lambda (file scode)
		  (newline) (write-string "Eval ")
		  (write (pathname->string (car file)))
		  (scode-eval scode (cdr file)))
		files
		(loop (map car files)))
      (newline)
      (write-string "Done"))
    (add-system! system)
    *the-non-printing-object*))

(define (split-list list n receiver)
  (if (or (not (pair? list)) (zero? n))
      (receiver '() list)
      (split-list (cdr list) (-1+ n)
	(lambda (head tail)
	  (receiver (cons (car list) head) tail)))))

(define (format-files-list files-lists compiled?)
  (mapcan (lambda (files-list)
	    (map (lambda (filename)
		   (let ((pathname (->pathname filename)))
		     (cons (if compiled?
			       pathname
			       (pathname-new-type pathname "bin"))
			   (car files-list))))
		 (cdr files-list)))
	  files-lists))

(define (query prompt)
  (newline)
  (write-string prompt)
  (write-string " (Y or N)? ")
  (let ((char (char-upcase (read-char))))
    (cond ((char=? #\Y char)
	   (write-string "Yes")
	   true)
	  ((char=? #\N char)
	   (write-string "No")
	   false)
	  (else (beep) (query prompt)))))

)