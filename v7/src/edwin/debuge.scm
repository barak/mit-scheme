;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/debuge.scm,v 1.36 1989/04/15 00:48:15 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Debugging Stuff

(declare (usual-integrations))

(define (debug-save-files)
  (for-each debug-save-buffer
	    (bufferset-buffer-list (editor-bufferset edwin-editor))))

(define (debug-save-buffer buffer)
  (if (and (buffer-modified? buffer)
	   (buffer-writeable? buffer))
      (let ((pathname
	     (let ((pathname (buffer-pathname buffer)))
	       (cond ((not pathname)
		      (and (y-or-n? "Save buffer "
				    (buffer-name buffer)
				    " (Y or N)? ")
			   (begin
			     (newline)
			     (write-string "Filename: ")
			     (->pathname (read)))))
		     ((integer? (pathname-version pathname))
		      (pathname-new-version pathname 'NEWEST))
		     (else
		      pathname)))))
	(if pathname
	    (let ((truename (pathname->output-truename pathname)))
	      (let ((filename (pathname->string truename)))
		(if (or (not (file-exists? filename))
			(y-or-n? "File '"
				 (pathname->string pathname)
				 "' exists.  Write anyway (Y or N)? "))
		    (begin
		      (newline)
		      (write-string "Writing file '")
		      (write-string filename)
		      (write-string "'")
		      (region->file (buffer-region buffer) filename)
		      (write-string " -- done")
		      (set-buffer-pathname! buffer pathname)
		      (set-buffer-truename! buffer truename)
		      (buffer-not-modified! buffer)))))))))

(define-command debug-show-rings
  ""
  ()
  (lambda ()
    (message "Mark Ring: "
	     (write-to-string (ring-size (buffer-mark-ring (current-buffer))))
	     "; Kill Ring: "
	     (write-to-string (ring-size (current-kill-ring))))))

(define-command debug-count-marks
  ""
  ()
  (lambda ()
    (count-marks-group (buffer-group (current-buffer))
		       (lambda (n-existing n-gced)
			 (message "Existing: " (write-to-string n-existing)
				  "; GCed: " (write-to-string n-gced))))))

(define (count-marks-group group receiver)
  (let loop ((marks (group-marks group)) (receiver receiver))
    (if (weak-pair? marks)
	(loop (weak-cdr marks)
	  (lambda (n-existing n-gced)
	    (if (weak-pair/car? marks)
		(receiver (1+ n-existing) n-gced)
		(receiver n-existing (1+ n-gced)))))
	(receiver 0 0))))
;;;; Object System Debugging

(define (po object)
  (for-each (lambda (entry)
	      (newline)
	      (write (car entry))
	      (write-string ": ")
	      (write (vector-ref object (cdr entry))))
	    (class-instance-transforms (object-class object))))

(define (instance-ref object name)
  (let ((entry (assq name (class-instance-transforms (object-class object)))))
    (if entry
	(vector-ref object (cdr entry))
	(error "Not a valid instance-variable name" name))))

(define (instance-set! object name value)
  (let ((entry (assq name (class-instance-transforms (object-class object)))))
    (if entry
	(vector-set! object (cdr entry) value)
	(error "Not a valid instance-variable name" name))))