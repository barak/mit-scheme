;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/regcom.scm,v 1.15 1989/03/14 08:02:00 cph Exp $
;;;
;;;	Copyright (c) 1987, 1989 Massachusetts Institute of Technology
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

;;;; Register Commands

(declare (usual-integrations))

(define-command ("Point to Register")
  "Store current location of point in a register."
  (set-register! (prompt-for-char "Point to Register")
		 (make-buffer-position (current-point) (current-buffer))))

(define-command ("Register to Point")
  "Move point to location stored in a register."
  (let ((register (prompt-for-char "Register to Point")))
    (let ((value (get-register register)))
      (if (not (buffer-position? value))
	  (register-error register "does not contain a buffer position."))
      (select-buffer
       (or (buffer-position-buffer value)
	   (register-error register
			   "points to a buffer which has been deleted")))
      (set-current-point! (buffer-position-mark value)))))

(define-command ("Number to Register" argument)
  "Store a number in a given register.
With prefix arg, stores that number in the register.
Otherwise, reads digits from the buffer starting at point."
  (set-register! (prompt-for-char "Number to Register")
		 (or argument
		     (let ((start (current-point))
			   (end (skip-chars-forward "[0-9]")))
		       (if (mark= start end)
			   0
			   (with-input-from-region (make-region start end)
						   read))))))

(define-command ("Increment Register" (argument 1))
  "Add the prefix arg to the contents of a given register.
The prefix defaults to one."
  (let ((register (prompt-for-char "Increment Register")))
    (let ((value (get-register register)))
      (if (not (integer? value))
	  (register-error register "does not contain a number"))
      (set-register! register (+ value argument)))))

(define-command ("Copy to Register" argument)
  "Copy region into given register.
With prefix arg, delete as well."
  (let ((region (current-region)))
    (set-register! (prompt-for-char "Copy to Register")
		   (region->string region))
    (if argument (region-delete! region))))

(define-command ("Insert Register" argument)
  "Insert contents of given register at point.
Normally puts point before and mark after the inserted text.
With prefix arg, puts mark before and point after."
  ((if argument unkill-reversed unkill)
   (let ((value (get-register (prompt-for-char "Insert Register"))))
     (cond ((string? value) value)
	   ((integer? value) (write-to-string value))
	   (else (register-error "does not contain text"))))))

(define-command ("Append to Register" argument)
  "Append region to text in given register.
With prefix arg, delete as well."
  (let ((region (current-region))
	(register (prompt-for-char "Append to Register")))
    (let ((value (get-register register)))
      (if (not (string? value))
	  (register-error register "does not contain text"))
      (set-register! register (string-append value (region->string region))))
    (if argument (region-delete! region))))

(define-command ("Prepend to Register" argument)
  "Prepend region to text in given register.
With prefix arg, delete as well."
  (let ((region (current-region))
	(register (prompt-for-char "Prepend to Register")))
    (let ((value (get-register register)))
      (if (not (string? value))
	  (editor-error register "does not contain text"))
      (set-register! register (string-append (region->string region) value)))
    (if argument (region-delete! region))))

(define-command ("View Register")
  "Display what is contained in a given register."
  (let ((register (prompt-for-char "View Register")))
    (let ((value (get-register register)))
      (if (not value)
	  (message "Register " (char-name register) " is empty")
	  (with-output-to-temporary-buffer "*Output*"
	    (lambda ()
	      (write-string "Register ")
	      (write-string (char-name register))
	      (write-string " contains ")
	      (cond ((integer? value)
		     (write value))
		    ((string? value)
		     (write-string "the string:\n")
		     (write-string value))
		    ((buffer-position? value)
		     (let ((buffer (buffer-position-buffer value)))
		       (if (not buffer)
			   (write-string "an invalid buffer position")
			   (begin
			    (write-string "a buffer position:\nbuffer ")
			    (write-string (buffer-name buffer))
			    (write-string ", position ")
			    (write
			     (mark-index (buffer-position-mark value)))))))
		    (else
		     (write-string "a random object:\n")
		     (write value)))))))))

(define (register-error register . strings)
  (apply editor-error "Register " (char-name register) " " strings))

(define (get-register char)
  (let ((entry (assv char register-alist)))
    (and entry
	 (cdr entry))))

(define (set-register! char value)
  (let ((entry (assv char register-alist)))
    (if entry
	(set-cdr! entry value)
	(set! register-alist
	      (cons (cons char value)
		    register-alist)))))

(define register-alist
  '())

(define (make-buffer-position mark buffer)
  (cons buffer-position-tag (cons mark (hash buffer))))

(define (buffer-position? object)
  (and (pair? object)
       (eq? buffer-position-tag (car object))))

(define buffer-position-tag
  "Buffer Position")

(define-integrable (buffer-position-mark position)
  (cadr position))

(define-integrable (buffer-position-buffer position)
  (unhash (cddr position)))