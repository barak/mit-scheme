;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;;; Auto Save

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-variable "Auto Save Visited File"
  "If not false, auto save into the visited file."
  #!FALSE)

(define-variable "Auto Save Default"
  "If not false, auto save all visited files."
  #!TRUE)

(define-variable "Auto Save Interval"
  "The number of keystrokes between auto saves."
  300)

(define-variable "Delete Auto Save Files"
  "If not false, delete auto save files when normal saves happen."
  #!FALSE)

(define-command ("Auto Save Mode" argument)
  "Toggle Auto Save mode.
With argument, turn Auto Save mode on iff argument is positive."
  (let ((buffer (current-buffer)))
    (if (if argument
	    (positive? argument)
	    (not (buffer-auto-save-pathname buffer)))
	(begin (enable-buffer-auto-save! buffer)
	       (temporary-message "Auto Save enabled"))
	(begin (disable-buffer-auto-save! buffer)
	       (temporary-message "Auto Save disabled")))))

(define (setup-buffer-auto-save! buffer)
  (if (ref-variable "Auto Save Default")
      (enable-buffer-auto-save! buffer)
      (disable-buffer-auto-save! buffer)))

(define (enable-buffer-auto-save! buffer)
  (define (set-to-string dirpath string)
    ;; **** Crock ****
    (if (> (string-length string) 15) (set-string-length! string 15))
    (set-buffer-auto-save-pathname!
     buffer
     (merge-pathnames dirpath
		      (string->pathname (string-append "&" string)))))
  (let ((pathname (buffer-pathname buffer)))
    (cond ((not pathname)
	   (set-to-string (working-directory-pathname)
			  (string-append "%" (buffer-name buffer))))
	  ((ref-variable "Auto Save Visited File")
	   (set-buffer-auto-save-pathname! buffer pathname))
	  (else
	   (set-to-string
	    (pathname-extract pathname 'DEVICE 'DIRECTORY)
	    (pathname->string (pathname-extract pathname 'NAME 'TYPE)))))))

(define (disable-buffer-auto-save! buffer)
  (set-buffer-auto-save-pathname! buffer #!FALSE))

(define *auto-save-keystroke-count*)

(define (do-auto-save)
  (let ((buffers
	 (list-transform-positive (buffer-list)
	   (lambda (buffer)
	     (and (buffer-auto-save-pathname buffer)
		  (buffer-auto-save-modified? buffer)
		  (<= (* 10 (buffer-save-length buffer))
		      (* 13 (buffer-length buffer))))))))
    (if (not (null? buffers))
	(begin (temporary-message "Auto saving...")
	       (for-each auto-save-buffer buffers)
	       (clear-message))))
  (set! *auto-save-keystroke-count* 0))

(define (auto-save-buffer buffer)
  (region->file (buffer-unclipped-region buffer)
		(buffer-auto-save-pathname buffer))
  (set-buffer-save-length! buffer)
  (set-buffer-auto-saved! buffer))

(define (delete-auto-save-file! buffer)
  (if (and (ref-variable "Delete Auto Save Files")
	   (buffer-auto-save-pathname buffer)
	   (file-exists? (buffer-auto-save-pathname buffer)))
      (delete-file (buffer-auto-save-pathname buffer))))

;;; end USING-SYNTAX
)