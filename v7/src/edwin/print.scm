;;; -*-Scheme-*-
;;;
;;;	$Id: print.scm,v 1.15 1998/01/08 18:13:07 cph Exp $
;;;
;;;	Copyright (c) 1991-97 Massachusetts Institute of Technology
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
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Print Buffers and Regions

(declare (usual-integrations))

(define-command lpr-buffer
  "Print buffer contents as with Unix command `lpr'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  '()
  (lambda ()
    (print-region/internal (buffer-region (current-buffer)) false)))

(define-command print-buffer
  "Print buffer contents as with Unix command `lpr -p'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  '()
  (lambda ()
    (print-region/internal (buffer-region (current-buffer)) true)))

(define-command lpr-region
  "Print region contents as with Unix command `lpr'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  "r"
  (lambda  (region)
    (print-region/internal region false)))

(define-command print-region
  "Print region contents as with Unix command `lpr -p'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  "r"
  (lambda  (region)
    (print-region/internal region true)))

(define (print-region/internal region print-headers?)
  (message "Spooling...")
  (let ((buffer (mark-buffer (region-start region)))
	(print-headers? (and print-headers? (not lpr-print-not-special?)))
	(title (print-region-title-string region)))
    (let ((call-printer
	   (lambda (region)
	     ((or (ref-variable lpr-procedure buffer)
		  (case microcode-id/operating-system
		    ((NT) print-region/nt)
		    (else print-region/default)))
	      region print-headers? title buffer)))
	  (width (ref-variable tab-width buffer)))
      (if (= width 8)
	  (call-printer region)
	  (call-with-temporary-buffer " *spool temp*"
	    (lambda (temp-buffer)
	      (insert-region (region-start region)
			     (region-end region)
			     (buffer-point temp-buffer))
	      (define-variable-local-value! temp-buffer
		(ref-variable-object tab-width)
		width)
	      (untabify-region (buffer-start temp-buffer)
			       (buffer-end temp-buffer))
	      (call-printer (buffer-region temp-buffer)))))))
  (append-message "done"))

(define (print-region/default region print-headers? title buffer)
  (shell-command region false false false
		 (string-append
		  (ref-variable lpr-command buffer)
		  (print/assemble-switches title
					   (if print-headers? '("-p") '())))))

(define (print-region/nt region print-headers? title buffer)
  print-headers? title buffer
  (call-with-temporary-file-pathname
   (lambda (pathname)
     (call-with-temporary-buffer " print"
       (lambda (temp)
	 (insert-region (region-start region)
			(region-end region)
			(buffer-end temp))
	 (insert-char #\page (buffer-end temp))
	 (write-region (buffer-region temp) pathname #f #t)))
     (shell-command #f #f #f #f
		    (string-append "copy "
				   (->namestring pathname)
				   " prn")))))

(define (print-region-title-string region)
  (let ((buffer-title
	 (let ((buffer (mark-buffer (region-start region))))
	   (and buffer
		(or (let ((pathname (buffer-pathname buffer)))
		      (and pathname
			   (let ((filename (file-namestring pathname)))
			     (and (not (string-null? filename))
				  filename))))
		    (string-append "Edwin buffer " (buffer-name buffer)))))))
    (if (or (not buffer-title)
	    (and (group-start? (region-start region))
		 (group-end? (region-end region))))
	buffer-title
	(string-append "region from " buffer-title))))

(define (print/assemble-switches title additional-switches)
  (apply string-append
	 (let loop
	     ((switches
	       (let ((switches (ref-variable lpr-switches)))
		 (append additional-switches
			 (let ((job-name (or (print/job-name) title)))
			   (if job-name
			       (list (string-append "-J \"" job-name "\""))
			       '()))
			 (if (and title
				  (not (there-exists? switches
					 (lambda (switch)
					   (string-prefix? "-T" switch)))))
			     (list (string-append "-T \"" title "\""))
			     '())
			 switches))))
	   (if (null? switches)
	       (list "")
	       (cons* " "
		      (car switches)
		      (loop (cdr switches)))))))

(define print/job-name
  (let ((most-recent-name false))
    (lambda ()
      (and lpr-prompt-for-name?
	   (let ((job-name
		  (prompt-for-string "Name to print on title page"
				     most-recent-name
				     'INSERTED-DEFAULT)))
	     (if (string-null? job-name)
		 false
		 (begin
		   (set! most-recent-name job-name)
		   job-name)))))))