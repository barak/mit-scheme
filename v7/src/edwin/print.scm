;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/print.scm,v 1.8 1992/08/28 21:02:43 cph Exp $
;;;
;;;	Copyright (c) 1991-92 Massachusetts Institute of Technology
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

(define (print-region/internal region print-command?)
  (let ((switches (print-region-switches region print-command?))
	(source-buffer (mark-buffer (region-start region)))
	(temp-buffer (temporary-buffer " *spool temp*")))
    (message "Spooling...")
    (insert-region (region-start region)
		   (region-end region)
		   (buffer-point temp-buffer))
    (let ((width (ref-variable tab-width source-buffer)))
      (if (not (= width 8))
	  (begin
	    (define-variable-local-value! temp-buffer
	      (ref-variable-object tab-width)
	      width)
	    (untabify-region (buffer-start temp-buffer)
			     (buffer-end temp-buffer)))))
    (shell-command region (buffer-end temp-buffer) false false
		   (apply string-append
			  (ref-variable lpr-command source-buffer)
			  (let loop ((switches switches))
			    (if (null? switches)
				(list "")
				(cons* " "
				       (car switches)
				       (loop (cdr switches)))))))
    (append-message "done")))

(define (print-region-switches region print-command?)
  (let ((switches (ref-variable lpr-switches))
	(title (print-region-title-string region)))
    (append (if (and print-command? (not lpr-print-not-special?))
		'("-p")
		'())
	    (let ((job-name
		   (or (and lpr-prompt-for-name?
			    (not (there-exists? switches
				   (lambda (switch)
				     (string-prefix? "-J" switch))))
			    (let ((job-name
				   (prompt-for-string
				    "Name to print on title page"
				    lpr-most-recent-name
				    'INSERTED-DEFAULT)))
			      (if (string-null? job-name)
				  false
				  (begin
				    (set! lpr-most-recent-name job-name)
				    job-name))))
		       title)))
	      (if job-name
		  (list (string-append "-J \"" job-name "\""))
		  '()))
	    (if (and title
		     (not (there-exists? switches
			    (lambda (switch)
			      (string-prefix? "-T" switch)))))
		(list (string-append "-T \"" title "\""))
		'())
	    switches)))

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