;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/print.scm,v 1.5 1992/02/27 19:14:03 arthur Exp $
;;;
;;;	Copyright (c) 1991 Massachusetts Institute of Technology
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
    (print-region/internal (buffer-region (current-buffer))
			   (ref-variable lpr-switches))))

(define-command print-buffer
  "Print buffer contents as with Unix command `lpr -p'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  '()
  (lambda ()
    (print-region/internal (buffer-region (current-buffer))
			   (cons "-p" (ref-variable lpr-switches)))))

(define-command lpr-region
  "Print region contents as with Unix command `lpr'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  "r"
  (lambda  (region)
    (print-region/internal region (ref-variable lpr-switches))))

(define-command print-region
  "Print region contents as with Unix command `lpr -p'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  "r"
  (lambda  (region)
    (print-region/internal region (cons "-p" (ref-variable lpr-switches)))))

(define (switches->string switches)
  (if (null? switches)
      ""
      (apply string-append
	     (cons (car switches)
		   (let loop ((remaining (cdr switches)))
		     (if (null? remaining)
			 '("")
			 (cons " "
			       (cons (car remaining)
				     (loop (cdr remaining))))))))))

(define (print-region/internal region switches)
  (let ((width (ref-variable tab-width)))
    (let ((buffer (temporary-buffer " *spool temp*")))
      (message "Spooling...")
      (region-insert-string! (buffer-point buffer)
			     (region->string region))
      (if (not (= width 8))
	  (begin (with-selected-buffer buffer
		   (lambda ()
		     (local-set-variable! tab-width width)))
		 (untabify-region (region-start region) (region-end region))))
      (shell-command
       region (buffer-end buffer) false false
       (string-append (ref-variable lpr-command (current-buffer))
		      " "
		      (switches->string switches)))
      (message "Spooling...done"))))