;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/modes.scm,v 1.26 1992/01/09 17:45:16 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
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

;;;; Modes

(declare (usual-integrations))

(define-structure (mode
		   (constructor %make-mode (name comtabs))
		   (print-procedure
		    (unparser/standard-method 'MODE
		      (lambda (state mode)
			(unparse-object state (mode-name mode))
			(if (not (mode-major? mode))
			    (unparse-string state " (minor)"))))))
  (name false read-only true)
  (comtabs false read-only true)
  display-name
  major?
  description
  initialization
  alist)

(define (make-mode name major? display-name super-mode description
		   initialization)
  (let ((mode
	 (let ((string (symbol->string name)))
	   (or (string-table-get editor-modes string)
	       (let ((mode (%make-mode name (list (make-comtab)))))
		 (string-table-put! editor-modes string mode)
		 mode)))))
    (set-mode-display-name! mode display-name)
    (set-mode-major?! mode major?)
    (set-cdr! (mode-comtabs mode)
	      (cond ((not super-mode)
		     '())
		    ((mode? super-mode)
		     (mode-comtabs super-mode))
		    (else
		     ;; Old code passes a comtabs list here, so accept
		     ;; that as a valid argument.  Later, this can be
		     ;; an error.
		     super-mode)))
    (set-mode-description! mode description)
    (set-mode-initialization! mode initialization)
    (set-mode-alist! mode '())
    mode))

(define editor-modes
  (make-string-table))

(define (->mode object)
  (if (mode? object)
      object
      (let ((name (canonicalize-name object)))
	(or (string-table-get editor-modes (symbol->string name))
	    (make-mode name
		       true
		       (symbol->string name)
		       false
		       ""
		       (lambda () (error "Undefined mode" name)))))))

(define (major-mode? object)
  (and (mode? object)
       (mode-major? object)))

(define (minor-mode? object)
  (and (mode? object)
       (not (mode-major? object))))

(define-integrable (minor-mode-comtab mode)
  (car (mode-comtabs mode)))