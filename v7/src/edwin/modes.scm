;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/modes.scm,v 1.24 1989/08/09 13:17:56 cph Exp $
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
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Modes

(declare (usual-integrations))

(define-named-structure "Mode"
  name
  display-name
  major?
  comtabs
  description
  initialization
  alist
  )
(define (make-mode name major? display-name comtabs description initialization)
  (let ((mode
	 (let ((name (symbol->string name)))
	   (or (string-table-get editor-modes name)
	       (let ((mode (%make-mode)))
		 (vector-set! mode mode-index:comtabs (list (make-comtab)))
		 (string-table-put! editor-modes name mode)
		 mode)))))
    (vector-set! mode mode-index:name name)
    (vector-set! mode mode-index:display-name display-name)
    (vector-set! mode mode-index:major? major?)
    (set-cdr! (vector-ref mode mode-index:comtabs) comtabs)
    (vector-set! mode mode-index:description description)
    (vector-set! mode mode-index:initialization initialization)
    (vector-set! mode mode-index:alist '())
    mode))

(define-integrable (mode-comtab mode)
  (car (mode-comtabs mode)))

(define editor-modes (make-string-table))

(define (name->mode name)
  (let ((name (canonicalize-name name)))
    (or (string-table-get editor-modes (symbol->string name))
	(make-mode name
		   true
		   (symbol->string name)
		   '()
		   ""
		   (lambda () (error "Undefined mode" name))))))

(define (->mode object)
  (if (mode? object) object (name->mode object)))