;;; -*-Scheme-*-
;;;
;;;	$Id: midas.scm,v 1.17 1992/11/17 17:40:02 cph Exp $
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

;;;; Midas Mode

(declare (usual-integrations))

(define-command midas-mode
  "Enter Midas mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object midas))))

(define-major-mode midas fundamental "Midas"
  "Major mode for editing assembly code."
  (lambda (buffer)
    (define-variable-local-value! buffer (ref-variable-object syntax-table)
      midas-mode:syntax-table)
    (define-variable-local-value! buffer (ref-variable-object comment-column)
      40)
    (define-variable-local-value! buffer
	(ref-variable-object comment-locator-hook)
      lisp-comment-locate)
    (define-variable-local-value! buffer
	(ref-variable-object comment-indent-hook)
      midas-comment-indentation)
    (define-variable-local-value! buffer (ref-variable-object comment-start)
      ";")
    (define-variable-local-value! buffer (ref-variable-object comment-end)
      "")
    (let ((paragraph-start "^$"))
      (define-variable-local-value! buffer
	  (ref-variable-object paragraph-start)
	paragraph-start)
      (define-variable-local-value! buffer
	  (ref-variable-object paragraph-separate)
	paragraph-start))
    (define-variable-local-value! buffer
	(ref-variable-object indent-line-procedure)
      (ref-command insert-tab))
    (event-distributor/invoke! (ref-variable midas-mode-hook buffer) buffer)))

(define midas-mode:syntax-table (make-syntax-table))
(modify-syntax-entry! midas-mode:syntax-table #\; "<   ")
(modify-syntax-entry! midas-mode:syntax-table #\newline ">   ")
(modify-syntax-entry! midas-mode:syntax-table #\. "w   ")
(modify-syntax-entry! midas-mode:syntax-table #\' "'   ")
(modify-syntax-entry! midas-mode:syntax-table #\$ "'   ")
(modify-syntax-entry! midas-mode:syntax-table #\% "'   ")
(modify-syntax-entry! midas-mode:syntax-table #\# "'   ")

(define (midas-comment-indentation mark)
  (if (match-forward ";;;" mark)
      0
      (max (1+ (mark-column (horizontal-space-start mark)))
	   (ref-variable comment-column))))