;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/midas.scm,v 1.12 1989/03/14 08:01:31 cph Exp $
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

;;;; Midas Mode

(declare (usual-integrations))

(define-command ("Midas Mode")
  "Enter Midas mode."
  (set-current-major-mode! midas-mode))

(define-major-mode "Midas" "Fundamental"
  "Major mode for editing assembly code."
  (local-set-variable! "Syntax Table" midas-mode:syntax-table)
  (local-set-variable! "Comment Column" 40)
  (local-set-variable! "Comment Locator Hook" lisp-comment-locate)
  (local-set-variable! "Comment Indent Hook" midas-comment-indentation)
  (local-set-variable! "Comment Start" ";")
  (local-set-variable! "Comment End" "")
  (local-set-variable! "Paragraph Start" "^$")
  (local-set-variable! "Paragraph Separate" (ref-variable "Paragraph Start"))
  (local-set-variable! "Indent Line Procedure" ^r-tab-command)
  (if (ref-variable "Midas Mode Hook") ((ref-variable "Midas Mode Hook"))))

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
	   comment-column)))