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

;;;; Alias Characters

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define alias-characters '())

(define (remap-alias-char char)
  (let ((entry (assq char alias-characters)))
    (if entry
	(remap-alias-char (cdr entry))
	char)))

(define (define-alias-char char char*)
  (let ((entry (assq char alias-characters)))
    (if entry
	(set-cdr! entry char*)
	(set! alias-characters (cons (cons char char*) alias-characters)))))

(define (undefine-alias-char char)
  (set! alias-characters (del-assq! char alias-characters)))

(define-alias-char #\C-h #\Backspace)
(define-alias-char #\C-H #\Backspace)
(define-alias-char #\C-i #\Tab)
(define-alias-char #\C-I #\Tab)
(define-alias-char #\C-j #\Linefeed)
(define-alias-char #\C-J #\Linefeed)
(define-alias-char #\C-k #\VT)
(define-alias-char #\C-K #\VT)
(define-alias-char #\C-l #\Page)
(define-alias-char #\C-L #\Page)
(define-alias-char #\C-m #\Return)
(define-alias-char #\C-M #\Return)
(define-alias-char #\C-z #\Call)
(define-alias-char #\C-Z #\Call)
(define-alias-char #\C-[ #\Altmode)
(define-alias-char #\C-- #\Backnext)

(define-alias-char #\C-M-h #\M-Backspace)
(define-alias-char #\C-M-H #\M-Backspace)
(define-alias-char #\C-M-i #\M-Tab)
(define-alias-char #\C-M-I #\M-Tab)
(define-alias-char #\C-M-j #\M-Linefeed)
(define-alias-char #\C-M-J #\M-Linefeed)
(define-alias-char #\C-M-k #\M-VT)
(define-alias-char #\C-M-K #\M-VT)
(define-alias-char #\C-M-l #\M-Page)
(define-alias-char #\C-M-L #\M-Page)
(define-alias-char #\C-M-m #\M-Return)
(define-alias-char #\C-M-M #\M-Return)
(define-alias-char #\C-M-z #\M-Call)
(define-alias-char #\C-M-Z #\M-Call)
(define-alias-char #\C-M-[ #\M-Altmode)
(define-alias-char #\C-M-- #\M-Backnext)

;;; These are definitions for the HP 9000 model 237.
;;; They should probably be isolated somehow, but there is no clear way.
(define-alias-char #\S-S #\Rubout)	;Home
(define-alias-char #\S-R #\Linefeed)	;Select

;;; These are definitions for the HP 9000 model 236.
(define-alias-char #\S-U #\Altmode)	;Run
(define-alias-char #\S-V #\Linefeed)	;Continue
(define-alias-char #\S-W #\Altmode)	;Execute

;;; end USING-SYNTAX
)
;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
