;;; -*-Scheme-*-
;;;
;;;	$Id: comatch.scm,v 1.3 1997/03/10 05:32:44 cph Exp $
;;;
;;;	Copyright (c) 1997 Massachusetts Institute of Technology
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

;;;; Combinatoric Matcher

(declare (usual-integrations))

;;; This matcher allows compex matching patterns to be built up from
;;; primitives using combinators.  With this implementation, the
;;; patterns are just procedures, but it is simple to change the
;;; implementation so that they use a different representation.

;;; Each pattern takes two arguments: a start mark and an end mark.
;;; The pattern matches itself against the buffer's contents between
;;; the two marks.  A successful match returns a mark to the right of
;;; the rightmost character in the match.  A failed match returns #F.

(define (comatch-apply comatcher start #!optional end)
  (comatcher start
	     (if (default-object? end) (group-end start) end)))

(define (comatch:general procedure) procedure)

(define comatch:false (comatch:general (lambda (start end) start end #f)))
(define comatch:true (comatch:general (lambda (start end) end start)))

(define comatch:to-sexp (comatch:general forward-to-sexp-start))
(define comatch:sexp (comatch:general forward-one-sexp))

(define (comatch:char char #!optional case-fold?)
  (if (or (default-object? case-fold?) (not case-fold?))
      (comatch:general
       (lambda (start end)
	 (and (mark< start end)
	      (char=? char (extract-right-char start))
	      (mark1+ start))))
      (comatch:general
       (lambda (start end)
	 (and (mark< start end)
	      (char-ci=? char (extract-right-char start))
	      (mark1+ start))))))

(define (comatch:string string #!optional case-fold?)
  (let ((case-fold? (if (default-object? case-fold?) #f case-fold?)))
    (comatch:general
     (lambda (start end)
       (match-forward string start end case-fold?)))))

(define (comatch:regexp regexp #!optional case-fold?)
  (let ((regexp
	 (if (compiled-regexp? regexp)
	     regexp
	     (re-compile-pattern regexp
				 (if (default-object? case-fold?)
				     #f
				     case-fold?)))))
    (comatch:general
     (lambda (start end)
       (re-match-forward regexp start end)))))

(define (comatch:skip-chars pattern)
  (comatch:general
   (lambda (start end)
     (skip-chars-forward pattern start end))))

;;;; Combinators

(define (comatch:* comatcher)
  (comatch:general
   (lambda (start end)
     (let loop ((start start))
       (let ((mark (comatch-apply comatcher start end)))
	 (if mark
	     (loop mark)
	     start))))))

(define (comatch:+ comatcher)
  (let ((tail (comatch:* comatcher)))
    (comatch:general
     (lambda (start end)
       (let ((mark (comatch-apply comatcher start end)))
	 (and mark
	      (tail mark end)))))))

(define (comatch:? comatcher)
  (comatch:general
   (lambda (start end)
     (or (comatch-apply comatcher start end) start))))

(define (comatch:not comatcher)
  (comatch:general
   (lambda (start end)
     (and (not (comatch-apply comatcher start end))
	  start))))

(define (comatch:combine-rest initial combine-2)
  (lambda comatchers
    (if (null? comatchers)
	initial
	(let loop ((comatchers comatchers))
	  (if (null? (cdr comatchers))
	      (car comatchers)
	      (combine-2 (car comatchers) (loop (cdr comatchers))))))))

(define comatch:append
  (comatch:combine-rest comatch:true
    (lambda (c1 c2)
      (comatch:general
       (lambda (start end)
	 (let ((start (comatch-apply c1 start end)))
	   (and start
		(comatch-apply c2 start end))))))))

(define comatch:or
  (comatch:combine-rest comatch:true
    (lambda (c1 c2)
      (comatch:general
       (lambda (start end)
	 (or (comatch-apply c1 start end)
	     (comatch-apply c2 start end)))))))

(define comatch:and
  (comatch:combine-rest comatch:true
    (lambda (c1 c2)
      (comatch:general
       (lambda (start end)
	 (and (comatch-apply c1 start end)
	      (comatch-apply c2 start end)))))))