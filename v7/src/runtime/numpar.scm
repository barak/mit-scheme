;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/numpar.scm,v 13.41 1987/01/23 00:16:30 jinx Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
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

;;;; Number Parser

(declare (usual-integrations))

(define string->number)

(define number-parser-package
  (make-environment

;;; These are not supported right now.

(define ->exact identity-procedure)
(define ->inexact identity-procedure)
(define ->long-flonum identity-procedure)
(define ->short-flonum identity-procedure)

(define *radix*)

(set! string->number
(named-lambda (string->number string #!optional exactness radix)
  ((cond ((or (unassigned? exactness) (not exactness)) identity-procedure)
	 ((eq? exactness 'E) ->exact)
	 ((eq? exactness 'I) ->inexact)
	 (else (error "Illegal exactness argument" exactness)))
   (fluid-let ((*radix*
		(cond ((unassigned? radix) *parser-radix*)
		      ((memv radix '(2 8 10 16)) radix)
		      ((eq? radix 'B) 2)
		      ((eq? radix 'O) 8)
		      ((eq? radix 'D) 10)
		      ((eq? radix 'X) 16)
		      (else (error "Illegal radix argument" radix)))))
     (parse-number (string->list string))))))

(define (parse-number chars)
  (parse-real chars
    (lambda (chars real)
      (if (null? chars)
	  real
	  (case (car chars)
	    ((#\+ #\-)
	     (parse-real chars
	       (lambda (chars* real*)
		 (and (not (null? chars*))
		      (null? (cdr chars*))
		      (or (char-ci=? (car chars*) #\i)
			  (char-ci=? (car chars*) #\j))
		      (make-rectangular real real*)))))
	    ((#\@)
	     (parse-real (cdr chars)
	       (lambda (chars real*)
		 (and (null? chars)
		      (make-polar real real*)))))
	    (else false))))))

(define (parse-real chars receiver)
  (and (not (null? chars))
       (case (car chars)
	 ((#\+)
	  (parse-unsigned-real (cdr chars)
	    receiver))
	 ((#\-)
	  (parse-unsigned-real (cdr chars)
	    (lambda (chars real)
	      (receiver chars (- real)))))
	 (else
	  (parse-unsigned-real chars
	    receiver)))))

(define (parse-unsigned-real chars receiver)
  (parse-prefix chars false false false
    (lambda (chars radix exactness precision)
      (fluid-let ((*radix*
		   (cdr (assv radix
			      '((#F . 10)
				(#\b . 2)
				(#\o . 8)
				(#\d . 10)
				(#\x . 16))))))
	(parse-body chars
	  (lambda (chars real)
	    (parse-suffix chars
	      (lambda (chars exponent)
		(receiver chars
			  ((case exactness
			     ((#F) identity-procedure)
			     ((#\e) ->exact)
			     ((#\i) ->inexact))
			   ((case precision
			      ((#F) identity-procedure)
			      ((#\s) ->short-flonum)
			      ((#\l) ->long-flonum))
			    (if exponent
				(* real (expt 10 exponent))
				real))))))))))))

(define (parse-prefix chars radix exactness precision receiver)
  (and (not (null? chars))
       (if (char=? (car chars) #\#)
	   (and (pair? (cdr chars))
		(let ((type (char-downcase (cadr chars)))
		      (rest (cddr chars)))
		  (let ((specify-prefix-type
			 (lambda (old)
			   (if old
			       (error "Respecification of prefix type" type)
			       type))))
		    (case type
		      ((#\b #\o #\d #\x)
		       (parse-prefix rest
				     (specify-prefix-type radix)
				     exactness
				     precision
				     receiver))
		      ((#\i #\e)
		       (parse-prefix rest
				     radix
				     (specify-prefix-type exactness)
				     precision
				     receiver))
		      ((#\s #\l)
		       (parse-prefix rest
				     radix
				     exactness
				     (specify-prefix-type precision)
				     receiver))
		      (else (error "Unknown prefix type" type))))))
	   (receiver chars radix exactness precision))))

(define (parse-suffix chars receiver)
  (if (and (not (null? chars))
	   (char-ci=? (car chars) #\e))
      (parse-signed-suffix (cdr chars) receiver)
      (receiver chars false)))

(define (parse-signed-suffix chars receiver)
  (and (not (null? chars))
       (case (car chars)
	 ((#\+)
	  (parse-unsigned-suffix (cdr chars)
	    receiver))
	 ((#\-)
	  (parse-unsigned-suffix (cdr chars)
	    (lambda (chars exponent)
	      (receiver chars (- exponent)))))
	 (else
	  (parse-unsigned-suffix chars
	    receiver)))))

(define (parse-unsigned-suffix chars receiver)
  (define (parse-digit chars value if-digit)
    (let ((digit (char->digit (car chars) 10)))
      (if digit
	  (if-digit (cdr chars) digit)
	  (receiver chars value))))

  (define (loop chars value)
    (if (null? chars)
	(receiver chars value)
	(parse-digit chars value
	  (lambda (chars digit)
	    (loop chars (+ digit (* value 10)))))))

  (and (not (null? chars))
       (parse-digit chars false
	 loop)))

(define (parse-body chars receiver)
  (and (not (null? chars))
       (if (char=? (car chars) #\.)
	   (require-digit (cdr chars)
	     (lambda (chars digit)
	       (parse-fraction chars digit 1
		 receiver)))
	   (parse-integer chars
	     (lambda (chars integer)
	       (if (null? chars)
		   (receiver chars integer)
		   (case (car chars)
		     ((#\/)
		      (parse-integer (cdr chars)
			(lambda (chars denominator)
			  (receiver chars (/ integer denominator)))))
		     ((#\.)
		      (parse-fraction (cdr chars) 0 0
			(lambda (chars fraction)
			  (receiver chars (+ integer fraction)))))
		     (else
		      (receiver chars integer)))))))))

(define (parse-integer chars receiver)
  (define (loop chars integer)
    (parse-digit/sharp chars
      (lambda (chars count)
	(receiver chars (->inexact (* integer (expt *radix* count)))))
      (lambda (chars digit)
	(loop chars (+ digit (* integer *radix*))))
      (lambda (chars)
	(receiver chars integer))))
  (require-digit chars loop))

(define (parse-fraction chars integer place-value receiver)
  (define (loop chars integer place-value)
    (parse-digit/sharp chars
      (lambda (chars count)
	(finish chars (->inexact integer) place-value))
      (lambda (chars digit)
	(loop chars
	      (+ digit (* integer *radix*))
	      (1+ place-value)))
      (lambda (chars)
	(finish chars integer place-value))))

  (define (finish chars integer place-value)
    (receiver chars (/ integer (expt *radix* place-value))))

  (loop chars integer place-value))

(define (require-digit chars receiver)
  (and (not (null? chars))
       (let ((digit (char->digit (car chars) *radix*)))
	 (and digit
	      (receiver (cdr chars) digit)))))

(define (parse-digit/sharp chars if-sharp if-digit otherwise)
  (cond ((null? chars) (otherwise chars))
	((char=? (car chars) #\#)
	 (let count-sharps ((chars (cdr chars)) (count 1))
	   (if (and (not (null? chars))
		    (char=? (car chars) #\#))
	       (count-sharps (cdr chars) (1+ count))
	       (if-sharp chars count))))
	(else
	 (let ((digit (char->digit (car chars) *radix*)))
	   (if digit
	       (if-digit (cdr chars) digit)
	       (otherwise chars))))))

;;; end NUMBER-PARSER-PACKAGE
))
