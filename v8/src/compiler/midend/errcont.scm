#| -*-Scheme-*-

$Id: errcont.scm,v 1.2 1996/07/30 19:24:42 adams Exp $

Copyright (c) 1996 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Error continuations.
;;; package: (compiler midend)

(declare (usual-integrations))

;; FUNCTION
;;
;;  Replace a continuation to known error operators which never returns by
;;  a bogus continuations which simply `halts'.  This breaks the
;;  `diamond' control flow into branching control flow.  It is up to
;;  rtlgen to generate interesting code for `halt'.

(define (errcont/rewrite program)

  (define (rewrite-bindings bs)
    (for-each (lambda (b) (rewrite (second b))) bs))

  (define (rewrite expr)
    (if (not (pair? expr))
	(illegal expr))
    (case (car expr)
      ((QUOTE))
      ((LOOKUP))
      ((DECLARE))
      ((LAMBDA)
       (rewrite (lambda/body expr)))
      ((LET)
       (rewrite-bindings (let/bindings expr))
       (rewrite (let/body expr)))
      ((CALL)
       (for-each rewrite (cdr expr))
       (let ((operator (call/operator expr)))
	 (define (select-on name)
	   (define (--> token)
	     (errcont/make-error-continuation (call/continuation expr) token))
	   (case name
	     ((ERROR:BAD-RANGE-ARGUMENT
	       ERROR:WRONG-TYPE-ARGUMENT
	       ERROR:WRONG-TYPE-DATUM)
	      (--> '%COMPILED-CODE-SUPPORT:NONRESTARTABLE-CONTINUATION))
	     ((%COMPILED-CODE-SUPPORT:SIGNAL-ERROR-IN-PRIMITIVE)
	      (--> '%COMPILED-CODE-SUPPORT:NONRESTARTABLE-CONTINUATION))
	     (else unspecific)))
	 (if (QUOTE/? operator)
	     (let ((rator (quote/text operator)))
	       (cond ((eq? rator %invoke-remote-cache)
		      (let ((name+arity
			     (quote/text (first (call/operands expr)))))
			(select-on (first name+arity)))))))))
      ((BEGIN)
       (for-each rewrite (begin/exprs expr)))
      ((IF)
       (rewrite (if/predicate expr))
       (rewrite (if/consequent expr))
       (rewrite (if/alternative expr)))
      ((LETREC)
       (rewrite-bindings (letrec/bindings expr))
       (rewrite (letrec/body expr)))
      (else
       (illegal expr))))

  (rewrite program))

;;(define (errcont/top-level program)
;;  (let  ((program* (copier/top-level program code-rewrite/remember)))
;;    (errcont/rewrite program*)
;;    program*))

(define errcont/make-error-continuation

  (let ((<ignored-continuation> (->pattern-variable 'IGNORED-CONTINUATION))
	(<results>              (->pattern-variable 'RESULTS))
	(<frame>                (->pattern-variable 'FRAME))
	(<vector>               (->pattern-variable 'VECTOR))
	(<continuation-body>    (->pattern-variable 'CONTINUATION-BODY))
	(<stuff>                (->pattern-variable 'STUFF)))

    (define pattern
      `(CALL
	',%make-stack-closure
	'#f
	(LAMBDA (,<ignored-continuation> . ,<results>)
	  (LET ((,<frame> (CALL ',%fetch-stack-closure '#f ',<vector>)))
	    ,<continuation-body>))
	. ,<stuff>))

    (lambda (cont token)

      (cond ((LOOKUP/? cont)
	     (sample/1 '(errcont/LOOKUP count) 1)
	     'ok)
	    ((call/%stack-closure-ref? cont)
	     ;; we could generate a new continuation with the same stack frame
             ;; format as the extant frame.  This would give us better
             ;; dbg info, at the cost of a continuation and the call
             ;; it contains.  (Another way to achieve this
             ;; continuation is to explicitly code the error case as a
             ;; sequence.
	     (sample/1 '(errcont/%stack-closure-ref count) 1)
	     'ok)
	    ((form/match pattern cont)
	     => (lambda (match)
		  (sample/1 '(errcont/%make-stack-closure/%halt count) 1)
		  (let ((body   (cadr (assq <continuation-body> match)))
			(frame  (cadr (assq <frame> match)))
			(vector (cadr (assq <vector> match))))
		    (form/rewrite! body
		      `(CALL ',%halt
			     (CALL ',%stack-closure-ref
				   '#F
				   (LOOKUP ,frame)
				   (QUOTE ,vector)
				   (QUOTE ,(vector-ref vector 0)))
			     ',token)))))
	    (else
	     (sample/1 '(errcont/other count) 1))))))
