#| -*-Scheme-*-

$Id: 1ef7d83d1c4ebc147e335edbc0b255b745edfeb4 $

Copyright (c) 1996, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

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
