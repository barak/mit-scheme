#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/compiler/etc/stackp.scm,v 1.1 1988/01/07 23:04:17 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Primitive Stack Parser

(declare (usual-integrations))

(define rcd)
(define rcr)
(define write-continuation)
(define continuation-ref)
(let ()

(set! rcd
  (named-lambda (rcd #!optional filename)
    (if (unassigned? filename)
	(write-continuation)
	(with-output-to-file filename write-continuation))))

(set! rcr
  (named-lambda (rcr n)
    (continuation-ref (rep-continuation) n)))

(set! write-continuation
  (named-lambda (write-continuation #!optional continuation)
    (if (unassigned? continuation) (set! continuation (rep-continuation)))
    (write-stack-stream (continuation->stream continuation) 0)))

(define (write-stack-stream stream n)
  (if (null? stream)
      *the-non-printing-object*
      (begin (if (return-address? (stack-stream-head stream))
		 (newline))
	     (newline)
	     (write n)
	     (write-string "\t")
	     (let ((string (write-to-string (stack-stream-head stream) 60)))
	       (write-string (cdr string))
	       (if (car string)
		   (write-string "...")))
	     (write-stack-stream (tail stream) (1+ n)))))

(set! continuation-ref
  (named-lambda (continuation-ref continuation n)
    (stack-stream-ref (continuation->stream continuation) n)))

(define (stack-stream-ref stream n)
  (cond ((null? stream) (error "index too large" n))
	((positive? n) (stack-stream-ref (tail stream) (-1+ n)))
	(else (stack-stream-head stream))))

(define (continuation->stream continuation)
  (control-point->stream (continuation->control-point continuation)))

(define (continuation->control-point continuation)
  (force (access promised-control-point (procedure-environment continuation))))

(define (control-point->stream control-point)
  (stack->stream
   ((access control-point->stack continuation-package) control-point)))

(define (stack->stream stack)
  (if (null? stack)
      '()
      (let ((item (stack-stream-head stack))
	    (rest (tail stack)))
	(cons-stream item
		     (if (and (return-address? item)
			      (= return-code:join-stacklets
				 (primitive-datum item)))
			 (cons-stream (head rest)
				      (control-point->stream (head rest)))
			 (stack->stream rest))))))

(define (stack-stream-head stack)
  (if (primitive-type? type-code:reference-trap (head stack))
      (map-reference-trap (lambda () (head stack)))
      (head stack)))

(define return-code:join-stacklets
  (microcode-return 'JOIN-STACKLETS))

(define type-code:reference-trap
  (microcode-type 'REFERENCE-TRAP))

)