#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/etc/comcmp.scm,v 1.1 1989/05/26 16:25:32 jinx Rel $

Copyright (c) 1989 Massachusetts Institute of Technology

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

;;;; Compiled code binary comparison program

(declare (usual-integrations))

(if (unassigned? compiled-code-block/bytes-per-object)
    (set! compiled-code-block/bytes-per-object 4))

(define comcmp:addressing-granularity 8)

(define comcmp:ignore-debugging-info? true)

(define compare-com-files
  (let ()

(define (compare-blocks b1 b2)
  (let ((l1 (system-vector-length b1))
	(l2 (system-vector-length b2)))
    (if (not (fix:= l1 l2))
	`(length ,l1 ,l2)
	(or (compare-code-sections b1 b2)
	    (compare-constant-sections b1 b2)))))

(define (read-code b s e)
  (let ((bs (bit-string-allocate (* comcmp:addressing-granularity (- e s)))))
    (read-bits! b (* comcmp:addressing-granularity s) bs)
    bs))

(define (compare-code-sections b1 b2)
  (let ((s1 (compiled-code-block/code-start b1))
	(s2 (compiled-code-block/code-start b2))
	(e1 (compiled-code-block/code-end b1))
	(e2 (compiled-code-block/code-end b2)))
    (cond ((not (fix:= s1 s2))
	   `(code-start ,s1 ,s2))
	  ((not (fix:= e1 e2))
	   `(code-end ,e1 ,e2))
	  ((not (bit-string=? (read-code b1 s1 e1)
			      (read-code b2 s2 e2)))
	   `(code))
	  (else
	   false))))

(define (constant-equal? c1 c2)
  (if (and (scode-constant? c1)
	   (scode-constant? c2))
      (equal? (unsyntax c1) (unsyntax c2))
      (equal? c1 c2)))

(define (compare-constant-sections b1 b2)
  (define (loop s e diff)
    (cond ((fix:> s e)
	   (if (null? diff)
	       false
	       (cons 'CONSTANTS (reverse! diff))))
	  ((not (constant-equal? (system-vector-ref b1 s)
				 (system-vector-ref b2 s)))
	   (loop (fix:1+ s)
		 e
		 `((,s ,(system-vector-ref b1 s)
		       ,(system-vector-ref b2 s))
		   ,@diff)))
	  (else
	   (loop (fix:1+ s) e diff))))

  ;; Kludge!
  (if comcmp:ignore-debugging-info?
      (begin
	(set-compiled-code-block/debugging-info! b1 '())
	(set-compiled-code-block/debugging-info! b2 '())))

  (let ((s1 (compiled-code-block/constants-start b1))
	(s2 (compiled-code-block/constants-start b2))
	(e1 (compiled-code-block/constants-end b1))
	(e2 (compiled-code-block/constants-end b2)))
    (cond ((not (fix:= s1 s2))
	   `(constant-start ,s1 ,s2))
	  ((not (fix:= e1 e2))
	   `(constant-end ,e1 ,e2))
	  (else
	   (loop s1 e1 '())))))

(lambda (f1 f2)
  (compare-blocks (compiled-code-address->block (fasload f1))
		  (compiled-code-address->block (fasload f2))))

))

(define (show-differences f1 f2)
  (define (->name f)
    (pathname->string (->pathname f)))

  (let ((result (compare-com-files f1 f2)))
    (if (pair? result)
	(begin
	  (newline)
	  (for-each display
		    (list "*** Files " (->name f1)
			  " and " (->name f2)
			  " differ : "))
	  (if (and (eq? 'CONSTANTS (car result))
		   (> (length result) 2))
	      (begin
		(display "***")
		(newline)
		(display "(CONSTANTS")
		(for-each (lambda (c)
			    (newline)
			    (display "   ")
			    (write c))
			  (cdr result))
		(newline)
		(display ")"))
	      (begin
		(write result)
		(display " ***")))))))
