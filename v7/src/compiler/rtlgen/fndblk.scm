#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/fndblk.scm,v 4.11 1990/05/03 15:11:36 jinx Rel $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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

;;;; RTL Generation: Environment Locatives
;;; package: (compiler rtl-generator find-block)

(declare (usual-integrations))

(define (find-block context extra-offset end-block?)
  (find-block/loop (reference-context/block context)
		   context
		   end-block?
		   (find-block/initial context extra-offset)))

(define (find-block/initial context extra-offset)
  (let ((block (reference-context/block context)))
    (if (not block)
	(error "find-block/initial: Null block!" block))
    (enumeration-case block-type (block-type block)
     ((STACK)
      (stack-locative-offset (rtl:make-fetch register:stack-pointer)
			     (+ extra-offset
				(reference-context/offset context))))
     ((IC)
      (rtl:make-fetch register:environment))
     (else
      (error "Illegal initial block type" block)))))

(define (find-block/loop block context end-block? locative)
  (cond ((null? block)
	 (error "find-block/loop: Null block!" block)
	 (values block locative))
	((or (end-block? block)
	     (ic-block? block))
	 (values block locative))
	(else
	 (find-block/loop
	  (block-parent block)
	  context
	  end-block?
	  ((find-block/parent-procedure block) block context locative)))))

(define (find-block/parent-procedure block)
  (enumeration-case block-type (block-type block)
    ((STACK)
     (let ((parent (block-parent block)))
       (cond ((not (procedure/closure? (block-procedure block)))
	      (if parent
		  (enumeration-case block-type (block-type parent)
		   ((STACK) internal-block/parent-locative)
		   ((IC) stack-block/static-link-locative)
		   ((CLOSURE) (error "Closure parent of open procedure" block))
		   (else (error "Illegal procedure parent" parent)))
		  (error "Block has no parent" block)))
	     ((procedure/trivial-closure? (block-procedure block))
#|
	      ;; This case cannot signal an error because of the way that
	      ;; find-block/loop is written.  The locative for the
	      ;; parent is needed, although it will be ignored by the
	      ;; receiver once it finds out that the block is
	      ;; ic/non-existent.  The references are found by using
	      ;; the variable caches.
	      (error "Block corresponds to trivial closure")
|#
	      trivial-closure/bogus-locative)
	     ((not parent)
	      (error "Block has no parent" block))
	     (else
	      (enumeration-case
	       block-type (block-type parent)
	       ((STACK) (error "Closure has a stack parent" block))
	       ((IC) stack-block/parent-of-dummy-closure-locative)
	       ((CLOSURE) stack-block/closure-parent-locative)
	       (else (error "Illegal procedure parent" parent)))))))
    ((CLOSURE) closure-block/parent-locative)
    ((CONTINUATION) continuation-block/parent-locative)
    (else (error "Illegal parent block type" block))))

(define (internal-block/parent-locative block context locative)
  (let ((link (block-stack-link block)))
    (if link
	(let ((end-block?
	       (let ((end-block (block-parent block)))
		 (lambda (block) (eq? block end-block)))))
	  (with-values
	      (lambda ()
		(find-block/loop
		 link
		 context
		 end-block?
		 (stack-locative-offset locative (block-frame-size block))))
	    (lambda (end-block locative)
	      (if (not (end-block? end-block))
		  (error "Couldn't find internal block parent!" block))
	      locative)))
	(stack-block/static-link-locative block context locative))))

(define (continuation-block/parent-locative block context locative)
  context
  (stack-locative-offset locative
			 (+ (block-frame-size block)
			    (continuation/offset (block-procedure block)))))

(define (stack-block/static-link-locative block context locative)
  (if (reference-context/adjacent-parent? context block)
      (stack-locative-offset locative (block-frame-size block))
      (rtl:make-fetch
       (stack-locative-offset locative (-1+ (block-frame-size block))))))

(define (stack-block/closure-parent-locative block context locative)
  context
  (rtl:make-fetch
   (stack-locative-offset
    locative
    (procedure-closure-offset (block-procedure block)))))

(define (trivial-closure/bogus-locative block context locative)
  block context locative
  ;; This value should make anyone trying to look at it crash.
  'TRIVIAL-CLOSURE-BOGUS-LOCATIVE)

(define (closure-block/parent-locative block context locative)
  context
  (rtl:make-fetch
   (rtl:locative-offset locative
			(closure-block-first-offset block))))

(define (stack-block/parent-of-dummy-closure-locative block context locative)
  (closure-block/parent-locative
   block
   context
   (stack-block/closure-parent-locative block context locative)))