#| -*-Scheme-*-

$Id: fndblk.scm,v 4.14 2003/02/14 18:28:08 cph Exp $

Copyright (c) 1988, 1990, 1999 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

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