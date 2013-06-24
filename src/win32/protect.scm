#| -*-Scheme-*-

$Id: protect.scm,v 1.5 2003/02/14 18:28:35 cph Exp $

Copyright (c) 1993, 1999 Massachusetts Institute of Technology

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

;;;; Protection lists
;;; package: (win32)

(declare (usual-integrations))

;; A protection list has a header cons cell.
;; Elements of the list are weak pairs (thing . info)
;; The car of the header contains the finalization procedure which is called
;; on the info field when the THING disappears.

(define-structure
  (protection-list
    (conc-name protection-list/)
    (constructor %make-protection-list))              
  head)


(define (make-protection-list finalizer)
  (let* ((header  (cons finalizer '()))
         (p       (%make-protection-list header)))
    (protection-list/add! *protection-lists* p header)
    p))


(define (protection-list/add! p thing info)
  ;; we return the weak pair allocated so that we can use it to get at thing
  ;; if this is really needed
  ;; do we need to wrap this in a without-interrupts?
  (let* ((weakpr  (weak-cons thing info))
         (link    (cons weakpr #f))
         (header  (protection-list/head p)))
    (let ((tail (cdr header)))
      (set-cdr! link tail)
      (set-cdr! header link)
      weakpr)))

(define (protection-list/finalize! p)
  ;; During finalization, the list is set to '().  This allows items
  ;; to be added to the list during finalization.  The call to append!
  ;; merges the new and old lists
  (let*  ((header    (if (protection-list? p)
                         (protection-list/head p)
			 p))
          (items     (cdr header))
          (finalize! (car header)))

    (define (loop/no-head items)
      (if (pair? items)
          (if (weak-car (car items))
	      (loop/with-head items items (cdr items))
	      (begin (finalize! (weak-cdr (car items)))
	             (loop/no-head (cdr items))))
	  '()))

    (define (loop/with-head first-cons mutate-point items)
      (if (pair? items)
	  (if (weak-car (car items))
	      (begin
		(set-cdr! mutate-point items)
		(loop/with-head first-cons items (cdr items)))
	      (begin
		(finalize! (weak-cdr (car items)))
		(set-cdr! mutate-point (cdr items))
		(loop/with-head first-cons mutate-point (cdr items))))
	  first-cons))

    (set-cdr! header '())
    (set-cdr! header (append! (cdr header) (loop/no-head items)))))


(define (protection-list/empty? p)
  (null? (cdr (protection-list/head p))))


(define (protection-list/finalize-protection-list hd)
  ;; if the list is still protecting something then add it back to the
  ;; master list
  (if (pair? (cdr hd))
    (protection-list/add! *protection-lists* (%make-protection-list hd) hd)))

(define (protection-list/gc-daemon)
;(display ";; protection-list/gc-daemon\n")
  (for-each (lambda (wp)
;              (display ";;Finalizing ") (display (weak-car wp)) (newline)
              (protection-list/finalize! (weak-cdr wp)))
            (cdr (protection-list/head *protection-lists*)))
  (protection-list/finalize! *protection-lists*))	      

(define (protection-list/for-each action p)
  (define (loop items)
    (if (pair? items)
      (begin
	(if (weak-car (car items))
	  (action (weak-car (car items))))
	(loop (cdr items)))))
  (loop (cdr (protection-list/head p))))

(define (protection-list/for-each-info action p)
  (define (loop items)
    (if (pair? items)
      (begin
	(if (weak-car (car items))
	  (action (weak-cdr (car items))))
	(loop (cdr items)))))
  (loop (cdr (protection-list/head p))))

(define (protection-list/find pred p)
  (define (loop items)
    (if (pair? items)
      (let* ((item  (car items))
	     (thing (weak-car item)))
	(if (and (weak-pair/car? item) (pred thing))
	    thing
	    (loop (cdr items))))
      #f))
  (loop (cdr (protection-list/head p))))
	  

(define *protection-lists*)

;(define p+ protection-list/add!)
(define (gc) (gc-flip) (protection-list/gc-daemon))

(define (initialize-protection-list-package!)
  (set! *protection-lists*
    (%make-protection-list
      (cons protection-list/finalize-protection-list '())))
  (add-gc-daemon! protection-list/gc-daemon)
)

                          