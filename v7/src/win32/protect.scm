#| -*-Scheme-*-

$Id: protect.scm,v 1.2 1993/11/10 21:41:48 adams Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

                          