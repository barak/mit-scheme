(declare (usual-integrations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         triangle.sch
; Description:  TRIANGLE benchmark
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     12-Apr-85 10:30:32 (Bob Shaw)
;               11-Aug-87 (Will Clinger)
;               22-Jan-88 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TRIANG -- Board game benchmark.
 
(define *board*
  (list->vector '(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)))

(define *sequence*
  (list->vector '(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define *a*
  (list->vector '(1 2 4 3 5 6 1 3 6 2 5 4 11 12
                  13 7 8 4 4 7 11 8 12 13 6 10
                  15 9 14 13 13 14 15 9 10
                  6 6)))

(define *b*
  (list->vector '(2 4 7 5 8 9 3 6 10 5 9 8
                  12 13 14 8 9 5 2 4 7 5 8
                  9 3 6 10 5 9 8 12 13 14
                  8 9 5 5)))

(define *c*
  (list->vector '(4 7 11 8 12 13 6 10 15 9 14 13
                  13 14 15 9 10 6 1 2 4 3 5 6 1
                  3 6 2 5 4 11 12 13 7 8 4 4)))

(define *answer* '())
(define *final* '())
 
(define (last-position)
  (do ((i 1 (+ i 1)))
      ((or (= i 16) (= 1 (vector-ref *board* i)))
       (if (= i 16) 0 i))))
 
(define (try i depth)
  (cond ((= depth 14)
         (let ((lp (last-position)))
           (if (not (member lp *final*))
             (set! *final* (cons lp *final*))))
         (set! *answer*
               (cons (cdr (vector->list *sequence*)) *answer*))
         '#t)
        ((and (= 1 (vector-ref *board* (vector-ref *a* i)))
              (= 1 (vector-ref *board* (vector-ref *b* i)))
              (= 0 (vector-ref *board* (vector-ref *c* i))))
         (vector-set! *board* (vector-ref *a* i) 0)
         (vector-set! *board* (vector-ref *b* i) 0)
         (vector-set! *board* (vector-ref *c* i) 1)
         (vector-set! *sequence* depth i)
         (do ((j 0 (+ j 1))
              (depth (+ depth 1)))
             ((or (= j 36) (try j depth)) '#f))
         (vector-set! *board* (vector-ref *a* i) 1)
         (vector-set! *board* (vector-ref *b* i) 1)
         (vector-set! *board* (vector-ref *c* i) 0) '#f)
        (else '#f)))
 
(define (gogogo i)
  (let ((*answer* '())
        (*final* '()))
    (try i 1)))
 
;;; call:  (gogogo 22))
 
(lambda () (gogogo 22))
