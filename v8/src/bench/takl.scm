(declare (usual-integrations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         takl.sch
; Description:  TAKL benchmark from the Gabriel tests
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     12-Apr-85 10:07:00 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; TAKL -- The TAKeuchi function using lists as counters.
 
(define (listn n)
  (if (not (= 0 n))
      (cons n (listn (- n 1)))
      '()))
 
(define _18l (listn 18))
(define _12l (listn 12))
(define  _6l (listn 6))
 
(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x)
                 y z)
            (mas (cdr y)
                 z x)
            (mas (cdr z)
                 x y))))
 
(define (shorterp x y)
  (if (null? y)
    #f
    (or (null? x)
        (shorterp (cdr x)
                  (cdr y)))))
 
;;; call: (mas _18l _12l _6l)
 
(lambda ()
  (mas _18l _12l _6l)
  (mas _18l _12l _6l)
  (mas _18l _12l _6l)
  (mas _18l _12l _6l)
  (mas _18l _12l _6l))