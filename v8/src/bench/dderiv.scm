(declare (usual-integrations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         dderiv.sch
; Description:  DDERIV benchmark from the Gabriel tests
; Author:       Vaughan Pratt
; Created:      8-Apr-85
; Modified:     10-Apr-85 14:53:29 (Bob Shaw)
;               23-Jul-87 (Will Clinger)
;               9-Feb-88 (Will Clinger)
; Language:     Scheme (but see note below)
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
; Note:  This benchmark uses property lists.  The procedures that must
; be supplied are get and put, where (put x y z) is equivalent to Common
; Lisp's (setf (get x y) z).

;;; DDERIV -- Symbolic derivative benchmark written by Vaughn Pratt.
 
;;; This benchmark is a variant of the simple symbolic derivative program
;;; (DERIV). The main change is that it is `table-driven.'  Instead of using a
;;; large COND that branches on the CAR of the expression, this program finds
;;; the code that will take the derivative on the property list of the atom in
;;; the CAR position. So, when the expression is (+ . <rest>), the code
;;; stored under the atom '+ with indicator DERIV will take <rest> and
;;; return the derivative for '+. The way that MacLisp does this is with the
;;; special form: (DEFUN (FOO BAR) ...). This is exactly like DEFUN with an
;;; atomic name in that it expects an argument list and the compiler compiles
;;; code, but the name of the function with that code is stored on the
;;; property list of FOO under the indicator BAR, in this case. You may have
;;; to do something like:
 
;;; :property keyword is not Common Lisp.
 
; Returns the wrong answer for quotients.
; Fortunately these aren't used in the benchmark.
 
;;(define get 2d-get)
;;(define put 2d-put!)

(define (dderiv-aux a)
  (list '_/ (dderiv a) a))
 
(define (+dderiv a)
  (cons '_+ (map dderiv a)))
 
(define (-dderiv a)
  (cons '_- (map dderiv a)))
 
(define (*dderiv a)
  (list '_* (cons '_* a)
        (cons '_+ (map dderiv-aux a))))
 
(define (/dderiv a)
  (list '_-
        (list '_/
              (dderiv (car a))
              (cadr a))
        (list '_/
              (car a)
              (list '_*
                    (cadr a)
                    (cadr a)
                    (dderiv (cadr a))))))
 
(define (dderiv a)
  (cond
    ((not (pair? a))
     (cond ((eq? a 'x) 1) (else 0)))
    (else (let ((dderiv (get (car a) '_dderiv)))
         (cond (dderiv (dderiv (cdr a)))
               (else 'error))))))
 
(define (run)
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (dderiv '(_+ (_* 3 x x) (_* a x x) (_* b x) 5))
    (dderiv '(_+ (_* 3 x x) (_* a x x) (_* b x) 5))
    (dderiv '(_+ (_* 3 x x) (_* a x x) (_* b x) 5))
    (dderiv '(_+ (_* 3 x x) (_* a x x) (_* b x) 5))
    (dderiv '(_+ (_* 3 x x) (_* a x x) (_* b x) 5))))

(put '_+ '_dderiv +dderiv)
(put '_- '_dderiv -dderiv)
(put '_* '_dderiv *dderiv)
(put '_/ '_dderiv /dderiv)
 
;;; call:  (run)
 
(lambda () (run) (run) (run) (run) (run))

