#| -*-Scheme-*-

$Id: 08ba64d9e0d5aa3ee81c4f85bbf3fea33d73a56a $

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

;;;; COERCE-TO-COMPILED-PROCEDURE
;;; package: (runtime coerce-to-compiled-procedure)

;;  This file must be syntaxed with the 8.0 compiler loaded

(declare (usual-integrations))

;;  This code is special.  It `hooks' into the 8.0 compiler's
;;  representation generate an unchecked call, which is necessary for
;;  calling procedures with listified rest args.  We also use
;;  vector-8b-ref on the procedure to get the arity info.  As this is
;;  on the critical path, calling a primitive just is not fast enough.
;;  Care has to be taken to ensure that none of the compiled code uses
;;  COERCE-TO-COMPILED-PROCEDURE, otherwise we would have a loop.

(define-macro (special-operator name)
  `(QUOTE ,(environment-lookup (->environment '(COMPILER MIDEND)) name)))


(define (coerce-to-compiled-procedure/compiled object arity)

  ;; XX and JUMP are in the let.  This is sufficiently obscure that SF
  ;; leaves the code looking nice if we pp a coercion procedure, but
  ;; not sufficiently obscure that the compiler generates wrong (if
  ;; JUMP were not integrated) or inefficent (if XX were not
  ;; integrated) code.

  (let ((xx ((lambda (#!optional xx) xx))) ; compute default-object
	(jump (special-operator %internal-apply-unchecked)))

    (define (default)
      ((ucode-primitive coerce-to-compiled-procedure 2) object arity))

    (define-integrable (entity-extra entity)
      (system-pair-cdr entity))

    ;;(define-integrable (entity-procedure entity)
    ;;  (system-pair-car entity))

    (define (try-arity-dispatched-procedure)
      (cond ((and (fix:> (vector-length (entity-extra object)) arity)
		  (vector-ref (entity-extra object) (fix:+ arity 1)))
	     => (lambda (dispatched-procedure)
		  (coerce-to-compiled-procedure dispatched-procedure arity)))
	    (else
	     (default))))	     

    (define (try-other-representations)
      (cond ((and (object-type? (ucode-type entity) object)
		  (vector? (entity-extra object))
		  (eq? (vector-ref (entity-extra object) 0)
		       arity-dispatcher-tag))
	     (try-arity-dispatched-procedure))
	    ((object-type? (ucode-type primitive) object)
	     (make-primitive-coercion object arity))
	    (else
	     (default))))

    (define (make-compiled-coercion procedure arity min max)
      ;; Note that min and max are +1-encoded, arity is not.

      (define-macro (coerce args . exprs)
	`(NAMED-LAMBDA (COERCED-PROCEDURE ,@args)
	   (JUMP ,(+ (length exprs) 1) PROCEDURE ,@exprs)))

      (if (fix:< arity (fix:- min 1))
	  (default)			; too few args
	  (case arity
	    ((0)
	     (case max
	       ((2)   (coerce () xx))
	       ((3)   (coerce () xx xx))
	       ((4)   (coerce () xx xx xx))
	       ((5)   (coerce () xx xx xx xx))
	       ((254) (coerce () '()))
	       ((253) (coerce () xx '()))
	       ((252) (coerce () xx xx '()))
	       ((251) (coerce () xx xx xx '()))
	       ((250) (coerce () xx xx xx xx '()))
	       (else (default))))
	    ((1)
	     (case max
	       ((3)   (coerce (a1) a1 xx))
	       ((4)   (coerce (a1) a1 xx xx))
	       ((5)   (coerce (a1) a1 xx xx xx))
	       ((254) (coerce (a1) (list a1)))
	       ((253) (coerce (a1) a1 '()))
	       ((252) (coerce (a1) a1 xx '()))
	       ((251) (coerce (a1) a1 xx xx '()))
	       ((250) (coerce (a1) a1 xx xx xx '()))
	       (else (default))))
	    ((2)
	     (case max
	       ((4)   (coerce (a1 a2) a1 a2 xx))
	       ((5)   (coerce (a1 a2) a1 a2 xx xx))
	       ((254) (coerce (a1 a2) (list a1 a2)))
	       ((253) (coerce (a1 a2) a1 (list a2)))
	       ((252) (coerce (a1 a2) a1 a2 '()))
	       ((251) (coerce (a1 a2) a1 a2 xx '()))
	       ((250) (coerce (a1 a2) a1 a2 xx xx '()))
	       (else (default))))
	    ((3)
	     (case max
	       ((5)   (coerce (a1 a2 a3) a1 a2 a3 xx))
	       ((254) (coerce (a1 a2 a3) (list a1 a2 a3)))
	       ((253) (coerce (a1 a2 a3) a1 (list a2 a3)))
	       ((252) (coerce (a1 a2 a3) a1 a2 (list a3)))
	       ((251) (coerce (a1 a2 a3) a1 a2 a3 '()))
	       ((250) (coerce (a1 a2 a3) a1 a2 a3 xx '()))
	       (else (default))))
	    ((4)
	     (case max
	       ((254) (coerce (a1 a2 a3 a4) (list a1 a2 a3 a4)))
	       ((253) (coerce (a1 a2 a3 a4) a1 (list a2 a3 a4)))
	       ((252) (coerce (a1 a2 a3 a4) a1 a2 (list a3 a4)))
	       ((251) (coerce (a1 a2 a3 a4) a1 a2 a3 (list a4)))
	       ((250) (coerce (a1 a2 a3 a4) a1 a2 a3 a4 '()))
	       (else  (default))))
	    ((5)
	     (case max
	       ((254) (coerce (a1 a2 a3 a4 a5) (list a1 a2 a3 a4 a5)))
	       ((253) (coerce (a1 a2 a3 a4 a5) a1 (list a2 a3 a4 a5)))
	       ((252) (coerce (a1 a2 a3 a4 a5) a1 a2 (list a3 a4 a5)))
	       ((251) (coerce (a1 a2 a3 a4 a5) a1 a2 a3 (list a4 a5)))
	       ((250) (coerce (a1 a2 a3 a4 a5) a1 a2 a3 a4 (list a5)))
	       (else (default))))
	    ((6)
	     (case max
	       ((254) (coerce (a1 a2 a3 a4 a5 a6) (list a1 a2 a3 a4 a5 a6)))
	       ((253) (coerce (a1 a2 a3 a4 a5 a6) a1 (list a2 a3 a4 a5 a6)))
	       ((252) (coerce (a1 a2 a3 a4 a5 a6) a1 a2 (list a3 a4 a5 a6)))
	       (else (default))))
	    (else (default)))))

    (if (fixnum? arity)
	(if (object-type? (ucode-type compiled-entry) object)
	    (let ((min (vector-8b-ref object -12))
		  (max (vector-8b-ref object -11)))
	      (if (< min 128)		; i.e. procedure, not continuation etc.
		  (if (= (- max 1) arity)
		      object		; the path to here is critical
		      (make-compiled-coercion object arity min max))
		  (default)))
	    (try-other-representations))
	(default))))


;; It is probably a better idea to bind CONS CAR CDR etc to compiled
;; procedures and be done with it.

(define (make-primitive-coercion primitive arity)
  ((ucode-primitive coerce-to-compiled-procedure 2) primitive arity))


(define arity-dispatcher-tag
  (string->symbol "#[(microcode)arity-dispatcher-tag]"))

(declare (ignore-reference-traps (set arity-dispatcher-tag)))


;;; Other procedures which the 8.0 compiler expects to be defined
;;

(define (%compiled-code-support:nonrestartable-continuation result)
  (error
   "You attempted to return to a subproblem with a non-restartable error in compiled code, using the value: ")
   result)

(define (%compiled-code-support:signal-error-in-primitive prim . args)
  (apply prim args))

;; This is defined in make.scm:
;;
;;   (define coerce-to-compiled-procedure)
;;

(define (initialize-package!)
  ;; The above code only works if compiled, so just use the primitive if
  ;; this file is not compiled.
  (set! coerce-to-compiled-procedure
	(if (compiled-procedure? coerce-to-compiled-procedure/compiled)
	    coerce-to-compiled-procedure/compiled
	    (ucode-primitive coerce-to-compiled-procedure)))

  ;; (set! arity-dispatcher-tag (fixed-objects-item 'ARITY-DISPATCHER-TAG))
  (set! arity-dispatcher-tag (vector-ref (get-fixed-objects-vector) 33))

  unspecific)
