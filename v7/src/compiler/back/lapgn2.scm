#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/lapgn2.scm,v 1.3 1987/07/08 22:01:02 jinx Exp $

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

;;;; LAP Generator

(declare (usual-integrations))

(define *register-map*)
(define *prefix-instructions*)
(define *needed-registers*)

(define-integrable (prefix-instructions! instructions)
  (set! *prefix-instructions* (LAP ,@*prefix-instructions* ,@instructions)))

(define-integrable (need-register! register)
  (set! *needed-registers* (cons register *needed-registers*)))

(define-integrable (need-registers! registers)
  ;; **** Assume EQ? works on registers here. ****
  (set! *needed-registers* (eq-set-union registers *needed-registers*)))

(define (maybe-need-register! register)
  (if register (need-register! register))
  register)

(define (register-has-alias? register type)
  (if (machine-register? register)
      (register-type? register type)
      (pseudo-register-alias *register-map* type register)))

(define-integrable (register-alias register type)
  (maybe-need-register! (pseudo-register-alias *register-map* type register)))

(define-integrable (register-alias-alternate register type)
  (maybe-need-register! (machine-register-alias *register-map* type register)))

(define-integrable (register-type? register type)
  (or (not type)
      (eq? (register-type register) type)))

(define ((register-type-predicate type) register)
  (register-type? register type))

(define-integrable (dead-register? register)
  (memv register *dead-registers*))

(define (guarantee-machine-register! register type)
  (if (and (machine-register? register)
	   (register-type? register type))
      register
      (load-alias-register! register type)))

(define (load-alias-register! register type)
  (bind-allocator-values (load-alias-register *register-map* type
					      *needed-registers* register)
    store-allocator-values!))

(define (allocate-alias-register! register type)
  (bind-allocator-values (allocate-alias-register *register-map* type
						  *needed-registers* register)
    (lambda (alias map instructions)
      (store-allocator-values! alias
			       (delete-other-locations map alias)
			       instructions))))

(define (allocate-assignment-alias! target type)
  (let ((target (allocate-alias-register! target type)))
    (delete-dead-registers!)
    target))

(define (allocate-temporary-register! type)
  (bind-allocator-values (allocate-temporary-register *register-map* type
						      *needed-registers*)
    store-allocator-values!))

(define (store-allocator-values! alias map instructions)
  (need-register! alias)
  (set! *register-map* map)
  (prefix-instructions! instructions)
  alias)

(define-integrable (reference-alias-register! register type)
  (register-reference (load-alias-register! register type)))

(define-integrable (reference-assignment-alias! register type)
  (register-reference (allocate-assignment-alias! register type)))

(define-integrable (reference-temporary-register! type)
  (register-reference (allocate-temporary-register! type)))

(define (move-to-alias-register! source type target)
  (reuse-pseudo-register-alias! source type
    (lambda (reusable-alias)
      (add-pseudo-register-alias! target reusable-alias false))
    (lambda ()
      (allocate-alias-register! target type))))

(define (move-to-temporary-register! source type)
  (reuse-pseudo-register-alias! source type
    need-register!
    (lambda ()
      (allocate-temporary-register! type))))

(define (reuse-pseudo-register-alias! source type if-reusable if-not)
  ;; IF-NOT is assumed to return a machine register.
  (let ((reusable-alias
	 (and (dead-register? source)
	      (register-alias source type))))
    (if reusable-alias
	(begin (delete-dead-registers!)
	       (if-reusable reusable-alias)
	       (register-reference reusable-alias))
	(let ((alias (if (machine-register? source)
			 source
			 (register-alias source false))))
	  (delete-dead-registers!)
	  (let ((target (if-not)))
	    (prefix-instructions!
	    (cond ((not alias) (home->register-transfer source target))
		  ((= alias target) '())
		  (else (register->register-transfer alias target))))
	    (register-reference target))))))

(define (add-pseudo-register-alias! register alias saved-into-home?)
  (set! *register-map*
	(add-pseudo-register-alias *register-map* register alias
				   saved-into-home?))
  (need-register! alias))

(define (clear-map!)
  (delete-dead-registers!)
  (let ((instructions (clear-map)))
    (set! *register-map* (empty-register-map))
    (set! *needed-registers* '())
    instructions))

(define-integrable (clear-map)
  (clear-map-instructions *register-map*))

(define (clear-registers! . registers)
  (if (null? registers)
      '()
      (let loop ((map *register-map*) (registers registers))
	(save-machine-register map (car registers)
	  (lambda (map instructions)
	    (let ((map (delete-machine-register map (car registers))))
	      (if (null? (cdr registers))
		  (begin (set! *register-map* map)
			 instructions)
		  (append! instructions (loop map (cdr registers))))))))))

(define (save-machine-register! register)
  (let ((contents (machine-register-contents *register-map* register)))
    (if contents
	(save-pseudo-register! contents))))

(define (save-pseudo-register! register)
  (if (not (dead-register? register))
      (save-pseudo-register *register-map* register
	(lambda (map instructions)
	  (set! *register-map* map)
	  (prefix-instructions! instructions)))))

(define (delete-machine-register! register)
  (set! *register-map* (delete-machine-register *register-map* register))
  (set! *needed-registers* (eqv-set-delete *needed-registers* register)))

(package (delete-pseudo-register! delete-dead-registers!)
  (define-export (delete-pseudo-register! register)
    (delete-pseudo-register *register-map* register delete-registers!))
  (define-export (delete-dead-registers!)
    (delete-pseudo-registers *register-map* *dead-registers* delete-registers!)
    (set! *dead-registers* '()))
  (define (delete-registers! map aliases)
    (set! *register-map* map)
    (set! *needed-registers* (eqv-set-difference *needed-registers* aliases))))