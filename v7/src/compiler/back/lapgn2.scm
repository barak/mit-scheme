#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/lapgn2.scm,v 1.9 1988/11/07 13:57:02 cph Rel $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

;;;; LAP Generator: high level register assignment operations

(declare (usual-integrations))

;; `*register-map*' holds the current register map.  The operations
;; which follow use and update this map appropriately, so that the
;; writer of LAP generator rules need not pass it around.

(define *register-map*)

;; `*needed-registers*' contains a set of machine registers which is
;; in use during the LAP generation of a single RTL instruction.  The
;; value of this variable is automatically supplied to many low level
;; register map operations.  The set is initialized to the empty set
;; at the beginning of each instruction.  Typically, each alias
;; register is added to this set as it is allocated.  This informs the
;; register map operations that it is unreasonable to reallocate that
;; alias for some other purpose for this instruction.

;; The operations that modify `*needed-registers*' assume that `eqv?'
;; can be used to compare machine registers.

(define *needed-registers*)

(define-integrable (need-register! register)
  (set! *needed-registers* (cons register *needed-registers*)))

(define-integrable (need-registers! registers)
  (set! *needed-registers* (eqv-set-union registers *needed-registers*)))

(define-integrable (dont-need-register! register)
  (set! *needed-registers* (delv! register *needed-registers*)))

(define-integrable (dont-need-registers! registers)
  (set! *needed-registers* (eqv-set-difference *needed-registers* registers)))

;; `*dead-registers*' is initialized at the beginning of each RTL
;; instruction to the set of pseudo registers which become dead during
;; that instruction.  This information is used to make informed
;; decisions about whether it is desirable to keep the contents of
;; a particular pseudo register in a machine register, or not.

;; All dead registers are deleted from the register map after the LAP
;; generation for that instruction, by calling
;; `delete-dead-registers!'.  Thus, RTL instructions which alter the
;; contents of any pseudo register must follow this pattern: (1)
;; generate the source operands for the instruction, (2) delete the
;; dead registers from the register map, and (3) generate the code for
;; the assignment.

(define *dead-registers*)

(define-integrable (dead-register? register)
  (memv register *dead-registers*))

(define (delete-dead-registers!)
  (set! *register-map*
	(delete-pseudo-registers *register-map* *dead-registers*))
  (set! *dead-registers* '()))

;; `*prefix-instructions*' is used to accumulate LAP instructions to
;; be inserted before the instructions which are the result of the
;; rule for this RTL instruction.  The register map operations
;; generate these automatically whenever alias registers need to be
;; loaded or stored, or when the aliases need to be shuffled in some
;; way.

(define *prefix-instructions*)

(define-integrable (prefix-instructions! instructions)
  (set! *prefix-instructions* (LAP ,@*prefix-instructions* ,@instructions)))

;; Register map operations that return `allocator-values' eventually
;; pass those values to `store-allocator-values!', perhaps after some
;; tweaking.

(define-integrable (store-allocator-values! allocator-values)
  (bind-allocator-values allocator-values
    (lambda (alias map instructions)
      (need-register! alias)
      (set! *register-map* map)
      (prefix-instructions! instructions)
      alias)))

;; Register map operations that return either an alias register or #F
;; typically are wrapped with a call to `maybe-need-register!' to
;; record the fact that the returned alias is in use.

(define (maybe-need-register! register)
  (if register (need-register! register))
  register)

(define (register-has-alias? register type)
  ;; True iff `register' has an alias of the given `type'.
  ;; `register' may be any kind of register.
  (if (machine-register? register)
      (register-type? register type)
      (pseudo-register-alias *register-map* type register)))

(define-integrable (alias-is-unique? alias)
  ;; `alias' must be a valid alias for some pseudo register.  This
  ;; predicate is true iff the pseudo register has no other aliases.
  (machine-register-is-unique? *register-map* alias))

(define-integrable (alias-holds-unique-value? alias)
  ;; `alias' must be a valid alias for some pseudo register.  This
  ;; predicate is true iff the contents of the pseudo register are not
  ;; stored anywhere else that the register map knows of.
  (machine-register-holds-unique-value? *register-map* alias))

(define-integrable (is-alias-for-register? potential-alias register)
  ;; True iff `potential-alias' is a valid alias for `register'.
  ;; `register' must be a pseudo register, and `potential-alias' must
  ;; be a machine register.
  (is-pseudo-register-alias? *register-map* potential-alias register))

(define-integrable (register-saved-into-home? register)
  ;; True iff `register' is known to be saved in its spill temporary.
  ;; `register' must be a pseudo register.
  (pseudo-register-saved-into-home? *register-map* register))

(define-integrable (register-alias register type)
  ;; Returns an alias for `register', of the given `type', if one
  ;; exists.  Otherwise returns #F.  `register' must be a pseudo
  ;; register.
  (maybe-need-register! (pseudo-register-alias *register-map* type register)))

(define (load-alias-register! register type)
  ;; Returns an alias for `register', of the given `type'.  If no such
  ;; alias exists, a new alias is assigned and loaded with the correct
  ;; value, and that alias is returned.  `register' must be a pseudo
  ;; register.
  (store-allocator-values!
   (load-alias-register *register-map* type *needed-registers* register)))

(define-integrable (reference-alias-register! register type)
  (register-reference (load-alias-register! register type)))

(define (allocate-alias-register! register type)
  ;; This operation is used to allocate an alias for `register',
  ;; assuming that it is about to be assigned.  It first deletes any
  ;; other aliases for register, then allocates and returns an alias
  ;; for `register', of the given `type'.  `register' must be a pseudo
  ;; register.
  (delete-pseudo-register! register)
  (store-allocator-values!
   (allocate-alias-register *register-map* type *needed-registers* register)))

(define-integrable (reference-target-alias! register type)
  (register-reference (allocate-alias-register! register type)))

(define (allocate-temporary-register! type)
  ;; Allocates a machine register of the given `type' and returns it.
  ;; This register is not associated with any pseudo register, and can
  ;; be reallocated for other purposes as soon as it is no longer a
  ;; member of `*needed-registers*'.
  (store-allocator-values!
   (allocate-temporary-register *register-map* type *needed-registers*)))

(define-integrable (reference-temporary-register! type)
  (register-reference (allocate-temporary-register! type)))

(define (add-pseudo-register-alias! register alias)
  ;; This operation records `alias' as a valid alias for `register'.
  ;; No instructions are generated.  `register' must be a pseudo
  ;; register, and `alias' must be a previously allocated register
  ;; (typically for some other pseudo register).  Additionally,
  ;; `alias' must no longer be a valid alias, that is, it must have
  ;; been deleted from the register map after it was allocated.

  ;; This is extremely useful when performing assignments that move
  ;; the value of one pseudo register into another, where the former
  ;; register becomes dead.  In this case, since no further reference
  ;; is made to the source register, it no longer requires any
  ;; aliases.  Thus the target register can "inherit" the alias, which
  ;; means that the assignment is accomplished without moving any
  ;; data.
  (set! *register-map*
	(add-pseudo-register-alias *register-map* register alias false))
  (need-register! alias))

(define (delete-machine-register! register)
  ;; Deletes `register' from the register map.  No instructions are
  ;; generated.  `register' must be either an alias or a temporary.
  (set! *register-map* (delete-machine-register *register-map* register))
  (dont-need-register! register))

(define (delete-pseudo-register! register)
  ;; Deletes `register' from the register map.  No instructions are
  ;; generated.  `register' must be a pseudo register.
  (delete-pseudo-register *register-map* register
    (lambda (map aliases)
      (set! *register-map* map)
      (dont-need-registers! aliases))))

(define (clear-map!)
  ;; Deletes all registers from the register map.  Generates and
  ;; returns instructions to save pseudo registers into their homes,
  ;; if necessary.  This is typically used just before a control
  ;; transfer to somewhere that can potentially flush the contents of
  ;; the machine registers.
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

(define (standard-register-reference register preferred-type)
  ;; Generate a standard reference for `register'.  This procedure
  ;; uses a number of heuristics, aided by `preferred-type', to
  ;; determine the optimum reference.  This should be used only when
  ;; the reference need not have any special properties, as the result
  ;; is not even guaranteed to be a register reference.
  (let ((no-preference
	 (lambda ()
	   ;; Next, attempt to find an alias of any type.  If there
	   ;; are no aliases, and the register is not dead, allocate
	   ;; an alias of the preferred type.  This is desirable
	   ;; because the register will be used again.  Otherwise,
	   ;; this is the last use of this register, so we might as
	   ;; well just use the register's home.
	   (let ((alias (register-alias register false)))
	     (cond (alias
		    (register-reference alias))
		   ((dead-register? register)
		    (pseudo-register-home register))
		   (else
		    (reference-alias-register! register preferred-type)))))))
    (cond ((machine-register? register)
	   (register-reference register))
	  ;; First, attempt to find an alias of the preferred type.
	  (preferred-type
	   (let ((alias (register-alias register preferred-type)))
	     (if alias
		 (register-reference alias)
		 (no-preference))))
	  (else
	   (no-preference)))))

(define (machine-register-reference register type)
  ;; Returns a reference to a machine register which contains the same
  ;; contents as `register', and which has the given `type'.
  (register-reference
   (if (machine-register? register)
       (if (register-type? register type)
	   register
	   (let ((temp (allocate-temporary-register! type)))
	     (prefix-instructions!
	      (register->register-transfer register temp))
	     temp))
       (load-alias-register! register type))))
(define (load-machine-register! source-register machine-register)
  (if (machine-register? source-register)
      (if (eqv? source-register machine-register)
	  (LAP)
	  (register->register-transfer source-register machine-register))
      (if (is-alias-for-register? machine-register source-register)
	  (LAP)
	  (reference->register-transfer
	   (standard-register-reference source-register false)
	   machine-register))))

(define (move-to-alias-register! source type target)
  ;; Performs an assignment from the pseudo register `source' to the
  ;; pseudo register `target', allocating an alias for `target' of the
  ;; given `type'.  Returns a reference to that alias.  If `source'
  ;; has a reusable alias of the appropriate type, that is used, in
  ;; which case no instructions are generated.
  (reuse-and-load-pseudo-register-alias! source type
    (lambda (alias)
      (add-pseudo-register-alias! target alias))
    (lambda ()
      (allocate-alias-register! target type))))

(define (move-to-temporary-register! source type)
  ;; Allocates a temporary register, of the given `type', and loads
  ;; the contents of the pseudo register `source' into it.  Returns a
  ;; reference to that temporary.  If `source' has a reusable alias of
  ;; the appropriate type, that is used, in which case no instructions
  ;; are generated.
  (reuse-and-load-pseudo-register-alias! source type
    need-register!
    (lambda ()
      (allocate-temporary-register! type))))

(define (reuse-and-load-pseudo-register-alias! source type if-reusable if-not)
  ;; Attempts to find a reusable alias for `source', of the given
  ;; `type'.  If one is found, `if-reusable' is invoked on it (for
  ;; effect only).  Otherwise, `if-not' is invoked with no arguments
  ;; to produce a machine register, and the contents of `source' are
  ;; transferred into that register.  The result of this procedure is
  ;; a register reference, to the alias if it is found, otherwise to
  ;; the result of `if-not'.  Note: dead registers are always deleted
  ;; by this procedure.
  (reuse-alias-deleting-dead-registers! source type
    (lambda (alias)
      (if-reusable alias)
      (register-reference alias))
    (lambda (source)
      (let ((target (if-not)))
	(prefix-instructions! (reference->register-transfer source target))
	(register-reference target)))))

(define (reuse-alias-deleting-dead-registers! source type if-reusable if-not)
  (reuse-pseudo-register-alias! source type
    (lambda (alias)
      (delete-dead-registers!)
      (if-reusable alias))
    (lambda ()
      (let ((source (standard-register-reference source false)))	(delete-dead-registers!)
	(if-not source)))))

(define (reuse-pseudo-register-alias! source type if-reusable if-not)
  (reuse-pseudo-register-alias source type
    (lambda (alias)
      (delete-machine-register! alias)
      (if-reusable alias))
    if-not))

(define (reuse-pseudo-register-alias source type if-reusable if-not)
  ;; Attempts to find a reusable alias for `source', of the given
  ;; `type'.  If one is found, `if-reusable' is tail-recursively
  ;; invoked on it.  Otherwise, `if-not' is tail-recursively invoked
  ;; with no arguments.  The heuristics used to decide if an alias is
  ;; reusable are as follows: (1) if `source' is dead, any of its
  ;; aliases may be reused, and (2) if `source' is live with multiple
  ;; aliases, then one of its aliases may be reused.
  (let ((alias (register-alias source type)))
    (cond ((not alias)
	   (if-not))
	  ((dead-register? source)
	   (if-reusable alias))
	  ((not (alias-is-unique? alias))
	   (if-reusable alias))
	  (else
	   (if-not)))))

;; The following procedures are used when the copy is going to be
;; transformed, and the machine has 3 operand instructions, which
;; allow an implicit motion in the transformation operation.

;; For example, on the DEC VAX it is cheaper to do
;;	bicl3	op1,source,target
;; than
;; 	movl	source,target
;; 	bicl2	op1,target

;; The extra arguments are
;; REC1, invoked if we are reusing an alias of source.
;;      It already contains the data to operate on.
;; REC2, invoked if a `brand-new' alias for target has been allocated.
;;      We must take care of moving the data ourselves.

(define (with-register-copy-alias! source type target rec1 rec2)
  (provide-copy-reusing-alias! source type rec1 rec2
    (lambda (reusable-alias)
      (add-pseudo-register-alias! target reusable-alias))
    (lambda ()
      (allocate-alias-register! target type))))

(define (with-temporary-register-copy! register type rec1 rec2)
  (provide-copy-reusing-alias! register type rec1 rec2
    need-register!
    (lambda ()
      (allocate-temporary-register! type))))

(define (provide-copy-reusing-alias! source type rec1 rec2 if-reusable if-not)
  (reuse-alias-deleting-dead-registers! source type
    (lambda (alias)
      (if-reusable alias)
      (rec1 (register-reference alias)))
    (lambda (source)
      (rec2 source (register-reference (if-not))))))