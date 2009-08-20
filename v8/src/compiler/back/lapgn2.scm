#| -*-Scheme-*-

$Id$

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

;;;; LAP Generator: High-Level Register Assignment
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;; `*register-map*' holds the current register map.  The operations
;; that follow use and update this map appropriately, so that the
;; writer of LAP generator rules need not pass it around.

(define *register-map*)
(define *preserved-registers*)
(define *recomputed-registers*)

;; `*needed-registers*' contains a set of machine registers that is
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

(define (need-register! register)
  (set! *needed-registers* (cons register *needed-registers*)))

(define (need-registers! registers)
  (set! *needed-registers* (eqv-set-union registers *needed-registers*)))

(define (dont-need-register! register)
  (set! *needed-registers* (delv! register *needed-registers*)))

(define (dont-need-registers! registers)
  (set! *needed-registers* (eqv-set-difference *needed-registers* registers)))

;; `*dead-registers*' is initialized at the beginning of each RTL
;; instruction to the set of pseudo registers that become dead during
;; that instruction.  This information is used to decide whether or
;; not to keep the contents of a particular pseudo register in a
;; machine register.

(define *dead-registers*)

(define (dead-register? register)
  (memv register *dead-registers*))

;; `*registers-to-delete*' is also initialized to the set of pseudo
;; registers that are dead after the current RTL instruction; these
;; registers are deleted from the register map after the LAP
;; generation for that instruction.  The LAP generation rules can
;; cause these deletions to happen at any time by calling
;; `delete-dead-registers!'.

;; RTL instructions that alter the contents of any pseudo register
;; must follow this pattern: (1) generate the source operands for the
;; instruction, (2) delete the dead registers from the register map,
;; and (3) generate the code for the assignment.

(define *registers-to-delete*)

(define (delete-dead-registers!)
  (set! *register-map*
	(delete-pseudo-registers *register-map* *registers-to-delete*))
  (set! *registers-to-delete* '())
  unspecific)

;; `*prefix-instructions*' is used to accumulate LAP instructions to
;; be inserted before the instructions that are the result of the
;; rule for this RTL instruction.  The register map operations
;; generate these automatically whenever alias registers need to be
;; loaded or stored, or when the aliases need to be shuffled in some
;; way.

(define *prefix-instructions*)
(define *suffix-instructions*)

(define (prefix-instructions! instructions)
  (set! *prefix-instructions* (LAP ,@*prefix-instructions* ,@instructions)))

(define (suffix-instructions! instructions)
  (set! *suffix-instructions* (LAP ,@instructions ,@*suffix-instructions*)))

;; Register map operations that return `allocator-values' eventually
;; pass those values to `store-allocator-values!', perhaps after some
;; tweaking.

(define (store-allocator-values! allocator-values)
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

(define (alias-is-unique? alias)
  ;; `alias' must be a valid alias for some pseudo register.  This
  ;; predicate is true iff the pseudo register has no other aliases.
  (machine-register-is-unique? *register-map* alias))

(define (alias-holds-unique-value? alias)
  ;; `alias' must be a valid alias for some pseudo register.  This
  ;; predicate is true iff the contents of the pseudo register are not
  ;; stored anywhere else that the register map knows of.
  (machine-register-holds-unique-value? *register-map* alias))

(define (is-alias-for-register? potential-alias register)
  ;; True iff `potential-alias' is a valid alias for `register'.
  ;; `register' must be a pseudo register, and `potential-alias' must
  ;; be a machine register.
  (is-pseudo-register-alias? *register-map* potential-alias register))

(define (register-saved-into-home? register)
  ;; True iff `register' is known to be saved in its spill temporary.
  (and (not (machine-register? register))
       (pseudo-register-saved-into-home? *register-map* register)))

(define (register-alias register type)
  ;; Returns an alias for `register', of the given `type', if one
  ;; exists.  Otherwise returns #F.
  (if (machine-register? register)
      (and (register-type? register type) register)
      (maybe-need-register!
       (pseudo-register-alias *register-map* type register))))

(define (load-alias-register! register type)
  ;; Returns an alias for `register', of the given `type'.  If no such
  ;; alias exists, a new alias is assigned and loaded with the correct
  ;; value, and that alias is returned.
  (if (machine-register? register)
      (if (register-type? register type)
	  register
	  (let ((temp (allocate-temporary-register! type)))
	    (prefix-instructions! (register->register-transfer register temp))
	    temp))
      (store-allocator-values!
       (load-alias-register *register-map* type *needed-registers* register))))

(define (reference-alias-register! register type)
  (register-reference (load-alias-register! register type)))

(define (allocate-alias-register! register type)
  ;; This operation is used to allocate an alias for `register',
  ;; assuming that it is about to be assigned.  It first deletes any
  ;; other aliases for register, then allocates and returns an alias
  ;; for `register', of the given `type'.
  (cond ((not (machine-register? register))
	 (delete-register! register)
	 (store-allocator-values!
	  (allocate-alias-register *register-map*
				   type
				   *needed-registers*
				   register)))
	((not (register-type? register type))
	 (delete-register! register)
	 (let ((temp (allocate-temporary-register! type)))
	   (suffix-instructions!
	    (register->register-transfer temp register))
	   temp))
	;; *** Lose ***
	((not (memq register available-machine-registers))
	 (delete-register! register)
	 register)
	(else
	 (prefix-instructions! (clean-registers! register))
	 (lock-register! register)
	 register)))

(define (reference-target-alias! register type)
  (register-reference (allocate-alias-register! register type)))

(define (allocate-temporary-register! type)
  ;; Allocates a machine register of the given `type' and returns it.
  ;; This register is not associated with any pseudo register, and can
  ;; be reallocated for other purposes as soon as it is no longer a
  ;; member of `*needed-registers*'.
  (store-allocator-values!
   (allocate-temporary-register *register-map* type *needed-registers*)))

(define (reference-temporary-register! type)
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

(define (delete-register! register)
  ;; Deletes `register' from the register map.  No instructions are
  ;; generated.
  (if (machine-register? register)
      (begin
	(set! *register-map* (delete-machine-register *register-map* register))
	(dont-need-register! register))
      (delete-pseudo-register *register-map* register
	(lambda (map aliases)
	  (set! *register-map* map)
	  (dont-need-registers! aliases)))))

(define (lock-register! register)
  ;; Makes register unavailable for allocation.
  ;; No instructions are generated.
  (set! *register-map* (lock-machine-register *register-map* register))
  unspecific)

(define (release-register! register)
  ;; Makes register unavailable for allocation.
  ;; No instructions are generated.
  (set! *register-map* (release-machine-register *register-map* register))
  unspecific)

(define (preserve-register! register how)
  (set! *preserved-registers*
	(cons (list register how)
	      *preserved-registers*))
  unspecific)

(define *recompute-me* '(Recompute entry but no fixed alias))

(define (clear-map!/preserving)
  ;; (values machine-regs pseudo-regs)
  ;; where machine-regs are the machine registers to preserve,
  ;; and pseudo-regs are the pseudo registers whose homes
  ;; must be preserved.
  ;; It also modifies the register map to reflect the registers
  ;; needed and available when restoring.

  (define (survive entries)
    (internal-error "Non-preserved registers survive clear-map!/preserving"
		    entries))

  (delete-dead-registers!)
  (let ((pairs (map (lambda (entry)
		      (cons (map-entry-home entry)
			    entry))
		    (list-transform-positive (map-entries *register-map*)
		      map-entry-home)))
	(preserved *preserved-registers*))
    
    (let ((bad (list-transform-negative pairs
		 (lambda (pair)
		   (assv (car pair) preserved)))))
      (if (not (null? bad))
	  (survive (map car bad))))
	    
    (let ((entries '())
	  (regs-needed '())
	  (reqs-home '())
	  (regs-reserved '()))

      (define (save-aliases entry)
	(let ((aliases (map-entry-aliases entry)))
	  (set! regs-needed (eqv-set-union aliases regs-needed))
	  (set! entries
		(cons (if (map-entry-saved-into-home? entry)
			  (make-map-entry (map-entry-home entry)
					  false
					  aliases
					  false)
			  entry)
		      entries))
	  unspecific))

      (define (remember-an-alias entry)
	;; Called only when guaranteed that there is an alias!
	(let ((reserved (car (map-entry-aliases entry))))
	  (set! regs-reserved (eqv-set-adjoin reserved regs-reserved))
	  (set! entries
		(cons (make-map-entry (map-entry-home entry)
				      reserved
				      '()
				      false)
		      entries))
	  unspecific))


      (define (remember-to-recompute register)
	(set! entries
	      (cons (make-map-entry register
				    *recompute-me*
				    '()
				    false)
		    entries))
	unspecific)

      (let loop ((preserved preserved))
	(if (null? preserved)
	    (begin
	      (set! *needed-registers* regs-needed)
	      (set! *register-map*
		    (make-register-map
		     entries
		     (eqv-set-difference
		      (eqv-set-difference available-machine-registers
					  regs-needed)
		      regs-reserved)))
	      (values regs-needed reqs-home))
	    (let* ((how (cadr (car preserved)))
		   (reg (car (car preserved)))
		   (entry (let ((pair (assv reg pairs)))
			    (and pair (cdr pair))))
		   (has-alias?
		    (and entry
			 (not (null? (map-entry-aliases entry))))))
	      (case how
		((SAVE)
		 (if has-alias?
		     (save-aliases entry)
		     (begin
		       (set! reqs-home (cons reg reqs-home))
		       (set! entries
			     (cons (or entry
				       (make-map-entry reg
						       true
						       '()
						       false))
				   entries)))))
		((IF-AVAILABLE)
		 (if has-alias?
		     (save-aliases entry)))
		((RECOMPUTE)
		 (if has-alias?
		     (remember-an-alias entry)
		     (remember-to-recompute reg)))
		(else
		 (error "Unknown preservation kind" how)))
	      (loop (cdr preserved))))))))

(define (restore-registers!)
  ;; This is called as part of processing (RETURN-ADDRESS ...);
  ;; it is *NOT* what is used to handle (RESTORE ...): that code
  ;; is in CGEN-RINST and CGEN-RESTORE.
  (call-with-values
   (lambda ()
     (list-split (map-entries *register-map*)
		 (lambda (entry)
		   (boolean? (map-entry-saved-into-home? entry)))))
   (lambda (normal alias-remembered)
     (call-with-values
      (lambda ()
	(list-split alias-remembered
		    (lambda (entry)
		      (eq? (map-entry-saved-into-home? entry)
			   *recompute-me*))))
      (lambda (no-alias have-alias)
	(set! *recomputed-registers*
	      ;; (Home Alias) pairs or (Home #F) if no fixed alias
	      (append
	       (map (lambda (entry)
		      (list (map-entry-home entry)
			    (map-entry-saved-into-home? entry)))
		 have-alias)
	       (map (lambda (entry)
		      (list (map-entry-home entry) #F))
		 no-alias)))
	(set! *register-map*
	      (make-register-map normal
				 (map-registers:add*
				  (map-registers *register-map*)
				  (map map-entry-saved-into-home?
				    have-alias))))))))
  unspecific)

(define (restored-register-home register)
  ;; (values available? reg-where-desired)
  (let* ((info (assq register *recomputed-registers*))
	 (alias (and info (cadr info))))
    (cond (alias (values false alias))	; RECOMPUTE, had alias
	  (info (values false false))	; RECOMPUTE, no alias
	  ((or (register-has-alias? register false)
	       (register-saved-into-home? register)) ; Available
	   (values true (list (register-has-alias? register false)
			      (register-saved-into-home? register))))
	  (else (values false false)))))

(define (clear-map!)
  ;; Deletes all registers from the register map.  Generates and
  ;; returns instructions to save pseudo registers into their homes,
  ;; if necessary.  This is typically used just before a control
  ;; transfer to somewhere that can potentially flush the contents of
  ;; the machine registers.
  (delete-dead-registers!)
  (if (not (null? *preserved-registers*))
      (error "clear-map! called with registers preserved"
	     *preserved-registers*))
  (clear-map!/finish (clear-map)))

(define (clear-map!/finish instructions)
  (set! *register-map* (empty-register-map))
  (set! *needed-registers* '())
  instructions)

(define (clear-map)
  (clear-map-instructions *register-map*))

(define (clear-registers! . registers)
  (if (null? registers)
      '()
      (let loop ((map *register-map*) (registers registers))
	(save-machine-register map (car registers)
	  (lambda (map instructions)
	    (let ((map (delete-machine-register map (car registers))))
	      (if (null? (cdr registers))
		  (begin
		    (set! *register-map* map)
		    instructions)
		  (append! instructions (loop map (cdr registers))))))))))

(define (clean-registers! . aregisters)
  (if (null? aregisters)
      '()
      (let loop ((map *register-map*) (registers aregisters))
	(preserve-machine-register map (car registers)
				   (eq-set-union aregisters *needed-registers*)
	  (lambda (map instructions)
	    (let ((map (delete-machine-register map (car registers))))
	      (if (null? (cdr registers))
		  (begin
		    (set! *register-map* map)
		    instructions)
		  (append! instructions (loop map (cdr registers))))))))))

(define (standard-register-reference register preferred-type alternate-types?)
  ;; Generate a standard reference for `register'.  This procedure
  ;; uses a number of heuristics, aided by `preferred-type', to
  ;; determine the optimum reference.  This should be used only when
  ;; the reference need not have any special properties, as the result
  ;; is not even guaranteed to be a register reference.
  (if (machine-register? register)
      (if alternate-types?
	  (register-reference register)
	  (reference-alias-register! register preferred-type))
      (let ((no-reuse-possible
	     (lambda ()
	       ;; If there are no aliases, and the register is not dead,
	       ;; allocate an alias of the preferred type.  This is
	       ;; desirable because the register will be used again.
	       ;; Otherwise, this is the last use of this register, so we
	       ;; might as well just use the register's home.
	       (if (and (register-saved-into-home? register)
			(or (dead-register? register)
			    (not (allocate-register-without-unload?
				  *register-map*
				  preferred-type
				  *needed-registers*))))
		   (pseudo-register-home register)
		   (reference-alias-register! register preferred-type)))))
	(let ((no-preference
	       (lambda ()
		 ;; Next, attempt to find an alias of any type.
		 (let ((alias (register-alias register false)))
		   (if alias
		       (register-reference alias)
		       (no-reuse-possible))))))
	  ;; First, attempt to find an alias of the preferred type.
	  (if preferred-type
	      (let ((alias (register-alias register preferred-type)))
		(cond (alias (register-reference alias))
		      (alternate-types? (no-preference))
		      (else (no-reuse-possible))))
	      (no-preference))))))

(define (%load-machine-register! source-register machine-register
				 clean-register-map!)
  ;; Copy the contents of `source-register' to `machine-register'.
  (cond ((machine-register? source-register)
	 (clean-register-map!)
	 (LAP ,@(clean-registers! machine-register)
	      ,@(if (eqv? source-register machine-register)
		    (LAP)
		    (register->register-transfer source-register
						 machine-register))))
	((is-alias-for-register? machine-register source-register)
	 (clean-register-map!)
	 (clean-registers! machine-register))
	(else
	 (let ((source-reference
		(if (register-value-class=word? source-register)
		    (standard-register-reference source-register false true)
		    (standard-register-reference
		     source-register
		     (register-type source-register)
		     false))))
	   (clean-register-map!)
	   (LAP ,@(clean-registers! machine-register)
		,@(reference->register-transfer source-reference
						machine-register))))))

(define (load-machine-register! source-register machine-register)
  ;; Copy the contents of `source-register' to `machine-register'.
  (%load-machine-register! source-register machine-register
			   (lambda () unspecific)))

(define (move-to-alias-register! source type target)
  ;; Performs an assignment from register `source' to register
  ;; `target', allocating an alias for `target' of the given `type';
  ;; returns that alias.  If `source' has a reusable alias of the
  ;; appropriate type, that is used, in which case no instructions are
  ;; generated.
  (cond ((and (machine-register? target)
	      (register-type? target type))
	 ;; *** Lose ***
	 ;; The following is done wrong on several counts:
	 ;; 1: memq should not be used, a vector of all machine registers
	 ;; containing booleans can be used instead.
	 ;; 2: lock-register! should wire down the register as being
	 ;; a "permanent" alias for source, since the pseudo register
	 ;; may still be referenced.  The register map abstraction
	 ;; needs to be extended for this.
	 (cond ((not (memq target available-machine-registers))
		(prefix-instructions!
		 (reference->register-transfer
		  (standard-register-reference source type true)
		  target)))
	       ((not (is-alias-for-register? target source))
		(prefix-instructions!
		 (%load-machine-register! source target
					  delete-dead-registers!))
		(lock-register! target))
	       (else
		;; *** The following may cause a cascade of copies
		;; since the following machine register assignment
		;; may assign to the register just picked. ***
		(delete-dead-registers!)
		(prefix-instructions! (clean-registers! target))
		(lock-register! target)))
	 target)
	((and (machine-register? source)
	      (register-type? source type)
	      ;; *** Lose ***
	      (memq source available-machine-registers))
	 (delete-register! target)
	 (add-pseudo-register-alias! target source)
	 source)
	(else
	 (reuse-pseudo-register-alias!
	  source type
	  (lambda (alias)
	    (delete-dead-registers!)
	    (if (machine-register? target)
		(suffix-instructions!
		 (register->register-transfer alias target))
		(add-pseudo-register-alias! target alias))
	    alias)
	  (lambda ()
	    (let ((source (standard-register-reference source type true)))
	      (delete-dead-registers!)
	      (let ((target (allocate-alias-register! target type)))
		(prefix-instructions!
		 (reference->register-transfer source target))
		target)))))))

(define (move-to-temporary-register! source type)
  ;; Allocates a temporary register, of the given `type', and loads
  ;; the contents of the register `source' into it.  Returns a
  ;; reference to that temporary.  If `source' has a reusable alias of
  ;; the appropriate type, that is used, in which case no instructions
  ;; are generated.
  (reuse-pseudo-register-alias! source type
    (lambda (alias)
      (need-register! alias)
      alias)
    (lambda ()
      (let ((target (allocate-temporary-register! type)))
	(prefix-instructions!
	 (reference->register-transfer
	  (standard-register-reference source type true)
	  target))
	target))))

(define (reuse-pseudo-register-alias! source type if-reusable if-not)
  (reuse-pseudo-register-alias source type
    (lambda (alias)
      (delete-register! alias)
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
  (if (machine-register? source)
      (if-not)
      (let ((alias (register-alias source type)))
	(cond ((not alias)
	       (if-not))
	      ((dead-register? source)
	       (if-reusable alias))
	      ((not (alias-is-unique? alias))
	       (if-reusable alias))
	      (else
	       (if-not))))))

;;; The following procedures are used when the copy is going to be
;;; transformed, and the machine has 3 operand instructions, which
;;; allow an implicit motion in the transformation operation.

;;; For example, on the DEC VAX it is cheaper to do
;;;	bicl3	op1,source,target
;;; than
;;; 	movl	source,target
;;; 	bicl2	op1,target

;;; The extra arguments are
;;; REC1, invoked if we are reusing an alias of source.
;;;      It already contains the data to operate on.
;;; REC2, invoked if a `brand-new' alias for target has been allocated.
;;;      We must take care of moving the data ourselves.

(define (with-register-copy-alias! source type target rec1 rec2)
  (if (and (machine-register? target)
	   (register-type? target type))
      (let* ((source (standard-register-reference source type true))
	     (target (register-reference target)))
	(rec2 source target))
      (reuse-pseudo-register-alias! source type
       (lambda (alias)
	 (delete-dead-registers!)
	 (if (machine-register? target)
	     (suffix-instructions! (register->register-transfer alias target))
	     (add-pseudo-register-alias! target alias))
	 (rec1 (register-reference alias)))
       (lambda ()
	 (let ((source (standard-register-reference source type true)))
	   (delete-dead-registers!)
	   (rec2 source (reference-target-alias! target type)))))))

(define (with-temporary-register-copy! source type rec1 rec2)
  (reuse-pseudo-register-alias! source type
    (lambda (alias)
      (need-register! alias)
      (rec1 (register-reference alias)))
    (lambda ()
      (rec2 (standard-register-reference source type true)
	    (reference-temporary-register! type)))))

(define (register-copy-if-available source type target)
  (and (not (machine-register? target))
       (reuse-pseudo-register-alias source type
	(lambda (reusable-alias)
	  (lambda ()
	    (delete-register! reusable-alias)
	    (delete-dead-registers!)
	    (add-pseudo-register-alias! target reusable-alias)
	    (register-reference reusable-alias)))
	(lambda () false))))

(define (temporary-copy-if-available source type)
  (reuse-pseudo-register-alias source type
    (lambda (reusable-alias)
      (lambda ()
	(delete-register! reusable-alias)
	(need-register! reusable-alias)
	(register-reference reusable-alias)))
    (lambda () false)))