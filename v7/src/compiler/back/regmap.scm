#| -*-Scheme-*-

$Id: regmap.scm,v 4.16 2003/02/14 18:28:01 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; Register Allocator
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

#|

The register allocator provides a mechanism for allocating and
deallocating machine registers.  It manages the available machine
registers as a cache, by maintaining a "map" that records two kinds of
information: (1) a list of the machine registers that are not in use;
and (2) a mapping that is the association between the allocated
machine registers and the "pseudo registers" that they represent.

An "alias" is a machine register that also holds the contents of a
pseudo register.  Usually an alias is used for a short period of time,
as a store-in cache, and then eventually the contents of the alias is
written back out to the home it is associated with.  Because of the
lifetime analysis, it is possible to identify those registers that
will no longer be referenced; these are deleted from the map when they
die, and thus do not need to be saved.

A "temporary" is a machine register with no associated home.  It is
used during the code generation of a single RTL instruction to hold
intermediate results.

Each pseudo register that has at least one alias has an entry in the
map.  While a home is entered in the map, it may have one or more
aliases added or deleted to its entry, but if the number of aliases
ever drops to zero, the entry is removed from the map.

Each temporary has an entry in the map, with the difference being that
the entry has no pseudo register associated with it.  Thus it need
never be written out.

All registers, both machine and pseudo, are represented by
non-negative integers.  Machine registers start at zero (inclusive)
and stop at `number-of-machine-registers' (exclusive).  All others are
pseudo registers.  Because they are integers, we can use `eqv?' to
compare register numbers.

`available-machine-registers' should be a list of the registers that
the allocator is allowed to allocate, in the preferred order of
allocation.

`(sort-machine-registers registers)' should reorder a list of machine
registers into some interesting sorting order.

|#

(define (register-type? register type)
  (if type
      (eq? type (register-type register))
      (register-value-class=word? register)))

(define ((register-type-predicate type) register)
  (register-type? register type))

;;;; Register Map

(define-integrable make-register-map cons)
(define-integrable map-entries car)
(define-integrable map-registers cdr)

(define (empty-register-map)
  (make-register-map '() available-machine-registers))

(define (map-entries:search map procedure)
  ;; This procedure is used only when attempting to free up an
  ;; existing register.  Because of this, it must find an LRU
  ;; register.  Since we order the map entries starting with the MRU
  ;; registers and working towards the LRU, search the entries
  ;; starting from the end of the list and working forward.
  (let loop ((entries (map-entries map)))
    (and (not (null? entries))
	 (or (loop (cdr entries))
	     (procedure (car entries))))))

(define (map-entries:find-home map pseudo-register)
  (let loop ((entries (map-entries map)))
    (and (not (null? entries))
	 (or (and (map-entry-home (car entries))
		  (eqv? (map-entry-home (car entries)) pseudo-register)
		  (car entries))
	     (loop (cdr entries))))))

(define (map-entries:find-alias map register)
  (let loop ((entries (map-entries map)))
    (and (not (null? entries))
	 ;; **** Kludge -- depends on fact that machine registers are
	 ;; fixnums, and thus EQ? works on them.
	 (or (and (memq register (map-entry-aliases (car entries)))
		  (car entries))
	     (loop (cdr entries))))))

(define-integrable (map-entries:add map entry)
  (cons entry (map-entries map)))

(define-integrable (map-entries:delete map entry)
  (eq-set-delete (map-entries map) entry))

(define-integrable (map-entries:delete* map entries)
  (eq-set-difference (map-entries map) entries))

(define (map-entries:replace map old new)
  (let loop ((entries (map-entries map)))
    (if (null? entries)
	'()
	(cons (if (eq? (car entries) old) new (car entries))
	      (loop (cdr entries))))))

(define (map-entries:replace&touch map old new)
  (cons new (map-entries:delete map old)))

(define-integrable (map-registers:add map register)
  (sort-machine-registers (cons register (map-registers map))))

(define-integrable (map-registers:add* map registers)
  (sort-machine-registers (append registers (map-registers map))))

(define-integrable (map-registers:delete map register)
  (eqv-set-delete (map-registers map) register))

(define-integrable (map-registers:replace map old new)
  (eqv-set-substitute (map-registers map) old new))

;;;; Map Entry

;; A map entry has four parts:
;;  HOME is either a pseudo-register (which has a physical address in
;;        memory associated with it) or #F indicating that the value
;;        can be flushed when the last alias is reused
;;  SAVED-INTO-HOME? is a boolean that tells whether the value in the
;;        live register can be dropped rather than pushed to the home
;;        if the last live register is needed for other purposes
;;  ALIASES is a list of machine registers that contain the quantity
;;        being mapped (pseudo-register, cached value, etc.)
;;  LABEL is a tag to associate with the computed contents of the live
;;        registers holding this value.  This allows individual back
;;        ends to remember labels or other hard-to-generate constant
;;        values and avoid regenerating them.

(define-integrable (make-map-entry home saved-into-home? aliases label)
  ;; HOME may be false, indicating that this is a temporary register.
  ;; SAVED-INTO-HOME? must be true when HOME is false.  ALIASES must
  ;; be a non-null list of registers.
  (vector home saved-into-home? aliases label))

(define-integrable (map-entry-home entry)
  (vector-ref entry 0))

(define-integrable (map-entry-saved-into-home? entry)
  (vector-ref entry 1))

(define-integrable (map-entry-aliases entry)
  (vector-ref entry 2))

(define-integrable (map-entry-label entry)
  (vector-ref entry 3))

(define-integrable (map-entry:any-alias entry)
  (car (map-entry-aliases entry)))

(define (map-entry:find-alias entry type needed-registers)
  (list-search-positive (map-entry-aliases entry)
    (lambda (alias)
      (and (register-type? alias type)
	   (not (memv alias needed-registers))))))

(define (map-entry:aliases entry type needed-registers)
  (list-transform-positive (map-entry-aliases entry)
    (lambda (alias)
      (and (register-type? alias type)
	   (not (memv alias needed-registers))))))

(define (map-entry:add-alias entry alias)
  (make-map-entry (map-entry-home entry)
		  (map-entry-saved-into-home? entry)
		  (cons alias (map-entry-aliases entry))
		  (map-entry-label entry)))

(define (map-entry:delete-alias entry alias)
  (make-map-entry (map-entry-home entry)
		  (map-entry-saved-into-home? entry)
		  (eq-set-delete (map-entry-aliases entry) alias)
		  (map-entry-label entry)))

(define (map-entry:replace-alias entry old new)
  (make-map-entry (map-entry-home entry)
		  (map-entry-saved-into-home? entry)
		  (eq-set-substitute (map-entry-aliases entry) old new)
		  (map-entry-label entry)))

(define-integrable (map-entry=? entry entry*)
  (eqv? (map-entry-home entry) (map-entry-home entry*)))

;;;; Map Constructors

;;; These constructors are responsible for maintaining consistency
;;; between the map entries and available registers.

(define (register-map:add-home map home alias saved-into-home?)
  (make-register-map (map-entries:add map
				      (make-map-entry home
						      saved-into-home?
						      (list alias)
						      false))
		     (map-registers:delete map alias)))

(define (register-map:add-alias map entry alias)
  (make-register-map
   (map-entries:replace&touch map
			      entry
			      (map-entry:add-alias entry alias))
   (map-registers:delete map alias)))

(define (register-map:replace-alias map entry old new)
  (make-register-map
   (map-entries:replace&touch map
			      entry
			      (map-entry:replace-alias entry old new))
   (map-registers:delete map new)))

(define (register-map:save-entry map entry)
  (make-register-map
   (map-entries:replace&touch map
			      entry
			      (make-map-entry (map-entry-home entry)
					      true
					      (map-entry-aliases entry)
					      (map-entry-label entry)))
   (map-registers map)))

(define-integrable (pseudo-register-entry->temporary-entry entry)
  (make-map-entry false
		  true
		  (map-entry-aliases entry)
		  (map-entry-label entry)))

(define (register-map:entry->temporary map entry)
  (make-register-map
   (map-entries:replace&touch map
			      entry
			      (pseudo-register-entry->temporary-entry entry))
   (map-registers map)))

(define (register-map:delete-entry map entry)
  (make-register-map (map-entries:delete map entry)
		     (map-registers:add* map (map-entry-aliases entry))))

(define (register-map:delete-entries regmap entries)
  (if (null? entries)
      regmap
      (make-register-map (map-entries:delete* regmap entries)
			 (map-registers:add* regmap
					     (apply append
						    (map map-entry-aliases
							 entries))))))

(define (register-map:delete-alias map entry alias)
  (make-register-map (if (null? (cdr (map-entry-aliases entry)))
			 (map-entries:delete map entry)
			 (map-entries:replace map
					      entry
					      (map-entry:delete-alias entry
								      alias)))
		     (map-registers:add map alias)))

(define (register-map:delete-other-aliases map entry alias)
  (make-register-map
   (map-entries:replace map
			entry
			(let ((home (map-entry-home entry)))
			  (make-map-entry home
					  (not home)
					  (list alias)
					  (map-entry-label entry))))
   (map-registers:add* map
		       ;; **** Kludge -- again, EQ? is
		       ;; assumed to work on machine regs.
		       (delq alias
			     (map-entry-aliases entry)))))

(define (register-map:entries->temporaries regmap entries)
  (if (null? entries)
      regmap
      (make-register-map
       (map* (map-entries:delete* regmap entries)
	     pseudo-register-entry->temporary-entry
	     entries)
       (map-registers regmap))))

(define (register-map:keep-live-entries map live-registers)
  (let loop
      ((entries (map-entries map))
       (registers (map-registers map))
       (entries* '()))
    (cond ((null? entries)
	   (make-register-map (reverse! entries*)
			      (sort-machine-registers registers)))
	  ((let ((home (map-entry-home (car entries))))
	     (and home
		  (regset-member? live-registers home)))
	   (loop (cdr entries)
		 registers
		 (cons (car entries) entries*)))
	  (else
	   (loop (cdr entries)
		 (append (map-entry-aliases (car entries)) registers)
		 entries*)))))

(define (map-equal? x y)
  (let loop
      ((x-entries (map-entries x))
       (y-entries (list-transform-positive (map-entries y) map-entry-home)))
    (cond ((null? x-entries)
	   (null? y-entries))
	  ((not (map-entry-home (car x-entries)))
	   (loop (cdr x-entries) y-entries))
	  (else
	   (and (not (null? y-entries))
		(let ((y-entry
		       (list-search-positive y-entries
			 (let ((home (map-entry-home (car x-entries))))
			   (lambda (entry)
			     (eqv? (map-entry-home entry) home))))))
		  (and y-entry
		       (boolean=? (map-entry-saved-into-home? (car x-entries))
				  (map-entry-saved-into-home? y-entry))
		       (eqv-set-same-set? (map-entry-aliases (car x-entries))
					  (map-entry-aliases y-entry))
		       (loop (cdr x-entries) (delq! y-entry y-entries)))))))))

;;;; Register Allocator

(define (make-free-register map type needed-registers)
  (or
   ;; First attempt to find a register that can be used without saving
   ;; its value.
   (find-free-register map type needed-registers)
   ;; Then try to recycle a register by saving its value elsewhere.
   (map-entries:search map
     (lambda (entry)
       (and
	(map-entry-home entry)
	(not (map-entry-saved-into-home? entry))
	(let ((alias (map-entry:find-alias entry type needed-registers)))
	  (and alias
	       (or
		;; If we are reallocating a register of a specific type, first
		;; see if there is an available register of some other
		;; assignment-compatible type that we can stash the value in.
		(and type
		     (let ((values
			    (find-free-register
			     map
			     (if (register-types-compatible? type false)
				 false
				 type)
			     (cons alias needed-registers))))
		       (and
			values
			(bind-allocator-values values
			  (lambda (alias* map instructions)
			    (allocator-values
			     alias
			     (register-map:replace-alias map
							 entry
							 alias
							 alias*)
			     (LAP ,@instructions
				  ,@(register->register-transfer alias
								 alias*))))))))
		;; There is no other register that we can use, so we
		;; must save the value out into the home.
		(allocator-values alias
				  (register-map:delete-alias map entry alias)
				  (save-into-home-instruction entry))))))))
   ;; Finally, see if there is a temporary label register that can be
   ;; recycled.  Label registers are considered after ordinary
   ;; registers, because on the RISC machines that use them, it is
   ;; more expensive to generate a new label register than it is to
   ;; save an ordinary register.
   (map-entries:search map
     (lambda (entry)
       (and (map-entry-label entry)
	    (not (map-entry-home entry))
	    (let ((alias (map-entry:find-alias entry type needed-registers)))
	      (and alias
		   (allocator-values
		    alias
		    (register-map:delete-alias map entry alias)
		    (LAP)))))))
   (error "MAKE-FREE-REGISTER: Unable to allocate register")))

(define (find-free-register map type needed-registers)
  (define (reallocate-alias entry)
    (let ((alias (map-entry:find-alias entry type needed-registers)))
      (and alias
	   (allocator-values alias
			     (register-map:delete-alias map entry alias)
			     (LAP)))))
  ;; First see if there is an unused register of the given type.
  (or (let ((register
	     (list-search-positive (map-registers map)
	       (lambda (alias)
		 (and (register-type? alias type)
		      (not (memv alias needed-registers)))))))
	(and register (allocator-values register map (LAP))))
      ;; There are no free registers available, so must reallocate
      ;; one.  First look for a temporary register that is no longer
      ;; needed.
      (map-entries:search map
	(lambda (entry)
	  (and (not (map-entry-home entry))
	       (not (map-entry-label entry))
	       (reallocate-alias entry))))
      ;; Then look for a register that contains the same thing as
      ;; another register.
      (map-entries:search map
	(lambda (entry)
	  (and (not (null? (cdr (map-entry-aliases entry))))
	       (reallocate-alias entry))))
      ;; Look for a non-temporary that has been saved into its home.
      (map-entries:search map
	(lambda (entry)
	  (and (map-entry-home entry)
	       (map-entry-saved-into-home? entry)
	       (reallocate-alias entry))))))

(define (allocate-register-without-spill? map type needed-registers)
  ;; True iff a register of `type' can be allocated without saving any
  ;; registers into their homes.
  (or (free-register-exists? map type needed-registers)
      (map-entries:search map
	(lambda (entry)
	  (let ((alias (map-entry:find-alias entry type needed-registers)))
	    (and alias
		 (free-register-exists?
		  map
		  (if (register-types-compatible? type false) false type)
		  (cons alias needed-registers))))))))

(define (free-register-exists? map type needed-registers)
  ;; True iff a register of `type' can be allocated without first
  ;; saving its contents.
  (or (allocate-register-without-unload? map type needed-registers)
      (map-entries:search map
	(lambda (entry)
	  (and (map-entry-home entry)
	       (map-entry-saved-into-home? entry)
	       (map-entry:find-alias entry type needed-registers))))))

(define (allocate-register-without-unload? map type needed-registers)
  ;; True iff a register of `type' can be allocated without displacing
  ;; any pseudo-registers from the register map.
  (or (list-search-positive (map-registers map)
	(lambda (alias)
	  (and (register-type? alias type)
	       (not (memv alias needed-registers)))))
      (map-entries:search map
	(lambda (entry)
	  (and (map-entry:find-alias entry type needed-registers)
	       (or (not (map-entry-home entry))
		   (not (null? (cdr (map-entry-aliases entry))))))))))

;;;; Allocator Operations

(define (load-alias-register map type needed-registers home)
  ;; Finds or makes an alias register for HOME, and loads HOME's
  ;; contents into that register.
  (or (let ((entry (map-entries:find-home map home)))
	(and entry
	     (let ((alias (list-search-positive (map-entry-aliases entry)
			    (register-type-predicate type))))
	       (and alias
		    (allocator-values alias map (LAP))))))
      (bind-allocator-values (make-free-register map type needed-registers)
	(lambda (alias map instructions)
	  (let ((entry (map-entries:find-home map home)))
	    (if entry
		(allocator-values
		 alias
		 (register-map:add-alias map entry alias)
		 (LAP ,@instructions
		      ,@(register->register-transfer
			 (map-entry:any-alias entry)
			 alias)))
		(allocator-values
		 alias
		 (register-map:add-home map home alias true)
		 (LAP ,@instructions
		      ,@(home->register-transfer home alias)))))))))

(define (allocate-alias-register map type needed-registers home)
  ;; Makes an alias register for `home'.  Used when about to modify
  ;; `home's contents.  It is assumed that no entry exists for `home'.
  (bind-allocator-values (make-free-register map type needed-registers)
    (lambda (alias map instructions)
      (allocator-values alias
			(register-map:add-home map home alias false)
			instructions))))

(define (allocate-temporary-register map type needed-registers)
  (bind-allocator-values (make-free-register map type needed-registers)
    (lambda (alias map instructions)
      (allocator-values alias
			(register-map:add-home map false alias true)
			instructions))))

(define (add-pseudo-register-alias map register alias saved-into-home?)
  (let ((map (delete-machine-register map alias)))
    (let ((entry (map-entries:find-home map register)))
      (if entry
	  (register-map:add-alias map entry alias)
	  (register-map:add-home map register alias saved-into-home?)))))

(define (machine-register-contents map register)
  (let ((entry (map-entries:find-alias map register)))
    (and entry
	 (map-entry-home entry))))

(define (pseudo-register-aliases map register)
  (let ((entry (map-entries:find-home map register)))
    (and entry
	 (map-entry-aliases entry))))

(define (machine-register-alias map type register)
  "Returns another machine register, of the given TYPE, which holds
the same value as REGISTER.  If no such register exists, returns #F."
  (let ((entry (map-entries:find-alias map register)))
    (and entry
	 (list-search-positive (map-entry-aliases entry)
	   (lambda (register*)
	     (and (not (eq? register register*))
		  (register-type? type register*)))))))

(define (pseudo-register-alias map type register)
  "Returns a machine register, of the given TYPE, which is an alias
for REGISTER.  If no such register exists, returns #F."
  (let ((entry (map-entries:find-home map register)))
    (and entry
	 (list-search-positive (map-entry-aliases entry)
	   (register-type-predicate type)))))

(define (machine-register-is-unique? map register)
  "True if REGISTER has no other aliases."
  (let ((entry (map-entries:find-alias map register)))
    (or (not entry)
	(null? (cdr (map-entry-aliases entry))))))

(define (machine-register-holds-unique-value? map register)
  "True if the contents of REGISTER is not saved anywhere else."
  (let ((entry (map-entries:find-alias map register)))
    (or (not entry)
	(and (null? (cdr (map-entry-aliases entry)))
	     (not (map-entry-saved-into-home? entry))))))

(define (is-pseudo-register-alias? map maybe-alias register)
  (let ((entry (map-entries:find-home map register)))
    (and entry
	 (list-search-positive (map-entry-aliases entry)
	   (lambda (alias)
	     (eqv? maybe-alias alias))))))

(define (save-machine-register map register receiver)
  (let ((entry (map-entries:find-alias map register)))
    (if (and entry
	     (not (map-entry-saved-into-home? entry))
	     (null? (cdr (map-entry-aliases entry))))
	(receiver (register-map:save-entry map entry)
		  (save-into-home-instruction entry))
	(receiver map (LAP)))))

(define (save-pseudo-register map register receiver)
  (let ((entry (map-entries:find-home map register)))
    (if (and entry
	     (not (map-entry-saved-into-home? entry)))
	(receiver (register-map:save-entry map entry)
		  (save-into-home-instruction entry))
	(receiver map (LAP)))))

(define (register-map-label map type)
  (let loop ((entries (map-entries map)))
    (if (null? entries)
	(values false false)
	(let ((alias
	       (and (map-entry-label (car entries))
		    (map-entry:find-alias (car entries) type '()))))
	  (if alias
	      (values (map-entry-label (car entries)) alias)
	      (loop (cdr entries)))))))

(define (register-map-labels map type)
  (let loop ((entries (map-entries map)))
    (if (null? entries)
	'()
	(let ((label (map-entry-label (car entries))))
	  (if label
	      (let ((aliases (map-entry:aliases (car entries) type '())))
		(if (not (null? aliases))
		    (cons (cons label aliases)
			  (loop (cdr entries)))
		    (loop (cdr entries))))
	      (loop (cdr entries)))))))

(define (set-machine-register-label map register label)
  (let ((entry (map-entries:find-alias map register)))
    (if entry
	(make-register-map (map-entries:replace
			    map
			    entry
			    (make-map-entry (map-entry-home entry)
					    (map-entry-saved-into-home? entry)
					    (map-entry-aliases entry)
					    label))
			   (map-registers map))
	(make-register-map (map-entries:add map
					    (make-map-entry false
							    true
							    (list register)
							    label))
			   (map-registers:delete map register)))))

(define (pseudo-register-saved-into-home? map register)
  (let ((entry (map-entries:find-home map register)))
    (or (not entry)
	(map-entry-saved-into-home? entry))))

(define (delete-machine-register map register)
  (let ((entry (map-entries:find-alias map register)))
    (if entry
	(register-map:delete-alias map entry register)
	map)))

(define (delete-pseudo-register map register receiver)
  ;; If the pseudo-register has any alias with a cached value --
  ;; indicated by a labelled entry --  then we convert the map entry to
  ;; represent a temporary register rather than a pseudo register.
  ;;
  ;; receiver gets the new map and the aliases that are no longer
  ;; needed (even if it is convenient to keep them around)
  (let ((entry (map-entries:find-home map register)))
    (cond ((not entry) (receiver map '()))
	  ((not (map-entry-label entry))
	   (receiver (register-map:delete-entry map entry)
		     (map-entry-aliases entry)))
	  (else				; Pseudo -> temporary
	   (receiver (register-map:entry->temporary map entry)
		     (map-entry-aliases entry))))))

(define (delete-pseudo-registers map registers)
  ;; Used to remove dead registers from the map.
  ;; See comments to delete-pseudo-register, above.

  (define (create-new-map delete transform)
    (register-map:entries->temporaries (register-map:delete-entries map delete)
				       transform))


  (let loop ((registers registers)
	     (entries-to-delete '())
	     (entries-to-transform '()))
    (if (null? registers)
	(create-new-map entries-to-delete entries-to-transform)
	(let ((entry (map-entries:find-home map (car registers))))
	  (loop (cdr registers)
		(if (and entry (not (map-entry-label entry)))
		    (cons entry entries-to-delete)
		    entries-to-delete)
		(if (and entry (map-entry-label entry))
		    (cons entry entries-to-transform)
		    entries-to-transform))))))

(define (delete-other-locations map register)
  ;; Used in assignments to indicate that other locations containing
  ;; the same value no longer contain the value for a given home.
  (register-map:delete-other-aliases
   map
   (or (map-entries:find-alias map register)
       (error "DELETE-OTHER-LOCATIONS: Missing entry" register))
   register))

(define-integrable (allocator-values alias map instructions)
  (vector alias map instructions))

(define (bind-allocator-values values receiver)
  (receiver (vector-ref values 0)
	    (vector-ref values 1)
	    (vector-ref values 2)))

(define (save-into-home-instruction entry)
  (register->home-transfer (map-entry:any-alias entry)
			   (map-entry-home entry)))

(define (register-map-live-homes map)
  (let loop ((entries (map-entries map)))
    (if (null? entries)
	'()
	(let ((home (map-entry-home (car entries))))
	  (if home
	      (cons home (loop (cdr entries)))
	      (loop (cdr entries)))))))

(define (register-map-clear? map)
  (for-all? (map-entries map) map-entry-saved-into-home?))

;;;; Map Coercion

;;; These operations generate the instructions to coerce one map into
;;; another.  They are used when joining two branches of a control
;;; flow graph that have different maps (e.g. in a loop.)

(package (coerce-map-instructions clear-map-instructions)

(define-export (coerce-map-instructions input-map output-map)
  (three-way-sort map-entry=?
		  (map-entries input-map)
		  (map-entries output-map)
    (lambda (input-entries shared-entries output-entries)
      (input-loop input-entries
		  (shared-loop shared-entries
			       (output-loop output-entries))))))

(define-export (clear-map-instructions input-map)
  input-map
  (input-loop (map-entries input-map) (LAP)))

(define (input-loop entries tail)
  (let loop ((entries entries))
    (cond ((null? entries)
	   tail)
	  ((map-entry-saved-into-home? (car entries))
	   (loop (cdr entries)))
	  (else
	   (LAP ,@(save-into-home-instruction (car entries))
		,@(loop (cdr entries)))))))

(define (shared-loop entries tail)
  (let entries-loop ((entries entries))
    (if (null? entries)
	tail
	(let ((input-aliases (map-entry-aliases (caar entries))))
	  (let aliases-loop
	      ((output-aliases
		(eqv-set-difference (map-entry-aliases (cdar entries))
				    input-aliases)))
	    (if (null? output-aliases)
		(entries-loop (cdr entries))
		(LAP ,@(register->register-transfer (car input-aliases)
						    (car output-aliases))
		     ,@(aliases-loop (cdr output-aliases)))))))))

(define (output-loop entries)
  (if (null? entries)
      (LAP)
      (let ((home (map-entry-home (car entries))))
	(if home
	    (let ((aliases (map-entry-aliases (car entries))))
	      (LAP ,@(home->register-transfer home (car aliases))
		   ,@(let registers-loop ((registers (cdr aliases)))
		       (if (null? registers)
			   (output-loop (cdr entries))
			   (LAP ,@(register->register-transfer
				   (car aliases)
				   (car registers))
				,@(registers-loop (cdr registers)))))))
	    (output-loop (cdr entries))))))

)