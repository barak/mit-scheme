;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Register Allocator

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/regmap.scm,v 1.85 1986/12/15 05:27:32 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

#|

The register allocator provides a mechanism for allocating and
deallocating machine registers.  It manages the available machine
registers as a cache, by maintaining a ``map'' which records two kinds
of information: (1) a list of the machine registers which are not in
use; and (2) a mapping which is the association between the allocated
machine registers and the ``pseudo registers'' which they represent.

An ``alias'' is a machine register which also holds the contents of a
pseudo register.  Usually an alias is used for a short period of time,
as a store-in cache, and then eventually the contents of the alias is
written back out to the home it is associated with.  Because of the
lifetime analysis, it is possible to identify those registers which
will no longer be referenced; these are deleted from the map when they
die, and thus do not need to be saved.

A ``temporary'' is a machine register with no associated home.  It
is used during the code generation of a single RTL instruction to
hold intermediate results.

Each pseudo register that has at least one alias has an entry in the
map.  While a home is entered in the map, it may have one or more
aliases added or deleted to its entry, but if the number of aliases
ever drops to zero, the entry is removed from the map.

Each temporary has an entry in the map, with the difference being
that the entry has no pseudo register associated with it.  Thus it
need never be written out.

All registers, both machine and pseudo, are represented by
non-negative integers.  Machine registers start at zero (inclusive)
and stop at NUMBER-OF-MACHINE-REGISTERS (exclusive).  All others are
pseudo registers.  Because they are integers, we can use MEMV on lists
of registers.

AVAILABLE-MACHINE-REGISTERS should be a list of the registers which
the allocator is allowed to allocate, in the preferred order of
allocation.

(SORT-MACHINE-REGISTERS REGISTERS) should reorder a list of machine
registers into some interesting sorting order if that is desired.

(PSEUDO-REGISTER=? X Y) is true iff X and Y are the ``same'' register.
Normally, two pseudo registers are the same if their
REGISTER-RENUMBERs are equal.

|#

(define empty-register-map)
(define bind-allocator-values)

(define load-alias-register)
(define allocate-alias-register)
(define allocate-temporary-register)
(define add-pseudo-register-alias)

(define machine-register-contents)
(define pseudo-register-aliases)

(define machine-register-alias)
(define pseudo-register-alias)

(define save-machine-register)
(define save-pseudo-register)

(define delete-machine-register)
(define delete-pseudo-register)

(define delete-pseudo-registers)
(define delete-other-locations)

(define coerce-map-instructions)
(define clear-map-instructions)

(define register-allocator-package
  (make-environment

;;;; Register Map

(define-integrable make-register-map cons)
(define-integrable map-entries car)
(define-integrable map-registers cdr)

(define-export (empty-register-map)
  (make-register-map '() available-machine-registers))

(define-integrable (map-entries:search map procedure)
  (set-search (map-entries map) procedure))

(define (map-entries:find-home map pseudo-register)
  (map-entries:search map
    (lambda (entry)
      (let ((home (map-entry-home entry)))
	(and home
	     (pseudo-register=? home pseudo-register)
	     entry)))))

(define (map-entries:find-alias map register)
  (map-entries:search map
    (lambda (entry)
      ;; **** Kludge -- depends on fact that machine registers are
      ;; fixnums, and thus EQ? works on them.
      (and (memq register (map-entry-aliases entry))
	   entry))))

(define-integrable (map-entries:add map entry)
  (cons entry (map-entries map)))

(define-integrable (map-entries:delete map entry)
  (set-delete (map-entries map) entry))

(define-integrable (map-entries:delete* map entries)
  (set-difference (map-entries map) entries))

(define-integrable (map-entries:replace map old new)
  (set-substitute (map-entries map) old new))

(define-integrable (map-registers:add map register)
  (sort-machine-registers (cons register (map-registers map))))

(define-integrable (map-registers:add* map registers)
  (sort-machine-registers (append registers (map-registers map))))

(define-integrable (map-registers:delete map register)
  (set-delete (map-registers map) register))

;;;; Map Entry

(define-integrable (make-map-entry home saved-into-home? aliases)
  ;; HOME may be false, indicating that this is a temporary register.
  ;; SAVED-INTO-HOME? must be true when HOME is false.  ALIASES must
  ;; be a non-null list of registers.
  (vector home saved-into-home? aliases))

(define-integrable (map-entry-home entry)
  (vector-ref entry 0))

(define-integrable (map-entry-saved-into-home? entry)
  (vector-ref entry 1))

(define-integrable (map-entry-aliases entry)
  (vector-ref entry 2))

(define-integrable (map-entry:any-alias entry)
  (car (map-entry-aliases entry)))

(define (map-entry:add-alias entry alias)
  (make-map-entry (map-entry-home entry)
		  (map-entry-saved-into-home? entry)
		  (cons alias (map-entry-aliases entry))))

(define (map-entry:delete-alias entry alias)
  (make-map-entry (map-entry-home entry)
		  (map-entry-saved-into-home? entry)
		  (set-delete (map-entry-aliases entry) alias)))

(define (map-entry=? entry entry*)
  (and (map-entry-home entry)
       (map-entry-home entry*)
       (pseudo-register=? (map-entry-home entry)
			  (map-entry-home entry*))))

;;;; Map Constructors

;;; These constructors are responsible for maintaining consistency
;;; between the map entries and available registers.

(define (register-map:add-home map home alias)
  (make-register-map (map-entries:add map
				      (make-map-entry home true (list alias)))
		     (map-registers:delete map alias)))

(define (register-map:add-alias map entry alias)
  (make-register-map (map-entries:replace map entry
					  (map-entry:add-alias entry alias))
		     (map-registers:delete map alias)))

(define (register-map:save-entry map entry)
  (make-register-map
   (map-entries:replace map entry
			(make-map-entry (map-entry-home entry)
					true
					(map-entry-aliases entry)))
   (map-registers map)))

(define (register-map:delete-entry map entry)
  (make-register-map (map-entries:delete map entry)
		     (map-registers:add* map (map-entry-aliases entry))))

(define (register-map:delete-entries regmap entries)
  (make-register-map (map-entries:delete* regmap entries)
		     (map-registers:add* regmap
					 (apply append
						(map map-entry-aliases
						     entries)))))

(define (register-map:delete-alias map entry alias)
  (make-register-map (if (null? (cdr (map-entry-aliases entry)))
			 (map-entries:delete map entry)
			 (map-entries:replace map entry
					      (map-entry:delete-alias entry
								      alias)))
		     (map-registers:add map alias)))

(define (register-map:delete-other-aliases map entry alias)
  (make-register-map (map-entries:replace map entry
					  (let ((home (map-entry-home entry)))
					    (make-map-entry home (not home)
							    (list alias))))
		     (map-registers:add* map
					 ;; **** Kludge -- again, EQ? is
					 ;; assumed to work on machine regs.
					 (delq alias
					       (map-entry-aliases entry)))))

;;;; Register Allocator

(define (make-free-register map type needed-registers)
  (define (reallocate-alias entry)
    (let ((alias (find-alias entry)))
      (and alias
	   (delete-alias entry alias '()))))

  (define (find-alias entry)
    (list-search-positive (map-entry-aliases entry)
      (lambda (alias)
	(and (register-type? alias type)
	     (not (memv alias needed-registers))))))

  (define (delete-alias entry alias instructions)
    (allocator-values alias
		      (register-map:delete-alias map entry alias)
		      instructions))

  (or
   ;; First see if there is an unused register of the given type.
   (let ((register (list-search-positive (map-registers map)
		     (register-type-predicate type))))
     (and register
	  (allocator-values register map '())))
   ;; There are no free registers available, so must reallocate one.
   ;; First look for a temporary register that is no longer needed.
   (map-entries:search map
     (lambda (entry)
       (and (not (map-entry-home entry))
	    (reallocate-alias entry))))
   ;; Then look for a register which contains the same thing as
   ;; another register.
   (map-entries:search map
     (lambda (entry)
       (and (not (null? (cdr (map-entry-aliases entry))))
	    (reallocate-alias entry))))
   ;; Look for a non-temporary which has been saved into its home.
   (map-entries:search map
     (lambda (entry)
       (and (map-entry-home entry)
	    (map-entry-saved-into-home? entry)
	    (reallocate-alias entry))))
   ;; Finally, save out a non-temporary and reallocate its register.
   (map-entries:search map
     (lambda (entry)
       (and (map-entry-home entry)
	    (not (map-entry-saved-into-home? entry))
	    (let ((alias (find-alias entry)))
	      (and alias
		   (delete-alias entry alias
				 (save-into-home-instruction entry)))))))
   ;; Reaching this point indicates all registers are allocated.
   (error "MAKE-FREE-REGISTER: Unable to allocate register")))

;;;; Allocator Operations

(let ()

(define-export (load-alias-register map type needed-registers home)
  ;; Finds or makes an alias register for HOME, and loads HOME's
  ;; contents into that register.
  (let ((entry (map-entries:find-home map home)))
    (or (use-existing-alias map entry type)
	(bind-allocator-values (make-free-register map type needed-registers)
	  (lambda (alias map instructions)
	    (if entry
		;; MAKE-FREE-REGISTER will not flush ENTRY because it
		;; has no aliases of the appropriate TYPE.
		(allocator-values
		 alias
		 (register-map:add-alias map entry alias)
		 (append! instructions
			  (register->register-transfer
			   (map-entry:any-alias entry)
			   alias)))
		(allocator-values
		 alias
		 (register-map:add-home map home alias)
		 (append! instructions
			  (home->register-transfer home alias)))))))))

(define-export (allocate-alias-register map type needed-registers home)
  ;; Finds or makes an alias register for HOME.  Used when about to
  ;; modify HOME's contents.
  (let ((entry (map-entries:find-home map home)))
    (or (use-existing-alias map entry type)
	(bind-allocator-values (make-free-register map type needed-registers)
	  (lambda (alias map instructions)
	    (allocator-values alias
			      (if entry
				  ;; MAKE-FREE-REGISTER will not flush
				  ;; ENTRY because it has no aliases
				  ;; of the appropriate TYPE.
				  (register-map:add-alias map entry alias)
				  (register-map:add-home map home alias))
			      instructions))))))

(define (use-existing-alias map entry type)
  (and entry
       (let ((alias (list-search-positive (map-entry-aliases entry)
		      (register-type-predicate type))))
	 (and alias
	      (allocator-values alias map '())))))

)

(define-export (allocate-temporary-register map type needed-registers)
  (bind-allocator-values (make-free-register map type needed-registers)
    (lambda (alias map instructions)
      (allocator-values alias
			(register-map:add-home map false alias)
			instructions))))

(define-export (add-pseudo-register-alias map register alias)
  (let ((entry (map-entries:find-home map register)))
    (if entry
	(register-map:add-alias map entry alias)
	(register-map:add-home map register alias))))

(define-export (machine-register-contents map register)
  (let ((entry (map-entries:find-alias map register)))
    (and entry
	 (map-entry-home entry))))

(define-export (pseudo-register-aliases map register)
  (let ((entry (map-entries:find-home map register)))
    (and entry
	 (map-entry-aliases entry))))

(define-export (machine-register-alias map type register)
  (let ((entry (map-entries:find-alias map register)))
    (and entry
	 (list-search-positive (map-entry-aliases entry)
	   (lambda (register*)
	     (and (not (eq? register register*))
		  (register-type? type register*)))))))

(define-export (pseudo-register-alias map type register)
  (let ((entry (map-entries:find-home map register)))
    (and entry
	 (list-search-positive (map-entry-aliases entry)
	   (register-type-predicate type)))))

(define-export (save-machine-register map register receiver)
  (let ((entry (map-entries:find-alias map register)))
    (if (and entry
	     (not (map-entry-saved-into-home? entry))
	     (null? (cdr (map-entry-aliases entry))))
	(receiver (register-map:save-entry map entry)
		  (save-into-home-instruction entry))
	(receiver map '()))))

(define-export (save-pseudo-register map register receiver)
  (let ((entry (map-entries:find-home map register)))
    (if (and entry
	     (not (map-entry-saved-into-home? entry)))
	(receiver (register-map:save-entry map entry)
		  (save-into-home-instruction entry))
	(receiver map '()))))

(define-export (delete-machine-register map register)
  (let ((entry (map-entries:find-alias map register)))
    (if entry
	(register-map:delete-alias map entry register)
	map)))

(define-export (delete-pseudo-register map register receiver)
  (let ((entry (map-entries:find-home map register)))
    (if entry
	(receiver (register-map:delete-entry map entry)
		  (map-entry-aliases entry))
	(receiver map '()))))

(define-export (delete-pseudo-registers map registers receiver)
  ;; Used to remove dead registers from the map.
  (let loop ((registers registers)
	     (receiver
	      (lambda (entries aliases)
		(receiver (register-map:delete-entries map entries)
			  aliases))))
    (if (null? registers)
	(receiver '() '())
	(loop (cdr registers)
	  (let ((entry (map-entries:find-home map (car registers))))
	    (if entry
		(lambda (entries aliases)
		  (receiver (cons entry entries) aliases))
		receiver))))))

(define-export (delete-other-locations map register)
  ;; Used in assignments to indicate that other locations containing
  ;; the same value no longer contain the value for a given home.
  (register-map:delete-other-aliases
   map
   (or (map-entries:find-alias map register)
       (error "DELETE-OTHER-LOCATIONS: Missing entry" register))
   register))

(define-integrable (allocator-values alias map instructions)
  (vector alias map instructions))

(define-export (bind-allocator-values values receiver)
  (receiver (vector-ref values 0)
	    (vector-ref values 1)
	    (vector-ref values 2)))

(define (save-into-home-instruction entry)
  (register->home-transfer (map-entry:any-alias entry)
			   (map-entry-home entry)))

;;;; Map Coercion

;;; These operations generate the instructions to coerce one map into
;;; another.  They are used when joining two branches of a control
;;; flow graph which have different maps (e.g. in a loop.)

(let ()

(define-export (coerce-map-instructions input-map output-map)
  (three-way-sort map-entry=?
		  (map-entries input-map)
		  (map-entries output-map)
    (lambda (input-entries shared-entries output-entries)
      ((input-loop input-map
		   ((shared-loop (output-loop (empty-register-map)
					      output-entries))
		    shared-entries))
       input-entries))))

(define-export (clear-map-instructions input-map)
  ((input-loop input-map '()) (map-entries input-map)))

(define (input-loop map tail)
  (define (loop entries)
    (if (null? entries)
	tail
	(let ((instructions (loop (cdr entries))))
	  (if (map-entry-saved-into-home? (car entries))
	      instructions
	      (append! (save-into-home-instruction (car entries))
		       instructions)))))
  loop)

(define (shared-loop tail)
  (define (loop entries)
    (if (null? entries)
	tail
	(let ((input-aliases (map-entry-aliases (caar entries))))
	  (define (loop output-aliases)
	    (if (null? output-aliases)
		(shared-loop (cdr entries))
		(append! (register->register-transfer (car input-aliases)
						      (car output-aliases))
			 (loop (cdr output-aliases)))))
	  (loop (set-difference (map-entry-aliases (cdar entries))
				input-aliases)))))
  loop)

(define (output-loop map entries)
  (if (null? entries)
      '()
      (let ((instructions (output-loop map (cdr entries)))
	    (home (map-entry-home (car entries))))
	(if home
	    (let ((aliases (map-entry-aliases (car entries))))
	      (define (loop registers)
		(if (null? registers)
		    instructions
		    (append! (register->register-transfer (car aliases)
							  (car registers))
			     (loop (cdr registers)))))
	      (append! (home->register-transfer home (car aliases))
		       (loop (cdr aliases))))
	    instructions))))

)

;;; end REGISTER-ALLOCATOR-PACKAGE
))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access register-allocator-package lap-generator-package compiler-package)
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
)