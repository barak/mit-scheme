;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
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

;;;; LAP Code Generation

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/lapgn1.scm,v 1.24 1987/01/01 19:41:05 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

(define *block-start-label*)
(define *code-object-label*)
(define *code-object-entry*)
(define *current-rnode*)
(define *dead-registers*)

(define (generate-lap quotations procedures continuations receiver)
  (with-new-node-marks
   (lambda ()
     (fluid-let ((*next-constant* 0)
		 (*interned-constants* '())
		 (*block-start-label* (generate-label))
		 (*code-object-label*)
		 (*code-object-entry*))
       (for-each (lambda (quotation)
		   (cgen-entry quotation quotation-rtl-entry))
		 quotations)
       (for-each (lambda (procedure)
		   (cgen-entry procedure procedure-rtl-entry))
		 procedures)
       (for-each (lambda (continuation)
		   (cgen-entry continuation continuation-rtl-entry))
		 continuations)
       (receiver *interned-constants* *block-start-label*)))))

(define (cgen-entry object extract-entry)
  (set! *code-object-label* (code-object-label-initialize object))
  (let ((rnode (extract-entry object)))
    (set! *code-object-entry* rnode)
    (cgen-rnode rnode)))

(define *cgen-rules*
  '())

(define (add-statement-rule! pattern result-procedure)
  (set! *cgen-rules*
	(cons (cons pattern result-procedure)
	      *cgen-rules*))
  pattern)

(define (cgen-rnode rnode)
  (define (cgen-right-node edge)
    (let ((next (edge-next-node edge)))
      (if (and next (not (node-marked? next)))
	  (begin (if (node-previous>1? next)
		     (let ((snode (statement->snode '(NOOP))))
		       (set-rnode-lap! snode
				       (clear-map-instructions
					(rnode-register-map rnode)))
		       (node-mark! snode)
		       (edge-insert-snode! edge snode)))
		 (cgen-rnode next)))))
  (node-mark! rnode)
  ;; LOOP is for easy restart while debugging.
  (let loop ()
    (let ((match-result (pattern-lookup *cgen-rules* (rnode-rtl rnode))))
      (if match-result
	  (fluid-let ((*current-rnode* rnode)
		      (*dead-registers* (rnode-dead-registers rnode))
		      (*register-map* (rnode-input-register-map rnode))
		      (*prefix-instructions* '())
		      (*needed-registers* '()))
	    (let ((instructions (match-result)))
	      (set-rnode-lap! rnode
			      (append! *prefix-instructions* instructions)))
	    (delete-dead-registers!)
	    (set-rnode-register-map! rnode *register-map*))
	  (begin (error "CGEN-RNODE: No matching rules" (rnode-rtl rnode))
		 (loop)))))
  (if (rtl-snode? rnode)
      (cgen-right-node (snode-next-edge rnode))
      (begin (cgen-right-node (pnode-consequent-edge rnode))
	     (cgen-right-node (pnode-alternative-edge rnode)))))

(define (rnode-input-register-map rnode)
  (if (or (eq? rnode *code-object-entry*)
	  (not (node-previous=1? rnode)))
      (empty-register-map)
      (let ((previous (node-previous-first rnode)))
	(let ((map (rnode-register-map previous)))
	  (if (rtl-pnode? previous)
	      (delete-pseudo-registers
	       map
	       (regset->list
		(regset-difference
		 (bblock-live-at-exit (node-bblock previous))
		 (bblock-live-at-entry (node-bblock rnode))))
	       (lambda (map aliases) map))
	      map)))))

;;;; Machine independent stuff

(define *register-map*)
(define *prefix-instructions*)
(define *needed-registers*)

(define-integrable (prefix-instructions! instructions)
  (set! *prefix-instructions* (append! *prefix-instructions* instructions)))

(define-integrable (need-register! register)
  (set! *needed-registers* (cons register *needed-registers*)))

(define (maybe-need-register! register)
  (if register (need-register! register))
  register)

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

(define (allocate-temporary-register! type)
  (bind-allocator-values (allocate-temporary-register *register-map* type
						      *needed-registers*)
    store-allocator-values!))

(define (store-allocator-values! alias map instructions)
  (need-register! alias)
  (set! *register-map* map)
  (prefix-instructions! instructions)
  alias)

(define (move-to-alias-register! source type target)
  (reuse-pseudo-register-alias! source type
    (lambda (reusable-alias)
      (add-pseudo-register-alias! target reusable-alias))
    (lambda ()
      (allocate-alias-register! target type))))

(define (move-to-temporary-register! source type)
  (reuse-pseudo-register-alias! source type
    need-register!
    (lambda ()
      (allocate-temporary-register! type))))

(define (reuse-pseudo-register-alias! source type if-reusable if-not)
  (let ((reusable-alias
	 (and (dead-register? source)
	      (register-alias source type))))
    (if reusable-alias
	(begin (delete-dead-registers!)
	       (if-reusable reusable-alias)
	       (register-reference reusable-alias))
	(let ((source (coerce->any source)))
	  (delete-dead-registers!)
	  (let ((target (register-reference (if-not))))
	    (prefix-instructions! `((MOVE L ,source ,target)))
	    target)))))

(define (add-pseudo-register-alias! register alias)
  (set! *register-map*
	(add-pseudo-register-alias *register-map* register alias))
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

(define *next-constant*)
(define *interned-constants*)

(define (constant->label constant)
  (let ((entry (assv constant *interned-constants*)))
    (if entry
	(cdr entry)
	(let ((label
	       (string->symbol
		(string-append "CONSTANT-"
			       (write-to-string *next-constant*)))))
	  (set! *next-constant* (1+ *next-constant*))
	  (set! *interned-constants*
		(cons (cons constant label)
		      *interned-constants*))
	  label))))

(define-integrable (set-current-branches! consequent alternative)
  (set-rtl-pnode-consequent-lap-generator! *current-rnode* consequent)
  (set-rtl-pnode-alternative-lap-generator! *current-rnode* alternative))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access lap-generator-package compiler-package)
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
  pattern)