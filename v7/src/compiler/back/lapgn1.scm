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

;;;; LAP Code Generation

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

(define *code-object-label*)

(define (generate-lap quotations procedures continuations receiver)
  (fluid-let ((*generation* (make-generation))
	      (*next-constant* 0)
	      (*interned-constants* '())
	      (*block-start-label* (generate-label))
	      (*code-object-label*))
    (for-each (lambda (continuation)
		(set! *code-object-label*
		      (code-object-label-initialize continuation))
		(let ((rnode (cfg-entry-node (continuation-rtl continuation))))
		  (hooks-disconnect! (node-previous rnode) rnode)
		  (cgen-rnode rnode)))
	      continuations)
    (for-each (lambda (quotation)
		(set! *code-object-label*
		      (code-object-label-initialize quotation))
		(cgen-rnode (cfg-entry-node (quotation-rtl quotation))))
	      quotations)
    (for-each (lambda (procedure)
		(set! *code-object-label*
		      (code-object-label-initialize procedure))
		(cgen-rnode (cfg-entry-node (procedure-rtl procedure))))
	      procedures)
    (receiver *interned-constants* *block-start-label*)))

(define *current-rnode*)
(define *dead-registers*)

(define (cgen-rnode rnode)
  (define (cgen-right-node next)
    (if (and next (not (eq? (node-generation next) *generation*)))
	(begin (if (not (null? (cdr (node-previous next))))
		   (let ((hook (find-hook rnode next))
			 (snode (statement->snode '(NOOP))))
		     (set-rnode-lap! snode
				     (clear-map-instructions
				      (rnode-register-map rnode)))
		     (hook-disconnect! hook next)
		     (hook-connect! hook snode)
		     (snode-next-connect! snode next)))
	       (cgen-rnode next))))
  (set-node-generation! rnode *generation*)
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
			      (append! *prefix-instructions*
				       instructions)))
	    (delete-dead-registers!)
	    (set-rnode-register-map! rnode *register-map*))
	  (begin (error "CGEN-RNODE: No matching rules" (rnode-rtl rnode))
		 (loop)))))
  ;; **** Works because of kludge in definition of RTL-SNODE.
  (cgen-right-node (pnode-consequent rnode))
  (cgen-right-node (pnode-alternative rnode)))

(define (rnode-input-register-map node)
  (let ((previous (node-previous node)))
    (if (and (not (null? previous))
	     (null? (cdr previous))
	     (not (entry-holder? (hook-node (car previous)))))
	(rnode-register-map (hook-node (car previous)))
	(empty-register-map))))

(define *cgen-rules*
  '())

(define (add-statement-rule! pattern result-procedure)
  (set! *cgen-rules*
	(cons (cons pattern result-procedure)
	      *cgen-rules*))
  pattern)

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

(define (guarantee-machine-register! register type receiver)
  (if (and (machine-register? register)
	   (register-type? register type))
      (receiver register)
      (with-alias-register! register type receiver)))

(define (with-alias-register! register type receiver)
  (bind-allocator-values (load-alias-register *register-map* type
					      *needed-registers* register)
    (lambda (alias map instructions)
      (set! *register-map* map)
      (need-register! alias)
      (append! instructions (receiver alias)))))

(define (allocate-register-for-assignment! register type receiver)
  (bind-allocator-values (allocate-alias-register *register-map* type
						  *needed-registers* register)
    (lambda (alias map instructions)
      (set! *register-map* (delete-other-locations map alias))
      (need-register! alias)
      (append! instructions (receiver alias)))))

(define (with-temporary-register! type receiver)
  (bind-allocator-values (allocate-temporary-register *register-map* type
						      *needed-registers*)
    (lambda (alias map instructions)
      (set! *register-map* map)
      (need-register! alias)
      (append! instructions (receiver alias)))))

(define (clear-map!)
  (let ((instructions (clear-map)))
    (set! *register-map* (empty-register-map))
    (set! *needed-registers* '())
    instructions))

(define-integrable (clear-map)
  (clear-map-instructions *register-map*))

(define (clear-registers! . registers)
  (if (null? registers)
      '()
      (let loop ((map *register-map*)
		 (registers registers))
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
  (set! *needed-registers* (set-delete *needed-registers* register)))

(package (delete-pseudo-register!
	  delete-dead-registers!)
  (define-export (delete-pseudo-register! register)
    (delete-pseudo-register *register-map* register delete-registers!))
  (define-export (delete-dead-registers!)
    (delete-pseudo-registers *register-map* *dead-registers* delete-registers!)
    (set! *dead-registers* '()))
  (define (delete-registers! map aliases)
    (set! *register-map* map)
    (set! *needed-registers* (set-difference *needed-registers* aliases))))

(define-integrable (dead-register? register)
  (memv register *dead-registers*))

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