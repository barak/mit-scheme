#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; RTL Rules for C.  Shared utilities.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Compiler error reporting

(define (comp-internal-error message location . irritants)
  (apply error (cons (string-append "Internal inconsistency in "
				    (if (symbol? location)
					(symbol->string location)
					location)
				    ":  "
				    message)
		     irritants)))

;;;; Register-Allocator Interface

(define (type->name type)
  (case type
    ((WORD)
     "machine_word")
    ((SCHEME_OBJECT)
     "SCHEME_OBJECT")
    ((SCHEME_OBJECT*)
     "SCHEME_OBJECT *")
    ((LONG)
     "long")
    ((CHAR*)
     "char *")
    ((ULONG)
     "unsigned long")
    ((DOUBLE)
     "double")
    ((DOUBLE*)
     "double *")
    (else
     (comp-internal-error "Unknown type" 'TYPE->NAME type))))

(define (reg*type->name reg type)
  (string-append
   (case type
     ((WORD) "Wrd")
     ((DOUBLE) "Dbl")
     (else
      (comp-internal-error "Unknown type" 'REG*TYPE->NAME type)))
   (number->string reg)))

(define (machine-register-name reg)
  (cond ((eq? reg regnum:stack-pointer)
	 "Rsp")
	((eq? reg regnum:free)
	 "Rhp")
	((eq? reg regnum:regs)
	 "Rrb")
	((eq? reg regnum:dynamic-link)
	 "Rdl")
	((eq? reg regnum:value)
	 "Rvl")
	(else
	 (comp-internal-error "Unknown machine register"
			      'MACHINE-REGISTER-NAME reg))))

(define (machine-register-type reg)
  (cond ((eq? reg regnum:value)
	 "SCHEME_OBJECT")
	(else
	 "SCHEME_OBJECT *")))

(define (machine-register-type-symbol reg)
  (cond ((eq? reg regnum:value)
	 'SCHEME_OBJECT)
	(else
	 'SCHEME_OBJECT*)))

(define-integrable (register-is-machine-register? reg)
  (< reg number-of-machine-registers))

(define (rhs-cast reg type)
  (c:cast (type->name type) reg))

(define (lhs-cast reg type)
  (c:* (c:cast (type->name type) (c:& reg))))

(define permanent-register-list)
(define current-register-list)

(define (find-register reg type)
  (let ((aliases (assq reg current-register-list)))
    (and aliases
	 (let ((alias (assq type (cdr aliases))))
	   (cond (alias)
		 ((not type)
		  (cadr aliases))
		 (else false))))))

(define (allocate-register! reg type)
  (let ((name (new-register-name reg type))
	(aliases (assq reg current-register-list)))
    (if (not aliases)
	(set! current-register-list
	      (cons (list reg (cons type name))
		    current-register-list))
	(set-cdr! aliases
		  (cons (cons type name) (cdr aliases))))
    name))

(define (find-register! reg type)
  (cond ((find-register reg type)
	 => cdr)
	(else
	 (allocate-register! reg type))))

(define-integrable (type->canonical-C-type type)
  (if (eq? type 'DOUBLE) 'DOUBLE 'WORD))

(define (reg-select reg type)
  (string-append
   reg
   (case type
     ((SCHEME_OBJECT) ".Obj")
     ((SCHEME_OBJECT*) ".pObj")
     ((LONG) ".Lng")
     ((CHAR*) ".pChr")
     ((ULONG) ".uLng")
     ((DOUBLE*) ".pDbl")
     (else
      (comp-internal-error "Unknown type" 'REG-SELECT type)))))

(define (standard-source! reg type)
  (let ((type* (type->canonical-C-type type)))
    (cond ((register-is-machine-register? reg)
	   (let ((name (machine-register-name reg)))
	     (if (eq? (machine-register-type-symbol reg) type)
		 name
		 (rhs-cast name type))))
	  ((find-register reg type*)
	   => (lambda (pair)
		(let ((reg (cdr pair)))
		  (if (eq? type* 'DOUBLE)
		      reg
		      (reg-select reg type)))))
	  (else
	   (comp-internal-error "Unallocated register"
				'STANDARD-SOURCE! reg)))))

(define (standard-target! reg type)
  (let* ((type* (type->canonical-C-type type))
	 (finish (lambda (reg)
		   (if (eq? type* 'DOUBLE)
		       reg
		       (reg-select reg type)))))
    
    (cond ((register-is-machine-register? reg)
	   (if (not (compatible/C*register? type (register-type reg)))
	       (error "standard-target!: Incompatible type register" reg type))
	   #|
	   ;; This should not be necessary.
	   ;; We should only assign correctly typed values
	   ;; to dedicated machine registers.
	   (lhs-cast (machine-register-name reg) type)
	   |#
	   (machine-register-name reg))
	  ((find-register reg type*)
	   => (lambda (pair)
		(finish (cdr pair))))
	  (else
	   (finish (allocate-register! reg type*))))))

(define (new-register-name reg type)
  (cond ((assq reg permanent-register-list)
	 => (lambda (aliases)
	      (let ((alias (assq type (cdr aliases))))
		(if alias
		    (cdr alias)
		    (let ((name (reg*type->name reg type)))
		      (set-cdr! aliases
				(cons (cons type name) (cdr aliases)))
		      name)))))
	(else
	 (let ((name (reg*type->name reg type)))
	   (set! permanent-register-list
		 (cons (list reg (cons type name))
		       permanent-register-list))
	   name))))

(define (register-declarations)
  (c:group*
   (append-map
    (lambda (register)
      (map (lambda (spec)
	     (c:decl (type->name (car spec)) (cdr spec)))
	   (cdr register)))
    permanent-register-list)))

(define (standard-move-to-target! src tgt)
  (let ((src-type (register-type src)))
    (cond ((not (eq? (register-type tgt) src-type))
	   (comp-internal-error "Incompatible registers"
				'STANDARD-MOVE-TO-TARGET!
				src tgt))
	  ((register-is-machine-register? tgt)
	   (let ((src (standard-source! src
					(machine-register-type-symbol tgt))))
	     (LAP ,(c:= (machine-register-name tgt) src))))
	  ((register-is-machine-register? src)
	   (let ((tgt (standard-target! tgt
					(machine-register-type-symbol src))))
	     (LAP ,(c:= tgt (machine-register-name src)))))
	  (else
	   (let ((reg-type
		  (case src-type
		    ((WORD) 'WORD)
		    ((FLOAT) 'DOUBLE)
		    (else
		     (comp-internal-error "Unknown RTL register type"
					  'STANDARD-MOVE-TO-TARGET!
					  src-type)))))
	     (LAP ,(c:= (find-register! tgt reg-type)
			(find-register! src reg-type))))))))

;;;; Communicate with "cout.scm"

(define (c:invoke-interface-0 code)
  (use-invoke-interface! 0)
  (c:scall "INVOKE_INTERFACE_0" code))

(define (c:invoke-interface-1 code arg1)
  (use-invoke-interface! 1)
  (c:scall "INVOKE_INTERFACE_1" code arg1))

(define (c:invoke-interface-2 code arg1 arg2)
  (use-invoke-interface! 2)
  (c:scall "INVOKE_INTERFACE_2" code arg1 arg2))

(define (c:invoke-interface-3 code arg1 arg2 arg3)
  (use-invoke-interface! 3)
  (c:scall "INVOKE_INTERFACE_3" code arg1 arg2 arg3))

(define (c:invoke-interface-4 code arg1 arg2 arg3 arg4)
  (use-invoke-interface! 4)
  (c:scall "INVOKE_INTERFACE_4" code arg1 arg2 arg3 arg4))

(define (use-invoke-interface! number)
  (set! *invoke-interface*
	(let ((old *invoke-interface*))
	  (if (eq? old 'INFINITY)
	      number
	      (min old number))))
  unspecific)

(define (c:invoke-primitive prim arity)
  (set! *used-invoke-primitive* #t)
  (c:scall "INVOKE_PRIMITIVE" prim arity))

(define (c:closure-interrupt-check)
  (use-invoke-interface! 0)
  (c:scall "CLOSURE_INTERRUPT_CHECK" code:compiler-interrupt-closure))

(define (c:interrupt-check code block-label)
  (use-invoke-interface! 1)
  (c:scall "INTERRUPT_CHECK" code block-label))

(define (c:dlink-interrupt-check block-label)
  (use-invoke-interface! 2)
  (c:scall "DLINK_INTERRUPT_CHECK"
	   code:compiler-interrupt-dlink
	   block-label))

(define (c:jump address)
  (set! *use-jump-execute-chache* #t)
  (c:scall "JUMP" address))

(define (c:pop-return)
  (set! *use-pop-return* #t)
  (c:goto 'pop_return))

(define (c:reg-block)
  "Rrb")

(define (c:free-reg)
  "Rhp")

(define (c:sp-reg)
  "Rsp")

(define (c:val-reg)
  "Rvl")

(define (c:dlink-reg)
  "Rdl")

(define (c:pc-reg)
  "Rpc")

(define (c:rref index)
  (c:aref (c:reg-block) index))

(define (c:env-reg)
  (c:rref "REGBLOCK_ENV"))

(define (c:push object)
  (c:= (c:* (c:predec (c:sp-reg))) object))

(define (c:pop)
  (c:* (c:postinc (c:sp-reg))))

(define (c:tos)
  (c:* (c:sp-reg)))

(define (c:sref index)
  (c:aref (c:sp-reg) index))

(define (c:sptr index)
  (c:aptr (c:sp-reg) index))

(define (c:cref index)
  (c:aref 'current_block index))

(define (c:cptr index)
  (c:aptr 'current_block index))

;;;; Constants, Labels, and Various Caches

(define-integrable make-entry cons)
(define-integrable entry-value car)
(define-integrable entry-label cdr)

(define-integrable (make-table)
  (cons 0 '()))

(define-integrable table->list-of-entries cdr)

(define (find-association table value)
  (let ((x (assoc value (cdr table))))
    (if x
	(entry-label x)
	#f)))

(define (add-object! table name value)
  (set-cdr! table
	    (cons (make-entry value name)
		  (cdr table)))
  unspecific)

(define (add-association! table value prefix)
  (let ((num (car table)))
    (add-object! table
		 (string-append prefix
				*disambiguator*
				(number->string num))
		 value)
    (set-car! table (1+ num))
    num))

(define (find-or-add table value prefix)
  (let ((x (find-association table value)))
    (if x
	x
	(begin
	  (add-association! table value prefix)
	  (find-association table value)))))

(define (define-object name value)
  (add-object! objects
	       (if (symbol? name)
		   (symbol->string name)
		   name)
	       value))

(define (object-label-value label)
  (let ((entry
	 (list-search-positive (table->list-of-entries objects)
	   (lambda (entry)
	     (string=? label (entry-label entry))))))
    (if (not entry)
	(error "object-label-value: Unknown" label)
	(entry-value entry))))

(define objects)
(define free-references)
(define free-assignments)
(define free-uuo-links)
(define global-uuo-links)

(define labels)
(define label-num)

(define (make-special-labels)
  (define (frob name)
    (string->uninterned-symbol (generate-new-label-symbol name)))

  (vector (frob "ENVIRONMENT_LABEL_")
	  (frob "FREE_REFERENCES_LABEL_")
	  (frob "NUMBER_OF_LINKER_SECTIONS_")
	  (frob "DEBUGGING_LABEL_")))

(define-integrable (special-label/environment)
  (vector-ref *special-labels* 0))

(define-integrable (special-label/free-references)
  (vector-ref *special-labels* 1))

(define-integrable (special-label/number-of-sections)
  (vector-ref *special-labels* 2))

(define-integrable (special-label/debugging)
  (vector-ref *special-labels* 3))

(define (prepare-constants-block)
  (values (LAP)
	  (special-label/environment)
	  (special-label/free-references)
	  (special-label/number-of-sections)))

(define (uuo-link-label table name frame-size prefix)
  (define-integrable (uuo-link-label name)
    name				; ignored
    (generate-new-label-symbol prefix))

  (let ((slot1 (assq name (cdr table))))
    (if (not slot1)
	(let ((label (uuo-link-label name)))
	  (set-cdr! table
		    (cons (list name (cons frame-size label))
			  (cdr table)))
	  label)
	(let ((slot2 (assq frame-size (cdr slot1))))
	  (if (not slot2)
	      (let ((label (uuo-link-label name)))
		(set-cdr! slot1
			  (cons (cons frame-size label)
				(cdr slot1)))
		label)
	      (cdr slot2))))))

(define (free-uuo-link-label name frame-size)
  (uuo-link-label free-uuo-links name frame-size "EXECUTE_CACHE_"))

(define (global-uuo-link-label name frame-size)
  (uuo-link-label global-uuo-links name frame-size "GLOBAL_EXECUTE_CACHE_"))

;; this alias is for lapgn1.scm

(define (constant->label object)
  (declare (integrate object->offset))
  (object->offset object))

(define (object->offset scheme-object)
  (find-or-add objects scheme-object "OBJECT_"))

(define (free-reference->offset name)
  (find-or-add free-references name "FREE_REFERENCE_"))

(define (free-assignment->offset name)
  (find-or-add free-assignments name "FREE_ASSIGNMENT_"))

(define-integrable label-1 vector-first)
(define-integrable label-2 vector-second)
(define-integrable symbol-1 vector-third)
(define-integrable symbol-2 vector-fourth)
(define-integrable dispatch-1 vector-fifth)
(define-integrable (set-dispatch-1! x d)
  (vector-set! x 4 d))
(define-integrable dispatch-2 vector-sixth)
(define-integrable code-word-sel vector-seventh)

(define (find-label label labels)
  (let loop ((labels labels))
    (and (not (null? labels))
	 (let ((next (car labels)))
	   (if (or (eq? label (label-1 next))
		   (eq? label (label-2 next)))
	       next
	       (loop (cdr labels)))))))

(define (generate-new-label-symbol prefix)
  (let ((num label-num))
    (set! label-num (1+ num))
    (string-append prefix
		   *disambiguator*
		   (number->string num))))

(define (define-label! label)
  (set! labels
	(cons (vector label #f
		      (generate-new-label-symbol "LABEL_")    
		      #f #f #f #f)
	      labels))
  unspecific)

(define (label->offset label)
  (let ((x (find-label label labels)))
    (if x
	(symbol-1 x)
	(begin
	   (define-label! label)
	   (label->offset label)))))

(define (label->dispatch-tag label)
  (let ((x (find-label label labels)))
    (if x
	(or (dispatch-1 x)
	    (let ((sym (generate-new-label-symbol "TAG_")))
	      (set-dispatch-1! x sym)
	      sym))
	(begin
	  (define-label! label)
	  (label->dispatch-tag label)))))

(define (declare-block-label! code-word label external-label)
  (define (add-new-entry symbol-x symbol-y dispatch-x dispatch-y)
    (set! labels
	  (cons (vector label external-label
			symbol-x symbol-y
			dispatch-x dispatch-y
			code-word)
		labels)))

  (let ((x (and label (find-label label labels)))
	(y (and external-label (find-label external-label labels))))
    (if x
	(set! labels (delq! x labels)))
    (if y
	(set! labels (delq! y labels)))
    (cond ((and x (eq? x y))
	   (add-new-entry (symbol-1 x) (symbol-2 x)
			  (dispatch-1 x) (dispatch-2 x)))
	  ((and x y)
	   (add-new-entry (symbol-1 x) (symbol-1 y)
			  (dispatch-1 x) (dispatch-1 y)))
	  (x
	   (add-new-entry (symbol-1 x) #f
			  (dispatch-1 x) #f))
	  (y
	   (add-new-entry (symbol-1 y) #f
			  (dispatch-1 y) #f))
	  (else
	   (add-new-entry (generate-new-label-symbol "LABEL_")
			  #f
			  #f
			  #f)))
    unspecific))

(define available-machine-registers
  ;; This is really a lie, but lets some things work
  (list
   regnum:stack-pointer regnum:regs regnum:free
   regnum:dynamic-link regnum:value))

(define (sort-machine-registers lst)
  lst)

(define (compatible/C*register? c-type reg-type)
  (if (eq? c-type 'DOUBLE)
      (eq? reg-type 'FLOAT)
      (not (eq? reg-type 'FLOAT))))

(define (compatible/C*C? type1 type2)
  (if (eq? type1 'DOUBLE)
      (eq? type2 'DOUBLE)
      (not (eq? type2 'DOUBLE))))

(define (register-type reg)
  (cond ((or (machine-register? reg)
	     (register-value-class=word? reg))
	 'WORD)
	((register-value-class=float? reg)
	 'FLOAT)
	(else
	 (error "unable to determine register type" reg))))

(define-integrable (word-register? reg)
  (eq? (register-type reg) 'WORD))

(define (register-reference num)
  (comp-internal-error "Should not be using register allocator"
		       'REGISTER-REFERENCE num))

(define (register->register-transfer one two)
  (comp-internal-error "Should not be using register allocator"
		       'REGISTER->REGISTER-TRANSFER one two))

(define (reference->register-transfer one two)
  (comp-internal-error "Should not be using register allocator"
		       'REFERENCE->REGISTER-TRANSFER one two))

(define (pseudo-register-home one)
  (comp-internal-error "Should not be using register allocator"
		       'PSEUDO-REGISTER-HOME one))

(define (home->register-transfer one two)
  (comp-internal-error "Should not be using register allocator"
		       'HOME->REGISTER-TRANSFER one two))

(define (register->home-transfer one two)
  (comp-internal-error "Should not be using register allocator"
		       'REGISTER->HOME-TRANSFER one two))

(define (lap:make-label-statement label)
  (LAP ,(c:label label)))

(define (lap:make-unconditional-branch label)
  (LAP ,(c:goto label)))

(define (lap:make-entry-point label block-start-label)
  block-start-label			; ignored
  (declare-block-label! expression-code-word label #f)
  (lap:make-label-statement label))

(define (branch-on-expr expr)
  (set-current-branches! (lambda (label) (LAP ,(c:if-goto expr label)))
			 (lambda (label) (LAP ,(c:if-goto (c:! expr) label))))
  (LAP))

(define (compare c:?? val1 val2)
  (branch-on-expr (c:?? val1 val2)))

(define (define-arithmetic-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
	(set-cdr! entry method)
	(set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-arithmetic-method operator methods)
  (cdr (or (assq operator (cdr methods))
	   (comp-internal-error "Unknown operator" 'LOOKUP-ARITHMETIC-METHOD
				operator))))

(define-syntax define-codes
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(BEGIN
	,@(let loop ((names (cddr form)) (index (cadr form)))
	    (if (pair? names)
		(cons `(DEFINE-INTEGRABLE
			 ,(symbol-append 'CODE:COMPILER-
					 (car names))
			 ,index)
		      (loop (cdr names) (1+ index)))
		`()))))))

(define-codes #x012
  primitive-apply primitive-lexpr-apply
  apply error lexpr-apply link
  interrupt-closure interrupt-dlink interrupt-procedure
  interrupt-continuation interrupt-ic-procedure
  assignment-trap cache-reference-apply
  reference-trap safe-reference-trap unassigned?-trap
  -1+ &/ &= &> 1+ &< &- &* negative? &+ positive? zero?
  access lookup safe-lookup unassigned? unbound?
  set! define lookup-apply primitive-error
  quotient remainder modulo
  reflect-to-interface interrupt-continuation-2
  compiled-code-bkpt compiled-closure-bkpt)


(define (pre-lapgen-analysis rgraphs)
  rgraphs
  unspecific)