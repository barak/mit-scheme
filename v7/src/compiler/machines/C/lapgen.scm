#| -*-Scheme-*-

$Id: lapgen.scm,v 1.4 1993/06/10 18:07:39 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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
    (else
     (comp-internal-error "Unknown type" 'TYPE->NAME type))))

(define (reg*type->name reg type)
  (case type
    ((SCHEME_OBJECT)
     (string-append "Obj" (number->string reg)))
    ((SCHEME_OBJECT*)
     (string-append "pObj" (number->string reg)))
    ((LONG)
     (string-append "Lng" (number->string reg)))
    ((CHAR*)
     (string-append "pChr" (number->string reg)))
    ((ULONG)
     (string-append "uLng" (number->string reg)))
    ((DOUBLE)
     (string-append "Dbl" (number->string reg)))
    (else
     (comp-internal-error "Unknown type" 'REG*TYPE->NAME type))))

(define (machine-register-name reg)
  (cond ((eq? reg regnum:stack-pointer)
	 "stack_pointer")
	((eq? reg regnum:free)
	 "free_pointer")
	((eq? reg regnum:regs)
	 "register_block")
	((eq? reg regnum:dynamic-link)
	 "dynamic_link")
	((eq? reg regnum:value)
	 "value_reg")
	(else
	 (comp-internal-error "Unknown machine register"
			      'MACHINE-REGISTER-NAME reg))))
    
(define (machine-register-type reg)
  (cond ((eq? reg regnum:value)
	 "SCHEME_OBJECT")
	#|
	((eq? reg regnum:stack-pointer)
	 "SCHEME_OBJECT *")
	((eq? reg regnum:free)
	 "SCHEME_OBJECT *")
	((eq? reg regnum:regs)
	 "SCHEME_OBJECT *")
	((eq? reg regnum:dynamic-link)
	 "SCHEME_OBJECT *")
	(else
	 (comp-internal-error "Unknown machine register"
			      'MACHINE-REGISTER-TYPE reg))
	|#
	(else
	 "SCHEME_OBJECT *")))

(define (machine-register-type-symbol reg)
  (cond ((eq? reg regnum:value)
	 'SCHEME_OBJECT)
	#|
	((eq? reg regnum:stack-pointer)
	 'SCHEME_OBJECT*)
	((eq? reg regnum:free)
	 'SCHEME_OBJECT*)
	((eq? reg regnum:regs)
	 'SCHEME_OBJECT*)
	((eq? reg regnum:dynamic-link)
	 'SCHEME_OBJECT*)
	(else
	 (comp-internal-error "Unknown machine register"
			      'MACHINE-REGISTER-TYPE-SYMBOL reg))
	|#
	(else
	 'SCHEME_OBJECT*)))

(define-integrable (register-is-machine-register? reg)
  (< reg number-of-machine-registers))

(define (rhs-cast reg type)
  (string-append "((" (type->name type) ") " reg ")"))

(define (lhs-cast reg type)
  (string-append "(* ((" (type->name type) " *) &" reg "))"))

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

(define (allocate-additional-alias reg type)
  ;; This is flakey.
  ;; After this, there are two aliases for the same RTL register,
  ;; with incompatible types.
  ;; Hopefully Liar will not mix the two up.
  (let ((aliases (assq reg current-register-list)))
    (if (not aliases)
	(error "allocate-additional-alias: No previous aliases" reg)
	(let ((alias (assq type (cdr aliases))))
	  (if alias
	      (error "allocate-additional-alias: Already has alias" reg)
	      (let ((name (new-register-name reg type)))
		;; Kludge!  This depends on having at most two!
		(if (eq? type 'DOUBLE)
		    (set-cdr! (last-pair aliases) (list (cons type name)))
		    (set-cdr! aliases
			      (cons (cons type name)
				    (cdr aliases))))
		name))))))

(define (standard-source! reg type)
  (cond ((register-is-machine-register? reg)
	 (let ((name (machine-register-name reg)))
	   (if (eq? (machine-register-type-symbol reg) type)
	       name
	       (rhs-cast name type))))
	((find-register reg type)
	 => cdr)
	((find-register reg false)
	 => (lambda (alias)
	      (if (compatible/C*C? (car alias) type)
		  (rhs-cast (cdr alias) type)
		  (allocate-additional-alias reg type))))
	(else
	 (comp-internal-error "Unallocated register"
			      'STANDARD-SOURCE! reg))))

(define (standard-target! reg type)
  (cond ((register-is-machine-register? reg)
	 (if (not (compatible/C*register? type (register-type reg)))
	     (error "standard-target!: Incompatible type register" reg type))
	 (machine-register-name reg))
	((find-register reg type)
	 => cdr)
	((find-register reg false)
	 => (lambda (alias)
	      (if (compatible/C*C? (car alias) type)
		  (lhs-cast (cdr alias) type)
		  (allocate-additional-alias reg type))))
	(else
	 (let ((name (new-register-name reg type)))
	   (set! current-register-list
		 (cons (list reg (cons type name))
		       current-register-list))
	   name))))

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
  (append-map
   (lambda (register)
     (map (lambda (spec)
	    (string-append (type->name (car spec)) " " (cdr spec) ";\n\t"))
	  (cdr register)))
   permanent-register-list))

(define (standard-move-to-target! src tgt)
  ;; This is bogus but we have no more information

  (define (do-tgt src src-type)
    (let ((tgt (standard-target! tgt src-type)))
      (LAP ,tgt " = " ,src ";\n\t")))

  (define (do-src tgt tgt-type)
    (let ((src (standard-source! src tgt-type)))
      (LAP ,tgt " = " ,src ";\n\t")))

  (cond ((register-is-machine-register? tgt)
	 (do-src (machine-register-name tgt)
		 (machine-register-type-symbol tgt)))
	((assq tgt current-register-list)
	 => (lambda (aliases)
	      (let ((alias (cadr aliases)))
		(do-src (cdr alias) (car alias)))))
	((register-is-machine-register? src)
	 (do-tgt (machine-register-name src)
		 (machine-register-type-symbol src)))
	((assq src current-register-list)
	 => (lambda (aliases)
	      (let ((alias (cadr aliases)))
		(do-tgt (cdr alias) (car alias)))))
	(else
	 (comp-internal-error "Unallocated register"
			      'STANDARD-MOVE-TO-TARGET! src))))

;;;; Communicate with cout.scm

(define (use-invoke-interface! number)
  (set! *invoke-interface*
	(let ((old *invoke-interface*))
	  (if (eq? old 'infinity)
	      number
	      (min old number)))))

(define (use-invoke-primitive!)
  (set! *used-invoke-primitive* true))

(define (use-closure-interrupt-check!)
  (use-invoke-interface! 0))

(define (use-interrupt-check!)
  (use-invoke-interface! 1))

(define (use-dlink-interrupt-check!)
  (use-invoke-interface! 2))

(define (use-jump-execute-chache!)
  (set! *use-jump-execute-chache* #t))

(define (use-pop-return!)
  (set! *use-pop-return* #t))

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

(define (register-types-compatible? type1 type2)
  (boolean=? (eq? type1 'FLOAT) (eq? type2 'FLOAT)))

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
  (LAP "\n" ,label ":\n\t" ))

(define (lap:make-unconditional-branch label)
  (LAP "goto " ,label ";\n\t"))

(define (lap:make-entry-point label block-start-label)
  block-start-label			; ignored
  (declare-block-label! expression-code-word label #f)
  (lap:make-label-statement label))

(define (compare cc val1 val2)
  (set-current-branches!
   (lambda (label)
     (LAP "if (" ,val1 ,cc ,val2 ")\n\t  goto " ,label ";\n\t"))
   (lambda (label)
     (LAP "if (!(" ,val1 ,cc ,val2 "))\n\t  goto " ,label ";\n\t")))
  (LAP))

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

(let-syntax ((define-codes
	       (macro (start . names)
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE-INTEGRABLE
				,(symbol-append 'CODE:COMPILER-
						(car names))
				,index)
			     (loop (cdr names) (1+ index)))))
		 `(BEGIN ,@(loop names start)))))
  (define-codes #x012
    primitive-apply primitive-lexpr-apply
    apply error lexpr-apply link
    interrupt-closure interrupt-dlink interrupt-procedure 
    interrupt-continuation interrupt-ic-procedure
    assignment-trap cache-reference-apply
    reference-trap safe-reference-trap unassigned?-trap
    -1+ &/ &= &> 1+ &< &- &* negative? &+ positive? zero?
    access lookup safe-lookup unassigned? unbound?
    set! define lookup-apply))