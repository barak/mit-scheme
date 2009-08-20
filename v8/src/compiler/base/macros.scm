#| -*-Scheme-*-

$Id$

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; Compiler Macros
;;; package: (compiler macros)

(declare (usual-integrations))

(define (initialize-package!)
  (for-each (lambda (entry)
	      (syntax-table-define compiler-syntax-table (car entry)
		(cadr entry)))
	    `((CFG-NODE-CASE ,transform/cfg-node-case)
	      (DEFINE-ENUMERATION ,transform/define-enumeration)
	      (DEFINE-EXPORT ,transform/define-export)
	      (DEFINE-LVALUE ,transform/define-lvalue)
	      (DEFINE-PNODE ,transform/define-pnode)
	      (DEFINE-ROOT-TYPE ,transform/define-root-type)
	      (DEFINE-RTL-EXPRESSION ,transform/define-rtl-expression)
	      (DEFINE-RTL-PREDICATE ,transform/define-rtl-predicate)
	      (DEFINE-RTL-STATEMENT ,transform/define-rtl-statement)
	      (DEFINE-RULE ,transform/define-rule)
	      (DEFINE-RVALUE ,transform/define-rvalue)
	      (DEFINE-SNODE ,transform/define-snode)
	      (DEFINE-VECTOR-SLOTS ,transform/define-vector-slots)
	      (DESCRIPTOR-LIST ,transform/descriptor-list)
	      (ENUMERATION-CASE ,transform/enumeration-case)
	      (INST-EA ,transform/inst-ea)
	      (LAP ,transform/lap)
	      (LAST-REFERENCE ,transform/last-reference)
	      (MAKE-LVALUE ,transform/make-lvalue)
	      (MAKE-PNODE ,transform/make-pnode)
	      (MAKE-RVALUE ,transform/make-rvalue)
	      (MAKE-SNODE ,transform/make-snode)
	      (PACKAGE ,transform/package)))
  (syntax-table-define lap-generator-syntax-table 'DEFINE-RULE
    transform/define-rule))

(define compiler-syntax-table
  (make-syntax-table syntax-table/system-internal))

(define lap-generator-syntax-table
  (make-syntax-table compiler-syntax-table))

(define assembler-syntax-table
  (make-syntax-table compiler-syntax-table))

(define early-syntax-table
  (make-syntax-table compiler-syntax-table))

(define transform/last-reference
  (macro (name)
    (let ((x (generate-uninterned-symbol)))
      `(IF COMPILER:PRESERVE-DATA-STRUCTURES?
	   ,name
	   (LET ((,x ,name))
	     (SET! ,name)
	     ,x)))))

(define (transform/package names . body)
  (make-syntax-closure
   (make-sequence
    `(,@(map (lambda (name)
	       (make-definition name (make-unassigned-reference-trap)))
	     names)
      ,(make-combination
	(let ((block (syntax* (append body (list unspecific)))))
	  (if (open-block? block)
	      (open-block-components block
		(lambda (names* declarations body)
		  (make-lambda lambda-tag:let '() '() false
			       (list-transform-negative names*
				 (lambda (name)
				   (memq name names)))
			       declarations
			       body)))
	      (make-lambda lambda-tag:let '() '() false '()
			   '() block)))
	'())))))

(define transform/define-export
  (macro (pattern . body)
    (parse-define-syntax pattern body
      (lambda (name body)
	name
	`(BEGIN
	   (SET! ,name ,@body)
	   (QUOTE ,name)))
      (lambda (pattern body)
	`(BEGIN
	   (SET! ,(car pattern)
		  (NAMED-LAMBDA ,pattern ,@body))
	   (QUOTE ,(car pattern)))))))

(define transform/define-vector-slots
  (macro (class index . slots)
    (define (loop slots n)
      (if (null? slots)
	  '()
	  (let ((make-defs
		 (lambda (slot)
		   (let ((ref-name (symbol-append class '- slot)))
		     `(BEGIN
			(DEFINE-INTEGRABLE (,ref-name ,class)
			  (VECTOR-REF ,class ,n))
			(DEFINE-INTEGRABLE (,(symbol-append 'SET- ref-name '!)
					    ,class ,slot)
			  (VECTOR-SET! ,class ,n ,slot))))))
		(rest (loop (cdr slots) (1+ n))))
	    (if (pair? (car slots))
		(map* rest make-defs (car slots))
		(cons (make-defs (car slots)) rest)))))
    (if (null? slots)
	'*THE-NON-PRINTING-OBJECT*
	`(BEGIN ,@(loop slots index)))))

(define transform/define-root-type
  (macro (type . slots)
    (let ((tag-name (symbol-append type '-TAG)))
      `(BEGIN (DEFINE ,tag-name
		(MAKE-VECTOR-TAG FALSE ',type FALSE))
	      (DEFINE ,(symbol-append type '?)
		(TAGGED-VECTOR/SUBCLASS-PREDICATE ,tag-name))
	      (DEFINE-VECTOR-SLOTS ,type 1 ,@slots)
	      (SET-VECTOR-TAG-DESCRIPTION!
	       ,tag-name
	       (LAMBDA (,type)
		 (DESCRIPTOR-LIST ,type ,@slots)))))))

(define transform/descriptor-list
  (macro (type . slots)
    (let ((ref-name (lambda (slot) (symbol-append type '- slot))))
      `(LIST ,@(map (lambda (slot)
		      (if (pair? slot)
			  (let ((ref-names (map ref-name slot)))
			    ``(,',ref-names ,(,(car ref-names) ,type)))
			  (let ((ref-name (ref-name slot)))
			    ``(,',ref-name ,(,ref-name ,type)))))
		    slots)))))

(let-syntax
 ((define-type-definition
    (macro (name reserved enumeration)
      (let ((parent (symbol-append name '-TAG)))
	`(DEFINE ,(symbol-append 'TRANSFORM/DEFINE- name)
	   (macro (type . slots)
	     (let ((tag-name (symbol-append type '-TAG)))
	       `(BEGIN (DEFINE ,tag-name
			 (MAKE-VECTOR-TAG ,',parent ',type ,',enumeration))
		       (DEFINE ,(symbol-append type '?)
			 (TAGGED-VECTOR/PREDICATE ,tag-name))
		       (DEFINE-VECTOR-SLOTS ,type ,,reserved ,@slots)
		       (SET-VECTOR-TAG-DESCRIPTION!
			,tag-name
			(LAMBDA (,type)
			  (APPEND!
			   ((VECTOR-TAG-DESCRIPTION ,',parent) ,type)
			   (DESCRIPTOR-LIST ,type ,@slots))))))))))))
 (define-type-definition snode 5 false)
 (define-type-definition pnode 6 false)
 (define-type-definition rvalue 2 rvalue-types)
 (define-type-definition lvalue 14 false))

;;; Kludge to make these compile efficiently.

(define transform/make-snode
  (macro (tag . extra)
    `((ACCESS VECTOR ,system-global-environment)
      ,tag FALSE '() '() FALSE ,@extra)))

(define transform/make-pnode
  (macro (tag . extra)
    `((ACCESS VECTOR ,system-global-environment)
      ,tag FALSE '() '() FALSE FALSE ,@extra)))

(define transform/make-rvalue
  (macro (tag . extra)
    `((ACCESS VECTOR ,system-global-environment)
      ,tag FALSE ,@extra)))

(define transform/make-lvalue
  (macro (tag . extra)
    (let ((result (generate-uninterned-symbol)))
      `(let ((,result
	      ((ACCESS VECTOR ,system-global-environment)
	       ,tag FALSE '() '() '() '() '() '() 'NOT-CACHED
	       FALSE '() FALSE FALSE '() ,@extra)))
	 (SET! *LVALUES* (CONS ,result *LVALUES*))
	 ,result))))

(define transform/define-rtl-expression)
(define transform/define-rtl-statement)
(define transform/define-rtl-predicate)
(let ((rtl-common
       (lambda (type prefix components wrap-constructor types)
	 `(BEGIN
	    (SET! ,types (CONS ',type ,types))
	    (DEFINE-INTEGRABLE
	      (,(symbol-append prefix 'MAKE- type) ,@components)
	      ,(wrap-constructor `(LIST ',type ,@components)))
	    (DEFINE-INTEGRABLE (,(symbol-append 'RTL: type '?) EXPRESSION)
	      (EQ? (CAR EXPRESSION) ',type))
	    ,@(let loop ((components components)
			 (ref-index 6)
			 (set-index 2))
		(if (null? components)
		    '()
		    (let* ((slot (car components))
			   (name (symbol-append type '- slot)))
		      `((DEFINE-INTEGRABLE (,(symbol-append 'RTL: name) ,type)
			  (GENERAL-CAR-CDR ,type ,ref-index))
			,(let ((slot (if (eq? slot type)
					 (symbol-append slot '-VALUE)
					 slot)))
			   `(DEFINE-INTEGRABLE
			      (,(symbol-append 'RTL:SET- name '!)
			       ,type ,slot)
			      (SET-CAR! (GENERAL-CAR-CDR ,type ,set-index)
					,slot)))
			,@(loop (cdr components)
				(* ref-index 2)
				(* set-index 2))))))))))
  (set! transform/define-rtl-expression
	(macro (type prefix . components)
	  (rtl-common type prefix components
		      identity-procedure
		      'RTL:EXPRESSION-TYPES)))

  (set! transform/define-rtl-statement
	(macro (type prefix . components)
	  (rtl-common type prefix components
		      (lambda (expression) `(STATEMENT->SRTL ,expression))
		      'RTL:STATEMENT-TYPES)))

  (set! transform/define-rtl-predicate
	(macro (type prefix . components)
	  (rtl-common type prefix components
		      (lambda (expression) `(PREDICATE->PRTL ,expression))
		      'RTL:PREDICATE-TYPES))))

;(define transform/define-rule
;  (macro (type pattern . body)
;    (parse-rule pattern body
;      (lambda (pattern variables qualifier actions)
;	`(,(case type
;	     ((STATEMENT) 'ADD-STATEMENT-RULE!)
;	     ((PREDICATE) 'ADD-STATEMENT-RULE!)
;	     ((REWRITING) 'ADD-REWRITING-RULE!)
;	     (else type))
;	  ',pattern
;	  ,(rule-result-expression variables qualifier
;				   `(BEGIN ,@actions)))))))

(define transform/define-rule
  (macro (type pattern . body)
    (parse-rule pattern body
      (lambda (pattern variables qualifier actions)
	`(,(case type
	     ((STATEMENT) 'ADD-STATEMENT-RULE!)
	     ((PREDICATE) 'ADD-STATEMENT-RULE!)
	     ((REWRITING) 'ADD-REWRITING-RULE!)
	     (else type))
	  ',pattern
	  ,(compile-pattern
	    pattern
	    (rule-result-expression variables qualifier
				    `(BEGIN ,@actions))))))))

;;;; Lap instruction sequences.

(define transform/lap
  (macro some-instructions
    (list 'QUASIQUOTE some-instructions)))

(define transform/inst-ea
  (macro (ea)
    (list 'QUASIQUOTE ea)))

(define transform/define-enumeration
  (macro (name elements)
    (let ((enumeration (symbol-append name 'S)))
      `(BEGIN (DEFINE ,enumeration
		(MAKE-ENUMERATION ',elements))
	      ,@(map (lambda (element)
		       `(DEFINE ,(symbol-append name '/ element)
			  (ENUMERATION/NAME->INDEX ,enumeration ',element)))
		     elements)))))

(define (macros/case-macro expression clauses predicate default)
  (let ((need-temp? (not (symbol? expression))))
    (let ((expression*
	   (if need-temp?
	       (generate-uninterned-symbol)
	       expression)))
      (let ((body
	     `(COND
	       ,@(let loop ((clauses clauses))
		   (cond ((null? clauses)
			  (default expression*))
			 ((eq? (caar clauses) 'ELSE)
			  (if (null? (cdr clauses))
			      clauses
			      (error "ELSE clause not last" clauses)))
			 (else
			  `(((OR ,@(map (lambda (element)
					  (predicate expression* element))
					(caar clauses)))
			     ,@(cdar clauses))
			    ,@(loop (cdr clauses)))))))))
	(if need-temp?
	    `(LET ((,expression* ,expression))
	       ,body)
	    body)))))

(define transform/enumeration-case
  (macro (name expression . clauses)
    (macros/case-macro expression
		       clauses
		       (lambda (expression element)
			 `(EQ? ,expression ,(symbol-append name '/ element)))
		       (lambda (expression)
			 expression
			 '()))))

(define transform/cfg-node-case
  (macro (expression . clauses)
    (macros/case-macro expression
		       clauses
		       (lambda (expression element)
			 `(EQ? ,expression ,(symbol-append element '-TAG)))
		       (lambda (expression)
			 `((ELSE (ERROR "Unknown node type" ,expression)))))))