#| -*-Scheme-*-

Copyright (c) 1994-1999 Massachusetts Institute of Technology

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

;;;; Continuation-passing style Converter
;;; package: (compiler midend)

(declare (usual-integrations))

(define (cpsconv/top-level program)
  (let* ((name (new-continuation-variable))
	 (program*
	  `(LET ((,name (CALL (QUOTE ,%fetch-continuation) (QUOTE #F))))
	     ,(cpsconv/expr (cpsconv/named-continuation name)
			    program))))
    (cpsconv/remember program* program)))

;; Important: this macro binds the name FORM to the whole form
;; thus the cps-converters can reference it and it will have the correct
;; value.  It also binds the names CONT and HANDLER.

(define-macro (define-cps-converter keyword bindings . body)
  (let ((proc-name (symbol-append 'CPSCONV/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler cont) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name CONT FORM)
	     (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	       (CPSCONV/REMEMBER ,code
				 form))))))))

(define-cps-converter LOOKUP (cont name)
  (cpsconv/return form cont `(LOOKUP ,name)))

(define-cps-converter LAMBDA (cont lambda-list body)
  (cpsconv/return form cont
		  (cpsconv/lambda* lambda-list body)))

(define-cps-converter LET (cont bindings body)
  (cpsconv/call** (map cpsconv/classify-let-binding bindings)
		  (lambda (names* rands*)
		    `(LET ,(map list names* rands*)
		       ,(cpsconv/expr cont body)))
		  form))

(define-cps-converter LETREC (cont bindings body)
  `(LETREC ,(map (lambda (binding)
		   (let ((value (cadr binding)))
		     (list (car binding)
			   (cpsconv/lambda** value))))
		 bindings)
     ,(cpsconv/expr cont body)))

(define (cpsconv/lambda* lambda-list body)
  `(LAMBDA ,lambda-list
     ,(cpsconv/expr (cpsconv/named-continuation (car lambda-list))
		    body)))

(define (cpsconv/lambda** lam-expr)
  (cpsconv/remember (cpsconv/lambda* (lambda/formals lam-expr)
				     (lambda/body lam-expr))
		    lam-expr))

(define-cps-converter CALL (cont rator orig-cont #!rest rands)
  (if (not (equal? orig-cont '(QUOTE #F)))
      (internal-error "Already cps-converted?"
		      `(CALL ,rator ,orig-cont ,@rands)))
  (cpsconv/call* cont rator rands form))

(define (cpsconv/call* cont rator rands form)
  (let* ((do-call
	  (lambda (elements names call-gen)
	    (cpsconv/call** (map cpsconv/classify-operand elements names)
			    call-gen
			    form)))
	 (default
	   (lambda ()
	     (let ((rator&rands (cons rator rands)))
	       (do-call rator&rands
			(map (lambda (x)
			       x	; ignored
			       false)
			     rator&rands)
			(lambda (new-names rator*&rands*)
			  new-names	; ignored
			  `(CALL ,(car rator*&rands*)
				 ,(cpsconv/invocation/continuation cont)
				 ,@(cdr rator*&rands*)))))))
	 (simple
	  (lambda (expr*)
	    (cond ((not (simple-operator? (quote/text rator)))
		   (cpsconv/hook-return form (quote/text rator) cont expr*))
		  ((operator/satisfies? (quote/text rator)
					'(UNSPECIFIC-RESULT))
		   `(BEGIN
		      ,expr*
		      ,(cpsconv/return form cont `(QUOTE ,%unspecific))))
		  (else
		   (cpsconv/return form cont expr*))))))
    (cond ((LAMBDA/? rator)
	   (if (there-exists? rands
		 (lambda (rand)
		   (or (LOOKUP/? rand)
		       (QUOTE/? rand))))
	       (internal-error "Silly arguments in lambda-combination" rands))
	   (let ((names (lambda/formals rator)))
	     (do-call rands (cdr names)
		      (lambda (names* rands*)
			`(CALL
			  (LAMBDA ,(cons (cpsconv/new-ignored-continuation)
					 names*)
			    ,(cpsconv/expr cont (lambda/body rator)))
			  (QUOTE #F)
			  ,@rands*)))))
	  ((not (QUOTE/? rator))
	   (default))
	  ((and (simple-operator? (quote/text rator))
		(for-all? rands form/simple&side-effect-free?))
	   (simple (cpsconv/simple/copy `(CALL ,rator (QUOTE #F) ,@rands))))
	  ((or (simple-operator? (quote/text rator))
	       (hook-operator? (quote/text rator)))
	   (do-call rands
		    (map (lambda (x)
			   x		; ignored
			   false)
			 rands)
		    (lambda (new-names rands*)
		      new-names		; ignored
		      (simple `(CALL ,rator (QUOTE ,#f) ,@rands*)))))
	  (else
	   (default)))))

(define (cpsconv/call** classified-operands call-gen form)
  (define (walk-simple simple)
    (if (null? simple)
	(call-gen
	 (map (lambda (classified)
		(vector-fourth classified))
	      classified-operands)
	 (map (lambda (classified)
		(let ((name (vector-second classified)))
		  (if name
		      `(LOOKUP ,name)
		      (cpsconv/simple/copy (vector-first classified)))))
	      classified-operands))
	`(LET ((,(vector-second (car simple))
		,(cpsconv/simple/copy (vector-first (car simple)))))
	   ,(walk-simple (cdr simple)))))

  (define (walk-hard hard)
    (if (null? hard)
	(walk-simple (cpsconv/sort/simple
		      (list-transform-positive classified-operands
			(lambda (operand)
			  (and (vector-second operand)
			       (vector-third operand))))))
	(let* ((next-name (cpsconv/new-name 'RECEIVER))
	       (ignore (cpsconv/new-ignored-continuation)))
	  `(LET ((,next-name
		  (LAMBDA (,ignore ,(vector-second (car hard)))
		    ,(walk-hard (cdr hard)))))
	     ,(let ((next (vector-first (car hard))))
		(cpsconv/expr
		 (cpsconv/value-continuation
		  next-name
		  (cpsconv/dbg-continuation/make 'COMBINATION-ELEMENT
						 form next))
		 next))))))

  (walk-hard (cpsconv/sort/hard
	      (list-transform-negative classified-operands
		(lambda (operand)
		  (vector-third operand))))))
	       
(define (cpsconv/classify-operand operand name)
  ;; operand -> #(operand early-name easy? late-name)
  ;; easy? if does not need a return address
  (let* ((early-name
	 (and (not (cpsconv/trivial? operand))
	      (or name
		  (cpsconv/new-name 'RAND))))
	 (late-name
	  (and name
	       (if early-name
		   (cpsconv/new-name 'DUMMY)
		   name))))
    (cpsconv/dbg-info-for-subproblem-value early-name late-name operand)
    (vector operand
	    early-name
	    (if (eq? *order-of-argument-evaluation* 'ANY)
		(form/simple&side-effect-free? operand)
		(form/simple&side-effect-insensitive? operand))
	    late-name)))

(define (cpsconv/trivial? operand)
  (or (LOOKUP/? operand)
      (QUOTE/? operand)
      (LAMBDA/? operand)
      (form/static? operand)))

(define (cpsconv/classify-let-binding binding)
  (let ((name    (car binding))
	(operand (cadr binding)))
    (let* ((early-name
	   (and (not (cpsconv/trivial? operand))
		name))
	   (late-name
	    (if early-name
		(cpsconv/new-name 'DUMMY)
		name)))
      (cpsconv/dbg-info-for-subproblem-value early-name late-name operand)
      (vector operand early-name true late-name))))

(define (cpsconv/dbg-info-for-subproblem-value early-name late-name form)
  late-name				; ignored
  (if early-name
      (let ((dbg-info (code-rewrite/original-form/previous form)))
	(if (and dbg-info
		 (new-dbg-expression? dbg-info))
	    (dbg-info/remember (new-dbg-expression/source-code dbg-info)
			       early-name)))))

(define (cpsconv/sort/hard operands)
  (case *order-of-argument-evaluation*
    ((LEFT-TO-RIGHT) operands)
    ((RIGHT-TO-LEFT) (reverse operands))
    (else
     ;; *** For now ***
     operands)))

(define (cpsconv/sort/simple operands)
  ;; Either order is ANY, or they are insensitive
  ;; *** For now ***
  operands)

(define (cpsconv/simple/copy form)
  (let walk ((form form))
    (cpsconv/remember
     (case (car form)
       ((LOOKUP)
	`(LOOKUP ,(lookup/name form)))
       ((QUOTE)
	`(QUOTE ,(quote/text form)))
       ((LAMBDA)
	(cpsconv/lambda* (lambda/formals form) (lambda/body form)))
       ((IF)
	`(IF ,(walk (if/predicate form))
	     ,(walk (if/consequent form))
	     ,(walk (if/alternate form))))
       ((CALL)
	(if (not (equal? (call/continuation form) '(QUOTE #F)))
	    (internal-error "Already cps-converted?" form))
	`(CALL ,(walk (call/operator form))
	       ,@(map walk (call/cont-and-operands form))))
       (else
	(internal-error "Non simple expression" form)))
     form)))  

(define-cps-converter QUOTE (cont object)
  (cpsconv/return form cont `(QUOTE ,object)))

(define-cps-converter DECLARE (cont #!rest anything)
  (cpsconv/return form cont `(DECLARE ,@anything)))

(define-cps-converter BEGIN (cont #!rest actions)
  (if (null? actions)
      (internal-error "Empty begin")
      (let walk ((next    (car actions))
		 (actions (cdr actions)))
	(cond ((null? actions)
	       (cpsconv/expr cont next))
	      ((form/simple? next)
	       ;; This clause is completely optional but makes for a smaller
	       ;; program that needs less simplification.
	       (let ((next* (cpsconv/simple/copy next))
		     (rest  (walk (car actions) (cdr actions))))
		 (if (BEGIN/? rest)
		     `(BEGIN ,next* ,@(begin/exprs rest))
		     `(BEGIN ,next* ,rest))))
	      (else
	       (let ((next-name (cpsconv/new-name 'NEXT)))
		 `(LET ((,next-name
			 (LAMBDA (,(cpsconv/new-ignored-continuation))
			   ,(walk (car actions)
				  (cdr actions)))))
		    ,(cpsconv/expr
		      (cpsconv/begin-continuation
		       next-name
		       (cpsconv/dbg-continuation/make 'SEQUENCE-ELEMENT
						      form next))
		      next))))))))

(define-cps-converter IF (cont pred conseq alt)
  (define (general)
    ;; This does anchor pointing by default?
    (let ((cons-name  (cpsconv/new-name 'CONS))
	  (alt-name   (cpsconv/new-name 'ALT))
	  (ignore1    (cpsconv/new-ignored-continuation))
	  (ignore2    (cpsconv/new-ignored-continuation)))
      `(LET ((,cons-name  (LAMBDA (,ignore1) ,(cpsconv/expr cont conseq)))
	     (,alt-name   (LAMBDA (,ignore2) ,(cpsconv/expr cont alt))))
	 ,(cpsconv/expr
	   (cpsconv/predicate-continuation
	    cons-name alt-name
	    (cpsconv/dbg-continuation/make 'CONDITIONAL-PREDICATE form pred))
	   pred))))
  (define (really-simple)
    (cpsconv/return form cont (cpsconv/simple/copy form)))
  (define (simple-predicate)
    `(IF ,(cpsconv/simple/copy pred)
	 ,(cpsconv/expr cont conseq)
	 ,(cpsconv/expr cont alt)))
  (cond ((eq? (cpsconv/cont/kind cont) 'BEGIN)
	 (general))
	((not (form/simple? pred))
	 (general))
	((and (not (eq? (cpsconv/cont/kind cont) 'NAMED))
	      (form/pseudo-simple? conseq)
	      (form/pseudo-simple? alt))
	 (really-simple))
	(else
	 (simple-predicate))))

(define (cpsconv/expr cont expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (cpsconv/quote cont expr))
    ((LOOKUP)   (cpsconv/lookup cont expr))
    ((LAMBDA)   (cpsconv/lambda cont expr))
    ((LET)      (cpsconv/let cont expr))
    ((DECLARE)  (cpsconv/declare cont expr))
    ((CALL)     (cpsconv/call cont expr))
    ((BEGIN)    (cpsconv/begin cont expr))
    ((IF)       (cpsconv/if cont expr))
    ((LETREC)   (cpsconv/letrec cont expr))
    (else       (illegal expr))))

(define (cpsconv/expr* cont exprs)
  (map (lambda (expr)
	 (cpsconv/expr cont expr))
       exprs))

(define (cpsconv/remember new old)
  (code-rewrite/remember new old))

(define (cpsconv/remember* new old)
  (code-rewrite/remember* new old))

(define (cpsconv/new-name prefix)
  (new-variable prefix))

(define (cpsconv/new-ignored-continuation)
  (new-ignored-continuation-variable))    

(define-structure (cpsconv/cont
		   (conc-name cpsconv/cont/)
		   (constructor cpsconv/cont/make))
  (kind false read-only true)
  (field1 false read-only true)
  (field2 false read-only true)
  (dbg-cont false read-only true))

(define (cpsconv/named-continuation name)
  (cpsconv/cont/make 'NAMED name false false))

(define (cpsconv/predicate-continuation conseq alt dbg-cont)
  (cpsconv/cont/make 'PREDICATE conseq alt dbg-cont))

(define (cpsconv/begin-continuation next dbg-cont)
  (cpsconv/cont/make 'BEGIN next false dbg-cont))

(define (cpsconv/value-continuation receiver dbg-cont)
  (cpsconv/cont/make 'VALUE receiver false dbg-cont))

(define (cpsconv/dbg-continuation/make kind outer inner)
  (new-dbg-continuation/make kind
			     (code-rewrite/original-form/previous outer)
			     (code-rewrite/original-form/previous inner)))

(define (cpsconv/return form cont expression)
  (cpsconv/remember expression form)
  (define (default name)
    `(CALL (LOOKUP ,name)
	   (QUOTE #F)
	   ,expression))
  (if (and (not (eq? (cpsconv/cont/kind cont) 'BEGIN))
	   (DECLARE/? expression))
      (internal-error "DECLARE expression in value position"))
  (case (cpsconv/cont/kind cont)
    ((VALUE)
     (default (cpsconv/cont/field1 cont)))
    ((NAMED)
     `(CALL (QUOTE ,%invoke-continuation)
	    (LOOKUP ,(cpsconv/cont/field1 cont))
	    ,expression))
    ((PREDICATE)
     (let* ((pred-default
	     (lambda (name)
	       `(CALL (LOOKUP ,name)
		      (QUOTE #F))))
	    (full-pred
	     (lambda ()
	       `(IF ,expression
		    ,(pred-default (cpsconv/cont/field1 cont))
		    ,(pred-default (cpsconv/cont/field2 cont))))))
       (cond ((QUOTE/? expression)
	      (case (boolean/discriminate (quote/text expression))
		((FALSE)
		 (pred-default (cpsconv/cont/field2 cont)))
		((TRUE)
		 (pred-default (cpsconv/cont/field1 cont)))
		(else
		 (full-pred))))
	     ((LAMBDA/? expression)
	      (pred-default (cpsconv/cont/field1 cont)))
	     (else
	      (full-pred)))))
    ((BEGIN)
     (let ((return
	    `(CALL (LOOKUP ,(cpsconv/cont/field1 cont))
		   (QUOTE #F))))
       (if (form/simple&side-effect-free? expression)
	   return
	   `(LET ((,(cpsconv/new-name 'IGNORE) ,expression))
	      ,return))))
    (else
     (internal-error "Unknown continuation kind" cont))))

(define (cpsconv/invocation/continuation cont)
  ;; This eta converts non-named continuations
  ;; to make the continuations be stack closed,
  ;; not the receivers, which may be shared.
  (case (cpsconv/cont/kind cont)
    ((NAMED)
     `(LOOKUP ,(cpsconv/cont/field1 cont)))
    ((VALUE)
     (let* ((value-name  (cpsconv/new-name 'VALUE))
	    (dbg-cont    (cpsconv/cont/dbg-cont cont))
	    (scode
	     (and (new-dbg-continuation/inner dbg-cont)
		  (new-dbg-expression/source-code
		   (new-dbg-continuation/inner dbg-cont)))))
       (if scode (dbg-info/remember scode value-name))
       (cpsconv/remember*
	`(LAMBDA (,(cpsconv/new-ignored-continuation) ,value-name)
	   (CALL (LOOKUP ,(cpsconv/cont/field1 cont))
		 (QUOTE #F)
		 (LOOKUP ,value-name)))
	dbg-cont)))
    ((PREDICATE)
     (let* ((value-name  (cpsconv/new-name 'VALUE))
	    (dbg-cont    (cpsconv/cont/dbg-cont cont))
	    (scode
	     (and (new-dbg-continuation/inner dbg-cont)
		  (new-dbg-expression/source-code
		   (new-dbg-continuation/inner dbg-cont)))))
       (if scode (dbg-info/remember scode value-name))
       (cpsconv/remember*
	`(LAMBDA (,(cpsconv/new-ignored-continuation) ,value-name)
	   (IF (LOOKUP ,value-name)
	       (CALL (LOOKUP ,(cpsconv/cont/field1 cont))
		     (QUOTE #F))
	       (CALL (LOOKUP ,(cpsconv/cont/field2 cont))
		     (QUOTE #F))))
	(cpsconv/cont/dbg-cont cont))))
    ((BEGIN)
     (cpsconv/remember*
      `(LAMBDA (,(cpsconv/new-ignored-continuation)
		,(cpsconv/new-name 'IGNORE))
	 (CALL (LOOKUP ,(cpsconv/cont/field1 cont))
	       (QUOTE #F)))
      (cpsconv/cont/dbg-cont cont)))
    (else
     (internal-error "Unknown continuation kind" cont))))

(define (cpsconv/hook-return form rator cont expr*)
  (define (default)
    (cpsconv/remember expr* form)
    (let ((name (cpsconv/new-name 'VALUE)))
      `(LET ((,name ,expr*))
	 ,(cpsconv/return form cont `(LOOKUP ,name)))))
  (if (not (operator/satisfies? rator '(OUT-OF-LINE-HOOK)))
      (default)
      (case (cpsconv/cont/kind cont)
	((PREDICATE)
	 (if (not (operator/satisfies? rator '(OPEN-CODED-PREDICATE)))
	     (default)
	     `(IF ,(cpsconv/remember expr* form)
		  (CALL (LOOKUP ,(cpsconv/cont/field1 cont))
			(QUOTE #F))
		  (CALL (LOOKUP ,(cpsconv/cont/field2 cont))
			(QUOTE #F)))))
	((NAMED)
	 `(CALL ,(cadr expr*)
		(LOOKUP ,(cpsconv/cont/field1 cont))
		,@(cdddr expr*)))
	(else
	 (default)))))