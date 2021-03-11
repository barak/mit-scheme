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

(declare (usual-integrations))

;;; Compile-time handling of booleans

(define (boolean/discriminate object)
  (cond ((eq? object #f)
	 'FALSE)
	((eq? object #t)
	 'TRUE)
	((eq? object '())
	 ;; 'UNKNOWN
	 'TRUE)
	(else
	 'TRUE)))

;;; Compile-time handling of numbers (*** For now ***)

(define machine-tag-renames
  '((floating-point-vector flonum)))

(define (machine-tag tag-name)
  (let ((place (assq tag-name machine-tag-renames)))
    (microcode-type
     (if (not place)
	 tag-name
	 (cadr place)))))     

(define (machine-fixnum? value)
  (fix:fixnum? value))

(define (small-fixnum? value nbits)
  (and (machine-fixnum? value)
       (machine-fixnum? (* (expt 2 nbits) value))))

;; Trivial pretty printer

(define kmp/pp-unparser-table
  (unparser-table/copy system-global-unparser-table))

(define *unparse-string
  (lexical-reference (->environment '(runtime unparser)) '*unparse-string))

(unparser-table/set-entry!
 kmp/pp-unparser-table
 'UNINTERNED-SYMBOL
 (lambda (symbol)
   (let ((name (symbol-name symbol)))
     (cond ((= 0 (vector-8b-ref name 0))
	    (*unparse-string (substring name 1 (string-length name))))
	   ((new-variable->index symbol)
	    => (lambda (index)
		 index			; ignored
		 (*unparse-string name)
		 ;;(*unparse-string kmp/pp-symbol-glue)
		 ;;(*unparse-string (number->string index))
		 ))
	   (else
	    ;;(*unparse-string "#[uninterned-symbol ")
	    (*unparse-string name)
	    ;;(*unparse-string " ")
	    ;;(*unparse-string (number->string (hash symbol)))
	    ;;(*unparse-string "]")
	    )))))
   
(define kmp/pp-symbol-glue "-")

(define (kmp/pp kmp-code)
  (fluid-let ((*pp-primitives-by-name* false)
	      (*pp-uninterned-symbols-by-name* false)
	      (*pp-avoid-circularity?* true)
	      (*pp-default-as-code?* true)
	      (*unparse-abbreviate-quotations?* #T))
    (pp kmp-code)))

(define (kmp/ppp kmp-code)
  (kmp/pp (kmp/->ppp kmp-code)))

(define (kmp/->ppp kmp-code)
  (define (->string x)
    (cond ((interned-symbol? x) (symbol-name x))
	  ((uninterned-symbol? x)
	   (let ((index  (new-variable->index x))
		 (name   (symbol-name x)))
	     (cond (index
		    ;;(string-append name kmp/pp-symbol-glue
		    ;;		   (number->string index))
		    name)
		   ((= 0 (vector-8b-ref name 0))
		    (substring name 1 (string-length name)))
		   (else
		    (string-append name "[#@"
				   (number->string (hash x)) "]")))))
	  (else x)))
  (define (->sym . stuff)
    (string->uninterned-symbol
     (apply string-append "\000" (map ->string stuff))))
  (let walk ((expr kmp-code))
    (define (format-ref get-closure get-name)
      (define (gen closure)
	(->sym closure  "."  (quote/text (get-name expr))))
      (let* ((expr    (map walk expr))
	     (closure (get-closure expr)))
	(cond ((symbol? closure)    (gen closure))
	      ((LOOKUP/? closure)   (gen (lookup/name closure)))
	      (else expr))))
    (cond ((or (QUOTE/? expr) (and (pair? expr) (eq? (car expr) 'CONSTANT)))
	   expr)
	  ;;((LET/? expr)
	  ;; (let do-let ((names '()) (values '()) (form expr))
	  ;;   (cond ((and (LET/? form)
	  ;;		  (= (length (let/bindings form)) 1))
	  ;;	  (do-let (cons (first  (first (let/bindings form))) names)
	  ;;		  (cons (second (first (let/bindings form))) values)
	  ;;		  (let/body form)))
	  ;;	    ((null? names)
	  ;;	     (map walk expr))
	  ;;	    ((= (length names) 1)
	  ;;  	      `(LET (,(car names) ,(walk (car values)))
	  ;;		 ,(walk form)))
	  ;;	    (else
	  ;;	     `(LET* ,(reverse (map (lambda (n v) `(,n ,(walk v)))
	  ;;				   names values))
	  ;;		,(walk form))))))
	  ((LOOKUP/? expr)
	   (lookup/name expr))
	  ((CALL/%heap-closure-ref? expr)
	   (format-ref CALL/%heap-closure-ref/closure
		       CALL/%heap-closure-ref/name))
	  ((CALL/%stack-closure-ref? expr)
	   (format-ref CALL/%stack-closure-ref/closure
		       CALL/%stack-closure-ref/name))
	  ((pair? expr)
	   (map walk expr))
	  (else expr))))

(define (kmp->standard form)
  ;; Convert non-cps kmp code to `ordinary' scheme
  (cond ((QUOTE/? form)
	 (let ((datum (quote/text form)))
	   (if (or (number? datum) (boolean? datum)
		   (string? datum) (char? datum))
	       datum
	       form)))
	((LOOKUP/? form)
	 (lookup/name form))
	((or (LET/? form) (LETREC/? form))
	 `(,(car form) ,(map (lambda (b)
			       (list (car b) (kmp->standard (cadr b))))
			     (second form))
		       ,(kmp->standard (third form))))
	((LAMBDA/? form)
	 `(LAMBDA ,(cdr (lambda/formals form)) ,(kmp->standard (third form))))
	((CALL/? form)
	 (if (not (equal? (call/continuation form) '(QUOTE #F)))
	     (internal-error "KMP->Standard: not pre-CPS:" form))
	 (let ((rator  (kmp->standard (call/operator form)))
	       (rands  (map kmp->standard (call/operands form))))
	   (cond ((and (QUOTE/? rator)
		       (or (known-operator? (quote/text rator))
			   (primitive-procedure? (quote/text rator))))
		  `(,(quote/text rator) ,@rands))
		 (else `(,rator ,@rands)))))
	((SET!/? form) `(set! ,(second form) ,(kmp->standard (third form))))
	((DECLARE/? form) form)
	(else
	 (cons (car form) (map kmp->standard (cdr form))))))

;;; Simple form utilities

(define (bind name value body)
  `(CALL (LAMBDA (,(new-continuation-variable) ,name)
	   ,body)
	 (QUOTE #F)
	 ,value))

(define (bind* names values body)
  `(CALL (LAMBDA (,(new-continuation-variable) ,@names)
	   ,body)
	 (QUOTE #F)
	 ,@values))

(define (andify left right)
  (cond ((equal? left '(QUOTE #T))   right)
	((equal? right '(QUOTE #T))   left)
	(else
	 `(IF ,left ,right (QUOTE #F)))))

(define (beginnify actions #!optional incremental?)
  ;; Flattens the ACTIONS, discarding any in non-tail position that
  ;; are side-effect free or static (compile-time only).  It
  ;; returns (BEGIN) or (BEGIN <action>+ <expression>) or <expression>
  ;; If INCREMENTAL? is specified and true, BEGIN forms within ACTIONS are
  ;; assumed to already satisfy the output conditions, otherwise they
  ;; will be processed.
  (let ((incremental? (and (not (default-object? incremental?)) incremental?)))
  (let loop ((actions (reverse actions))
	     (actions* '()))
    (cond ((null? actions)
	   (if (or (null? actions*)
		   (not (null? (cdr actions*))))
	       `(BEGIN ,@actions*)
	       (car actions*)))
	  ((BEGIN/? (car actions))
	   (if incremental?
	       (loop (cdr actions)
		     (append (begin/exprs (car actions)) actions*))
	       (loop (append (reverse (begin/exprs (car actions)))
			     (cdr actions))
		     actions*)))
	  ((and (not (null? actions*))
		(or (form/satisfies? (car actions) '(SIDE-EFFECT-FREE))
		    (form/satisfies? (car actions) '(STATIC))))
	   (if compiler:guru?
	       (write-line `(BEGINNIFY ELIDING ,(car actions))))
	   (loop (cdr actions) actions*))
	  (else
	   (loop (cdr actions)
		 (cons (car actions) actions*)))))))

(define (simplify-actions expressions)
  ;; Takes a list of expressions, as in a BEGIN body, and produces a
  ;; simplified list of expressions (i.e. removes side-effect-free
  ;; expressions in non-tail position).
  (let ((simplified (beginnify expressions)))
    (if (and (pair? simplified)
	     (eq? (car simplified) 'BEGIN))
	(cdr simplified)
	(list simplified))))

(define (pseudo-letify rator bindings body remember)
  ;; Using pseudo-letify ensures that LET is only inserted for simple,
  ;; non-continuation bindings.
  (if (and (for-all? bindings
	     (lambda (binding)
	       (and (form/pseudo-simple? (cadr binding))
		    (not (continuation-variable? (car binding))))))
	   *after-cps-conversion?*)
      `(LET ,bindings
	 ,body)
      (let ((cont-binding
	     (list-search-positive bindings
	       (lambda (binding)
		 (continuation-variable? (car binding)))))
	    (finish
	     (lambda (cont-name cont-expr bindings*)
	       (let ((rator* `(LAMBDA (,cont-name
				       ,@(map car bindings*))
				,body)))
		 `(CALL ,(remember rator* rator)
			,cont-expr
			,@(map cadr bindings*))))))
	(if (not cont-binding)
	    (finish (new-continuation-variable)
		    `(QUOTE #F)
		    bindings)
	    (finish (car cont-binding)
		    (cadr cont-binding)
		    (delq cont-binding bindings))))))

(define (hash-table/copy table make-hash-table)
  (let ((new-table (make-hash-table (hash-table/size table))))
    (hash-table/for-each table
			 (lambda (key datum)
			   (hash-table/put! new-table key datum)))
    new-table))

(define (make-variable-properties)
  (make-eq-hash-table))

(define (copy-variable-properties)
  (let ((var-props *variable-properties*))
    (and var-props
	 (hash-table/copy var-props make-eq-hash-table))))

(define (get-variable-properties var)
  (let ((var-props *variable-properties*))
    (and var-props
	 (hash-table/get var-props var '()))))

(define (set-variable-properties! var alist)
  (let ((var-props *variable-properties*))
    (and var-props
	 (hash-table/put! var-props var alist))))

(define (get-variable-property var property)
  (let ((properties (get-variable-properties var)))
    (and properties
	 (assq property properties))))

(define (declare-variable-property! var property)
  (let ((var-props *variable-properties*))
    (and var-props
	 (hash-table/put!
	  var-props
	  var
	  (let* ((all (hash-table/get var-props var '()))
		 (place (assq (car property) all)))
	    (cons property
		  (if (not place)
		      all
		      (delq place all))))))))

;; NEW-VARIABLE
;;
;; The only reason for this table is to canonocalize the names to allow
;; comparison across compilations.  If you want to use something like
;; this for a code rewrite, dont use this table.  Use the variable
;; properties or something else.

(define new-variable-index)
(define new-variable-table #F)

(define (initialize-new-variable!)
  (set! new-variable-index 0)
  (set! new-variable-table (make-eq-hash-table)))

(define (new-variable prefix)
  ;;(generate-uninterned-symbol prefix)
  (set! new-variable-index (+ new-variable-index 1))
  (let ((symbol (string->uninterned-symbol
		 (string-append
		  (if (symbol? prefix)
		      (symbol-name prefix)
		      prefix)
		  "-"
		  (number->string new-variable-index)))))
    (hash-table/put! new-variable-table symbol new-variable-index)
    symbol))

(define (new-variable->index symbol)
  (and new-variable-table
       (hash-table/get new-variable-table symbol #F)))


(define (closure-variable? var)
  (get-variable-property var 'CLOSURE))

(define (new-closure-variable)
  (let ((name (new-variable 'CLOSURE)))
    (declare-variable-property! name '(CLOSURE))
    name))

(define-integrable (new-ignored-variable name)
  (let ((name (new-variable name)))
    (declare-variable-property! name '(IGNORED))
    name))

(define-integrable (ignored-variable? var)
  (get-variable-property var 'IGNORED))

(define (continuation-variable? var)
  (get-variable-property var 'CONTINUATION))

(define (ignored-continuation-variable? var)
  (and (get-variable-property var 'CONTINUATION)
       (ignored-variable? var)))

(define (referenced-continuation-variable? var)
  (and (get-variable-property var 'CONTINUATION)
       (not (ignored-variable? var))))

(define (new-continuation-variable)
  (let ((name (new-variable 'CONT)))
    (declare-variable-property! name '(CONTINUATION))
    name))

(define (new-ignored-continuation-variable)
  (let ((name (new-ignored-variable 'IGNORED-CONTINUATION)))
    (declare-variable-property! name '(CONTINUATION))
    name))

(define (environment-variable? var)
  (get-variable-property var 'ENVIRONMENT))

(define (new-environment-variable)
  (let ((name (new-variable 'ENV)))
    (declare-variable-property! name '(ENVIRONMENT))
    name))

(define (new-variable-cache-variable name desc)
  name					; ignored
  (let ((name* (new-variable 'CACHE)))
    (declare-variable-property! name* `(CACHE ,desc))
    name*))

(define (variable-cache-variable? var)
  (get-variable-property var 'CACHE))

(define (variable/rename var)
  (let ((new
	 ;;(generate-uninterned-symbol (string-append (symbol-name var) "-"))
	 (new-variable var)
	 )
	(original-properties (get-variable-properties var)))
    (if original-properties
	(set-variable-properties! new (alist-copy original-properties)))
    (declare-variable-property! new `(ORIGINAL-NAME ,var))
    new))

(define (variable/original-name var)
  (let loop ((var var))
    (let ((place (get-variable-property var 'ORIGINAL-NAME)))
      (if (not place)
	  var
	  (loop (cadr place))))))

(define (pseudo-static-variable? var)
  (let ((var-props *variable-properties*))
    (and var-props
	 (let ((props (hash-table/get var-props var false)))
	   (and props
		(or (assq 'CONTINUATION props)
		    (assq 'ENVIRONMENT props)))))))


(define (lifter/letrecify program)
  ;; Ensure that there is a place to attach lifted stuff,
  ;; by introducing a LETREC if necessary.
  (if (LETREC/? program)
      program
      `(LETREC () ,program)))

(define (lifter/make find-static-form)
  (lambda (env lamname form*)
    (define (clobber-letrec! form)
      (set-car! (cdr form)
		(cons (list lamname form*)
		      (cadr form))))

    (let ((form (find-static-form env)))
      (if (or (not form) (not (pair? form)))
	  (internal-error "Nowhere to insert" form)
	  (case (car form)
	    ((LETREC)
	     (clobber-letrec! form))
	    ((LET LAMBDA)
	     (let ((body (caddr form)))
	       (if (LETREC/? body)
		   (clobber-letrec! body)
		   (set-car! (cddr form)
			     `(LETREC ((,lamname ,form*))
				,body)))))
	    (else
	     (internal-error "Invalid place to insert" form)))))))

(define (form/rewrite! old new)
  (set-car! old (car new))
  (set-cdr! old (cdr new)))

(define (form/preserve form)
  ;; This makes a copy that won't be affected by later rewriting
  ;; of the original.  Rewritten components will be present in both.
  (cons (car form) (cdr form)))

(define (form/copy form)
  (let walk ((form form))
    (cond ((not (pair? form))
	   form)
	  ((QUOTE/? form)
	   `(QUOTE ,(quote/text form)))
	  (else
	   (cons (walk (car form))
		 (walk (cdr form)))))))

(define (form/replace form replacements)
  (let walk ((form form))
    (cond ((not (pair? form))
	   (let ((place (assq form replacements)))
	     (if (not place)
		 form
		 (cadr place))))
	  ((QUOTE/? form)
	   `(QUOTE ,(quote/text form)))
	  (else
	   (cons (walk (car form))
		 (walk (cdr form)))))))


(define (form/copy-transforming specialized-copier expr)
  ;; specialized-copier = (lambda (expr recursive-copy uninteresting) ...)
  (define (copy expr)
    (if (pair? expr)
	(specialized-copier expr copy uninteresting)
	expr))

  (define (uninteresting expr)
    (cond ((not (pair? expr)) expr)
	  ((QUOTE/? expr)
	   `(QUOTE ,(quote/text expr)))
	  ((LOOKUP/? expr)
	   `(LOOKUP ,(lookup/name expr)))
	  ((DECLARE/? expr)
	   `(DECLARE ,@(list-copy (declare/declarations expr))))
	  ((LAMBDA/? expr)
	   `(LAMBDA ,(lambda/formals expr) ,(copy (lambda/body expr))))
	  ((LET/? expr)
	   `(LET ,(copy-bindings (let/bindings expr))
	      ,(copy (let/body expr))))
	  ((LETREC/? expr)
	   `(LETREC ,(copy-bindings (letrec/bindings expr))
	      ,(copy (letrec/body expr))))
	  ((or (CALL/? expr) (BEGIN/? expr) (IF/? expr))
	   `(,(car expr) . ,(map copy (cdr expr))))
	  ((SET!/? expr)
	   `(SET! (set!/name expr) (copy (set!/expr expr))))
	  (else
	   (internal-error "FORM/COPY-TRANSFORMING - illegal form" expr))))

  (define (copy-bindings bindings)
    (map (lambda (binding)
	   (list (first binding) (copy (second binding))))
	 bindings))

  (copy expr))
#|
Example use of FORM/COPY-TRANSFORMING:
(define (begin->nigeb expr)
  (form/copy-transforming 
   (lambda (expr copy uninteresting)
     (if (BEGIN/? expr)
	 `(NIGEB . ,(map copy (cdr expr)))
	 (uninteresting expr)))
   expr))
|#
			  

(define (begin-sans-declarations form)
  ;; If the begin form is a sequence of declarations followed by a single
  ;; form then return that form, else return #F
  (let loop ((exprs (begin/exprs form)))
    (cond ((null? exprs) (internal-error "No non-declare subform" form))
	  ((DECLARE/? (car exprs)) (loop (cdr exprs)))
	  (else (if (null? (cdr exprs)) (car exprs) #F)))))

(define (form/satisfies? form operator-properties)
  (let walk ((expr form))
    (and (pair? expr)
	 (case (car expr)
	   ((LOOKUP QUOTE LAMBDA) true)
	   ((IF)
	    (and (walk (if/predicate expr))
		 (walk (if/consequent expr))
		 (walk (if/alternate expr))))
	   ((CALL)
	    (let ((rator (call/operator expr)))
	      (and (QUOTE/? rator)
		   (operator/satisfies? (quote/text rator) operator-properties)
		   (for-all? (call/cont-and-operands expr) walk))))
	   ((BEGIN)
	    (cond ((begin-sans-declarations expr) => walk)
		  (else false)))
	   (else false)))))

(define (form/simple&side-effect-free? operand)
  (form/satisfies? operand '(SIMPLE SIDE-EFFECT-FREE)))

(define (form/simple&side-effect-insensitive? operand)
  (form/satisfies? operand '(SIMPLE SIDE-EFFECT-INSENSITIVE)))


(define ((form/head-operator-test predicate) form)
  (let walk ((form form))
    (and (pair? form)
	 (case (car form)
	   ((LOOKUP QUOTE LAMBDA) true)
	   ((IF)
	    (and (form/simple&side-effect-free? (if/predicate form))
		 (walk (if/consequent form))
		 (walk (if/alternate form))))
	   ((CALL)
	    (let ((rator (call/operator form)))
	      (and (QUOTE/? rator)
		   (predicate (quote/text rator))
		   (for-all? (call/cont-and-operands form)
		     form/simple&side-effect-free?))))
	   ((BEGIN)
	    (cond ((begin-sans-declarations form) => walk)
		  (else false)))
	   (else false)))))

(define (pseudo-simple-operator? rator)
  (or (operator/satisfies? rator '(SIMPLE))
      (operator/satisfies? rator '(OUT-OF-LINE-HOOK))))

(define form/simple?
  (form/head-operator-test (lambda (op) (simple-operator? op))))

(define form/pseudo-simple?
  (form/head-operator-test pseudo-simple-operator?))

(define form/pseudo-simple&side-effect-free?
  (form/head-operator-test
   (lambda (rator)
     (and (pseudo-simple-operator? rator)
	  (operator/satisfies? rator '(SIDE-EFFECT-FREE))))))

(define (form/number? form)
  (and (QUOTE/? form)
       (number? (quote/text form))
       (quote/text form)))

(define (form/exact-integer? form)
  (and (QUOTE/? form)
       (exact-integer? (quote/text form))
       (quote/text form)))

(define (binding-context-type keyword context bindings)
  (if (or (eq? keyword 'LETREC)
	  (eq? context 'DYNAMIC))
      context
      (call-with-values
       (lambda ()
	 (list-split
	  (list-transform-negative bindings 
	    (lambda (binding)
	      ;; eliminate any continuation variables.  They will not
	      ;; be considered as either dynamic or static (as
	      ;; suggested by Jinx)
	      ;; --JBANK
	      (continuation-variable? (car binding))))
	  (lambda (binding) (form/static? (cadr binding)))))
       (lambda (static dynamic)
	 (cond ((null? dynamic) 'STATIC)
	       ((null? static) 'DYNAMIC)
	       (else (internal-error
		      "Frame with static and dynamic bindings")))))))

(define (form/static? form)
  ;; This assumes that the operands are OK.
  (and (CALL/? form)
       (let ((rator (call/operator form)))
	 (and (QUOTE/? rator)
	      (operator/satisfies? (quote/text rator) '(STATIC))))))

(define (form/free-vars form)
  (form/%free-vars form true))

(define (form/%free-vars form inside-lambda?)
  ;;  Only valid after environment conversion.
  (define (free-vars* exprs bound acc)
    (let loop ((acc acc)
	       (exprs exprs))
      (if (null? exprs)
	  acc
	  (loop (free-vars (car exprs) bound acc)
		(cdr exprs)))))

  (define (maybe-add var bound acc)
    (if (or (memq var bound) (memq var acc))
	acc
	(cons var acc)))

  (define (free-vars expr bound acc)
    (if (not (pair? expr))
	(internal-error "form/free-vars: Not a KMP expression" expr))
    (case (car expr)
      ((LOOKUP)
       (maybe-add (cadr expr) bound acc))
      ((LAMBDA)
       (if (not inside-lambda?)
	   acc
	   (free-vars (caddr expr)
		      (append (lambda-list->names (cadr expr))
			      bound)
		      acc)))
      ((LET)
       (free-vars* (map cadr (cadr expr))
		   bound
		   (free-vars (caddr expr)
			      (map* bound car (cadr expr))
			      acc)))
      ((CALL BEGIN IF DELAY OR)
       (free-vars* (cdr expr) bound acc))
      ((LETREC)
       (free-vars* (cons (caddr expr) (map cadr (cadr expr)))
		   (map* bound car (cadr expr))
		   acc))
      ((SET!)
       (maybe-add (cadr expr)
		  bound
		  (free-vars (caddr expr) bound acc)))
      ((QUOTE DECLARE)
       acc)
      ((ACCESS DEFINE IN-PACKAGE THE-ENVIRONMENT)
       (no-longer-legal expr 'FORM/FREE-VARS))
      (else
       (illegal expr))))

  (free-vars form '() '()))

(define-structure (pattern-variable
		   (conc-name pattern-variable/)
		   (constructor ->pattern-variable)
		   (print-procedure
		    (standard-unparser-method 'PATTERN-VARIABLE
		      (lambda (v port)
			(write-char #\space port)
			(display (pattern-variable/name v) port)))))
  (name false read-only true))

(define (form/equal? form1 form2)
  (define (walk form1 form2)
    (or (eq? form1 form2)
	(and (pair? form1)
	     (pair? form2)
	     (walk (car form1) (car form2))
	     (walk (cdr form1) (cdr form2)))))

  (walk form1 form2))

(define (form/match pattern form)
  (define (walk pattern form dict)
    (and dict
	 (cond ((pattern-variable? pattern)
		(let ((place (assq pattern (cdr dict))))
		  (cond ((not place)
			 (cons 'DICT
			       (cons (list pattern form)
				     (cdr dict))))
			((form/equal? (cadr place) form)
			 dict)
			(else
			 false))))
	       ((eq? pattern form)
		dict)
	       ((pair? pattern)
		(and (pair? form)
		     (walk (cdr pattern)
			   (cdr form)
			   (walk (car pattern)
				 (car form)
				 dict))))
	       (else
		false))))

  (let ((result (walk pattern form (list 'DICT))))
    (and result
	 (or (null? (cdr result))
	     (cdr result)))))

;;;; Lambda-list utilities

(define (lambda-list-keyword? object)
  (or (eq? #!optional object)
      (eq? #!rest object)
      (eq? #!aux object)))

(define (lambda-list->names lambda-list)
  (cond ((null? lambda-list)
	 lambda-list)
	((lambda-list-keyword? (car lambda-list))
	 (lambda-list->names (cdr lambda-list)))
	(else
	 (cons (car lambda-list) (lambda-list->names (cdr lambda-list))))))

(define (lambda-list/count-names lambda-list)
  (let loop  ((list lambda-list) (count 0))
    (cond ((null? list)  count)
	  ((lambda-list-keyword? (car list))
	   (loop (cdr list) count))
	  (else
	   (loop (cdr list) (+ count 1))))))

(define (hairy-lambda-list? lambda-list)
  (there-exists? lambda-list lambda-list-keyword?))

(define (guarantee-simple-lambda-list lambda-list)
  (if (hairy-lambda-list? lambda-list)
      (internal-error "Unexpected lambda list keywords" lambda-list)))

(define (guarantee-argument-list args len)
  (if (not (= (length args) len))
      (internal-error "Wrong number of arguments" len args)))

(define (lambda-list/applicate form lambda-list args)
  ;; If LAMBDA-LIST is to be simplified by removing #!optional and #!rest
  ;; markers, then the ARGS must be processed to ensure the lambda
  ;; bindings are bould to the same values.  Returns a list of
  ;; expressions. #!aux is not allowed.  FORM is used only for error
  ;; reporting to locate the user's source.
  (define (bad message)
    (user-error message	(form->source-irritant form)))
  (let loop ((ll lambda-list)
	     (ops args)
	     (ops* '()))
    (cond ((null? ll)
	   (if (not (null? ops))
	       (bad "Too many arguments"))
	   (reverse! ops*))
	  ((eq? (car ll) #!optional)
	   (loop (if (or (null? (cddr ll))
			 (eq? #!rest (caddr ll)))
		     (cddr ll)
		     (cons #!optional (cddr ll)))
		 (if (null? ops)
		     ops
		     (cdr ops))
		 (cons (if (null? ops)
			   `(QUOTE ,%unassigned)
			   (car ops))
		       ops*)))
	  ((eq? (car ll) #!rest)
	   ;; This only works before CPS conversion.
	   ;; By that time, all "lexprs" should have been split.
	   (reverse!
	    (cons (let listify ((ops ops))
		    (if (null? ops)
			`(QUOTE ())
			`(CALL (QUOTE ,%cons)
			       (QUOTE #F)
			       ,(car ops)
			       ,(listify (cdr ops)))))
		  ops*)))
	  ((null? ops)
	   (bad "Too few arguments"))
	  (else
	   (loop (cdr ll) (cdr ops) (cons (car ops) ops*))))))

(define (lambda-list/parse lambda-list)
  ;; (values required optional rest aux)
  (let parse ((ll lambda-list))
    (cond ((null? ll)
	   (values '() '() false '()))
	  ((eq? (car ll) #!optional)
	   (call-with-values
	    (lambda () (parse (cdr ll)))
	    (lambda (opt opt* rest aux)
	      (if (not (null? opt*))
		  (internal-error "Multiple #!optional specifiers"
				  lambda-list))
	      (values '() opt rest aux))))
	  ((eq? (car ll) #!rest)
	   (call-with-values
	    (lambda () (parse (cdr ll)))
	    (lambda (req opt rest aux)
	      (if (or (null? req)
		      (not (null? (cdr req)))
		      (not (null? opt))
		      rest)
		  (internal-error "Unexpected stuff after #!rest" lambda-list))
	      (values '() '() (car req) aux))))
	  ((eq? (car ll) #!aux)
	   (call-with-values
	    (lambda () (parse (cdr ll)))
	    (lambda (req opt rest aux)
	      (if (or (null? req)
		      (not (null? opt))
		      rest
		      (not (null? aux)))
		  (internal-error "Unexpected stuff after #!aux" lambda-list))
	      (values '() '() false req))))
	  (else
	   (call-with-values
	    (lambda () (parse (cdr ll)))
	    (lambda (req opt rest aux)
	      (values (cons (car ll) req) opt rest aux)))))))

(define (lambda-list/arity-info lambda-list)
  ;; This includes the return address, since the
  ;; current convention includes that.
  (call-with-values
   (lambda () (lambda-list/parse lambda-list))
   (lambda (required optional rest aux)
     ;; min includes the continuation, since after CPS!
     aux				; ignored
     (let* ((min (length required))
	    (max (+ min (length optional))))
       (list min
	     (if rest
		 (- 0 (+ max 1))
		 max))))))

(define (lambda-list/minimum-arity lambda-list)
  (call-with-values
      (lambda () (lambda-list/parse lambda-list))
    (lambda (required optional rest aux)
      optional rest aux			; ignored
      (length required))))

(define (lambda-body/frame-vector body)
  (and (LET/? body)
       (pair? (let/bindings body))
       (CALL/%fetch-stack-closure?
	(second (first (let/bindings body))))
       (QUOTE/text 
	(CALL/%fetch-stack-closure/vector
	 (second (first (let/bindings body)))))))

;;;; List & vector utilities

(define-integrable (for-every things proc)
  (for-each proc things))

(define (delq* to-remove some-list)
  (if (null? to-remove)
      some-list
      (let loop ((al some-list)
		 (names '()))
	(cond ((null? al)
	       (reverse! names))
	      ((memq (car al) to-remove)
	       (loop (cdr al) names))
	      (else
	       (loop (cdr al)
		     (cons (car al) names)))))))

(define (list-prefix ol tail)
  (let loop ((elements '())
	     (l ol))
    (cond ((eq? l tail)
	   (reverse! elements))
	  ((null? l)
	   (error "list-prefix: not a prefix" ol tail))
	  (else
	   (loop (cons (car l) elements)
		 (cdr l))))))

(define (difference set1 set2)
  (list-transform-negative set1
    (lambda (element)
      (memq element set2))))

(define (intersection set1 set2)
  (cond ((null? set1)
	 '())
	((null? set2)
	 '())
	(else
	 (list-transform-positive set1
	   (lambda (element)
	     (memq element set2))))))

(define (union set1 set2)
  (cond ((null? set1)
	 set2)
	((null? set2)
	 set1)
	(else
	 (append (delq* set2 set1) set2))))

(define (union-map* set0 proc l)
  ;; Apply PROC to each element of L and union the results with SET0
  (let loop ((set set0)
	     (l l))
    (if (null? l)
	set
	(loop (union (proc (car l)) set)
	      (cdr l)))))

(define (remove-duplicates l)
  (let loop ((l l) (l* '()))
    (cond ((null? l)           (reverse! l*))
	  ((memq (car l) l*)   (loop (cdr l) l*))
	  (else                (loop (cdr l) (cons (car l) l*))))))

(define (null-intersection? set1 set2)
  (cond ((null? set1)  #T)
	((null? set2)  #T)
	((memq (car set1) set2) #F)
	(else  (null-intersection? (cdr set1) set2))))

(define (list-split ol predicate)
  ;; (values yes no)
  (let loop ((l (reverse ol))
	     (yes '())
	     (no '()))
    (cond ((null? l)
	   (values yes no))
	  ((predicate (car l))
	   (loop (cdr l) (cons (car l) yes) no))
	  (else
	   (loop (cdr l) yes (cons (car l) no))))))

(define (rassq value alist)
  (let loop ((alist alist))
    (and (pair? alist)
	 (pair? (car alist))
	 (if (eq? value (cdar alist))
	     (car alist)
	     (loop (cdr alist))))))

(define (pick-random l)
  (let ((len (length l)))
    (list-ref l (if *allow-random-choices?*
		    (random len)
		    (quotient len 2)))))

(define (vector-index vector name)
  (if (not (vector? vector))
      (internal-error "vector-index: Not a vector" vector name)
      (do ((i (- (vector-length vector) 1) (- i 1)))
	  ((eq? name (vector-ref vector i)) i)
	(if (= i 0)
	    (internal-error "vector-index: component not found"
			    vector name)))))

(define (pair-up oone otwo)
  (let loop ((one oone) (two otwo) (result '()))
    (cond ((and (not (null? one))
		(not (null? two)))
	   (loop (cdr one)
		 (cdr two)
		 (cons (cons (car one) (car two))
		       result)))
	  ((or (null? one)
	       (null? two))
	   (internal-error "pair-up: Mismatched lengths" oone otwo))
	  (else
	   (reverse! result)))))

(define-structure (queue
		   (conc-name queue/)
		   (constructor queue/%make))
  (head false read-only true)
  (tail false read-only false))

(define (queue/make)
  (let ((pair (cons '*HEAD* '())))
    (queue/%make pair pair)))

(define (queue/enqueue! queue object)
  (let ((pair (cons object '())))
    (set-cdr! (queue/tail queue) pair)
    (set-queue/tail! queue pair)))

(define (queue/enqueue!* queue objects)
  (if (not (null? objects))
      (let ((objects* (list-copy objects)))
	(set-cdr! (queue/tail queue) objects*)
	(set-queue/tail! queue (last-pair objects*)))))

(define (queue/drain! queue process)
  ;; process can cause more queueing
  (let loop ((pair (queue/head queue)))
    (if (not (null? (cdr pair)))
	(begin
	  (process (cadr pair))
	  ;; This can GC by bashing the queue!
	  (loop (cdr pair))))))	

(define (queue/contents queue)
  (cdr (queue/head queue)))

;;;; Miscellaneous

(define (eq?-memoize function)
  (let  ((table  (make-eq-hash-table))
	 (absent (cons #f #f)))
    (lambda (arg)
      (let ((value  (hash-table/get table arg absent)))
	(if (eq? value absent)
	    (let  ((value  (function arg)))
	      (hash-table/put! table arg value)
	      value)
	    value)))))

;; Missing SCODE utilities for input

(define (the-environment-components tenv receiver)
  tenv					; ignored
  (receiver))

(define (scode/absolute-reference? object)
  (and (access? object)
       (null? (access-environment object))))

(define (absolute-reference-name reference)
  (access-name reference))

(define (good-factor? value)
  (and (machine-fixnum? value)
       (< (abs value) *sup-good-factor*)))

(define (good-factor->nbits value)
  (if (not (good-factor? value))
      (internal-error "constant factors can only be good factors"
		      value)
      (ceiling->exact (/ (log (abs value)) (log 2)))))

(define (power-of-two? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) false)
	  ((= n power) exponent)
	  (else
	   (loop (* 2 power) (1+ exponent))))))



;; Careful constant folding.  Returns a `result' or #F if not possible to
;; compute.  Use UNMAP-CAREFUL to get the actual result.

(define *careful-operations-complain?* #F)

(define (careful-error message . irritants)
  (if *careful-operations-complain?*
      (apply user-warning message irritants))
  false)

(define map-careful)
(define unmap-careful)

(let ((careful-false (list 'careful-false)))
  (set! map-careful   (lambda (v)  (if (eq? v '#F) careful-false v)))
  (set! unmap-careful (lambda (v)  (if (eq? v careful-false) '#F v))))

(define (careful/quotient x y)
  (if (and (number? x) (number? y) (not (zero? y)))
      (quotient x y)
      (careful-error "quotient: Domain error" x y)))

(define (careful/remainder x y)
  (if (and (number? x) (number? y) (not (zero? y)))
      (remainder x y)
      (careful-error "remainder: Domain error" x y)))

(define (careful// x y)
  (if (and (number? x) (number? y) (not (zero? y)))
      (/ x y)
      (careful-error "/: Domain error" x y)))

;; This is slow:
(define (apply-carefully operator operands)
  (let ((result  (ignore-errors (lambda () (apply operator operands)))))
    (if (condition? result)
	(begin
	  (if *careful-operations-complain?*
	      (user-warning
	       (with-string-output-port
		(lambda (port) (write-condition-report result port)))
	       (cons operator operands))
	      #F))
	(map-careful result))))

(define (iota n)
  (do ((i (- n 1) (- i 1))
       (acc '() (cons i acc)))
      ((< i 0) acc)))

;;(define code/rewrite-table/make
;;  (strong-hash-table/constructor eq-hash-mod eq? true))

(define code-rewrite/remember
  (let ((not-found (list '*NOT-FOUND*)))
    (lambda (new old)
      (let ((crt *code-rewrite-table*))
	(if (and crt (eq? not-found (code-rewrite-table/get crt new not-found)))
	    (let* ((pcrt *previous-code-rewrite-table*)
		   (old* (if (not pcrt)
			     not-found
			     (code-rewrite-table/get pcrt
						     old
						     not-found))))
	      (cond ((not (eq? old* not-found))
		     (code-rewrite-table/put! crt new old*))
		    ((eq? pcrt #t)
		     (code-rewrite-table/put! crt new old))))))
      new)))

(define code-rewrite/remember*
  (let ((not-found (list '*NOT-FOUND*)))
    (lambda (new old)
      (let ((crt *code-rewrite-table*))
	(if (and crt (eq? not-found (code-rewrite-table/get crt new not-found)))
	    (code-rewrite-table/put! crt new old)))
      new)))

(define code-rewrite/remember*!
  (lambda (new old)
    (code-rewrite-table/put! *code-rewrite-table* new old)))

(define (code-rewrite/original-form new)
  (and *code-rewrite-table*
       (code-rewrite-table/get *code-rewrite-table* new false)))

(define code-rewrite/original-form*/previous
  (let ((not-found (list '*NOT-FOUND*)))
    (lambda (old)
      ;; (values available? form)
      (if (not *previous-code-rewrite-table*)
	  (values false old)
	  (let ((ancient
		 (code-rewrite-table/get *previous-code-rewrite-table*
					 old not-found)))
	    (if (eq? not-found ancient)
		(values false old)
		(values true ancient)))))))      

(define (code-rewrite/original-form/previous old)
  (and *previous-code-rewrite-table*
       (code-rewrite-table/get *previous-code-rewrite-table* old false)))

;;(define (code/rewrite-table/copy table)
;;  (hash-table/copy table
;;		   code/rewrite-table/make))
(define (code/rewrite-table/copy table)
  (monotonic-strong-eq-hash-table/copy table))

(define (kmp-program-size program)
  (let walk ((program program) (size 0))
    (cond ((not (pair? program))
	   size)
	  ((QUOTE/? program)
	   (+ size 1))
	  (else
	   (walk (car program) (walk (cdr program) (+ size 1)))))))

;; (compile-boolean-property expr env bound? free? assigned?)
;;   -> #F  or  name -> Bool
;;
;; bound?, free?, assigned? : (env * name -> Bool) or #F
;;

(define (compile-boolean-property expr env bound? free? assigned?)

  (let compile ((expr expr))
    (define (binary-operator? tag operate)
      (and (list? expr)
	   (= (length expr) 3)
	   (eq? (car expr) tag)
	   (let ((a (compile (second expr)))
		 (b (compile (third expr))))
	     (and a b (operate a b)))))
    (define-integrable (environment-query predicate?)
      (and predicate?
	   (lambda (name)
	     (predicate? env name))))
    (cond ((eq? expr 'NONE)     (lambda (name) name #F))
	  ((eq? expr 'ALL)      (lambda (name) name #T))
	  ((eq? expr 'FREE)     (environment-query free?))
	  ((eq? expr 'BOUND)    (environment-query bound?))
	  ((eq? expr 'ASSIGNED) (environment-query assigned?))
	  ((not (pair? expr))   #F)
	  ((and (eq? (car expr) 'SET)
		(list? expr)
		(for-all? expr symbol?)
		(lambda (name)
		  (memq name (cdr expr)))))
	  ((binary-operator? 'UNION
			     (lambda (a b)
			       (lambda (name)
				 (or (a name) (b name))))))
	  ((binary-operator? 'DIFFERENCE
			     (lambda (a b)
			       (lambda (name)
				 (and (a name) (not (b name)))))))
	  ((binary-operator? 'INTERSECTION
			     (lambda (a b)
			       (lambda (name)
				 (and (a name) (b name))))))
	  (else #F))))

;; FORM-MAPs are mappings form KMP forms to something
;;  Implemented as monotonic-strong-eq-hash-table

(define make-form-map make-monotonic-strong-eq-hash-table)
(define form-map/get monotonic-strong-eq-hash-table/get)
(define form-map/put! monotonic-strong-eq-hash-table/put!)


(define code/rewrite-table/make make-form-map)

(define code-rewrite-table/get  form-map/get)
(define code-rewrite-table/put! form-map/put!)
