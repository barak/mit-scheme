#| -*-Scheme-*-

$Id: fakeprim.scm,v 1.21 1995/08/19 01:36:13 adams Exp $

Copyright (c) 1994-1995 Massachusetts Institute of Technology

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

;;;; Pseudo primitives
;;; package: (compiler midend)

(declare (usual-integrations))

;;;; Pseudo primitives

(define *operator-properties*
  (make-eq-hash-table))

(define (define-operator-properties rator properties)
  (hash-table/put! *operator-properties* rator properties))

(define (known-operator? rator)
  (hash-table/get *operator-properties* rator false))

(define (simple-operator? rator)
  (assq 'SIMPLE (hash-table/get *operator-properties* rator '())))

(define (hook-operator? rator)
  (assq 'OUT-OF-LINE-HOOK (hash-table/get *operator-properties* rator '())))

(define (operator/satisfies? rator properties)
  (let ((props (hash-table/get *operator-properties* rator '())))
    (for-all? properties
      (lambda (prop)
	(assq prop props)))))

(define (make-constant name)
  (intern name))

(define (make-operator name . properties)
  (let ((operator (make-constant name)))
    (define-operator-properties operator
      (if (null? properties)
	  (list '(KNOWN))
	  properties))
    operator))

(define (make-operator/simple name . more)
  (apply make-operator name
	 '(SIMPLE) '(SIDE-EFFECT-INSENSITIVE) '(SIDE-EFFECT-FREE) more))

(define (make-operator/effect-sensitive name . more)
  (apply make-operator name '(SIMPLE) '(SIDE-EFFECT-FREE) more))

(define (make-operator/simple* name . more)
  (apply make-operator name '(SIMPLE) more))


(define-macro (cookie-call name . parts)
  (define (->string x)  (if (symbol? x) (symbol-name x) x))
  (define (->sym . stuff)
    (intern (apply string-append (map ->string stuff))))
  (define (make-predicate)
    `(DEFINE-INTEGRABLE (,(->sym "call/" name "?") FORM)
       (AND (PAIR? FORM)
	    (EQ? (CAR FORM) 'CALL)
	    (PAIR? (CDR FORM))
	    (PAIR? (CADR FORM))
	    (PAIR? (CDADR FORM))
	    (EQ? (CAADR FORM) 'QUOTE)
	    (EQ? (CADADR FORM) ,name))))
  (define (loop  args path defs)
    (define (add-def field path quoted?)
      (let* ((base-name   (->sym "call/" name "/" field))
	     (safe-name   (->sym base-name "/safe"))
	     (unsafe-name (->sym base-name "/unsafe")))
	(cons*
	 `(DEFINE-INTEGRABLE (,base-name FORM)
	    (,safe-name FORM))
	 `(DEFINE (,safe-name FORM)
	    (IF (AND (,(->sym "call/" name "?") FORM)(PAIR? FORM)
		     ,@(if quoted?
			   `((PAIR? ,path)
			     (EQ? (CAR ,path) 'QUOTE)
			     (PAIR? (CDR ,path)))
			   `()))
		,path
		(INTERNAL-ERROR "Illegal Cookie call syntax" ',name FORM)))
	 ;;`(DEFINE-INTEGRABLE (,unsafe-name FORM)
	 ;;   ,(if quoted?
	 ;;	 `(CADR ,path)
	 ;;	 path))
	 defs)))
    (cond ((null? args)
	   defs)
	  ((eq? (car args) '#!REST)
	   (add-def (cadr args) path #F))
	  ((eq? (car args) '#F)
	   (loop (cdr args) `(CDR ,path) defs))
	  ((equal? (car args) ''#F)
	   (loop (cdr args) `(CDR ,path) defs))
	  ((and (pair? (car args)) (eq? (car (car args)) 'QUOTE))
	   (loop (cdr args)
		 `(CDR ,path)
		 (add-def (cadr (car args)) `(CAR ,path) #T)))
	  (else
	   (loop (cdr args)
		 `(CDR ,path)
		 (add-def (car args) `(CAR ,path) #F)))))
  `(BEGIN ,(make-predicate)
	  ,@(reverse (loop parts `(CDDR FORM) '()))))

(define %*lookup
  ;; (CALL ',%*lookup <continuation> <environment>
  ;;       'VARIABLE-NAME 'DEPTH 'OFFSET)
  ;; Note:
  ;;   DEPTH and OFFSET are #F (unknown) or non-negative fixnums
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced
  ;;     by a call to the primitive LEXICAL-REFERENCE)
  (make-operator "#[*lookup]" '(SIDE-EFFECT-FREE)))

(cookie-call %*lookup cont environment 'variable-name 'depth 'offset)


(define %*set!
  ;; (CALL ',%*set! <continuation> <environment>
  ;;       'VARIABLE-NAME <value> 'DEPTH 'OFFSET)
  ;; Note:
  ;;   DEPTH and OFFSET are #F (unknown) or non-negative fixnums
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced
  ;;     by a call to the primitive LEXICAL-ASSIGNMENT)
  (make-operator "#[*set!]"))

(cookie-call %*set! cont environment 'VARIABLE-NAME value 'DEPTH 'OFFSET)

(define %*unassigned?
  ;; (CALL ',%*unassigned? <continuation> <environment>
  ;;       'VARIABLE-NAME 'DEPTH 'OFFSET)
  ;; Note:
  ;;   DEPTH and OFFSET are #F (unknown) or non-negative fixnums
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced
  ;;     by a call to the primitive LEXICAL-UNASSIGNED?)
  ;;   Returns a boolean value
  (make-operator "#[*unassigned?]" '(SIDE-EFFECT-FREE)))

(cookie-call %*unassigned? cont environment 'variable-name 'depth 'offset)


(define %*define
  ;; (CALL ',%*define <continuation> <environment>
  ;;       'VARIABLE-NAME <value>)
  ;; Note:
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced
  ;;     by a call to the primitive LOCAL-ASSIGNMENT)
  (make-operator "#[*define]"))

(cookie-call %*define cont environment 'VARIABLE-NAME value)


(define %*define*
  ;; (CALL ',%*define* <continuation> <environment>
  ;;       <vector of names> <vector of values>)
  ;; Note:
  ;;   Introduced by expand.scm, removed by compat.scm (replaced
  ;;     by a call to the global procedure DEFINE-MULTIPLE)
  (make-operator "#[*define*]"))

(cookie-call %*define* cont environment 'names-vector 'values-vector)


(define %*make-environment
  ;; (CALL ',%*make-environment <continuation>
  ;;       <parent environment> <vector of names> <value>*)
  ;; Note:
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced
  ;;     by a call to the global procedure *MAKE-ENVIRONMENT)
  ;;   The vector of names has one MORE name than values: the
  ;;     first name is the value of the variable
  ;;     LAMBDA-TAG:MAKE-ENVIRONMENT (self-name for use by
  ;;     unsyntaxer).
  (make-operator "#[*make-environment]" '(SIDE-EFFECT-FREE)))

(cookie-call %*make-environment cont env 'names-vector #!rest values)


;; %fetch-environment, %fetch-continuation, %fetch-stack-closure, and
;; the variable cache operations are simple, but should not be
;; substituted or reordered, hence they are not defined as
;; effect-insensitive or effect-free

(define %fetch-environment
  ;; (CALL ',%fetch-environment '#F)
  ;; Note:
  ;;   Introduced by envconv.scm, open coded by RTL generator.
  ;;   Appears at the top-level of expressions to be executed at
  ;;     load-time or in first-class environment code.
  (make-operator/simple* "#[fetch-environment]" '(STATIC)))

(cookie-call %fetch-environment '#F)

(define %make-operator-variable-cache
  ;; (CALL ',%make-operator-variable-cache '#F <environment>
  ;;       'NAME 'NARGS)
  ;; Note:
  ;;   Introduced by envconv.scm, ignored by RTL generator.
  ;;   It is required to make the trivial KMP-Scheme evaluator work
  ;;     and to guarantee no free variable references in KMP-Scheme
  ;;     code.
  (make-operator/effect-sensitive "#[make-operator-variable-cache]"
				  '(STATIC)))

(cookie-call %make-operator-variable-cache '#F environment 'NAME 'NARGS)

(define %make-remote-operator-variable-cache
  ;; (CALL ',%make-remote-operator-variable-cache '#F
  ;;       'PACKAGE-DESCRIPTOR 'NAME 'NARGS)
  ;; Note: 
  ;;   For now, the linker only supports #F (global environment) for
  ;;     the PACKAGE-DESCRIPTOR.
  ;;   Introduced by envconv.scm, ignored by RTL generator.
  ;;   It is required to make the trivial KMP-Scheme evaluator
  ;;     work and to guarantee no free variable references in
  ;;     KMP-Scheme code.
  (make-operator/effect-sensitive "#[make-remote-operator-variable-cache]"
				  '(STATIC)))


(cookie-call %make-remote-operator-variable-cache '#F
	     'PACKAGE-DESCRIPTOR 'NAME 'NARGS)

(define %make-read-variable-cache
  ;; (CALL ',%make-read-variable-cache '#F <environment> 'NAME)
  ;; Note:
  ;;   Introduced by envconv.scm, ignored by RTL generator.
  ;;   It is required to make the trivial KMP-Scheme evaluator
  ;;     work and to guarantee no free variable references in
  ;;     KMP-Scheme code.
  (make-operator/effect-sensitive "#[make-read-variable-cache]"
				  '(STATIC)))

(cookie-call %make-read-variable-cache '#F environment 'NAME)

(define %make-write-variable-cache
  ;; (CALL ',%make-write-variable-cache '#F <environment> 'NAME)
  ;; Note:
  ;;   Introduced by envconv.scm, ignored by RTL generator.
  ;;   It is required to make the trivial KMP-Scheme evaluator
  ;;     work and to guarantee no free variable references in
  ;;     KMP-Scheme code.
  (make-operator/effect-sensitive "#[make-write-variable-cache]"
				  '(STATIC)))

(cookie-call %make-write-variable-cache '#F environment 'NAME)


(define %invoke-operator-cache
  ;; (CALL ',%invoke-operator-cache <continuation>
  ;;       '(NAME NARGS) <operator-cache> <value>*)
  ;; Note:
  ;;   Introduced by envconv.scm.
  ;;   NARGS is redundant with both the number of <value>*
  ;;     expressions and the expression creating the <operator-cache>
  ;;   This is used for operators to be referenced from the top-level
  ;;     (load-time) environment.
  (make-operator "#[invoke-operator-cache]"))

(cookie-call %invoke-operator-cache cont
	     'descriptor operator-cache #!rest values)

(define %invoke-remote-cache
  ;; (CALL ',%invoke-remote-cache <continuation>
  ;;       '(NAME NARGS) <operator-cache> <value>*)
  ;; Note:
  ;;   Introduced by envconv.scm.
  ;;   NARGS is mostly redundant with both the number of <value>*
  ;;     expressions and the expression creating the <operator-cache>
  ;;     (but see *make-environment in compat.scm for an exception)
  ;;   This is used for operators to be referenced from arbitrary
  ;;     named packages, although the linker currently only supports
  ;;     the global environment.
  (make-operator "#[invoke-remote-operator-cache]"))

(cookie-call %invoke-remote-cache cont
	     'descriptor operator-cache #!rest values)

(define %variable-cache-ref
  ;; (CALL %variable-cache-ref '#F <read-variable-cache> 'ignore-traps? 'NAME)
  ;; Note:
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced by a
  ;;     lot of hairy code)
  ;;   The NAME is redundant with the code that creates the variable cache
  ;;   Errors if the variable is unassigned or unbound.
  (make-operator "#[variable-cache-ref]"
		 '(SIDE-EFFECT-FREE) '(OUT-OF-LINE-HOOK)))

(cookie-call %variable-cache-ref '#F read-variable-cache 'IGNORE-TRAPS? 'NAME)

(define %variable-cache-set!
  ;; (CALL ',%variable-cache-set! '#F <write-variable-cache>
  ;;       <value> 'IGNORE-TRAPS? 'NAME)
  ;; Note:
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced by a
  ;;     lot of hairy code)
  ;;   The NAME is redundant with the code that creates the variable cache
  (make-operator "#[variable-cache-set!]" '(OUT-OF-LINE-HOOK)))

(cookie-call %variable-cache-set! '#F write-variable-cache value
	     'IGNORE-TRAPS? 'NAME)

(define %safe-variable-cache-ref
  ;; (CALL ',%safe-variable-cache-ref '#F <read-variable-cache>
  ;;       'IGNORE-TRAPS? 'NAME)
  ;; Note:
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced by a
  ;;     lot of hairy code)
  ;;   Doesn't error if the variable is currently unassigned (but it
  ;;     does error on unbound)
  ;;   The NAME is redundant with the code that creates the variable cache
  (make-operator "#[safe-variable-cache-ref]"
		 '(SIDE-EFFECT-FREE) '(OUT-OF-LINE-HOOK)))

(cookie-call %safe-variable-cache-ref '#F read-variable-cache
	     'IGNORE-TRAPS? 'NAME)

(define %variable-read-cache
  ;; (CALL ',%variable-read-cache '#F <read-variable-cache> 'NAME)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-ref and %safe-variable-cache-ref
  ;;   This declares the existence of a variable cache, but doesn't
  ;;     read the contents.
  (make-operator/simple "#[variable-read-cache]"))

(cookie-call %variable-read-cache '#F read-variable-cache 'NAME)

(define %variable-write-cache
  ;; (CALL ',%variable-write-cache '#F <write-variable-cache> 'NAME)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-set!
  ;;   This declares the existence of a variable cache, but doesn't
  ;;     read the contents.
  (make-operator/simple "#[variable-write-cache]"))

(cookie-call %variable-write-cache '#F write-variable-cache 'NAME)

(define %variable-cell-ref
  ;; (CALL ',%variable-cell-ref '#F <read-variable-cache>)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-ref and %safe-variable-cache-ref
  ;;   Does not checking of contents of cache (i.e. doesn't look for
  ;;     unassigned or unbound trap objects).  Simply a data selector.
  (make-operator/effect-sensitive "#[variable-cell-ref]"))

(cookie-call %variable-cell-ref '#F read-variable-cache)

(define %variable-cell-set!
  ;; (CALL ',%variable-cell-ref '#F <write-variable-cache> <value>)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-set!
  ;;   Simply a data mutator.
  (make-operator/simple* "#[variable-cell-set!]"))

(cookie-call %variable-cell-set! '#F write-variable-cache value)

(define %hook-variable-cell-ref
  ;; (CALL ',%hook-variable-cell-ref <continuation or '#F>
  ;;       <read-variable-cache>)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-ref
  ;;   The reference must be done out of line for some reason.
  ;;   If the continuation is #F then the code generator is
  ;;     responsible for creating a return address and preserving all
  ;;     necessary state (registers) needed upon return.  Otherwise
  ;;     there is no need to save state, but the variable reference
  ;;     should tail call into the continuation.
  (make-operator "#[hook-variable-cell-ref]"
		 '(OUT-OF-LINE-HOOK) '(SPECIAL-INTERFACE)))

(cookie-call %hook-variable-cell-ref cont read-variable-cache)


(define %hook-safe-variable-cell-ref
  ;; (CALL ',%hook-safe-variable-cell-ref <continuation or '#F>
  ;;       <read-variable-cache>)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %safe-variable-cache-ref (qv)
  ;;   The reference must be done out of line for some reason.
  ;;   If the continuation is #F then the code generator is
  ;;     responsible for creating a return address and preserving all
  ;;     necessary state (registers) needed upon return.  Otherwise
  ;;     there is no need to save state, but the variable reference
  ;;     should tail call into the continuation.
  (make-operator "#[hook-safe-variable-cell-ref]"
		 '(OUT-OF-LINE-HOOK) '(SPECIAL-INTERFACE)))

(cookie-call %hook-safe-variable-cell-ref cont read-variable-cache)

(define %hook-variable-cell-set!
  ;; (CALL ',%hook-safe-variable-cell-set! '#F
  ;;       <write-variable-cache> <value>) 
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-set!
  ;;   The reference must be done out of line for some reason.
  ;;   No <continuation> is allowed because this would have been
  ;;   rewritten into something like
  ;;      (LET ((old-value ...)) (set! ...) (LOOKUP old-value))
  (make-operator "#[hook-variable-cell-set!]"
		 '(OUT-OF-LINE-HOOK) '(SPECIAL-INTERFACE)))

(cookie-call %hook-variable-cell-set! '#F write-variable-cache value) 

(define %copy-program
  ;; (CALL ',%copy-program <continuation> <program>)
  ;; Note:
  ;;   Introduced by envconv.scm and removed by compat.scm (replaced
  ;;     by a call to the global procedure COPY-PROGRAM).
  ;;   The value of <program> is a compiled expression object.
  ;;   This is generated under the following (unusual?) circumstances:
  ;;     when a closure must be generated with variable caches (for
  ;;     free references) to an environment that is reified (because,
  ;;     for example, of a call to (the-environment)) and is neither the
  ;;     global nor the top-level (load-time) environment.  By
  ;;     default, the compiler switches don't allow variable caches in
  ;;     this case.
  ;;   Typical code:
  ;;     (CALL ',%copy-program (LOOKUP CONT47)
  ;;           '#[COMPILED-EXPRESSION 23])
  (make-operator "#[copy-program]"))

(cookie-call %copy-program cont program)


(define %execute
  ;; (CALL ',%execute <continuation> <program> <environment>)
  ;; Note:
  ;;   Introduced by envconv.scm and removed by compat.scm (replaced
  ;;     by a call to the primitive procedure SCODE-EVAL).
  ;;   The value of <program> is a compiled expression object.
  ;;   Typical code:
  ;;     (CALL ',%execute (LOOKUP CONT47)
  ;;           '#[COMPILED-EXPRESSION 23] (LOOKUP ENV43))
  (make-operator "#[execute]"))

(cookie-call %execute cont program environment)

(define %internal-apply
  ;; (CALL ',%internal-apply <continuation> 'NARGS <procedure> <value>*)
  ;; Note:
  ;;   NARGS = number of <value> expressions
  ;;   Introduced by applicat.scm.
  (make-operator "#[internal-apply]"))
(cookie-call %internal-apply cont 'NARGS procedure #!REST values)


(define %internal-apply-unchecked
  ;; (CALL ',%internal-apply-unchecked <continuation> 'NARGS <procedure>
  ;;        <value>*)
  ;; Note:
  ;;   NARGS = number of <value> expressions
  ;;   Like %internal-apply, but assumes that the procedure is compiled and
  ;;   of the correct arity.
  (make-operator "#[internal-apply-unchecked]"))
(cookie-call %internal-apply-unchecked cont 'NARGS procedure #!REST values)

(define %primitive-apply
  ;; (CALL ',%primitive-apply <continuation>
  ;;       'NARGS '<primitive-object> <value>*) 
  ;; Note:
  ;;   NARGS = number of <value> expressions
  ;;   Introduced by applicat.scm and removed by compat.scm (replaced
  ;;     by %PRIMITIVE-APPLY/COMPATIBLE).
 (make-operator "#[primitive-apply]"))

(cookie-call %primitive-apply cont 'NARGS 'primitive-object #!rest values)

(define %arity-dispatcher-tag
  (make-constant "#[(microcode)arity-dispatcher-tag]"))

(define %unspecific
  ;; Magic cookie representing an ignorable value
  (make-constant "#[unspecific]"))

(define %unassigned
  ;; The value of variables that do not yet have values ...
  (make-constant "#[unassigned]"))

(define %unassigned?
  ;; (CALL ',%unassigned? '#F <value>)
  ;; Note:
  ;;   Introduced by envconv.scm and expand.scm from the MIT Scheme
  ;;     special form (UNASSIGNED? <variable name>)
  (make-operator/simple "#[unassigned?]" '(PROPER-PREDICATE)))

(cookie-call %unassigned? '#F value)


(define %reference-trap?
  ;; (CALL ',%reference-trap? '#F <value>)
  ;; Note:
  ;;   Introduced by compat.scm as part of the rewrite of
  ;;   %variable-cache-ref, %safe-variable-cache-ref, and
  ;;   %variable-cache-set!
  (make-operator/simple "#[reference-trap?]" '(PROPER-PREDICATE)))

(cookie-call %reference-trap? '#F value)

(define %primitive-error
  ;;  (CALL ',%primitive-error '#F '<primitive> <arg1> .. <argN>
  ;;        <expr1> ... <exprn>
  ;;  Call <primitive> with <arg1> ... <argN> to signal an error.
  ;; Note:
  ;;   Introduced at any stage that we know that <primitive> will fail, for
  ;;   example, when replacing a primitive by a checked
  ;;     (if <type-check> <open-coded-version> <error>)
  ;;   diamond.  The <arg_i> are the arguments to the primitive (N is
  ;;   determined from the primitive's arity) that cause it to fail.
  ;;
  ;;   The additional expressions are inserted to keep data live to enhance
  ;;   debugging information (at the cost of keeping it live, both in
  ;;   compile time, compiled code performance, and GC opportunities).
  ;;   For example, we may choose to introduce this code and/or insert
  ;;   the expressions as follows:
  ;;     . Add the containing procedure's full set of parameters before
  ;;       lambda-lifting/1.  This may force procedures to be
  ;;       implemented as closures.
  ;;     . After lambda-lifting/1.  This gives us everything that can be made
  ;;       available without creating extra closures.  The cost is
  ;;       keeping extra values in the stack.
  ;;     . After lambda-lifting/2 (same as not at all).  This gives us
  ;;       everything that would be available in fully optimized code.
  ;;
  ;;   This operation may be implemented in two ways: restartable and
  ;;   non-restartable (perhaps we will introduce another operator to
  ;;   distinguish the two).  If restartable, we need a hook to call
  ;;   arbitrary primitives with preservation.  If non-restartable we
  ;;   need a space-efficient method of constructing a continuation
  ;;   and passing the arguments, and some work on rtlgen to make it
  ;;   understand expressions that never terminate (i.e. to construct
  ;;   a non-diamond rgraph), and some work on conpar and dbg-info to
  ;;   understand the continuation, and some work on uerror to
  ;;   understand that restarts are not an option.
  (make-operator "#[primitive-error]"))

(define %cons
  ;; (CALL ',%cons '#F <value> <value>)
  ;; Open-coded CONS operation.
  ;; Note:
  ;;   Introduced by LAMBDA-LIST/APPLICATE to do early application of
  ;;     a known lexpr (avoids an out-of-line call at runtime)
  (make-operator/simple "#[cons]"))

(cookie-call %cons '#F car-value cdr-value)


;; Unchecked operations on pairs.  Result is unspecified if the pair
;; argument is not a pair.
(define %car
  ;; (CALL ',%car '#F <pair>)
  (make-operator/effect-sensitive "#[car]"))

(define %cdr
  ;; (CALL ',%cdr '#F <pair>)
  (make-operator/effect-sensitive "#[cdr]"))

(define %set-car!
  ;; (CALL ',%set-car '#F <pair> <value>)
  (make-operator/simple* "#[set-car!]" '(UNSPECIFIC-RESULT)))

(define %set-cdr!
  ;; (CALL ',%set-cdr '#F <pair> <value>)
  (make-operator/simple* "#[set-cdr!]" '(UNSPECIFIC-RESULT)))


(define %make-entity
  ;; (CALL ',%make-entity '#F <value> <value>)
  (make-operator/simple "#[make-entity]"))


(define %vector
  ;; (CALL ',%vector '#F <value>*)
  ;; Open-coded version of VECTOR primitive.
  ;; Note:
  ;;   Introduced by expand.scm for DEFINE-MULTIPLE
  (make-operator/simple "#[vector]"))

(cookie-call %vector '#F #!rest values)

(define %vector-length
  ;; (CALL ',%vector-length '#F <vector>)
  ;; Unchecked.
  (make-operator/simple "#[vector-length]"))

(define %vector-check
  ;; (CALL ',%vector-check '#F <object> <index>)
  ;; ->#T if <object> is a vector and <index> is a valid index
  (make-operator/simple "#[vector-check]" '(PROPER-PREDICATE)))

(define %vector-check/index
  ;; (CALL ',%vector-check/index '#F <vector> <index>)
  ;; ->#T if <index> is a valid index for the vector <vector>
  (make-operator/simple "#[vector-check/index]" '(PROPER-PREDICATE)))

(define %vector-ref
  ;; (CALL ',%vector-ref '#F <vector> <index>)
  ;; Unchecked.
  (make-operator/effect-sensitive "#[vector-ref]"))

(define %vector-set!
  ;; (CALL ',%vector-set! '#F <vector> <index> <value>)
  ;; Unchecked.
  (make-operator/simple* "#[vector-set!]" '(UNSPECIFIC-RESULT)))

(define %vector-ref/check
  ;; (CALL ',%vector-ref/check '#F <vector> <limit> <index>)
  ;;  Range-check component of VECTOR-REF.
  ;;   VECTOR-REF
  ;;    == (%vector-ref/check <vector> (VECTOR-LENGTH <vector>) <index>)
  ;; Note
  ;;   This operation can be used in a loop, with the vector-length operation
  ;;   lifted out.  Further, if the length is computed as
  ;;     (if (vector? <vector>) (%vector-length <vector>) 0)
  ;;   then the inner check is effectively coerced to a type check as well.
  (make-operator/effect-sensitive "#[vector-ref/check]"))

(define %vector-set/check!
  ;; (CALL ',%vector-set/check! '#F <vector> <limit> <index> <value>)
  ;; See %vector-ref/check
  (make-operator/simple* "#[vector-set/check!]" '(UNSPECIFIC-RESULT)))


(define %make-promise
  ;; (CALL ',%make-promise '#F <thunk>)
  ;; Note:
  ;;   Introduced by expand.scm for DELAY
  (make-operator/simple "#[make-promise]"))
(cookie-call %make-promise '#F thunk)

(define %make-cell
  ;; (CALL ',%make-cell '#F <value> 'NAME)
  (make-operator/simple "#[make-cell]"))
(cookie-call %make-cell '#F value 'NAME)

(define %cell-ref
  ;; (CALL ',%cell-ref '#F <cell> 'NAME)
  (make-operator/effect-sensitive "#[cell-ref]"))
(cookie-call %cell-ref '#F cell 'NAME)

(define %cell-set!
  ;; (CALL ',%cell-set '#F <cell> <value> 'NAME)
  ;; Note:
  ;;   Returns no value, because the rewrite is to something like
  ;;     (LET ((old-value ...))
  ;;       (CALL ',%cell-set! ...)
  ;;       (LOOKUP old-value))
  (make-operator/simple* "#[cell-set!]" '(UNSPECIFIC-RESULT)))
(cookie-call %cell-set! '#F cell value 'NAME)

;; Multicells are collections of cells.  Each cell is named.  LAYOUT
;; describes the arrangment of the cells in memory.  Currently it is
;; just a vector of names.
;; Multicells are introduced by assconv.scm for references to local
;; mutable variables.

(define %make-multicell
  ;; (CALL ',%make-multicell '#F 'LAYOUT <value> <value> ...)
  (make-operator/simple "#[make-multicell]"))
(cookie-call %make-multicell '#F 'LAYOUT #!rest values)

(define %multicell-ref
  ;; (CALL ',%multicell-ref '#F cell 'LAYOUT 'NAME)
  (make-operator/effect-sensitive "#[multicell-ref]"))
(cookie-call %multicell-ref '#F cell 'LAYOUT 'NAME)

(define %multicell-set!
  ;; (CALL ',%multicell-set! '#F cell value 'LAYOUT 'NAME)
  ;; Note:
  ;;   Always used in statement position - has no value.
  (make-operator/simple* "#[multicell-set!]" '(UNSPECIFIC-RESULT)))
(cookie-call %multicell-set! '#F cell value 'LAYOUT 'NAME)


;; Tuples are collections of values.  Each slot is named.  LAYOUT
;; describes the arrangment of the slots in memory.  Currently it is
;; just a vector of names.  Tuples are immutable.  The intention is
;; that they can be introduced to collect values together without
;; committing to a representation.

(define %make-tuple
  ;; (CALL ',%make-tuple '#F 'LAYOUT <value> <value> ...)
  (make-operator/simple "#[make-tuple]"))
;;(cookie-call %make-tuple '#F 'LAYOUT #!rest values)

(define %tuple-ref
  ;; (CALL ',%tuple-ref '#F cell 'LAYOUT 'NAME)
  (make-operator/simple "#[tuple-ref]"))
;;(cookie-call %tuple-ref '#F tuple 'LAYOUT 'NAME)

;; OBSOLETE
;;(define %vector-index
;;  ;; (CALL ',%vector-index '#F 'VECTOR 'NAME)
;;  ;; Note:
;;  ;;   VECTOR is a vector of symbols, including NAME
;;  ;;   Returns the index of NAME within the vector.
;;  ;;   Introduced by closconv.scm and removed (constant folded) by
;;  ;;     indexify.scm.  Used for referencing variables in closures and
;;  ;;     stack frames.
;;  (make-operator/simple "#[vector-index]"))
;;(cookie-call %vector-index '#F 'VECTOR 'NAME)

;; %heap-closure-ref, %stack-closure-ref, and %static-binding-ref are not
;; properly simple, but they can be considered such because %heap-closure-set!,
;; %make-stack-closure, and %static-binding-set! are used only in limited ways.

(define %make-heap-closure
  ;; (CALL ',%make-heap-closure '#F <lambda-expression> 'VECTOR
  ;;       <value>*)
  ;; Note:
  ;;   Introduced by closconv.scm (first time it is invoked).
  ;;   VECTOR is a vector of symbols whose length is the same as the
  ;;     number of <value> expressions.
  (make-operator/simple "#[make-heap-closure]"))

(cookie-call %make-heap-closure '#F lambda-expression 'VECTOR #!rest values)

(define %heap-closure-ref
  ;; (CALL ',%heap-closure-ref '#F <closure> <offset> 'NAME)
  ;; Note:
  ;;   Introduced by closconv.scm (first time it is invoked)
  (make-operator/simple "#[heap-closure-ref]"))
(cookie-call %heap-closure-ref '#F closure offset 'NAME)

(define %heap-closure-set!
  ;; (CALL ',%heap-closure-set! '#F <closure> <offset> <value> 'NAME)
  (make-operator/simple* "#[heap-closure-set!]" '(UNSPECIFIC-RESULT)))
(cookie-call %heap-closure-set! '#F closure offset value 'NAME)

(define %make-trivial-closure
  ;; (CALL ',%make-trivial-closure '#F <lambda-expression or LOOKUP>)
  ;; Note:
  ;;   Introduced by closconv.scm (first time it is invoked).
  ;;   Constructs an externally callable procedure object (all free
  ;;     variables are accessible through the variable caching
  ;;     mechanism).
  ;;   A LOOKUP is permitted only in a LETREC at the top level of a
  ;;     program.  It is used to export one of the mutually recursive
  ;;     procedures introduced by the LETREC to the external
  ;;     environment.
  (make-operator/simple "#[make-trivial-closure]"))
(cookie-call %make-trivial-closure '#F procedure)

(define %make-static-binding
  ;; (CALL ',%make-static-binding '#F <value> 'NAME)
  ;; Note:
  ;;   Generate a static binding cell for NAME, containing <value>.
  ;;   Introduced by staticfy.scm (not currently working).
  (make-operator/simple "#[make-static-binding]"))
(cookie-call %make-static-binding '#F value 'NAME)

(define %static-binding-ref
  ;; (CALL ',%static-binding-ref '#F <static-cell> 'NAME)
  ;; Note:
  ;;   Introduced by staticfy.scm (not currently working).
  (make-operator/simple "#[static-binding-ref]"))
(cookie-call %static-binding-ref '#F static-cell 'NAME)

(define %static-binding-set!
  ;; (CALL ',%static-binding-set! '#F <static-cell> <value> 'NAME)
  ;; Note:
  ;;   Introduced by staticfy.scm (not currently working).
  (make-operator/simple* "#[static-binding-set!]" '(UNSPECIFIC-RESULT)))
(cookie-call %static-binding-set! '#F static-cell value 'NAME)

(define %make-return-address
  ;; (CALL ',%make-return-address '#F <lambda-expression>)
  ;; Note:
  ;;   Used internally in rtlgen.scm when performing trivial rewrites
  ;;     before calling itself recursively.
  (make-operator/simple "#[make-return-address]"))
(cookie-call %make-return-address '#F lambda-expression)

;; %fetch-continuation is not static, but things get confused otherwise
;; It is handled specially by lamlift and closconv

(define %fetch-continuation
  ;; (CALL ',%fetch-continuation '#F)
  ;; Note:
  ;;   Grab return address, for use in top-level expressions since they
  ;;     (unlike procedures) do not receive a continuation.
  ;;   Introduced by cpsconv.scm.
  (make-operator/simple* "#[fetch-continuation]" '(STATIC)))
(cookie-call %fetch-continuation '#F)

(define %invoke-continuation
  ;; (CALL ',%invoke-continuation <continuation> <value>*)
  ;; Note:
  ;;   Introduced by cpsconv.scm
  (make-operator "#[invoke-continuation]"))
(cookie-call %invoke-continuation cont #!rest values)

(define %fetch-stack-closure
  ;; (CALL ',%fetch-stack-closure '#F 'VECTOR)
  ;; Note:
  ;;   VECTOR contains symbols only.
  ;;   This is supposed to return a pointer to the current top of
  ;;     stack, which contains values (or cells for values) of the
  ;;     variables named in VECTOR.  In fact, rtlgen.scm knows about
  ;;     this special case and generates no output code.
  (make-operator/simple* "#[fetch-stack-closure]"))
(cookie-call %fetch-stack-closure '#F 'VECTOR)

(define %make-stack-closure
  ;; (CALL ',%make-stack-closure '#F <lambda-expression or '#F>
  ;;       'VECTOR <value>*)
  ;; Note:
  ;;   This appears *only* as the continuation of some KMP-Scheme CALL.
  ;;   If a lambda-expression is supplied, it pushes the values on the
  ;;     stack (creating a stack closure of the format specified) and
  ;;     loads the return address specified by the lambda-expression
  ;;     into the return address location (register or stack
  ;;     location).  If no lambda expression is provided, simply
  ;;     pushes the values.
  ;;   Introduced by closconv.scm specifying a lambda expression, and
  ;;     by compat.scm with #F.
  (make-operator/simple "#[make-stack-closure]"))
(cookie-call %make-stack-closure '#F lambda-expression 'VECTOR #!rest values)

(define %stack-closure-ref
  ;; (CALL ',%stack-closure-ref '#F <closure> <offset> 'NAME)
  ;; Note:
  ;;   Introduced by closconv.scm.
  ;;   Handled specially by rtlgen.scm
  (make-operator/simple "#[stack-closure-ref]"))
(cookie-call %stack-closure-ref '#F closure offset 'NAME)

(define %machine-fixnum?
  ;; (CALL ',%machine-fixnum? '#F <value>)
  ;; Note:
  ;;   #T if <value> is a fixnum on the target machine, else #F
  (make-operator/simple "#[machine-fixnum?]" '(PROPER-PREDICATE)))
(cookie-call %machine-fixnum? '#F value)

(define %small-fixnum?
  ;; (CALL ',%small-fixnum? '#F <value> 'FIXNUM)
  ;; Note:
  ;;  #T iff <value> is a fixnum on the target machine and all of top
  ;;    FIXNUM+1 bits are the same (i.e. the top FIXNUM precision bits
  ;;    match the sign bit, i.e. it can be represented in FIXNUM fewer
  ;;    bits than a full fixnum on the target machine).  This is used
  ;;    in the expansion of generic arithmetic to guarantee no
  ;;    overflow is possible on the target machine.
  ;;  If FIXNUM is 0, then this is the same as %machine-fixnum?
  (make-operator/simple "#[small-fixnum?]" '(PROPER-PREDICATE)))

(cookie-call %small-fixnum? '#F value 'precision-bits)

(define %word-less-than-unsigned?
  ;; (CALL ', %word-less-than-unsigned? '#F <smaller> <larger>
  (make-operator/simple "#[word-less-than-unsigned?]" '(PROPER-PREDICATE)))

(define %compiled-entry?
  (make-operator/simple "#[compiled-entry?]"  '(PROPER-PREDICATE)))

(cookie-call %compiled-entry? '#F object)

(define %compiled-entry-maximum-arity?
  ;; (call ',%compiled-entry-maximum-arity? '#F 'count value)
  ;;  Tests if the compiled entry has the specified maximum arity.
  (make-operator/simple "#[compiled-entry-maximum-arity?]"
			'(PROPER-PREDICATE)))

(cookie-call %compiled-entry-maximum-arity? '#F 'n entry)

(define %profile-data
  ;; (CALL ',%profile-data '#F '<data>)
  (make-operator/simple* "#[profile-data]" '(UNSPECIFIC-RESULT)))

(cookie-call %profile-data '#F 'data)


(define (make-operator/out-of-line name . more)
  (apply make-operator name
	 '(SIDE-EFFECT-INSENSITIVE)
	 '(SIDE-EFFECT-FREE)
	 '(OUT-OF-LINE-HOOK)
	 more))

;; The following operations are used as:
;; (CALL ',<operator> <continuation or #F> <value1> <value2>)
;; Note:
;;   If the continuation is #F then the code generator is responsible
;;     for creating a return address and preserving all necessary
;;     state (registers) needed upon return.  Otherwise there is no
;;     need to save state, but the operation should tail call into the
;;     continuation.

(define %+ (make-operator/out-of-line "#[+]"))
(define %- (make-operator/out-of-line "#[-]"))
(define %* (make-operator/out-of-line "#[*]"))
(define %/ (make-operator/out-of-line "#[/]"))
(define %quotient (make-operator/out-of-line "#[quotient]"))
(define %remainder (make-operator/out-of-line "#[remainder]"))
(define %= (make-operator/out-of-line "#[=]" '(PROPER-PREDICATE)))
(define %< (make-operator/out-of-line "#[<]" '(PROPER-PREDICATE)))
(define %> (make-operator/out-of-line "#[>]" '(PROPER-PREDICATE)))

(define *vector-cons-max-open-coded-length* 5)

(define %vector-cons
  ;; (CALL ',%vector-cons <continuation or #F> <length> <fill-value>)
  ;; Note:
  ;;   If the continuation is #F then the code generator is responsible
  ;;     for creating a return address and preserving all necessary
  ;;     state (registers) needed upon return.  Otherwise there is no
  ;;     need to save state, but the operation should tail call into the
  ;;     continuation.
  (make-operator/out-of-line "#[vector-cons]"))

;; These limit the size of an object the is open-coded by bumping free
;; without a rigourous heap check.
(define *string-allocate-max-open-coded-length* 4000)
(define *floating-vector-cons-max-open-coded-length* 500)

(define %string-allocate
  ;; (CALL ',%string-allocate <continuation or #F> <length>)
  ;; Note:
  ;;   If the continuation is #F then the code generator is responsible
  ;;     for creating a return address and preserving all necessary
  ;;     state (registers) needed upon return.  Otherwise there is no
  ;;     need to save state, but the operation should tail call into the
  ;;     continuation.
  (make-operator/out-of-line "#[string-allocate]"))

(define %floating-vector-cons
  ;; (CALL ',%floating-vector-cons <continuation or #F> <length>)
  ;; Note:
  ;;   If the continuation is #F then the code generator is responsible
  ;;     for creating a return address and preserving all necessary
  ;;     state (registers) needed upon return.  Otherwise there is no
  ;;     need to save state, but the operation should tail call into the
  ;;     continuation.
  (make-operator/out-of-line "#[floating-vector-cons]"))

;;; Inform the compiler about system primitives

(for-each
    (lambda (simple-operator)
      (define-operator-properties simple-operator
	(list '(SIMPLE)
	      '(SIDE-EFFECT-INSENSITIVE)
	      '(SIDE-EFFECT-FREE)
	      '(PROPER-PREDICATE))))
  (list not eq? null? false?
	boolean? cell? pair? vector? %record? string? bit-string?
	fixnum? index-fixnum? flo:flonum? object-type?
	fix:= fix:> fix:< fix:<= fix:>=
	fix:zero? fix:positive? fix:negative? 
	flo:= flo:> flo:< #| flo:<= flo:>= |#
	flo:zero? flo:positive? flo:negative?))

(for-each
 (lambda (simple-operator)
   (define-operator-properties simple-operator
     (list '(SIMPLE)
	   '(SIDE-EFFECT-FREE)
	   '(PROPER-PREDICATE))))
 (list (make-primitive-procedure 'HEAP-AVAILABLE? 1)
       ))

(for-each
 (lambda (simple-operator)
   (define-operator-properties
     simple-operator
     (list '(SIMPLE)
	   '(SIDE-EFFECT-INSENSITIVE)
	   '(SIDE-EFFECT-FREE))))
 (list make-cell cons vector %record string-allocate flo:vector-cons
       system-pair-cons %record-length vector-length flo:vector-length
       object-type object-datum
       bit-string-length
       (make-primitive-procedure 'PRIMITIVE-OBJECT-SET-TYPE)
       fix:-1+ fix:1+ fix:+ fix:- fix:*
       fix:quotient fix:remainder ; fix:gcd
       fix:andc fix:and fix:or fix:xor fix:not fix:lsh
       flo:+ flo:- flo:* flo:/
       flo:negate flo:abs flo:sqrt
       flo:floor flo:ceiling flo:truncate flo:round
       flo:exp flo:log flo:sin flo:cos flo:tan flo:asin
       flo:acos flo:atan flo:atan2 flo:expt
       flo:floor->exact flo:ceiling->exact
       flo:truncate->exact flo:round->exact
       ascii->char integer->char char->ascii char-code char->integer))

(for-each
 (lambda (simple-operator)
   (define-operator-properties
     simple-operator
     (list '(SIMPLE)
	   '(SIDE-EFFECT-FREE))))
 (list cell-contents car cdr %record-ref
       vector-ref
       string-ref
       string-length vector-8b-ref flo:vector-ref
       system-pair-car system-pair-cdr
       system-hunk3-cxr0 system-hunk3-cxr1 system-hunk3-cxr2
       (make-primitive-procedure 'PRIMITIVE-GET-FREE)
       (make-primitive-procedure 'PRIMITIVE-OBJECT-REF)))

(for-each
 (lambda (operator)
   (define-operator-properties
     operator
     (list '(SIMPLE) '(UNSPECIFIC-RESULT))))
 (list set-cell-contents! set-car! set-cdr! %record-set!
       vector-set!
       string-set! vector-8b-set! flo:vector-set!
       (make-primitive-procedure 'PRIMITIVE-INCREMENT-FREE)
       (make-primitive-procedure 'PRIMITIVE-OBJECT-SET!)))

(for-each
 (lambda (prim-name)
   (define-operator-properties
     (make-primitive-procedure prim-name)
     (list '(SIDE-EFFECT-FREE)
	   '(SIDE-EFFECT-INSENSITIVE)
	   '(OUT-OF-LINE-HOOK)
	   '(OPEN-CODED-PREDICATE)
	   '(PROPER-PREDICATE))))
 '(&= &< &>
   zero? negative? positive?  ; translated into &= &< &>
   ))

(for-each
 (lambda (prim-name)
   (define-operator-properties
     (make-primitive-procedure prim-name)
     (list '(SIDE-EFFECT-FREE)
	   '(SIDE-EFFECT-INSENSITIVE)
	   '(OUT-OF-LINE-HOOK))))
 '(&+ &- &* &/ quotient remainder))

(for-each
    (lambda (prim-name)
      (let ((prim  (make-primitive-procedure prim-name)))
	(set! compiler:primitives-with-no-open-coding
	      (cons prim-name compiler:primitives-with-no-open-coding))
	(define-operator-properties
	  prim
	  (list ;;'(SIMPLE)
	        '(SIDE-EFFECT-FREE)
	        '(SIDE-EFFECT-INSENSITIVE)))))
  '(COERCE-TO-COMPILED-PROCEDURE))

(for-each
    (lambda (prim-name)
      (let ((prim  (make-primitive-procedure prim-name)))
	(set! compiler:primitives-with-no-open-coding
	      (cons prim-name compiler:primitives-with-no-open-coding))))
  '(VECTOR-REF VECTOR-SET! CAR CDR))

;;;; Compatibility operators

(define %primitive-apply/compatible
  ;; (CALL ',%primitive-apply/compatible '#F 'NARGS
  ;;       '<primitive-object>)
  ;; Note:
  ;;   Introduced by compat.scm from %primitive-apply
  (make-operator "#[primitive-apply 2]"))
(cookie-call %primitive-apply/compatible '#F 'NARG primitive-object)

;;; Operators for calling procedures, with a description of the calling
;;  convention.
;;
;; Note these have not been implemented but please leave them here for
;; when we come back to passing unboxed floats.

(define %call/convention
  ;; (CALL ',%call/convention <cont> <convention> <op> <value*>)
  ;; Note:
  ;;   Introduced by compat.scm from CALL
  (make-operator "#[call/convention]"))

(define %invoke-operator-cache/convention
  ;; (CALL ',%invoke-operator-cache/convention <cont> <convention>
  ;;      '(NAME NARGS) <cache> <value>*)
  ;; Note:
  ;;   Introduced by compat.scm from %invoke-operator-cache
  (make-operator "#[invoke-operator-cache/convention]"))

(define %invoke-remote-cache/convention
  ;; (CALL ',%invoke-remote-cache/convention <cont> <convention>
  ;;       '(NAME NARGS) <cache> <value>*)
  ;; Note:
  ;;   Introduced by compat.scm from %invoke-remote-cache
  (make-operator "#[invoke-remote-cache/convention]"))

(define %internal-apply/convention
  ;; (CALL ',%interna-apply/convention <cont> <convention>
  ;;       'NARGS <procedure> <value>*)
  ;; Note:
  ;;   Introduced by compat.scm from %internal-apply
  (make-operator "#[internal-apply/convention]"))

(define %primitive-apply/convention
  ;; (CALL ',%primitive-apply/convention <cont> <convention>
  ;;       'NARGS '<primitive-object> <value>*)
  ;; Note:
  ;;   Introduced by compat.scm from %primitive-apply
  (make-operator "#[primitive-apply/convention]"))

(define %invoke-continuation/convention
  ;; (CALL ',%invoke-continuation/convention <cont> <convention>
  ;;       <value>*)
  ;; Note:
  ;;   Introduced by compat.scm from %invoke-continuation
  (make-operator "#[invoke-continuation/convention]"))

(define %fetch-parameter-frame
  ;; (CALL ',%fetch-parameter-frame '#F <convention>)
  ;; Note:
  ;;   This is supposed to return an accessor for local parameters.
  ;;   In fact, rtlgen.scm knows about this special case and generates
  ;;   no output code.  It is used to set an initial model of how
  ;;   parameters are passed in to a procedure, so it must appear
  ;;   immediately after the parameter list for a LAMBDA expression.
  (make-operator "#[fetch-parameter-frame]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntax abstractions

(let-syntax
    ((kmp-form-accessors
      (macro (name . args)
	(define (->string x)  (if (symbol? x) (symbol-name x) x))
	(define (->sym . stuff)
	  (intern (apply string-append (map ->string stuff))))
	(define (loop  args path defs)
	  (define (add-def field path)
	    (let  ((base-name    (->sym name "/" field))
		   (safe-name    (->sym name "/" field "/safe"))
		   (unsafe-name  (->sym name "/" field "/unsafe")))
	      (cons* `(DEFINE-INTEGRABLE (,base-name FORM)
			(,safe-name FORM))
		     `(DEFINE-INTEGRABLE (,unsafe-name FORM)
			,path)
		     `(DEFINE            (,safe-name FORM)
			(IF (AND (PAIR? FORM)
				 (EQ? (CAR FORM) ',name))
			    ,path
			    (INTERNAL-ERROR "Illegal KMP syntax" ',name FORM)))
		     defs)))
	    (cond ((null? args)
		   defs)
		  ((eq? (car args) '#!REST)
		   (add-def (cadr args) path))
		  ((eq? (car args) '#F)
		   (loop (cdr args) `(CDR ,path) defs))
		  (else
		   (loop (cdr args)
			 `(CDR ,path)
			 (add-def (car args) `(CAR ,path))))))
	  `(BEGIN 1			;bogon for 0 defs
		  ,@(reverse (loop args `(CDR FORM) '())))))

     (alternate-kmp-form
      (macro (name . args)
	`(kmp-form-accessors ,name . ,args)))
     (kmp-form
      (macro (name . args)
	`(BEGIN (DEFINE-INTEGRABLE (,(symbol-append name '/?) FORM)
		  (AND (PAIR? FORM)
		       (EQ? (CAR FORM) ',name)))
		(kmp-form-accessors ,name . ,args)))))  

  ;; Generate KMP accessors like QUOTE/TEXT (doesn't check head of
  ;; form) and QUOTE/TEXT/SAFE (requires head of form to be QUOTE)

  (kmp-form QUOTE   text)
  (kmp-form LOOKUP  name)
  (kmp-form LAMBDA  formals body)
  (kmp-form LET     bindings body)
  (kmp-form DECLARE #!rest declarations)
  (kmp-form CALL    operator continuation #!rest operands)
  (alternate-kmp-form
            CALL    #F #!rest cont-and-operands)
  (kmp-form BEGIN   #!rest exprs)	; really 1 or more
  (kmp-form IF      predicate consequent alternate)
  (kmp-form LETREC  bindings body)

  (kmp-form SET!    name expr)
  (kmp-form ACCESS  name env-expr)
  (kmp-form DEFINE  name expr)
  (kmp-form THE-ENVIRONMENT)
  (kmp-form IN-PACKAGE env-expr expr)
  )

(define-integrable (call/operand1 form)  (first  (call/operands form)))
(define-integrable (call/operand2 form)  (second (call/operands form)))
(define-integrable (call/operand3 form)  (third  (call/operands form)))
