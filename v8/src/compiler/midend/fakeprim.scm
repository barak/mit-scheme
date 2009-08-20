#| -*-Scheme-*-

$Id$

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

;;;; Pseudo primitives
;;; package: (compiler midend)

(declare (usual-integrations))

;;;; Pseudo primitives

(define *operator-properties* (make-eq-hash-table))

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

(define (make-operator/out-of-line name . more)
  (apply make-operator name 
	 '(SIDE-EFFECT-INSENSITIVE)
	 '(SIDE-EFFECT-FREE)
	 '(OUT-OF-LINE-HOOK)
	 more))

;; By using DEFINE-OPERATOR rather than DEFINE, we get integration of the
;; operator tokens:
(define-macro (define-operator op spec)
  (if (and (list? spec)
	   (>= (length spec) 2)
	   (memq (first spec)
		 '(MAKE-OPERATOR	; all variants above
		   MAKE-OPERATOR/SIMPLE	   MAKE-OPERATOR/EFFECT-SENSITIVE
		   MAKE-OPERATOR/SIMPLE*   MAKE-OPERATOR/OUT-OF-LINE))
	   (string? (second spec)))
      `(BEGIN
	 ,spec
	 (DEFINE-INTEGRABLE ,op ',(string->symbol (second spec))))
      `(DEFINE ,op ,spec)))

;; COOKIE-CALL generates a predicate and accessors to a CALL <operator>
;; expression based on a pattern matching the call expression

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
	  ((eq? (car args) #!rest)
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

(define-operator %*lookup
  ;; (CALL ',%*lookup <continuation> <environment>
  ;;       'VARIABLE-NAME 'DEPTH 'OFFSET)
  ;; Note:
  ;;   DEPTH and OFFSET are #F (unknown) or non-negative fixnums
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced
  ;;     by a call to the primitive LEXICAL-REFERENCE)
  (make-operator "#[*lookup]" '(SIDE-EFFECT-FREE)))

;;(cookie-call %*lookup cont environment 'variable-name 'depth 'offset)


(define-operator %*set!
  ;; (CALL ',%*set! <continuation> <environment>
  ;;       'VARIABLE-NAME <value> 'DEPTH 'OFFSET)
  ;; Note:
  ;;   DEPTH and OFFSET are #F (unknown) or non-negative fixnums
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced
  ;;     by a call to the primitive LEXICAL-ASSIGNMENT)
  (make-operator "#[*set!]"))

;;(cookie-call %*set! cont environment 'VARIABLE-NAME value 'DEPTH 'OFFSET)

(define-operator %*unassigned?
  ;; (CALL ',%*unassigned? <continuation> <environment>
  ;;       'VARIABLE-NAME 'DEPTH 'OFFSET)
  ;; Note:
  ;;   DEPTH and OFFSET are #F (unknown) or non-negative fixnums
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced
  ;;     by a call to the primitive LEXICAL-UNASSIGNED?)
  ;;   Returns a boolean value
  (make-operator "#[*unassigned?]" '(SIDE-EFFECT-FREE)))

;;(cookie-call %*unassigned? cont environment 'variable-name 'depth 'offset)


(define-operator %*define
  ;; (CALL ',%*define <continuation> <environment>
  ;;       'VARIABLE-NAME <value>)
  ;; Note:
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced
  ;;     by a call to the primitive LOCAL-ASSIGNMENT)
  (make-operator "#[*define]"))

(cookie-call %*define cont environment 'VARIABLE-NAME value)


(define-operator %*define*
  ;; (CALL ',%*define* <continuation> <environment>
  ;;       <vector of names> <vector of values>)
  ;; Note:
  ;;   Introduced by expand.scm, removed by compat.scm (replaced
  ;;     by a call to the global procedure DEFINE-MULTIPLE)
  (make-operator "#[*define*]"))

(cookie-call %*define* cont environment 'names-vector 'values-vector)


(define-operator %*make-environment
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

(define-operator %fetch-environment
  ;; (CALL ',%fetch-environment '#F)
  ;; Note:
  ;;   Introduced by envconv.scm, open coded by RTL generator.
  ;;   Appears at the top-level of expressions to be executed at
  ;;     load-time or in first-class environment code.
  (make-operator/simple* "#[fetch-environment]" '(STATIC)))

(cookie-call %fetch-environment '#F)

(define-operator %make-operator-variable-cache
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

(define-operator %make-remote-operator-variable-cache
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

(define-operator %make-read-variable-cache
  ;; (CALL ',%make-read-variable-cache '#F <environment> 'NAME)
  ;; Note:
  ;;   Introduced by envconv.scm, ignored by RTL generator.
  ;;   It is required to make the trivial KMP-Scheme evaluator
  ;;     work and to guarantee no free variable references in
  ;;     KMP-Scheme code.
  (make-operator/effect-sensitive "#[make-read-variable-cache]"
				  '(STATIC)))

(cookie-call %make-read-variable-cache '#F environment 'NAME)

(define-operator %make-write-variable-cache
  ;; (CALL ',%make-write-variable-cache '#F <environment> 'NAME)
  ;; Note:
  ;;   Introduced by envconv.scm, ignored by RTL generator.
  ;;   It is required to make the trivial KMP-Scheme evaluator
  ;;     work and to guarantee no free variable references in
  ;;     KMP-Scheme code.
  (make-operator/effect-sensitive "#[make-write-variable-cache]"
				  '(STATIC)))

(cookie-call %make-write-variable-cache '#F environment 'NAME)


(define-operator %invoke-operator-cache
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

(define-operator %invoke-remote-cache
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

(define-operator %variable-cache-ref
  ;; (CALL %variable-cache-ref '#F <read-variable-cache> 'ignore-traps? 'NAME)
  ;; Note:
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced by a
  ;;     lot of hairy code)
  ;;   The NAME is redundant with the code that creates the variable cache
  ;;   Errors if the variable is unassigned or unbound.
  (make-operator "#[variable-cache-ref]"
		 '(SIDE-EFFECT-FREE) '(OUT-OF-LINE-HOOK)))

(cookie-call %variable-cache-ref '#F read-variable-cache 'IGNORE-TRAPS? 'NAME)

(define-operator %variable-cache-set!
  ;; (CALL ',%variable-cache-set! '#F <write-variable-cache>
  ;;       <value> 'IGNORE-TRAPS? 'NAME)
  ;; Note:
  ;;   Introduced by envconv.scm, removed by compat.scm (replaced by a
  ;;     lot of hairy code)
  ;;   The NAME is redundant with the code that creates the variable cache
  (make-operator "#[variable-cache-set!]" '(OUT-OF-LINE-HOOK)))

(cookie-call %variable-cache-set! '#F write-variable-cache value
	     'IGNORE-TRAPS? 'NAME)

(define-operator %safe-variable-cache-ref
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

(define-operator %variable-read-cache
  ;; (CALL ',%variable-read-cache '#F <read-variable-cache> 'NAME)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-ref and %safe-variable-cache-ref
  ;;   This declares the existence of a variable cache, but doesn't
  ;;     read the contents.
  (make-operator/simple "#[variable-read-cache]"))

(cookie-call %variable-read-cache '#F read-variable-cache 'NAME)

(define-operator %variable-write-cache
  ;; (CALL ',%variable-write-cache '#F <write-variable-cache> 'NAME)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-set!
  ;;   This declares the existence of a variable cache, but doesn't
  ;;     read the contents.
  (make-operator/simple "#[variable-write-cache]"))

(cookie-call %variable-write-cache '#F write-variable-cache 'NAME)

(define-operator %variable-cell-ref
  ;; (CALL ',%variable-cell-ref '#F <read-variable-cache>)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-ref and %safe-variable-cache-ref
  ;;   Does not checking of contents of cache (i.e. doesn't look for
  ;;     unassigned or unbound trap objects).  Simply a data selector.
  (make-operator/effect-sensitive "#[variable-cell-ref]"))

(cookie-call %variable-cell-ref '#F read-variable-cache)

(define-operator %variable-cell-set!
  ;; (CALL ',%variable-cell-ref '#F <write-variable-cache> <value>)
  ;; Note:
  ;;   Introduced by compat.scm as part of rewriting
  ;;     %variable-cache-set!
  ;;   Simply a data mutator.
  (make-operator/simple* "#[variable-cell-set!]"))

(cookie-call %variable-cell-set! '#F write-variable-cache value)

(define-operator %hook-variable-cell-ref
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


(define-operator %hook-safe-variable-cell-ref
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

(define-operator %hook-variable-cell-set!
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

(define-operator %copy-program
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


(define-operator %execute
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

(define-operator %internal-apply
  ;; (CALL ',%internal-apply <continuation> 'NARGS <procedure> <value>*)
  ;; Note:
  ;;   NARGS = number of <value> expressions
  ;;   Introduced by applicat.scm.
  (make-operator "#[internal-apply]"))
(cookie-call %internal-apply cont 'NARGS procedure #!rest values)


(define-operator %internal-apply-unchecked
  ;; (CALL ',%internal-apply-unchecked <continuation> 'NARGS <procedure>
  ;;        <value>*)
  ;; Note:
  ;;   NARGS = number of <value> expressions
  ;;   Like %internal-apply, but assumes that the procedure is compiled and
  ;;   of the correct arity.
  (make-operator "#[internal-apply-unchecked]"))
(cookie-call %internal-apply-unchecked cont 'NARGS procedure #!rest values)

(define-operator %primitive-apply
  ;; (CALL ',%primitive-apply <continuation>
  ;;       'NARGS '<primitive-object> <value>*) 
  ;; Note:
  ;;   NARGS = number of <value> expressions
  ;;   Introduced by applicat.scm and removed by compat.scm (replaced
  ;;     by %PRIMITIVE-APPLY/COMPATIBLE).
 (make-operator "#[primitive-apply]"))
(cookie-call %primitive-apply cont 'NARGS 'primitive-object #!rest values)


(define %primitive-apply/compatible
  ;; (CALL ',%primitive-apply/compatible '#F 'NARGS
  ;;       '<primitive-object>)
  ;; Call a primitive with arguments on the stack
  ;; Note:
  ;;   Introduced by compat.scm from %primitive-apply
  (make-operator "#[primitive-apply 2]"))
(cookie-call %primitive-apply/compatible '#F 'NARG primitive-object)

(define-operator %arity-dispatcher-tag
  (make-constant "#[(microcode)arity-dispatcher-tag]"))

(define-operator %unspecific
  ;; Magic cookie representing an ignorable value
  (make-constant "#[unspecific]"))

(define-operator %unassigned
  ;; The value of variables that do not yet have values ...
  (make-constant "#[unassigned]"))

(define-operator %unassigned?
  ;; (CALL ',%unassigned? '#F <value>)
  ;; Note:
  ;;   Introduced by envconv.scm and expand.scm from the MIT Scheme
  ;;     special form (UNASSIGNED? <variable name>)
  (make-operator/simple "#[unassigned?]" '(PROPER-PREDICATE)))

(cookie-call %unassigned? '#F value)


(define-operator %reference-trap?
  ;; (CALL ',%reference-trap? '#F <value>)
  ;; Note:
  ;;   Introduced by compat.scm as part of the rewrite of
  ;;   %variable-cache-ref, %safe-variable-cache-ref, and
  ;;   %variable-cache-set!
  (make-operator/simple "#[reference-trap?]" '(PROPER-PREDICATE)))

(cookie-call %reference-trap? '#F value)

(define-operator %halt
  ;; (CALL ',%halt <cont> 'code)
  ;; This marks a piece of code that should never be executed.  Valid
  ;; only as the body of a continuations.
  ;;  Note:
  ;;   . Introduced by errcont (called from closconv/2)
  ;;   . RTLGEN should recognize this expression and generate either
  ;;      - a continuation to which the runtime system will refuse to return
  ;;      - a continuation which traps or signals an error if invoked.
  (make-operator "#[halt]"))

(define-operator %reference
  ;; (CALL ',%reference '#F <expr> ...)
  ;; For control over debugging info.
  ;;  This is a dummy statement which causes references to be kept through
  ;;  optimizations, for example, lambda-lifting.
  ;; Note:
  ;;   Introduced anywhere (nowhere at present)
  ;;   Removed by laterew.
  (make-operator "#[reference]"))


(define-operator %cons
  ;; (CALL ',%cons '#F <value> <value>)
  ;; Open-coded CONS operation.
  ;; Note:
  ;;   Introduced by LAMBDA-LIST/APPLICATE to do early application of
  ;;     a known lexpr (avoids an out-of-line call at runtime)
  (make-operator/simple "#[cons]"))

(cookie-call %cons '#F car-value cdr-value)


;; Unchecked operations on pairs.  Result is unspecified if the pair
;; argument is not a pair.
(define-operator %car
  ;; (CALL ',%car '#F <pair>)
  (make-operator/effect-sensitive "#[car]"))

(define-operator %cdr
  ;; (CALL ',%cdr '#F <pair>)
  (make-operator/effect-sensitive "#[cdr]"))

(define-operator %set-car!
  ;; (CALL ',%set-car '#F <pair> <value>)
  (make-operator/simple* "#[set-car!]" '(UNSPECIFIC-RESULT)))

(define-operator %set-cdr!
  ;; (CALL ',%set-cdr '#F <pair> <value>)
  (make-operator/simple* "#[set-cdr!]" '(UNSPECIFIC-RESULT)))


(define-operator %make-entity
  ;; (CALL ',%make-entity '#F <value> <value>)
  (make-operator/simple "#[make-entity]"))


(define-operator %vector
  ;; (CALL ',%vector '#F <value>*)
  ;; Open-coded version of VECTOR primitive.
  ;; Note:
  ;;   Introduced by expand.scm for DEFINE-MULTIPLE
  (make-operator/simple "#[vector]"))

(cookie-call %vector '#F #!rest values)

(define-operator %vector-length
  ;; (CALL ',%vector-length '#F <vector>)
  ;; Unchecked.
  (make-operator/simple "#[vector-length]"))

(define-operator %vector-ref
  ;; (CALL ',%vector-ref '#F <vector> <index>)
  ;; Unchecked.
  (make-operator/effect-sensitive "#[vector-ref]"))

(define-operator %vector-set!
  ;; (CALL ',%vector-set! '#F <vector> <index> <value>)
  ;; Unchecked.
  (make-operator/simple* "#[vector-set!]" '(UNSPECIFIC-RESULT)))


(define-operator %generic-index-check/ref
  ;; (CALL ',%generic-index-check '#F <collection> <index>
  ;;       '#(<type> <length-ref>))
  ;;   Generic type & range check.
  ;;   Returns #T if   <collection> has typecode <type> (or omits if #F)
  ;;              and  0  <=  <index>  <  (<length-ref> <collection>)
  (make-operator/simple "#[generic-index-check/ref]" '(PROPER-PREDICATE)))

(define-operator %generic-index-check/set!
  ;; (CALL ',%generic-index-check '#F <collection> <index> <elt>
  ;;       '#(<type> <length-ref> <elt-type>))
  ;;   Generic type & range check.
  ;;   Returns #T if   <collection> has typecode <type> (or omits check if #F)
  ;;              and  <elt> has typecode <elt-type> (or omits check if #F)
  ;;              and  0  <=  <index>  <  (<length-ref> <collection>)
  (make-operator/simple "#[generic-index-check/set!]" '(PROPER-PREDICATE)))


(define-operator %%record-length  (make-operator/simple "#[%record-length]"))
(define-operator %%record-ref     (make-operator/effect-sensitive "#[%record-ref]"))
(define-operator %%record-set!
  (make-operator/simple* "#[%record-set!]" '(UNSPECIFIC-RESULT)))

(define-operator %string-length   (make-operator/effect-sensitive "#[string-length]"))
(define-operator %string-ref      (make-operator/effect-sensitive "#[string-ref]"))
(define-operator %string-set!
  (make-operator/simple* "#[string-set!]" '(UNSPECIFIC-RESULT)))
(define-operator %vector-8b-ref   (make-operator/effect-sensitive "#[vector-8b-ref]"))
(define-operator %vector-8b-set!
  (make-operator/simple* "#[vector-8b-set!]" '(UNSPECIFIC-RESULT)))

(define-operator %floating-vector-length
  (make-operator/simple "#[floating-vector-length]"))
(define-operator %floating-vector-ref
  (make-operator/effect-sensitive "#[floating-vector-ref]"))
(define-operator %floating-vector-set!
  (make-operator/simple* "#[floating-vector-set!]" '(UNSPECIFIC-RESULT)))

(define-operator %bit-string-length  (make-operator/simple "#[bit-string-length]"))

;;(define %vector-ref/check
;;  ;; (CALL ',%vector-ref/check '#F <vector> <limit> <index>)
;;  ;;  Range-check component of VECTOR-REF.
;;  ;;   VECTOR-REF
;;  ;;    == (%vector-ref/check <vector> (VECTOR-LENGTH <vector>) <index>)
;;  ;; Note
;;  ;;   This operation can be used in a loop, with the vector-length operation
;;  ;;   lifted out.  Further, if the length is computed as
;;  ;;     (if (vector? <vector>) (%vector-length <vector>) 0)
;;  ;;   then the inner check is effectively coerced to a type check as well.
;;  (make-operator/effect-sensitive "#[vector-ref/check]"))
;;
;;(define %vector-set/check!
;;  ;; (CALL ',%vector-set/check! '#F <vector> <limit> <index> <value>)
;;  ;; See %vector-ref/check
;;  (make-operator/simple* "#[vector-set/check!]" '(UNSPECIFIC-RESULT)))


(define-operator %make-promise
  ;; (CALL ',%make-promise '#F <thunk>)
  ;; Note:
  ;;   Introduced by expand.scm for DELAY
  (make-operator/simple "#[make-promise]"))
(cookie-call %make-promise '#F thunk)

(define-operator %make-cell
  ;; (CALL ',%make-cell '#F <value> 'NAME)
  (make-operator/simple "#[make-cell]"))
(cookie-call %make-cell '#F value 'NAME)

(define-operator %cell-ref
  ;; (CALL ',%cell-ref '#F <cell> 'NAME)
  (make-operator/effect-sensitive "#[cell-ref]"))
(cookie-call %cell-ref '#F cell 'NAME)

(define-operator %cell-set!
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

(define-operator %make-multicell
  ;; (CALL ',%make-multicell '#F 'LAYOUT <value> <value> ...)
  (make-operator/simple "#[make-multicell]"))
;;(cookie-call %make-multicell '#F 'LAYOUT #!rest values)

(define-operator %multicell-ref
  ;; (CALL ',%multicell-ref '#F cell 'LAYOUT 'NAME)
  (make-operator/effect-sensitive "#[multicell-ref]"))
(cookie-call %multicell-ref '#F cell 'LAYOUT 'NAME)

(define-operator %multicell-set!
  ;; (CALL ',%multicell-set! '#F cell value 'LAYOUT 'NAME)
  ;; Note:
  ;;   Always used in statement position - has no value.
  (make-operator/simple* "#[multicell-set!]" '(UNSPECIFIC-RESULT)))
;;(cookie-call %multicell-set! '#F cell value 'LAYOUT 'NAME)

(define-operator %flo:make-multicell
  ;; (CALL ',%flo:make-multicell '#F 'LAYOUT <value> <value> ...)
  (make-operator/simple "#[flo:make-multicell]"))

(define-operator %flo:multicell-ref
  ;; (CALL ',%flo:multicell-ref '#F cell 'LAYOUT 'NAME)
  (make-operator/effect-sensitive "#[flo:multicell-ref]" '(RESULT-TYPE FLONUM)))
(cookie-call %flo:multicell-ref '#F cell 'LAYOUT 'NAME)

(define-operator %flo:multicell-set!
  ;; (CALL ',%flo:multicell-set! '#F cell value 'LAYOUT 'NAME)
  ;; Note:
  ;;   Always used in statement position - has no value.
  (make-operator/simple* "#[flo:multicell-set!]" '(UNSPECIFIC-RESULT)))


(define-operator %fixnum->flonum
  ;; (CALL ',%fixnum->flonum '#F fixnum)
  ;;  Convert a fixnum into a flonum.
  (make-operator/simple "#[fixnum->flonum]" '(RESULT-TYPE FLONUM)))

;; Tuples are collections of values.  Each slot is named.  LAYOUT
;; describes the arrangment of the slots in memory.  Currently it is
;; just a vector of names.  Tuples are immutable.  The intention is
;; that they can be introduced to collect values together without
;; committing to a representation.

(define-operator %make-tuple
  ;; (CALL ',%make-tuple '#F 'LAYOUT <value> <value> ...)
  (make-operator/simple "#[make-tuple]"))
;;(cookie-call %make-tuple '#F 'LAYOUT #!rest values)

(define-operator %tuple-ref
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

(define-operator %make-heap-closure
  ;; (CALL ',%make-heap-closure '#F <lambda-expression> 'VECTOR
  ;;       <value>*)
  ;; Note:
  ;;   Introduced by closconv.scm (first time it is invoked).
  ;;   VECTOR is a vector of symbols whose length is the same as the
  ;;     number of <value> expressions.
  (make-operator/simple "#[make-heap-closure]"))

(cookie-call %make-heap-closure '#F lambda-expression 'VECTOR #!rest values)

(define-operator %heap-closure-ref
  ;; (CALL ',%heap-closure-ref '#F <closure> <offset> 'NAME)
  ;; Note:
  ;;   Introduced by closconv.scm (first time it is invoked)
  (make-operator/simple "#[heap-closure-ref]"))
(cookie-call %heap-closure-ref '#F closure offset 'NAME)

(define-operator %heap-closure-set!
  ;; (CALL ',%heap-closure-set! '#F <closure> <offset> <value> 'NAME)
  (make-operator/simple* "#[heap-closure-set!]" '(UNSPECIFIC-RESULT)))
(cookie-call %heap-closure-set! '#F closure offset value 'NAME)

(define-operator %make-trivial-closure
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

(define-operator %make-static-binding
  ;; (CALL ',%make-static-binding '#F <value> 'NAME)
  ;; Note:
  ;;   Generate a static binding cell for NAME, containing <value>.
  ;;   Introduced by staticfy.scm (not currently working).
  (make-operator/simple "#[make-static-binding]"))
(cookie-call %make-static-binding '#F value 'NAME)

(define-operator %static-binding-ref
  ;; (CALL ',%static-binding-ref '#F <static-cell> 'NAME)
  ;; Note:
  ;;   Introduced by staticfy.scm (not currently working).
  (make-operator/simple "#[static-binding-ref]"))
(cookie-call %static-binding-ref '#F static-cell 'NAME)

(define-operator %static-binding-set!
  ;; (CALL ',%static-binding-set! '#F <static-cell> <value> 'NAME)
  ;; Note:
  ;;   Introduced by staticfy.scm (not currently working).
  (make-operator/simple* "#[static-binding-set!]" '(UNSPECIFIC-RESULT)))
(cookie-call %static-binding-set! '#F static-cell value 'NAME)

(define-operator %make-return-address
  ;; (CALL ',%make-return-address '#F <lambda-expression>)
  ;; Note:
  ;;   Used internally in rtlgen.scm when performing trivial rewrites
  ;;     before calling itself recursively.
  (make-operator/simple "#[make-return-address]"))
(cookie-call %make-return-address '#F lambda-expression)

;; %fetch-continuation is not static, but things get confused otherwise
;; It is handled specially by lamlift and closconv

(define-operator %fetch-continuation
  ;; (CALL ',%fetch-continuation '#F)
  ;; Note:
  ;;   Grab return address, for use in top-level expressions since they
  ;;     (unlike procedures) do not receive a continuation.
  ;;   Introduced by cpsconv.scm.
  (make-operator/simple* "#[fetch-continuation]" '(STATIC)))
(cookie-call %fetch-continuation '#F)

(define-operator %invoke-continuation
  ;; (CALL ',%invoke-continuation <continuation> <value>*)
  ;; Note:
  ;;   Introduced by cpsconv.scm
  (make-operator "#[invoke-continuation]"))
(cookie-call %invoke-continuation cont #!rest values)

(define-operator %fetch-stack-closure
  ;; (CALL ',%fetch-stack-closure '#F 'VECTOR)
  ;; Note:
  ;;   VECTOR contains symbols only.
  ;;   This is supposed to return a pointer to the current top of
  ;;     stack, which contains values (or cells for values) of the
  ;;     variables named in VECTOR.  In fact, rtlgen.scm knows about
  ;;     this special case and generates no output code.
  (make-operator/simple* "#[fetch-stack-closure]"))
(cookie-call %fetch-stack-closure '#F 'VECTOR)

(define-operator %make-stack-closure
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

(define-operator %stack-closure-ref
  ;; (CALL ',%stack-closure-ref '#F <closure> <offset> 'NAME)
  ;; Note:
  ;;   Introduced by closconv.scm.
  ;;   Handled specially by rtlgen.scm
  (make-operator/simple "#[stack-closure-ref]"))
(cookie-call %stack-closure-ref '#F closure offset 'NAME)

(define-operator %small-fixnum?
  ;; (CALL ',%small-fixnum? '#F <value> 'FIXNUM)
  ;; Note:
  ;;  #T iff <value> is a fixnum on the target machine and all of top
  ;;    FIXNUM+1 bits are the same (i.e. the top FIXNUM precision bits
  ;;    match the sign bit, i.e. it can be represented in FIXNUM fewer
  ;;    bits than a full fixnum on the target machine).  This is used
  ;;    in the expansion of generic arithmetic to guarantee no
  ;;    overflow is possible on the target machine.
  ;;  If FIXNUM is 0, then this is the same as FIXNUM? on the target
  ;;  machine.
  (make-operator/simple "#[small-fixnum?]" '(PROPER-PREDICATE)))

(cookie-call %small-fixnum? '#F value 'precision-bits)

(define-operator %word-less-than-unsigned?
  ;; (CALL ', %word-less-than-unsigned? '#F <smaller> <larger>
  (make-operator/simple "#[word-less-than-unsigned?]" '(PROPER-PREDICATE)))

(define-operator %compiled-entry?
  (make-operator/simple "#[compiled-entry?]"  '(PROPER-PREDICATE)))

(cookie-call %compiled-entry? '#F object)

(define-operator %compiled-entry-maximum-arity?
  ;; (call ',%compiled-entry-maximum-arity? '#F 'count value)
  ;;  Tests if the compiled entry has the specified maximum arity.
  (make-operator/simple "#[compiled-entry-maximum-arity?]"
			'(PROPER-PREDICATE)))

(cookie-call %compiled-entry-maximum-arity? '#F 'n entry)

(define-operator %profile-data
  ;; (CALL ',%profile-data '#F '<data>)
  (make-operator/simple* "#[profile-data]" '(UNSPECIFIC-RESULT)))

(cookie-call %profile-data '#F 'data)


;; The following operations are used as:
;; (CALL ',<operator> <continuation or #F> <value1> <value2>)
;; Note:
;;   If the continuation is #F then the code generator is responsible
;;     for creating a return address and preserving all necessary
;;     state (registers) needed upon return.  Otherwise there is no
;;     need to save state, but the operation should tail call into the
;;     continuation.

(define-operator %+ (make-operator/out-of-line "#[+]"))
(define-operator %- (make-operator/out-of-line "#[-]"))
(define-operator %* (make-operator/out-of-line "#[*]"))
(define-operator %/ (make-operator/out-of-line "#[/]"))
(define-operator %quotient (make-operator/out-of-line "#[quotient]"))
(define-operator %remainder (make-operator/out-of-line "#[remainder]"))
(define-operator %= (make-operator/out-of-line "#[=]" '(PROPER-PREDICATE)))
(define-operator %< (make-operator/out-of-line "#[<]" '(PROPER-PREDICATE)))
(define-operator %> (make-operator/out-of-line "#[>]" '(PROPER-PREDICATE)))

(define *vector-cons-max-open-coded-length* 5)

(define-operator %vector-cons
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

(define-operator %string-allocate
  ;; (CALL ',%string-allocate <continuation or #F> <length>)
  ;; Note:
  ;;   If the continuation is #F then the code generator is responsible
  ;;     for creating a return address and preserving all necessary
  ;;     state (registers) needed upon return.  Otherwise there is no
  ;;     need to save state, but the operation should tail call into the
  ;;     continuation.
  (make-operator/out-of-line "#[string-allocate]"))

(define-operator %floating-vector-cons
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
	boolean? cell? char? pair? string? bit-string?
	vector? %record?
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
       (make-primitive-procedure 'VECTOR-CONS)
       system-pair-cons
       ;;%record-length
       ;;vector-length
       ;;flo:vector-length
       object-type object-datum
       (make-primitive-procedure 'PRIMITIVE-OBJECT-TYPE)
       ;;bit-string-length
       (make-primitive-procedure 'PRIMITIVE-OBJECT-SET-TYPE)
       fix:-1+ fix:1+ fix:+ fix:- fix:*
       fix:quotient fix:remainder ; fix:gcd
       fix:andc fix:and fix:or fix:xor fix:not fix:lsh
       flo:floor->exact flo:ceiling->exact
       flo:truncate->exact flo:round->exact
       ascii->char integer->char char->ascii char-code char->integer))

(for-each
 (lambda (simple-operator)
   (define-operator-properties
     simple-operator
     (list '(SIMPLE)
	   '(SIDE-EFFECT-INSENSITIVE)
	   '(SIDE-EFFECT-FREE)
	   '(RESULT-TYPE FLONUM))))
 (list flo:+ flo:- flo:* flo:/
       flo:negate flo:abs flo:sqrt
       flo:floor flo:ceiling flo:truncate flo:round
       flo:exp flo:log flo:sin flo:cos flo:tan flo:asin
       flo:acos flo:atan flo:atan2 flo:expt
       (make-primitive-procedure 'FLONUM-NORMALIZE)
       (make-primitive-procedure 'FLONUM-DENORMALIZE)))

(for-each
 (lambda (simple-operator)
   (define-operator-properties
     simple-operator
     (list '(SIMPLE)
	   '(SIDE-EFFECT-FREE))))
 (list cell-contents
       ;;car cdr %record-ref
       ;;vector-ref
       ;;string-ref
       ;;string-length vector-8b-ref
       system-pair-car system-pair-cdr
       system-hunk3-cxr0 system-hunk3-cxr1 system-hunk3-cxr2
       system-vector-ref
       (make-primitive-procedure 'SYSTEM-VECTOR-SIZE)
       (make-primitive-procedure 'GET-INTERRUPT-ENABLES)
       (make-primitive-procedure 'PRIMITIVE-GET-FREE)
       (make-primitive-procedure 'PRIMITIVE-OBJECT-REF)))


(define-operator-properties (make-primitive-procedure 'SET-INTERRUPT-ENABLES!)
  (list '(OUT-OF-LINE-HOOK)))

;;(for-each
;; (lambda (simple-operator)
;;   (define-operator-properties
;;     simple-operator
;;     (list ;;'(SIMPLE)
;;	   '(SIDE-EFFECT-FREE)
;;	   '(RESULT-TYPE FLONUM))))
;; (list flo:vector-ref))

(for-each
 (lambda (operator)
   (define-operator-properties
     operator
     (list '(SIMPLE) '(UNSPECIFIC-RESULT))))
 (list set-cell-contents!
       set-string-length!
       ;;set-car! set-cdr! %record-set!
       ;;vector-set!
       ;;string-set! vector-8b-set! flo:vector-set!
       system-pair-set-car! system-pair-set-cdr!
       system-hunk3-set-cxr0! system-hunk3-set-cxr1! system-hunk3-set-cxr2!
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
