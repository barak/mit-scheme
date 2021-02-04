#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Code generation library
;;; package: (runtime cgen)

;;; This is a code-generation library for functional code, complete
;;; with peephole optimization and common-subexpression elimination.
;;; The optimizer and CSE assume the absence of side effects of any
;;; kind, which makes them relatively simple.
;;;
;;; Note that the CSE is not particularly simple, but adding side
;;; effects would make it horrendous.
;;;
;;; The library can be used by a syntax transformer, and in particular
;;; can use the effective renaming procedures to guarantee hygiene.
;;; It was developed to support the implementation of syntax-rules,
;;; which is a syntax transformer that generates another syntax
;;; transformer.  With this library, the code generator can do really
;;; dumb code generation, for example writing large expressions to
;;; access parts of the input form, and relying on the library to
;;; optimize the generated code.
;;;
;;; Both the peephole optimizer and the output optimizer are
;;; term-rewriting systems, to simplify adding new optimizations.

(declare (usual-integrations))

(add-boot-deps! '(runtime dynamic)
		'(runtime compound-predicate)
		'(runtime trie))

(define (cgen:new-name base)
  (string->uninterned-symbol (string base "-" (name-count base))))

(define (cgen:call operator . operands)
  `(call ,operator ,@operands))

(define (cgen:call* operator operands)
  `(call ,operator ,@operands))

(define (cgen:pcall procedure-name . operands)
  `(pcall ,procedure-name ,@operands))

(define (cgen:rename datum)
  `(rename ,datum))

(define (cgen:lambda bvl body)
  `(lambda ,bvl ,body))

(define (cgen:quote datum)
  `(quote ,datum))

(define (cgen:if predicate consequent alternative)
  `(if ,predicate ,consequent ,alternative))

(define (cgen:or . exprs)
  (cgen:or* exprs))

(define (cgen:or* exprs)
  (fold-right (lambda (expr acc)
                (cond ((eq? #f acc) expr)
                      ((eq? #f expr) acc)
                      (else
                       (let ((name (cgen:new-name 'or)))
                         (cgen:let `((,name ,expr))
                                   (cgen:if name name acc))))))
              '#f
              exprs))

(define (cgen:and . exprs)
  (cgen:and* exprs))

(define (cgen:and* exprs)
  (fold-right (lambda (expr acc)
                (cond ((eq? #t acc) expr)
                      ((eq? #t expr) acc)
                      (else (cgen:if expr acc '#f))))
              '#t
              exprs))

(define (cgen:let bindings body)
  (guarantee-list-of let-like-binding? bindings 'cgen:let)
  (cgen:call* (cgen:lambda (map car bindings) body)
              (map cadr bindings)))

(define (cgen:finish rename compare expr)
  (if (cgen:raw-output?)
      expr
      (cgen-expr->scheme rename compare
                         (cse-cgen-expr (optimize-cgen-expr expr)))))

(define-deferred cgen:raw-output?
  (make-settable-parameter #f))

(define (compound-expr? expr)
  (let ((sf (expr->specform expr)))
    (and sf
         (let loop
             ((fps (specform-fixed-predicates sf))
              (elts (cdr expr)))
           (if (pair? fps)
               (and (car elts)
                    ((car fps) (car elts))
                    (loop (cdr fps) (cdr elts)))
               (let ((rp (specform-rest-predicate sf)))
                 (if rp
                     (every rp elts)
                     (null? elts))))))))

(define-deferred simple-expr?
  (disjoin identifier? number? char? boolean? string?))

(define-deferred expr?
  (disjoin simple-expr? compound-expr?))

(define (expr-map proc expr #!optional kons)
  (let ((kons (if (default-object? kons) cons kons)))
    (let loop ((expr expr) (path '()))
      (proc (let ((sf (expr->specform expr)))
              (if sf
                  (kons (car expr)
                        (sf-map sf
                                (lambda (expr* index)
                                  (loop expr* (cons index path)))
                                expr))
                  (guarantee simple-expr? expr)))
            (reverse path)))))

(define (expr-for-each proc expr)
  (let loop ((expr expr) (path '()))
    (let ((sf (expr->specform expr)))
      (if sf
          (sf-for-each sf
                       (lambda (expr* index)
                         (loop expr* (cons index path)))
                       expr)
          (guarantee simple-expr? expr)))
    (proc expr (reverse path))))

(define (name-count base)
  (let ((counters (name-counters)))
    (if (not counters)
        (error "Can't generate name, not in cgen context:" base))
    (let ((p (assq base (cdr counters))))
      (if p
	  ((cdr p))
	  (let ((counter (make-range-generator 0)))
	    (set-cdr! counters
		      (cons (cons base counter)
			    (cdr counters)))
	    (counter))))))

(define (cgen:in-context thunk)
  (parameterize ((name-counters (list 'counters)))
    (thunk)))

(define-deferred name-counters
  (make-settable-parameter #f))

(define (let-like-binding? object)
  (and (pair? object)
       (identifier? (car object))
       (pair? (cdr object))
       (null? (cddr object))))
(register-predicate! let-like-binding? 'let-like-binding)

(define (let-body-path let-path)
  `(,@let-path 1 2))

(define (let-name-path let-path index)
  `(,@let-path 1 1 ,index))

(define (let-value-path let-path index)
  `(,@let-path ,(+ 2 index)))

;;;; Special forms

(define (expr->specform expr)
  (and (pair? expr)
       (list? (cdr expr))
       (find (lambda (sf)
               (eq? (car expr) (specform-name sf)))
             specforms)))

(define (sf-map sf proc expr)
  (let loop
      ((fps (specform-fixed-predicates sf))
       (elts (cdr expr))
       (index 1))
    (if (pair? fps)
        (begin
          (if (not (pair? elts))
              (error:not-a expr? expr))
          (cons (if (eqv? expr? (car fps))
                    (proc (car elts) index)
                    (guarantee (car fps) (car elts)))
                (loop (cdr fps) (cdr elts) (+ index 1))))
        (let ((rp (specform-rest-predicate sf)))
          (if rp
              (if (eqv? expr? rp)
                  (map proc elts (iota (length elts) index))
                  (begin
                    (if (not (every rp elts))
                        (error:not-a expr? expr))
                    elts))
              (begin
                (if (not (null? elts))
                    (error:not-a expr? expr))
                '()))))))

(define (sf-for-each sf proc expr)
  (let loop
      ((fps (specform-fixed-predicates sf))
       (elts (cdr expr))
       (index 1))
    (if (pair? fps)
        (begin
          (if (not (pair? elts))
              (error:not-a expr? expr))
          (if (eqv? expr? (car fps))
              (proc (car elts) index)
              (guarantee (car fps) (car elts)))
          (loop (cdr fps) (cdr elts) (+ index 1)))
        (let ((rp (specform-rest-predicate sf)))
          (if rp
              (if (eqv? expr? rp)
                  (for-each proc elts (iota (length elts) index))
                  (if (not (every rp elts))
                      (error:not-a expr? expr)))
              (if (not (null? elts))
                  (error:not-a expr? expr)))))))

(define-record-type <specform>
    (make-specform name fixed-predicates rest-predicate)
    specform?
  (name specform-name)
  (fixed-predicates specform-fixed-predicates)
  (rest-predicate specform-rest-predicate))

(define-deferred specforms
  (list (make-specform 'call (list expr?) expr?)
        (make-specform 'pcall (list symbol?) expr?)
        (make-specform 'rename (list identifier?) #f)
        (make-specform 'lambda (list r4rs-lambda-list? expr?) #f)
        (make-specform 'quote (list any-object?) #f)
        (make-specform 'if (list expr? expr? expr?) #f)))

;;;; Rule sets

(define (make-cgen-rule-set . defns)
  (let ((trie (make-trie equal?)))
    (for-each (lambda (defn) (defn trie)) defns)
    trie))

(define (define-cgen-rule pattern rewriter)
  (lambda (rule-trie)
    (trie-set! rule-trie pattern
	       (make-rule pattern
			  (make-simple-matcher pattern)
			  rewriter))))

(define-record-type <rule>
    (make-rule pattern matcher rewriter)
    rule?
  (pattern rule-pattern)
  (matcher rule-matcher)
  (rewriter rule-rewriter))

(define (cgen-rules-applier rule-set #!optional rw-args id=?)
  (let ((rw-args (if (default-object? rw-args) (lambda (r) r) rw-args))
        (id=? (if (default-object? id=?) eq? id=?)))
    (define (apply-rules expr)
      (cgen-trace rule-set 'enter-apply-rules expr)
      (let per-rule ((rules (get-applicable-rules expr rule-set)))
        (if (pair? rules)
            (let ((result
                   (apply-simple-matcher (rule-matcher (car rules))
                                         expr
                                         id=?)))
              (if result
                  (let ((expr*
                         (apply (rule-rewriter (car rules))
                                (rw-args result))))
                    (cgen-trace rule-set 'apply-rule
                                 (car rules) `(results ,@result) expr*)
                    (apply-rules expr*))
                  (per-rule (cdr rules))))
            (begin
              (cgen-trace rule-set 'exit-apply-rules expr)
              expr))))
    apply-rules))

(define (cgen-trace rule-set . args)
  (if (let ((trace? (trace-cgen-rules?)))
        (or (eq? #t trace?)
            (eq? rule-set trace?)))
      (pp args (notification-output-port))))

(define-deferred trace-cgen-rules?
  (make-settable-parameter #f))

(define (get-applicable-rules expr rule-set)
  (let ((prefix (discrimination-prefix expr)))
    (let ((trie (and prefix (find-subtrie rule-set prefix))))
      (if trie
          (trie-values trie)
          '()))))

(define (discrimination-prefix expr)
  (and (pair? expr)
       (cond ((eq? 'pcall (car expr)) (take expr 2))
             ((symbol? (car expr)) (take expr 1))
             (else '()))))