#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Rule-based Syntactic Expanders
;;; package: (runtime syntax syntax-rules)

;;; See "Syntactic Extensions in the Programming Language Lisp", by
;;; Eugene Kohlbecker, Ph.D. dissertation, Indiana University, 1986.
;;; See also "Macros That Work", by William Clinger and Jonathan Rees
;;; (reference? POPL?).  This implementation is derived from an
;;; implementation by Kent Dybvig, and includes some ideas from
;;; another implementation by Jonathan Rees.

;;; Implementation comments:

;;; Parsing of syntax-rules clauses is complex due to the interaction of quoting
;;; mechanisms, overriding of ellipses, and identifier comparison.  This is
;;; mitigated here by a rewriting phase that transforms them into an
;;; easily-walked form that consists of nested lists keyed by symbols.  The form
;;; memoizes identifiers so that they can be compared using eq?.

;;; The rewritten forms are then checked for various syntactic restrictions that
;;; are different for patterns and templates.  Patterns are checked for excess
;;; ellipses, but not templates where it is allowed.  Ellipsis depth is computed
;;; for the pattern variables, and then checked in the template to make sure
;;; that references have the correct depth and nesting relationships.

;;; One special exception is dotted-list patterns, where the RHS of the dotted
;;; list is an identifier.  In that case no ellipses are allowed in the LHS of
;;; the pattern, because otherwise matching would require backtracking and there
;;; would be more than one possible match.

;;; The generated code is then simple, deferring most of the work to the
;;; procedures syntax-rules:match-datum and syntax-rules:expand-template.  These
;;; names must be bound in the global environment since that's the only means we
;;; have for efficiently referencing them in the generated code.

(declare (usual-integrations))

(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (form rename compare)
     (let-values
	 (((ellipsis literals clauses)
	   (cond ((syntax-match? '((* identifier)
				   * ((identifier . datum) datum))
				 (cdr form))
		  (values (rename '...) (cadr form) (cddr form)))
		 ((syntax-match? '(identifier
				   (* identifier)
				   * ((identifier . datum) datum))
				 (cdr form))
		  (values (cadr form) (caddr form) (cdddr form)))
		 (else
		  (ill-formed-syntax form)))))
       (let ((underscore (rename '_))
	     (r-form (new-identifier 'form))
	     (r-rename (new-identifier 'rename))
	     (r-compare (new-identifier 'compare)))
	 `(,(rename 'er-macro-transformer)
	   (,(rename 'lambda)
	    (,r-form ,r-rename ,r-compare)
	    ,@(if (null? clauses)
		  `((,(rename 'declare) (ignore ,r-rename ,r-compare)))
		  '())
	    ,(let loop
		 ((clauses
		   (parse-clauses ellipsis literals clauses underscore
				  compare)))
	       (if (pair? clauses)
		   (let ((pattern (caar clauses))
			 (template (cadar clauses))
			 (r-dict (new-identifier 'dict)))
		     `(let ((,r-dict
			     (,(rename 'syntax-rules:match-datum)
			      ,(syntax-quote pattern)
			      (cdr ,r-form)
			     ,r-rename
			     ,r-compare)))
			(if ,r-dict
			    (,(rename 'syntax-rules:expand-template)
			     ,(syntax-quote template)
			     ,r-dict
			     ,r-rename)
			    ,(loop (cdr clauses)))))
		   `(,(rename 'ill-formed-syntax) ,r-form))))))))))

(define (parse-clauses ellipsis literals clauses underscore compare)
  (if (any-duplicates? literals compare)
      (syntax-error "Literals list contains duplicates:" literals))
  (let ((rewrite (make-rewriter ellipsis literals underscore compare)))
    (map (lambda (clause)
	   (parse-clause rewrite (car clause) (cadr clause)))
	 clauses)))

(define (parse-clause rewrite pattern template)
  (if (not (and (pair? pattern) (identifier? (car pattern))))
      (syntax-error "Pattern must start with identifier:" pattern))
  (let ((p (rewrite (cdr pattern)))
	(t (rewrite template)))
    (check-for-multiple-segments p pattern)
    (let ((pvs (compute-segments p))
	  (tvs (compute-segments t)))
      (if (any-duplicates? pvs eq? car)
	  (syntax-error "Duplicate vars in pattern:" pattern))
      (check-template-var-references tvs pvs))
    (list p t)))

(define (make-rewriter ellipsis literals underscore compare)

  (define (rewriter ellipsis literals)

    (define (rewrite x)
      (cond ((pair? x)
	     (if (and (ellipsis-id? (car x))
		      (pair? (cdr x))
		      (null? (cddr x)))
		 ((rewriter #f (cons ellipsis literals))
		  (cadr x))
		 (let-values (((x y) (scan-elts x '())))
		   (if (null? x)
		       (cons 'list (reverse y))
		       (cons 'dotted-list (reverse (cons (rewrite x) y)))))))
	    ((vector? x)
	     (let-values (((x y) (scan-elts (vector->list x) '())))
	       (declare (ignore x))
	       (cons 'vector (reverse y))))
	    ((identifier? x)
	     (cond ((member x literals compare)
		    (list 'literal (strip-syntactic-closures x)))
		   ((compare underscore x)
		    (list 'anon-var))
		   (else
		    (if (ellipsis? x)
			(syntax-error "Misplaced ellipsis"))
		    (list 'var (memoize x)))))
	    ((null? x)
	     (list 'list))
	    (else
	     (if (not (or (string? x) (char? x) (boolean? x) (number? x)
			  (lambda-tag? x)))
		 (syntax-error "Ill-formed pattern:" x))
	     (list 'literal x))))

    (define (scan-elts x y)
      (if (pair? x)
	  (let loop ((t (cdr x)) (w (rewrite (car x))))
	    (if (and (pair? t)
		     (ellipsis-id? (car t)))
		(loop (cdr t) (list '* w))
		(scan-elts t (cons w y))))
	  (values x y)))

    (define (ellipsis? id)
      (and ellipsis (compare id ellipsis)))

    (define (ellipsis-id? object)
      (and (identifier? object)
	   (ellipsis? object)))

    rewrite)

  (define memoize
    (let ((ids '()))
      (lambda (id)
	(let ((p (member id ids compare)))
	  (if p
	      (car p)
	      (begin
		(set! ids (cons id ids))
		id))))))

  (rewriter (if (member ellipsis literals compare) #f ellipsis)
	    literals))

(define (check-for-multiple-segments p pattern)
  (let loop ((p p))
    (case (car p)
      ((list dotted-list vector)
       (if (fix:> (count segment? (cdr p)) 1)
	   (syntax-error "Only one ellipsis allowed in pattern:" pattern))
       (if (any (lambda (elt) (fix:> (count-segments elt) 1)) (cdr p))
	   (syntax-error "No nested ellipses allowed in pattern:" pattern))
       (for-each (lambda (elt)
		   (loop (strip-segments elt)))
		 (cdr p)))
      ((var literal anon-var) unspecific)
      (else (error "Unknown element marker:" p)))))

(define (compute-segments y)
  (reverse
   (let loop ((y y) (segs '()) (vars '()))
     (case (car y)
       ((list dotted-list vector)
	(fold (lambda (elt vars)
		(loop (strip-segments elt)
		      (append (make-list (count-segments elt) '*) segs)
		      vars))
	      vars
	      (cdr y)))
       ((var) (cons (cons (cadr y) segs) vars))
       ((literal anon-var) vars)
       (else (error "Unknown element marker:" y))))))

(define (check-template-var-references tvs pvs)
  (let ((vars
	 (remove (lambda (tv.s)
		   (let ((pv.s (assq (car tv.s) pvs)))
		     (or (not pv.s)
			 (fix:>= (length (cdr tv.s)) (length (cdr pv.s))))))
		 tvs)))
    (if (pair? vars)
	(syntax-error "Mismatched ellipsis depth in template:" vars)))
  (let ((table (make-hash-table eq-comparator)))
    (for-each
     (lambda (tv.s)
       (let* ((tv (car tv.s))
	      (pv.s (assq tv pvs)))
	 (if pv.s
	     (let ((ts (cdr tv.s))
		   (ps (cdr pv.s)))
	       (let loop
		   ((ts (drop ts (fix:- (length ts) (length ps))))
		    (ps ps))
		 (if (pair? ts)
		     (begin
		       (hash-table-update!/default table ts
			 (lambda (entry)
			   (let ((part (assq ps entry)))
			     (if part
				 (begin
				   (if (not (memq tv (cdr part)))
				       (set-cdr! part (cons tv (cdr part))))
				   entry)
				 (cons (list ps tv) entry))))
			 '())
		       (loop (cdr ts) (cdr ps)))))))))
     tvs)
    (let ((mismatches
	   (filter-map (lambda (value)
			 (and (pair? (cdr value))
			      (map cdr value)))
		       (hash-table-values table))))
      (if (pair? mismatches)
	  (syntax-error "Mismatched ellipses in template:" mismatches)))))

(define (syntax-rules:match-datum pattern datum rename compare)

  (define (match-datum pat datum dict k)

    (define (k-list pats data dict)
      (and (null? pats)
	   (null? data)
	   (k dict)))

    (let ((x (cdr pat)))
      (case (car pat)
	((list)
	 (and (list? datum)
	      (match-segment x datum dict (length x) (length datum) k-list)))
	((vector)
	 (and (vector? datum)
	      (match-segment x (vector->list datum) dict (length x)
			     (vector-length datum) k-list)))
	((dotted-list)
	 (match-segment x datum dict (fix:- (length x) 1) (count-pairs datum)
	   (lambda (pats datum dict)
	     (match-datum (car pats) datum dict k))))
	((literal)
	 (and (let ((literal (car x)))
		(if (identifier? literal)
		    (and (identifier? datum)
			 (compare (rename literal) datum))
		    (equal? literal datum)))
	      (k dict)))
	((var) (k (dict-add (car x) datum dict)))
	((anon-var) (k dict))
	(else (error "Unknown element marker:" pat)))))

  (define (match-segment pats data dict n m k)
    (let ((i (list-index segment? pats)))
      (if (and i (fix:<= i m))
	  (fixed pats data dict i
	    (lambda (pats data dict)
	      (let ((n (fix:- (fix:- n i) 1))
		    (m (fix:- m i))
		    (pat (segment-body (car pats))))
		(let loop ((data data) (m m) (dicts '()))
		  (if (fix:< n m)
		      (match-datum pat (car data) (new-dict)
			(lambda (dict)
			  (loop (cdr data) (fix:- m 1) (cons dict dicts))))
		      (fixed (cdr pats) data (wrap-dicts dicts dict)
			     n k))))))
	  (and (fix:<= n m)
	       (fixed pats data dict n k)))))

  (define (fixed pats data dict n k)
    (if (fix:> n 0)
	(match-datum (car pats) (car data) dict
	  (lambda (dict)
	    (fixed (cdr pats) (cdr data) dict (fix:- n 1) k)))
	(k pats data dict)))

  (match-datum pattern datum (new-dict) (lambda (dict) dict)))

(define (syntax-rules:expand-template template dict rename)

  (define (loop t dict)

    (define (per-elt elt)
      (let expand-segment ((elt elt) (dict dict))
	(if (segment? elt)
	    (append-map (lambda (dict)
			  (expand-segment (segment-body elt) dict))
			(unwrap-dict dict (segment-vars elt)))
	    (list (loop elt dict)))))

    (case (car t)
      ((list) (append-map per-elt (cdr t)))
      ((vector) (list->vector (append-map per-elt (cdr t))))
      ((dotted-list)
       (let ((n (fix:- (length (cdr t)) 1)))
	 (let scan ((i 0) (elts (cdr t)))
	   (if (fix:< i n)
	       (append (per-elt (car elts)) (scan (fix:+ i 1) (cdr elts)))
	       (loop (car elts) dict)))))
      ((var)
       (let ((datum (dict-lookup (cadr t) dict)))
	 (if (eq? datum no-datum)
	     (rename (cadr t))
	     datum)))
      ((literal) (cadr t))
      ((anon-var) (rename '_))
      (else (error "Unknown element marker:" t))))

  (add-segment-vars! template)
  (loop template dict))

(define (add-segment-vars! t)
  (let loop ((t t) (ids '()))
    (case (car t)
      ((list dotted-list vector)
       (fold (lambda (elt ids)
	       (if (segment? elt)
		   (let ((ids* (loop (strip-segments (segment-body elt)) '())))
		     (do ((elt elt (segment-body elt)))
			 ((not (segment? elt)))
		       (set-cdr! (cdr elt) (list ids*)))
		     (lset-union eq? ids* ids))
		   (loop elt ids)))
	     ids
	     (cdr t)))
      ((var) (lset-adjoin eq? ids (cadr t)))
      ((literal anon-var) ids)
      (else (error "Unknown element marker:" t)))))

(define-integrable (segment? elt) (eq? '* (car elt)))
(define-integrable (segment-body elt) (cadr elt))
(define-integrable (segment-vars elt) (caddr elt))

(define (strip-segments elt)
  (if (segment? elt)
      (strip-segments (segment-body elt))
      elt))

(define (count-segments elt)
  (let loop ((elt elt) (n 0))
    (if (segment? elt)
	(loop (segment-body elt) (fix:+ n 1))
	n)))

;; Like quote but doesn't strip syntactic closures:
(define (syntax-quote expression)
  `(,(classifier->keyword
      (lambda (form senv hist)
	(scheck '(_ datum) form senv hist)
	(constant-item (serror-ctx form senv hist) (cadr form))))
    ,expression))

(define-integrable (new-dict) (make-dict '()))
(define-integrable (make-dict bindings) bindings)
(define-integrable (dict-add id datum dict)
  (cons (make-binding id datum 0) dict))
(define-integrable (dict-bindings dict) dict)
(define (dict-ids dict) (map binding-id (dict-bindings dict)))
(define-integrable (make-binding id datum depth) (list id datum depth))
(define-integrable (binding-id binding) (car binding))
(define-integrable (binding-datum binding) (cadr binding))
(define-integrable (binding-depth binding) (caddr binding))
(define no-datum (list 'no-datum))

(define (dict-lookup id dict)
  (let ((binding (assq id (dict-bindings dict))))
    (if binding
	(binding-datum binding)
	no-datum)))

(define (wrap-dicts dicts tail)
  (make-dict
   (map* tail
	 (lambda (id)
	  (let ((matches
		 (map (lambda (dict)
			(assq id (dict-bindings dict)))
		      dicts)))
	    (make-binding id
			  (reverse (map (lambda (match)
					  (if match (binding-datum match) '()))
					matches))
			  (fix:+ (binding-depth
				  (find (lambda (match) match) matches))
				 1))))
	(apply lset-union eq? (map dict-ids dicts)))))

(define (unwrap-dict dict ids)
  (let-values (((seg non-seg)
		(partition (lambda (binding)
			     (fix:> (binding-depth binding) 0))
			   (filter (lambda (binding)
				     (memq (binding-id binding) ids))
				   (dict-bindings dict)))))
    (let loop
	((items
	  (map (lambda (binding)
		 (let ((id (binding-id binding))
		       (depth (fix:- (binding-depth binding) 1)))
		   (map (lambda (datum)
			  (make-binding id datum depth))
			(binding-datum binding))))
	       seg)))
      (if (and (pair? items) (pair? (car items)))
	  (cons (make-dict (append non-seg (map car items)))
		(loop (map cdr items)))
	  '()))))