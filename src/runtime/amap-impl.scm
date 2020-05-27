#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Associative-map implementation
;;; package: (runtime amap impl)

(declare (usual-integrations))

(add-boot-deps! '(runtime comparator))

(define (define-amap-implementation name props comparator-pred operations)
  (register-abstract-impl!
   (make-amap-implementation name props comparator-pred operations)))

(define (make-amap-implementation name props comparator-pred operations)
  (let ((caller 'make-amap-implementation))
    (guarantee amap-operations? operations caller)
    (let ((metadata (make-metadata name props comparator-pred caller)))
      (make-abstract-impl metadata
	(trivial-selector
	 (%make-amap-impl metadata
			  (organize-operations metadata operations)))))))

(define (define-amap-implementation-selector name props comparator-pred select)
  (let ((caller 'define-amap-implementation-selector))
    (guarantee binary-procedure? select caller)
    (register-abstract-impl!
     (make-abstract-impl (make-metadata name props comparator-pred caller)
			 select))))

(define (amap-implementation-names)
  (alist-table-keys registered-implementations))

(define (all-amap-args)
  (append-map prop-claimed-args (alist-table-values properties)))

(define (amap-implementation-supported-args name)
  (impl-supported-args (get-impl name 'amap-implementation-supported-args)))

(define (amap-implementation-supports-args? name args)
  (impl-supports-args? (get-impl name 'amap-implementation-supports-args?)
		       args))

(define (amap-implementation-supports-comparator? name comparator)
  (impl-supports-comparator?
   (get-impl name 'amap-implementation-supports-comparator?)
   comparator))

(define (select-impl comparator args)

  (define (supported-comparator? impl)
    (impl-supports-comparator? impl comparator))

  (define (supported-args? impl)
    (impl-supports-args? impl args))

  (define (finish impl)
    (values (deref-abstract-impls impl comparator args)
	    (impl-name impl)))

  (let-values (((args names)
		(lset-diff+intersection eqv? args
					(amap-implementation-names))))
    (if (pair? names)
	;; Select by implementation name.
	(let ((name (car names)))
	  (if (pair? (cdr names))
	      (error "Can't specify multiple implementations:" names))
	  (let ((impl (alist-table-ref registered-implementations name)))
	    (if (not (supported-comparator? impl))
		(error "Implementation doesn't support this comparator:"
		       name comparator))
	    (if (not (supported-args? impl))
		(error "Implementation doesn't support these args:"
		       name args))
	    (finish impl)))
	;; Select by matching.
	(let ((impl
	       (find (lambda (impl)
		       (and (supported-comparator? impl)
			    (supported-args? impl)))
		     (alist-table-values registered-implementations))))
	  (if (not impl)
	      (error "Unable to find matching implementation:" comparator args))
	  (finish impl)))))

;;;; Abstract implementations

(define-record-type <abstract-impl>
    (make-abstract-impl metadata selector)
    abstract-impl?
  (metadata abstract-impl-metadata)
  (selector abstract-impl-selector))

(define (trivial-selector impl)
  (lambda (comparator args)
    (declare (ignore comparator args))
    impl))

(define (deref-abstract-impls impl comparator args)
  (let loop ((impl impl))
    (if (abstract-impl? impl)
	(loop ((abstract-impl-selector impl) comparator args))
	(begin
	  (guarantee amap-impl? impl 'deref-abstract-impls)
	  impl))))

(define (impl-name impl)
  (metadata-name (abstract-impl-metadata impl)))

(define (impl-supported-args impl)
  (metadata-supported-args (abstract-impl-metadata impl)))

(define (impl-supports-args? impl args)
  ((metadata-args-predicate (abstract-impl-metadata impl)) args))

(define (impl-supports-comparator? impl comparator)
  ((metadata-comparator-predicate (abstract-impl-metadata impl)) comparator))

(define (register-abstract-impl! impl)
  (alist-table-set! registered-implementations
		    (metadata-name (abstract-impl-metadata impl))
		    impl))

(define (get-impl name caller)
  (alist-table-ref registered-implementations name
		   (lambda () (error:bad-range-argument name caller))))

(define registered-implementations
  (alist-table eq?))

;;;; Metadata

(define (make-metadata name specs comparator-predicate caller)
  (guarantee interned-symbol? name caller)
  (guarantee amap-properties? specs caller)
  (if comparator-predicate
      (guarantee unary-procedure? comparator-predicate caller))
  (let ((specs
	 (map (lambda (spec)
		(let ((prop (get-prop (car spec)))
		      (vals (delete-duplicates (cdr spec) equal?)))
		  (if (not (and (lset<= equal? vals (prop-vals prop))
				((prop-vals-predicate prop) vals)))
		      (error "Ill-formed property:" spec))
		  (cons prop vals)))
	      specs)))

    (define (spec-vals prop)
      (let ((spec (assv prop specs)))
	(if spec
	    (cdr spec)
	    '())))

    (let ((missing
	   (remove (lambda (prop)
		     (assq prop specs))
		   (required-props))))
      (if (pair? missing)
	  (error "Missing required properties:" missing)))
    (let ((prop-argsets
	   (map (lambda (prop)
		  ((prop-impl-supported-argsets prop) (spec-vals prop)))
		(all-props))))
      (%make-metadata
       name
       (encode-mutability (cdr (assq (get-prop 'mutability) specs)))
       (append-map argsets->supported-args prop-argsets)
       (combine-args-predicates
	(map impl-args-predicate
	     (all-props)
	     prop-argsets))
       (or comparator-predicate any-object?)))))

(define (amap-properties? object)
  (list-of-type? object
    (lambda (elt)
      (and (pair? elt)
	   (have-prop? (car elt))
	   (non-empty-list? (cdr elt))))))
(register-predicate! amap-properties? 'amap-properties '<= list?)

(define (combine-args-predicates preds)
  (lambda (args)
    (let loop ((preds preds) (unused-args args))
      (if (pair? preds)
	  (let ((used-args ((car preds) unused-args)))
	    (and used-args
		 (loop (cdr preds)
		       (lset-difference eqv? unused-args used-args))))
	  unused-args))))

(define-record-type <metadata>
    (%make-metadata name mutability supported-args args-predicate
		    comparator-predicate)
    metadata?
  (name metadata-name)
  (mutability metadata-mutability)
  (supported-args metadata-supported-args)
  (args-predicate metadata-args-predicate)
  (comparator-predicate metadata-comparator-predicate))

;;;; Properties

(define (define-prop keyword required? alist vals-predicate arg-lists->argsets)
  (alist-table-set! properties keyword
    (make-prop keyword required? alist vals-predicate arg-lists->argsets)))

(define (make-prop keyword required? alist vals-predicate arg-lists->argsets)
  (let ((claimed-args (apply lset-union equal? (map cdr alist))))
    (%make-prop keyword
		required?
		(map car alist)
		(or vals-predicate any-object?)
		claimed-args
		(lambda (impl-vals)
		  (let ((arg-lists
			 (map (lambda (val)
				(cdr (assoc val alist)))
			      impl-vals)))
		    (if arg-lists->argsets
			(arg-lists->argsets arg-lists)
			arg-lists)))
		(disjoin* (args->predicates claimed-args)))))

(define-record-type <prop>
    (%make-prop keyword required? vals vals-predicate claimed-args
		impl-supported-argsets args-filter)
    prop?
  (keyword prop-keyword)
  (required? prop-required?)
  (vals prop-vals)
  (vals-predicate prop-vals-predicate)
  (claimed-args prop-claimed-args)
  (impl-supported-argsets prop-impl-supported-argsets)
  (args-filter prop-args-filter))

(define (argsets->supported-args argsets)
  (apply lset-union equal? argsets))

(define (impl-args-predicate prop argsets)
  (let ((args-filter (prop-args-filter prop))
	(pred
	 (disjoin*
	  (map (lambda (argset)
		 (let ((preds (args->predicates argset)))
		   (lambda (args)
		     (lset= (lambda (pred arg)
			      (pred arg))
			    preds
			    args))))
	       argsets))))
    (lambda (args)
      (let ((args-to-match (filter args-filter args)))
	(and (pred args-to-match)
	     args-to-match)))))

(define (args->predicates args)
  (map (lambda (arg)
	 (if (procedure? arg)
	     arg
	     (lambda (arg*) (equal? arg arg*))))
       args))

(define (have-prop? keyword)
  (alist-table-contains? properties keyword))

(define (get-prop keyword)
  (alist-table-ref properties keyword))

(define (required-props)
  (filter prop-required? (all-props)))

(define (all-props)
  (alist-table-values properties))

(define properties
  (alist-table eq?))

(add-boot-init!
 (lambda ()

   (define-prop 'mutability #t
     '((mutable) (immutable))
     #f
     #f)

   (define-prop 'kv-types #t
     '(((strong strong))
       ((weak strong) weak-keys)
       ((strong weak) weak-values)
       ((weak weak) weak-keys weak-values)
       ((ephemeral strong) ephemeral-keys)
       ((strong ephemeral) ephemeral-values)
       ((ephemeral ephemeral) ephemeral-keys ephemeral-values))
     #f
     #f)

   (define-prop 'time-complexity #f
     '((amortized-constant amortized-constant-time sublinear-time)
       (log log-time sublinear-time)
       (linear linear-time))
     (lambda (impl-vals)
       (or (null? impl-vals)
	   (null? (cdr impl-vals))))
     (lambda (arg-lists)
       (cons '()
	     (append-map (lambda (args)
			   (map list args))
			 arg-lists))))

   (define-prop 'other #f
     `((thread-safe thread-safe)
       (initial-size ,exact-nonnegative-integer?)
       (ordered-by-key ordered-by-key))
     #f
     (lambda (arg-lists)
       (every-combination-of (map car arg-lists))))))

(define (encode-mutability vals)
  (if (null? (cdr vals))
      (eq? 'mutable (car vals))
      'both))

(define (every-combination-of items)

  (define (choose k items)
    (if (> k 0)
	(if (< k (length items))
	    (append (map (lambda (tail)
			   (cons (car items) tail))
			 (choose (- k 1) (cdr items)))
		    (choose k (cdr items)))
	    (list items))
	'(())))

  (let ((n (length items)))
    (let loop ((k 0))
      (if (< k n)
	  (append (choose k items)
		  (loop (+ k 1)))
	  (choose n items)))))

;;;; Operations

(define (amap-operations? object)
  (and (list-of-type? object
	 (lambda (elt)
	   (and (pair? elt)
		(operator? (car elt))
		(pair? (cdr elt))
		(procedure? (cadr elt))
		(null? (cddr elt)))))
       (not (any-duplicates? object eq? car))))
(register-predicate! amap-operations? 'amap-operations '<= alist?)

(define (organize-operations metadata operations)
  (let ((missing-operators (missing-inputs metadata operations)))
    (if (pair? missing-operators)
	(error "Missing required amap operators:" missing-operators)))
  (let ((table (alist-table eq?)))

    (define (evolve ops)
      (let-values (((ready unready)
		    (partition (lambda (op)
				 (every (lambda (dep)
					  (alist-table-contains? table dep))
					(defop-deps op)))
			       ops)))
	(if (pair? ready)
	    (begin
	      (for-each (lambda (op)
			  (alist-table-set! table (defop-operator op)
			    (apply (defop-procedure op)
				   (map (lambda (dep)
					  (alist-table-ref table dep))
					(defop-deps op)))))
			ready)
	      (evolve unready))
	    (if (pair? unready)
		(error "Dependency loop in default operations:" unready)))))

    (for-each (lambda (op)
		(alist-table-set! table (car op) (cadr op)))
	      operations)
    (if (not (alist-table-contains? table 'clean!))
	(alist-table-set! table 'clean! default:clean!))
    (if (not (alist-table-contains? table 'mutable?))
	(alist-table-set! table 'mutable?
			  (default:mutable? (metadata-mutability metadata))))

    (evolve
     (map default-operation
	  (lset-difference eq?
			   (required-outputs metadata)
			   (alist-table-keys table))))
    (lambda (operator)
      (alist-table-ref table
		       operator
		       (lambda () (error-operation operator))))))

(define (missing-inputs metadata operations)

  (define (remove-provided operators)
    (remove (lambda (operator)
	      (assq operator operations))
	    operators))

  (let ((mutability (metadata-mutability metadata))
	(weakish?
	 (any (lambda (arg)
		(memq arg
		      '(weak-keys weak-values ephemeral-keys ephemeral-values)))
	      (metadata-supported-args metadata))))
    (remove-provided
     (fold (lambda (operator acc)
	     (lset-union eq? (operator-deps operator) acc))
	   '()
	   (remove-provided
	    (filter (lambda (operator)
		      (case operator
			((clean!) (and mutability weakish?))
			((mutable?) (eq? 'both mutability))
			(else
			 (or mutability
			     (not (operator-mutates? operator))))))
		    (all-operators)))))))

(define (required-outputs metadata)
  (if (metadata-mutability metadata)
      (all-operators)
      (remove operator-mutates? (all-operators))))

(define (error-operation operator)
  (lambda args
    (error "Unimplemented amap operation:" (cons operator args))))

;;;; Default operations

(define (operator-deps operator)
  (let loop ((operator operator) (deps '()))
    (cond ((memq operator deps) deps)
	  ((alist-table-ref default-operations operator (lambda () #f))
	   => (lambda (defop)
		(fold loop deps (defop-deps defop))))
	  (else (cons operator deps)))))

(define (define-default-operation operator deps procedure)
  (alist-table-set! default-operations
		    operator
		    (make-defop operator deps procedure)))

(define (default-operation operator)
  (alist-table-ref default-operations operator))

(define-record-type <defop>
    (make-defop operator deps procedure)
    defop?
  (operator defop-operator)
  (deps defop-deps)
  (procedure defop-procedure))

(define default-operations
  (alist-table eq?))

;;; Special cases:

(define (default:mutable? mutable?)
  (lambda (state)
    (declare (ignore state))
    mutable?))

(define (default:clean! state)
  (declare (ignore state))
  unspecific)

(define-default-operation '->alist '(fold)
  (lambda (:fold)
    (lambda (state)
      (:fold (lambda (key value acc)
	       (cons (cons key value) acc))
	     '()
	     state))))

(define-default-operation 'contains? '(ref)
  (lambda (:ref)
    (lambda (state key)
      (:ref state
	    key
	    (lambda () #f)
	    (lambda (value) (declare (ignore value)) #t)))))

(define-default-operation 'copy '(for-each empty-copy set!)
  (lambda (:for-each :empty-copy :set!)
    (lambda (state mutable?)
      (declare (ignore mutable?))
      (let ((state* (:empty-copy state)))
	(:for-each (lambda (key value)
		     (:set! state* key value))
		   state)
	state*))))

(define-default-operation 'count '(fold)
  (lambda (:fold)
    (lambda (predicate state)
      (:fold (lambda (key value acc)
	       (if (predicate key value)
		   (+ acc 1)
		   acc))
	     0
	     state))))

(define-default-operation 'delete! '(delete-1!)
  (lambda (:delete-1!)
    (lambda (state . keys)
      (fold (lambda (key count)
	      (if (:delete-1! state key)
		  (+ count 1)
		  count))
	    0
	    keys))))

(define-default-operation 'difference! '(prune!)
  (lambda (:prune!)
    (lambda (state1 amap2)
      (:prune! (lambda (key value)
		 (declare (ignore value))
		 (amap-contains? amap2 key))
	       state1))))

(define-default-operation 'empty? '(size)
  (lambda (:size)
    (lambda (state)
      (= 0 (:size state)))))

(define-default-operation 'entries '(fold)
  (lambda (:fold)
    (lambda (state)
      (let ((p
	     (:fold (lambda (key value acc)
		       (cons (cons key (car acc))
			     (cons value (cdr acc))))
		     (cons '() '())
		     state)))
	(values (car p) (cdr p))))))

(define-default-operation 'for-each '(fold)
  (lambda (:fold)
    (lambda (procedure state)
      (:fold (lambda (key value acc)
	       (procedure key value)
	       acc)
	     unspecific
	     state))))

(define-default-operation 'intern! '(ref set!)
  (lambda (:ref :set!)
    (lambda (state key fail)
      (:ref state
	    key
	    (lambda ()
	      (let ((value (fail)))
		(:set! state key value)
		value))
	    (lambda (value) value)))))

(define-default-operation 'intersection! '(prune!)
  (lambda (:prune!)
    (lambda (state amap2)
      (:prune! (lambda (key value)
		 (declare (ignore value))
		 (not (amap-contains? amap2 key)))
	       state))))

(define-default-operation 'keys '(fold)
  (lambda (:fold)
    (lambda (state)
      (:fold (lambda (key value acc)
	       (declare (ignore value))
	       (cons key acc))
	     '()
	     state))))

(define-default-operation 'map '(new-state for-each set!)
  (lambda (:new-state :for-each :set!)
    (lambda (procedure comparator args state)
      (let ((state* (:new-state comparator args)))
	(:for-each (lambda (key value)
		     (:set! state* key (procedure value)))
		   state)
	state*))))

(define-default-operation 'map->list '(fold)
  (lambda (:fold)
    (lambda (procedure state)
      (:fold (lambda (key value acc)
	       (cons (procedure key value) acc))
	     '()
	     state))))

(define-default-operation 'pop! '(find delete!)
  (lambda (:find :delete!)
    (let ((fail (lambda () (error "Can't pop! an empty amap"))))
      (lambda (state)
	(let ((p (:find cons fail)))
	  (:delete! state (car p))
	  (values (car p) (cdr p)))))))

(define-default-operation 'ref/default '(ref)
  (lambda (:ref)
    (lambda (state key default)
      (:ref state key (lambda () default)))))

(define-default-operation 'prune! '(fold delete!)
  (lambda (:fold :delete!)
    (lambda (predicate state)
      (for-each (lambda (key)
		  (:delete! state key))
		(:fold (lambda (key datum acc)
			 (if (predicate key datum)
			     (cons key acc)
			     acc))
		       '()
		       state)))))

(define-default-operation 'set! '(set-1!)
  (lambda (:set-1!)
    (lambda (state . plist)
      (let loop ((plist plist))
	(if (not (null-list? plist 'amap-set!))
	    (let ((key (car plist))
		  (rest (cdr plist)))
	      (if (null-list? rest 'amap-set!)
		  (error:bad-range-argument plist 'amap-set!))
	      (:set-1! state key (car rest))
	      (loop (cdr rest))))))))

(define-default-operation 'union! '(contains? set!)
  (lambda (:contains? :set!)
    (lambda (state amap2)
      (amap-for-each (lambda (key value)
		       (if (not (:contains? state key))
			   (:set! state key value)))
		     amap2))))

(define-default-operation 'update! '(ref set!)
  (lambda (:ref :set!)
    (lambda (state key updater fail succeed)
      (:set! state key (updater (:ref state key fail succeed))))))

(define-default-operation 'update!/default '(ref/default set!)
  (lambda (:ref/default :set!)
    (lambda (state key updater default)
      (:set! state key (updater (:ref/default state key default))))))

(define-default-operation 'values '(fold)
  (lambda (:fold)
    (lambda (state)
      (:fold (lambda (key value acc)
	       (declare (ignore key))
	       (cons value acc))
	     '()
	     state))))

(define-default-operation 'xor! '(contains? delete! set!)
  (lambda (:contains? :delete! :set!)
    (lambda (state amap2)
      (amap-for-each (lambda (key value)
		       (if (:contains? state key)
			   (:delete! state key)
			   (:set! state key value)))
		     amap2))))