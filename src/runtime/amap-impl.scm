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

(define (define-amap-impl name properties comparator-predicate new-state
	  operations)
  (guarantee interned-symbol? name 'define-amap-impl)
  (guarantee amap-properties? properties 'define-amap-impl)
  (if comparator-predicate
      (guarantee unary-procedure? comparator-predicate 'define-amap-impl))
  (guarantee binary-procedure? new-state 'define-amap-impl)
  (guarantee amap-operations? operations 'define-amap-impl)
  (let ((props (make-impl-props properties comparator-predicate)))
    (alist-table-set! registered-implementations name
      (make-amap-impl name
		      props
		      new-state
		      (organize-operations props operations))))
  name)

(define registered-implementations
  (alist-table eq?))

(define (implementation-names)
  (alist-table-keys registered-implementations))

(define (implementations)
  (alist-table-values registered-implementations))

(define (get-implementation name)
  (alist-table-ref registered-implementations name))

(define (amap-impl-supported-args impl)
  (impl-props-supported-args (amap-impl:props impl)))

;;;; Properties

(define (amap-properties? object)
  (list-of-type? object
    (lambda (elt)
      (and (pair? elt)
	   (have-prop? (car elt))
	   (non-empty-list? (cdr elt))))))
(register-predicate! amap-properties? 'amap-properties '<= list?)

(define (make-impl-props specs comparator-predicate)
  (let ((specs
	 (map (lambda (spec)
		(let ((keyword (car spec))
		      (vals (delete-duplicates (cdr spec) equal?)))
		  (let ((prop (get-prop keyword)))
		    (if (not (and (lset<= equal? vals (prop-vals prop))
				  ((prop-vals-predicate prop) vals)))
			(error "Ill-formed property:" spec))
		    (cons prop vals))))
	      specs)))
    (let ((missing
	   (remove (lambda (prop)
		     (assq prop specs))
		   (required-props))))
      (if (pair? missing)
	  (error "Missing required properties:" missing)))
    (let ((mutability (cdr (assq (get-prop 'mutability) specs)))
	  (props (map car specs)))
      (%make-impl-props
       (and (memq 'mutable mutability) #t)
       (and (memq 'immutable mutability) #t)
       (any (lambda (kv)
	      (any (lambda (x) (memq x '(weak ephemeron))) kv))
	    (cdr (assq (get-prop 'kv-types) specs)))
       (append-map (lambda (spec)
		     ((prop-matching-args (car spec)) (cdr spec)))
		   specs)
       (conjoin* (map prop-supports-args? props))
       (or comparator-predicate comparator?)))))

(define-record-type <impl-props>
    (%make-impl-props mutable? immutable? weakish?
		      supported-args supports-args? supports-comparator?)
    impl-props?
  (mutable? impl-props-mutable?)
  (immutable? impl-props-immutable?)
  (weakish? impl-props-weakish?)
  (supported-args impl-props-supported-args)
  (supports-args? impl-props-supports-args?)
  (supports-comparator? impl-props-supports-comparator?))

(define (define-property keyword required? vals vals-predicate
	  matching-args make-supports-args?)
  (alist-table-set! props keyword
    (make-prop keyword required?
	       vals
	       (or vals-predicate
		   (lambda (vals)
		     (declare (ignore vals))
		     #t))
	       (or matching-args
		   (lambda (vals)
		     (declare (ignore vals))
		     '()))
	       (if make-supports-args?
		   (make-args-support-predicate (matching-args vals)
						(make-supports-args? vals))
		   (lambda (args)
		     (declare (ignore args))
		     '()))))
  keyword)

(define (have-prop? keyword)
  (alist-table-contains? props keyword))

(define (get-prop keyword)
  (alist-table-ref props keyword))

(define (required-props)
  (filter prop-required? (alist-table-values props)))

(define props
  (alist-table eq?))

(define-record-type <prop>
    (make-prop keyword required? vals vals-predicate matching-args
	       supports-args?)
    prop?
  (keyword prop-keyword)
  (required? prop-required?)
  (vals prop-vals)
  (vals-predicate prop-vals-predicate)
  (matching-args prop-matching-args)
  (supports-args? prop-supports-args?))

(define (make-args-support-predicate claimed-args predicate)
  (let ((matcher (make-args-matcher claimed-args)))
    (lambda (args)
      (predicate (matcher args)))))

(define (make-args-matcher claimed-args)
  (let-values (((matchers syms) (partition arg-matcher? claimed-args)))
    (lambda (args)
      (let-values (((diff int) (lset-diff+intersection eq? args syms)))
	(append int
		(filter-map (lambda (arg)
			      (find (lambda (matcher)
				      (and ((arg-matcher-predicate matcher) arg)
					   matcher))
				    matchers))
			    diff))))))

(define (define-arg-matcher name predicate)
  (alist-table-set! arg-matchers name predicate))

(define (arg-matcher? name)
  (alist-table-contains? arg-matchers name))

(define (arg-matcher-predicate name)
  (alist-table-ref arg-matchers name))

(define arg-matchers
  (alist-table eq?))

(define-property 'mutability #t
  '(mutable immutable)
  #f
  #f
  #f)

(let ((alist
       '(((strong strong))
	 ((weak strong) weak-keys)
	 ((strong weak) weak-values)
	 ((weak weak) weak-keys weak-values)
	 ((ephemeral strong) ephemeral-keys)
	 ((strong ephemeral) ephemeral-values)
	 ((ephemeral ephemeral) ephemeral-keys ephemeral-values))))
  (define-property 'kv-types #t
    (map car alist)
    #f
    (lambda (vals)
      (apply lset-union
	     eq?
	     (filter-map (lambda (p)
			   (and (member (car p) vals)
				(cdr p)))
			 alist)))
    (lambda (vals)
      (let ((argsets
	     (map (lambda (val)
		    (cdr (assoc val alist)))
		  vals)))
	(lambda (args)
	  (any (lambda (argset)
		 (lset= eq? argset args))
	       argsets))))))

(let ((alist
       '((amortized-constant amortized-constant-time sublinear-time)
	 (log log-time sublinear-time)
	 (linear linear-time))))
  (define-property 'time-complexity #f
    (map car alist)
    (lambda (vals)
      (= 1 (length vals)))
    (lambda (vals)
      (apply lset-union
	     eq?
	     (filter-map (lambda (p)
			   (and (memq (car p) vals)
				(cdr p)))
			 alist)))
    (lambda (vals)
      (let ((supported (cdr (assq (car vals) alist))))
	(lambda (args)
	  (and (= 1 (length args))
	       (memq (car args) supported)))))))

(let ((all-vals '(thread-safe initial-size)))
  (define-property 'other #f
    all-vals
    #f
    (lambda (vals)
      vals)
    (lambda (vals)
      (lambda (args)
	(lset= eq? vals args)))))

(add-boot-init!
 (lambda ()
   (define-arg-matcher 'initial-size
     exact-nonnegative-integer?)))

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

(define (organize-operations props operations)
  (let ((missing-operators (missing-inputs props operations)))
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
			  (default:mutable? (impl-props-mutable? props))))

    (evolve
     (map default-operation
	  (lset-difference eq?
			   (required-outputs props)
			   (alist-table-keys table))))
    (lambda (operator)
      (alist-table-ref table
		       operator
		       (lambda () (error-operation operator))))))

(define (missing-inputs props operations)

  (define (remove-provided operators)
    (remove (lambda (operator)
	      (assq operator operations))
	    operators))

  (remove-provided
   (fold (lambda (operator acc)
	   (lset-union eq? (operator-deps operator) acc))
	 '()
	 (remove-provided
	  (filter (lambda (operator)
		    (case operator
		      ((clean!)
		       (and (impl-props-mutable? props)
			    (impl-props-weakish? props)))
		      ((mutable?)
		       (and (impl-props-mutable? props)
			    (impl-props-immutable? props)))
		      (else
		       (or (impl-props-mutable? props)
			   (not (operator-mutates? operator))))))
		  (all-operators))))))

(define (required-outputs props)
  (if (impl-props-mutable? props)
      (all-operators)
      (remove operator-mutates? (all-operators))))

(define (error-operation operator)
  (lambda args
    (error "Unimplemented amap operation:" (cons operator args))))

(define (select-impl comparator args)
  (for-each (lambda (group)
	      (let ((given (filter (lambda (arg) (memq arg args)) group)))
		(if (> (length given) 1)
		    (error "These args are mutually exclusive:" given))))
	    mutually-exclusive-args)
  (let ((predicate (impl-requirement-predicate comparator args)))
    (or (find predicate
	      (map get-implementation
		   (lset-intersection eq? args (implementation-names))))
	(find predicate (implementations))
	(error "Unable to select implementation:" args))))

(define mutually-exclusive-args
  '((ephemeral-keys weak-keys)
    (ephemeral-values weak-values)
    (amortized-constant-time log-time linear-time sublinear-time)))

(define (impl-requirement-predicate comparator args)
  (lambda (impl)
    (let ((props (amap-impl:props impl)))
      (and ((impl-props-supports-comparator? props) comparator)
	   ((impl-props-supports-args? props) args)))))

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
  (if mutable?
      (lambda (state) (declare (ignore state)) #t)
      (lambda (state) (declare (ignore state)) #f)))

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
    (lambda (state predicate)
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

(define-default-operation 'map '(for-each)
  (lambda (:for-each)
    (lambda (procedure comparator state)
      (let ((result (make-amap comparator)))
	(:for-each (let ((:set! (amap-impl:set! (amap-impl result))))
		     (lambda (key value)
		       (:set! state key (procedure value))))
		   state)
	result))))

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