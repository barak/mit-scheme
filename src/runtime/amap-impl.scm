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

(define (define-amap-impl name keywords new-state operations)
  (guarantee interned-symbol? name 'define-amap-impl)
  (guarantee amap-keywords? keywords 'define-amap-impl)
  (guarantee binary-procedure? new-state 'define-amap-impl)
  (guarantee amap-operations? operations 'define-amap-impl)
  (let* ((keywords (delete-duplicates keywords eq?))
	 (has? (lambda (keyword) (memq keyword keywords))))
    (if (not (any has? mutability-support-keywords))
	(error "No mutability support specified:" keywords))
    (for-each (lambda (group)
		(let ((given (filter has? group)))
		  (if (> (length given) 1)
		      (error "These keywords are mutually exclusive:" given))))
	      mutually-exclusive-keywords)
    (alist-table-set! registered-implementations name
      (make-amap-impl name
		      keywords
		      new-state
		      (organize-operations keywords operations))))
  name)

(define registered-implementations
  (alist-table eq?))

(define (implementation-names)
  (alist-table-keys registered-implementations))

(define (implementations)
  (alist-table-values registered-implementations))

(define (get-implementation name)
  (alist-table-ref registered-implementations name))

(define (amap-keywords? object)
  (list-of-type? object
    (lambda (elt)
      (memq elt all-impl-keywords))))
(register-predicate! amap-keywords? 'amap-keywords '<= list?)

(define mutability-support-keywords
  '(mutable immutable))

(define weakish-support-keywords
  '(ephemeral-keys
    ephemeral-values
    ephemeral-keys+values
    weak-keys
    weak-values
    weak-keys+values))

(define mutually-exclusive-keywords
  '((amortized-constant-time log-time linear-time)
    (key-is-uniform-list key-is-lset key-is-uniform-vector)))

(define other-keywords
  '(strong-keys+values ordered hashed thread-safe))

(define all-impl-keywords
  (append mutability-support-keywords
	  weakish-support-keywords
	  (apply append mutually-exclusive-keywords)
	  other-keywords))

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

(define (organize-operations keywords operations)
  (let ((mutable? (memq 'mutable keywords))
	(immutable? (memq 'immutable keywords))
	(weakish?
	 (any (lambda (keyword)
		(memq keyword keywords))
	      weakish-support-keywords)))
    (if (not (or mutable? immutable?))
	(error "No mutability support specified:" keywords))
    (let ((missing-operators
	   (missing-inputs mutable? immutable? weakish? operations)))
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
      (if (not (alist-table-contains? table 'mutable?))
	  (alist-table-set! table 'mutable? (default:mutable? mutable?)))
      (if (not (alist-table-contains? table 'clean!))
	  (alist-table-set! table 'clean! default:clean!))

      (evolve
       (map default-operation
	    (lset-difference eq?
			     (required-outputs mutable?)
			     (alist-table-keys table))))
      (lambda (operator)
	(alist-table-ref table
			 operator
			 (lambda () (error-operation operator)))))))

(define (missing-inputs mutable? immutable? weakish? operations)

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
		      ((clean!) (and mutable? weakish?))
		      ((mutable?) (and mutable? immutable?))
		      (else (or mutable? (not (operator-mutates? operator))))))
		  (all-operators))))))

(define (required-outputs mutable?)
  (if mutable?
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
  (let ((impl-requirements (impl-support-requirements args)))
    (lambda (impl)
      (and (impl-requirements-supported? impl-requirements impl)
	   (comparator-requirements-satisfied? (comparator-requirements impl)
					       comparator)))))

(define (comparator-requirements impl)
  (filter-map (lambda (p)
		(and (memq (car p) (amap-impl:keywords impl))
		     (cdr p)))
	      comparator-restrictions))

(define-deferred comparator-restrictions
  (list (cons 'ordered comparator-ordered?)
	(cons 'hashed comparator-hashable?)
	(cons 'key-is-uniform-list uniform-list-comparator?)
	(cons 'key-is-lset lset-comparator?)
	(cons 'key-is-uniform-vector uniform-vector-comparator?)))

(define (comparator-requirements-satisfied? requirements comparator)
  (every (lambda (predicate) (predicate comparator))
	 requirements))

(define (impl-support-requirements args)
  (let ((req
	 (filter (lambda (rs)
		   (every (lambda (arg) (memq arg args))
			  (car rs)))
		 request-supports)))
    (if (null? (lset-intersection eq? req weakish-support-keywords))
	(cons 'strong-keys+values req)
	req)))

(define request-supports
  '(((ephemeral-keys ephemeral-values) ephemeral-keys+values)
    ((ephemeral-keys) ephemeral-keys)
    ((ephemeral-values) ephemeral-values)
    ((weak-keys weak-values) (or weak-keys+values ephemeral-keys+values))
    ((weak-keys) (or weak-keys ephemeral-keys))
    ((weak-values) (or weak-values ephemeral-values))
    ((thread-safe) thread-safe)
    ((amortized-constant-time) amortized-constant-time)
    ((log-time) log-time)
    ((linear-time) linear-time)
    ((sublinear-time) (or amortized-constant-time log-time))))

(define (impl-requirements-supported? requirements impl)

  (define (has-support? keyword)
    (memq keyword (amap-impl:keywords impl)))

  (every (lambda (rs)
	   (let ((req (cadr rs)))
	     (cond ((interned-symbol? req)
		    (has-support? req))
		   ((and (pair? req)
			 (eq? 'or (car req))
			 (list-of-type? interned-symbol?
					(cdr req)))
		    (any has-support? (cdr req)))
		   (else
		    (error "Unknown requirement:" req)))))
	 requirements))

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