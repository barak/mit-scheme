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

;;;; Hash Tables
;;; package: (runtime hash-table)

(declare (usual-integrations))

(add-boot-deps! '(runtime stream))

;;;; Structures

(define-record-type <hash-table-type>
    (%make-table-type key-hash key=? rehash-after-gc? method:get method:put!
		      method:remove! method:clean! method:rehash! method:fold
		      method:copy-bucket method:prune! method:map! method:find
		      method:every)
    hash-table-type?
  (key-hash table-type-key-hash)
  (key=? table-type-key=?)
  (rehash-after-gc? table-type-rehash-after-gc?)
  (method:get table-type-method:get)
  (method:put! table-type-method:put!)
  (method:remove! table-type-method:remove!)
  (method:clean! table-type-method:clean!)
  (method:rehash! table-type-method:rehash!)
  (method:fold table-type-method:fold)
  (method:copy-bucket table-type-method:copy-bucket)
  (method:prune! table-type-method:prune!)
  (method:map! table-type-method:map!)
  (method:find table-type-method:find)
  (method:every table-type-method:every))

(define (make-table type)
  (%make-table type
	       default-rehash-threshold
	       default-rehash-size
	       0
	       minimum-size
	       0
	       #f
	       prime-numbers-stream
	       #f
	       #f))

(define-record-type <hash-table>
    (%make-table type rehash-threshold rehash-size count grow-size shrink-size
		 buckets primes needs-rehash? initial-size-in-effect?)
    hash-table?
  (type hash-table-type)

  ;; Parameters of the hash table.
  (rehash-threshold hash-table-rehash-threshold set-table-rehash-threshold!)
  (rehash-size hash-table-rehash-size set-table-rehash-size!)

  ;; Internal state variables.
  (count table-count set-table-count!)
  (grow-size hash-table-grow-size set-table-grow-size!)
  (shrink-size hash-table-shrink-size set-table-shrink-size!)
  (buckets table-buckets set-table-buckets!)
  (primes table-primes set-table-primes!)
  (needs-rehash? table-needs-rehash? set-table-needs-rehash?!)
  (initial-size-in-effect? table-initial-size-in-effect?
			   set-table-initial-size-in-effect?!))

(define-integrable (increment-table-count! table)
  (set-table-count! table (fix:+ (table-count table) 1)))

(define-integrable (decrement-table-count! table)
  (set-table-count! table (fix:- (table-count table) 1)))

(define-integrable minimum-size 4)
(define-integrable default-rehash-threshold 1)
(define-integrable default-rehash-size 2.)

;;;; Table operations

(define (make-hash-table . args)
  (define (k type initial-size)
    (%make-hash-table type initial-size 'make-hash-table))
  (cond ((and (pair? args) (comparator? (car args)))
	 (k (apply comparator->hash-table-type args)
	    (find exact-nonnegative-integer? (cdr args))))
	((and (pair? args)
	      (hash-table-type? (car args))
	      (null? (cdr args)))
	 (k (car args) #f))
	(else
	 (k (apply srfi-69-hash-table-type args) #f))))

(define (alist->hash-table alist . args)
  (guarantee alist? alist 'alist->hash-table)
  (let ((table (apply make-hash-table args)))
    (for-each (lambda (p)
		(%hash-table-set! table (car p) (cdr p)))
	      alist)
    table))

(define (hash-table comparator . args)
  (let ((table (make-hash-table comparator)))
    (let loop ((scan args))
      (if (pair? scan)
	  (let ((key (car scan))
		(scan (cdr scan)))
	    (if (not (pair? scan))
		(error "Not an even number of arguments:" args))
	    (%hash-table-set! table key (car scan))
	    (loop (cdr scan)))))
    table))

(define (hash-table-unfold stop? mapper successor seed comparator . args)
  (let ((result (apply make-hash-table comparator args)))
    (let loop ((seed seed))
      (if (stop? seed)
	  result
	  (let-values (((key value) (mapper seed)))
	    (%hash-table-set! result key value)
	    (loop (successor seed)))))))

(define (hash-table-constructor type . args)
  (let ((type
	 (if (comparator? type)
	     (apply comparator->hash-table-type type args)
	     (guarantee hash-table-type? type 'hash-table-constructor))))
    (lambda (#!optional initial-size)
      (%make-hash-table type initial-size 'hash-table-constructor))))

(define (%make-hash-table type initial-size caller)
  (let ((initial-size
	 (if (or (default-object? initial-size) (not initial-size))
	     #f
	     (begin
	       (guarantee exact-nonnegative-integer? initial-size caller)
	       initial-size))))
    (let ((table (make-table type)))
      (if (and initial-size (> initial-size minimum-size))
	  ;; If an initial size is given, it means that the table
	  ;; should be initialized with that usable size.  The
	  ;; table's usable size remains fixed at the initial size
	  ;; until the count exceeds the usable size, at which point
	  ;; normal table resizing takes over.
	  (begin
	    (set-table-grow-size! table initial-size)
	    (set-table-initial-size-in-effect?! table #t)))
      (reset-table! table)
      (if (table-type-rehash-after-gc? type)
	  (record-address-hash-table! table))
      table)))

(define (record-address-hash-table! table)
  (add-new-to-population! address-hash-tables table))

(define address-hash-tables)
(add-boot-init!
 (lambda ()
   (set! address-hash-tables (make-serial-population))
   (add-primitive-gc-daemon! mark-address-hash-tables!)
   unspecific))

(define (mark-address-hash-tables!)
  (for-each-inhabitant address-hash-tables
		       (lambda (table)
			 (set-table-needs-rehash?! table #t))))

(define (hash-table-mutable? table)
  (guarantee hash-table? table 'hash-table-mutable?)
  #t)

(define (hash-table-empty? table)
  (= 0 (hash-table-size table)))

(define (hash-table-size table)
  (let loop ()
    (let ((count (table-count table)))
      (if (table-needs-rehash? table)
	  (begin
	    (rehash-table! table)
	    (loop))
	  count))))

(define (hash-table-contains? table key)
  (%hash-table-ref table key
    (lambda () #f)
    (lambda (value) (declare (ignore value)) #t)))

(define (hash-table-ref table key #!optional fail succeed)
  (%hash-table-ref table key
    (if (default-object? fail)
	(lambda () (error:bad-range-argument key 'hash-table-ref))
	fail)
    (if (default-object? succeed) #f succeed)))

(define (hash-table-ref/default table key default)
  (%hash-table-ref table key (lambda () default) #f))

(define-integrable (%hash-table-ref table key fail succeed)
  ((table-type-method:get (hash-table-type table)) table key fail succeed))

(define (hash-table-set! table . plist)
  (let ((:set! (table-type-method:put! (hash-table-type table))))
    (let loop ((plist plist))
      (if (not (null-list? plist 'hash-table-set!))
          (let ((key (car plist))
                (rest (cdr plist)))
            (if (null-list? rest 'hash-table-set!)
                (error:bad-range-argument plist 'hash-table-set!))
            (:set! table key (car rest))
            (loop (cdr rest)))))))

(define (%hash-table-set! table key datum)
  ((table-type-method:put! (hash-table-type table)) table key datum))

(define (hash-table-update! table key procedure #!optional fail succeed)
  (%hash-table-set! table key
		    (procedure (hash-table-ref table key fail succeed))))

(define (hash-table-update!/default table key procedure default)
  (hash-table-update! table key procedure (lambda () default)))

(define (hash-table-intern! table key generator)
  (%hash-table-ref table key
		   (lambda ()
		     (let ((datum (generator)))
		       (%hash-table-set! table key datum)
		       datum))
		   #f))

(define (hash-table-delete! table . keys)
  (fold (let ((:remove! (table-type-method:remove! (hash-table-type table))))
	  (lambda (key count)
	    (if (:remove! table key)
		(+ count 1)
		count)))
	0
	keys))

(define (hash-table-clear! table)
  (without-interruption
    (lambda ()
      (if (not (table-initial-size-in-effect? table))
	  (set-table-grow-size! table minimum-size))
      (set-table-count! table 0)
      (reset-table! table))))

(define (hash-table-clean! table)
  (without-interruption
    (lambda ()
      ((table-type-method:clean! (hash-table-type table)) table)
      (maybe-shrink-table! table))))

(define (hash-table-fold kons knil table)
  (if (hash-table? kons)
      (%hash-table-fold knil table kons)
      (%hash-table-fold kons knil table)))

(define (%hash-table-fold kons knil table)
  ((table-type-method:fold (hash-table-type table)) kons knil table))

(define (hash-table-find predicate table fail)
  ((table-type-method:find (hash-table-type table)) predicate table fail))

(define (hash-table-every predicate table)
  ((table-type-method:every (hash-table-type table)) predicate table))

(define (hash-table-map! procedure table)
  (without-interruption
    (lambda ()
      ((table-type-method:map! (hash-table-type table)) procedure table)
      (maybe-shrink-table! table))))

(define (hash-table-prune! predicate table)
  (without-interruption
    (lambda ()
      ((table-type-method:prune! (hash-table-type table)) predicate table)
      (maybe-shrink-table! table))))

(define (hash-table-for-each procedure table)
  (%hash-table-fold (lambda (key datum acc)
		      (procedure key datum)
		      acc)
		    unspecific
		    table))

(define (hash-table-count predicate table)
  (%hash-table-fold (lambda (key datum acc)
		      (if (predicate key datum)
			  (+ acc 1)
			  acc))
		    0
		    table))

(define (hash-table-map procedure comparator table)
  (let ((result (make-hash-table comparator)))
    (%hash-table-fold (lambda (key value acc)
			(%hash-table-set! result key (procedure value))
			acc)
		      unspecific
		      table)
    result))

(define (hash-table-map->list procedure table)
  (%hash-table-fold (lambda (key value acc)
		      (cons (procedure key value) acc))
		    '()
		    table))

(define (hash-table->alist table)
  (%hash-table-fold (lambda (key datum alist)
		     (cons (cons key datum) alist))
		   '()
		   table))

(define (hash-table-keys table)
  (%hash-table-fold (lambda (key datum keys)
		     (declare (ignore datum))
		     (cons key keys))
		   '()
		   table))

(define (hash-table-values table)
  (%hash-table-fold (lambda (key datum values)
		     (declare (ignore key))
		     (cons datum values))
		   '()
		   table))

(define (hash-table-entries table)
  (let ((p
	 (%hash-table-fold (lambda (key value acc)
			     (cons (cons key (car acc))
				   (cons value (cdr acc))))
			  (cons '() '())
			  table)))
    (values (car p) (cdr p))))

(define (hash-table-pop! table)
  (let ((p
	 (hash-table-find cons table
	   (lambda ()
	     (error:bad-range-argument table 'hash-table-pop!)))))
    (hash-table-delete! table (car p))
    (values (car p) (cdr p))))

(define (hash-table<=? value-comparator table1 table2)
  (let ((val=? (comparator-equality-predicate value-comparator)))
    (%hash-table<=? val=? table1 table2)))

(define (hash-table=? value-comparator table1 table2)
  (let ((val=? (comparator-equality-predicate value-comparator)))
    (and (%hash-table<=? val=? table1 table2)
	 (%hash-table<=? val=? table2 table1))))

(define (%hash-table<=? val=? table1 table2)
  (hash-table-every (lambda (key value)
		      (hash-table-ref table2 key
				      (lambda () #f)
				      (lambda (value*) (val=? value value*))))
		    table1))

(define (hash-table-union! table1 table2)
  (hash-table-for-each (lambda (key value)
			 (if (not (hash-table-contains? table1 key))
			     (%hash-table-set! table1 key value)))
		       table2)
  table1)

(define (hash-table-intersection! table1 table2)
  (hash-table-prune! (lambda (key value)
		       (declare (ignore value))
		       (not (hash-table-contains? table2 key)))
		     table1)
  table1)

(define (hash-table-difference! table1 table2)
  (hash-table-prune! (lambda (key value)
		       (declare (ignore value))
		       (hash-table-contains? table2 key))
		     table1)
  table1)

(define (hash-table-xor! table1 table2)
  (hash-table-for-each (lambda (key value)
			 (if (hash-table-contains? table1 key)
			     (hash-table-delete! table1 key)
			     (%hash-table-set! table1 key value)))
		       table2)
  table1)

(define (hash-table-copy table #!optional mutable?)
  (if (not mutable?)
      (error "Immutable hash tables not supported."))
  (without-interruption
    (lambda ()
      (let ((table* (copy-record table))
	    (type (hash-table-type table)))
	(set-table-buckets! table*
			    (vector-map (table-type-method:copy-bucket type)
					(table-buckets table)))
	(if (table-type-rehash-after-gc? type)
	    (record-address-hash-table! table*))
	table*))))

(define (hash-table-empty-copy table)
  (%make-hash-table (hash-table-type table) #f 'hash-table-empty-copy))

(define (set-hash-table-rehash-threshold! table threshold)
  (let ((threshold
	 (check-arg threshold
		    default-rehash-threshold
		    (lambda (x)
		      (and (real? x)
			   (< 0 x)
			   (<= x 1)))
		    "real number between 0 (exclusive) and 1 (inclusive)"
		    'set-hash-table-rehash-threshold!)))
    (without-interruption
      (lambda ()
	(set-table-rehash-threshold! table threshold)
	(new-size! table (hash-table-grow-size table))))))

(define (set-hash-table-rehash-size! table size)
  (let ((size
	 (check-arg size
		    default-rehash-size
		    (lambda (x)
		      (cond ((exact-integer? x) (< 0 x))
			    ((real? x) (< 1 x))
			    (else #f)))
		    "real number > 1 or exact integer >= 1"
		    'set-hash-table-rehash-size!)))
    (without-interruption
      (lambda ()
	(set-table-rehash-size! table size)
	(reset-shrink-size! table)
	(maybe-shrink-table! table)))))

;;;; Entry abstraction

(define-integrable (make-entry-type make valid? c-w-k c-w-k&d set-datum!)
  (lambda (receiver)
    (declare (integrate-operator receiver))
    (receiver make valid? c-w-k c-w-k&d set-datum!)))

(define-integrable (make-entry type key datum)
  (type (lambda (make valid? c-w-k c-w-k&d set-datum!)
	  (declare (integrate-operator make))
	  (declare (ignore valid? c-w-k c-w-k&d set-datum!))
	  (make key datum))))

(define-integrable (entry-valid? type entry)
  (type (lambda (make valid? c-w-k c-w-k&d set-datum!)
	  (declare (integrate-operator valid?))
	  (declare (ignore make c-w-k c-w-k&d set-datum!))
	  (valid? entry))))

;;; Rather than expose an ENTRY-KEY and an ENTRY-DATUM, this entry
;;; abstraction has only aggregate operations that (1) fetch the key
;;; (and datum), and (2) branch depending on whether the entry is
;;; valid, guaranteeing that the key and datum will have been fetched
;;; before the branch.  This prevents users of the entry abstraction
;;; from mistakenly using the key or datum of a weak entry without
;;; checking whether the entry is valid -- a bug present in previous
;;; revisions of this file.  For strong entries, the branch is
;;; integrated away into nothing.

(define-integrable (call-with-entry-key type entry if-valid if-not-valid)
  (type (lambda (make valid? c-w-k c-w-k&d set-datum!)
	  (declare (integrate-operator c-w-k))
	  (declare (ignore make valid? c-w-k&d set-datum!))
	  (c-w-k entry if-valid if-not-valid))))

(define-integrable (call-with-entry-key&datum type entry if-valid if-not-valid)
  (type (lambda (make valid? c-w-k c-w-k&d set-datum!)
	  (declare (integrate-operator c-w-k&d))
	  (declare (ignore make valid? c-w-k set-datum!))
	  (c-w-k&d entry if-valid if-not-valid))))

(define-integrable (set-entry-datum! type entry object)
  (type (lambda (make valid? c-w-k c-w-k&d set-datum!)
	  (declare (integrate-operator set-datum!))
	  (declare (ignore make valid? c-w-k c-w-k&d))
	  (set-datum! entry object))))

(define (make-table-type key-hash key=? rehash-after-gc? compute-hash!
			 entry-type)
  (declare (integrate rehash-after-gc?))
  (declare (integrate-operator key-hash key=? compute-hash! entry-type))
  (declare (no-type-checks) (no-range-checks))
  (%make-table-type key-hash key=? rehash-after-gc?
		    (make-method:get compute-hash! key=? entry-type)
		    (make-method:put! compute-hash! key=? entry-type)
		    (make-method:remove! compute-hash! key=? entry-type)
		    (if (eq? entry-type hash-table-entry-type:strong)
			(named-lambda (method:no-clean! table)
			  (declare (ignore table))
			  unspecific)
			(make-method:clean! entry-type))
		    (make-method:rehash! key-hash entry-type)
		    (make-method:fold entry-type)
		    (make-method:copy-bucket entry-type)
		    (make-method:prune! entry-type)
		    (make-method:map! entry-type)
		    (make-method:find entry-type)
		    (make-method:every entry-type)))

(define-integrable (maybe-weak-cons a d)
  ;; Use an ordinary pair for objects that aren't pointers or that have
  ;; unbounded extent.
  (if (or (object-non-pointer? a)
	  (number? a)
	  (interned-symbol? a))
      (cons a d)
      (weak-cons a d)))

;;;; Entries of various flavours

(define all-entry-types '())

(define (register-entry-type! name type)
  (set! all-entry-types
	(cons (cons name type)
	      all-entry-types))
  unspecific)

(define (entry-type-name? name)
  (and (assq name all-entry-types) #t))

(define (get-entry-type name)
  (cdr (assq name all-entry-types)))

(define (hash-table-entry-type-names)
  (map car all-entry-types))

;;; Strong

(define-integrable make-strong-entry cons)
(define-integrable (strong-entry-valid? entry) entry #t)
(define-integrable strong-entry-key car)
(define-integrable strong-entry-datum cdr)
(define-integrable set-strong-entry-datum! set-cdr!)

(define-integrable (call-with-strong-entry-key entry if-valid if-not-valid)
  (declare (ignore if-not-valid))
  (if-valid (strong-entry-key entry) (lambda () unspecific)))

(define-integrable (call-with-strong-entry-key&datum entry if-valid if-not)
  (declare (ignore if-not))
  (if-valid (strong-entry-key entry)
	    (strong-entry-datum entry)
	    (lambda () unspecific)))

(declare (integrate-operator hash-table-entry-type:strong))
(define hash-table-entry-type:strong
  (make-entry-type make-strong-entry
		   strong-entry-valid?
		   call-with-strong-entry-key
		   call-with-strong-entry-key&datum
		   set-strong-entry-datum!))
(register-entry-type! 'strong hash-table-entry-type:strong)

;;; Key-weak -- if the key is GC'd, the entry is dropped, but the datum
;;; may be retained arbitrarily long.

(define-integrable (make-key-weak-entry key datum)
  (maybe-weak-cons key datum))

(define-integrable (key-weak-entry-valid? entry)
  (or (pair? entry)
      (not (gc-reclaimed-object? (weak-car entry)))))

(define-integrable key-weak-entry-key system-pair-car)
(define-integrable key-weak-entry-datum system-pair-cdr)
(define-integrable set-key-weak-entry-datum! system-pair-set-cdr!)

(define-integrable (call-with-key-weak-entry-key entry if-valid if-not-valid)
  (let ((k (key-weak-entry-key entry)))
    ;** Do not integrate K!  It must be fetched and saved *before* we
    ;** determine whether the entry is valid.
    (if (not (gc-reclaimed-object? k))
	(if-valid k (lambda () (reference-barrier k)))
	(if-not-valid))))

(define-integrable (call-with-key-weak-entry-key&datum entry if-valid if-not)
  (let ((k (key-weak-entry-key entry)))
    ;** Do not integrate K!  It is OK to integrate D only because these
    ;** are weak pairs, not ephemerons, so the entry holds D strongly
    ;** anyway.
    (if (not (gc-reclaimed-object? k))
	(if-valid k
		  (key-weak-entry-datum entry)
		  (lambda () (reference-barrier k)))
	(if-not))))

(declare (integrate-operator hash-table-entry-type:key-weak))
(define hash-table-entry-type:key-weak
  (make-entry-type make-key-weak-entry
		   key-weak-entry-valid?
		   call-with-key-weak-entry-key
		   call-with-key-weak-entry-key&datum
		   set-key-weak-entry-datum!))
(register-entry-type! 'key-weak hash-table-entry-type:key-weak)

;;; Datum-weak -- if the datum is GC'd, the entry is dropped, but the
;;; key may be retained arbitrarily long.

(define-integrable (make-datum-weak-entry key datum)
  (maybe-weak-cons datum key))

(define-integrable (datum-weak-entry-valid? entry)
  (not (gc-reclaimed-object? (system-pair-car entry))))

(define-integrable datum-weak-entry-key system-pair-cdr)
(define-integrable datum-weak-entry-datum system-pair-car)
(define-integrable set-datum-weak-entry-datum! system-pair-set-car!)

(define-integrable (call-with-datum-weak-entry-key entry if-valid if-not)
  (let ((d (datum-weak-entry-datum entry)))
    (if (not (gc-reclaimed-object? d))
	(if-valid (datum-weak-entry-key entry)
		  (lambda () (reference-barrier d)))
	(if-not))))

(define-integrable (call-with-datum-weak-entry-key&datum entry if-valid if-not)
  (let ((d (datum-weak-entry-datum entry)))
    (if (not (gc-reclaimed-object? d))
	(if-valid (datum-weak-entry-key entry)
		  d
		  (lambda () (reference-barrier d)))
	(if-not))))

(declare (integrate-operator hash-table-entry-type:datum-weak))
(define hash-table-entry-type:datum-weak
  (make-entry-type make-datum-weak-entry
		   datum-weak-entry-valid?
		   call-with-datum-weak-entry-key
		   call-with-datum-weak-entry-key&datum
		   set-datum-weak-entry-datum!))
(register-entry-type! 'datum-weak hash-table-entry-type:datum-weak)

;;; Key-or-datum-weak -- if either is GC'd, the entry is dropped.

(define-integrable (make-key&datum-weak-entry key datum)
  (maybe-weak-cons key (maybe-weak-cons datum '())))

(define-integrable (key&datum-weak-entry-valid? entry)
  (and (not (gc-reclaimed-object? (system-pair-car entry)))
       (not (gc-reclaimed-object? (system-pair-car (system-pair-cdr entry))))))

(define-integrable key&datum-weak-entry-key system-pair-car)
(define-integrable (key&datum-weak-entry-datum entry)
  (system-pair-car (system-pair-cdr entry)))

(define-integrable (set-key&datum-weak-entry-datum! entry object)
  (system-pair-set-car! (system-pair-cdr entry) object))

(define-integrable (call-with-key&datum-weak-entry-key entry if-valid if-not)
  (call-with-key&datum-weak-entry-key&datum entry
    (lambda (k d barrier) d (if-valid k barrier))
    if-not))

(define-integrable (call-with-key&datum-weak-entry-key&datum entry
		     if-valid
		     if-not)
  (let ((k (key&datum-weak-entry-key entry))
	(d (key&datum-weak-entry-datum entry)))
    (if (and (not (gc-reclaimed-object? k))
	     (not (gc-reclaimed-object? d)))
	(if-valid k d (lambda () (reference-barrier k) (reference-barrier d)))
	(if-not))))

(declare (integrate-operator hash-table-entry-type:key&datum-weak))
(define hash-table-entry-type:key&datum-weak
  (make-entry-type make-key&datum-weak-entry
		   key&datum-weak-entry-valid?
		   call-with-key&datum-weak-entry-key
		   call-with-key&datum-weak-entry-key&datum
		   set-key&datum-weak-entry-datum!))
(register-entry-type! 'key&datum-weak hash-table-entry-type:key&datum-weak)

;;; Key-ephemeral -- if the key is GC'd, the entry is dropped.

(define-integrable make-key-ephemeral-entry make-ephemeron)

(define-integrable (key-ephemeral-entry-valid? entry)
  (not (ephemeron-broken? entry)))

(define-integrable key-ephemeral-entry-key ephemeron-key)
(define-integrable key-ephemeral-entry-datum ephemeron-datum)
(define-integrable set-key-ephemeral-entry-datum! set-ephemeron-datum!)

(define-integrable (call-with-key-ephemeral-entry-key entry if-valid if-not)
  (let ((k (key-ephemeral-entry-key entry)))
    (if (key-ephemeral-entry-valid? entry)
	(if-valid k (lambda () (reference-barrier k)))
	(if-not))))

(define-integrable (call-with-key-ephemeral-entry-key&datum entry
		     if-valid
		     if-not)
  (let ((k (key-ephemeral-entry-key entry))
	(d (key-ephemeral-entry-datum entry)))
    ;** Do not integrate K or D here.  It is tempting to integrate D,
    ;** but if the caller ignores the barrier, and its last reference
    ;** to K precedes any reference to D, then the entry may be broken
    ;** before we read the datum.
    (if (key-ephemeral-entry-valid? entry)
	(if-valid k d (lambda () (reference-barrier k)))
	(if-not))))

(declare (integrate-operator hash-table-entry-type:key-ephemeral))
(define hash-table-entry-type:key-ephemeral
  (make-entry-type make-key-ephemeral-entry
		   key-ephemeral-entry-valid?
		   call-with-key-ephemeral-entry-key
		   call-with-key-ephemeral-entry-key&datum
		   set-key-ephemeral-entry-datum!))
(register-entry-type! 'key-ephemeral hash-table-entry-type:key-ephemeral)

;;; Datum-ephemeral -- if the datum is GC'd, the entry is dropped

(define-integrable (make-datum-ephemeral-entry key datum)
  (make-ephemeron datum key))

(define-integrable (datum-ephemeral-entry-valid? entry)
  (not (ephemeron-broken? entry)))

(define-integrable datum-ephemeral-entry-key ephemeron-datum)
(define-integrable datum-ephemeral-entry-datum ephemeron-key)
(define-integrable set-datum-ephemeral-entry-datum! set-ephemeron-key!)

(define-integrable (call-with-datum-ephemeral-entry-key entry if-valid if-not)
  (call-with-datum-ephemeral-entry-key&datum entry
    (lambda (k d barrier) d (if-valid k barrier))
    if-not))

(define-integrable (call-with-datum-ephemeral-entry-key&datum entry
		     if-valid
		     if-not)
  (let ((k (datum-ephemeral-entry-key entry))
	(d (datum-ephemeral-entry-datum entry)))
    (if (datum-ephemeral-entry-valid? entry)
	(if-valid k d (lambda () (reference-barrier d)))
	(if-not))))

(declare (integrate-operator hash-table-entry-type:datum-ephemeral))
(define hash-table-entry-type:datum-ephemeral
  (make-entry-type make-datum-ephemeral-entry
		   datum-ephemeral-entry-valid?
		   call-with-datum-ephemeral-entry-key
		   call-with-datum-ephemeral-entry-key&datum
		   set-datum-ephemeral-entry-datum!))
(register-entry-type! 'datum-ephemeral hash-table-entry-type:datum-ephemeral)

;;; Key-and-datum-ephemeral -- the entry is dropped iff both key and
;;; datum are GC'd.

(define (make-key&datum-ephemeral-entry key datum)
  (cons (make-ephemeron key datum) (make-ephemeron datum key)))

(define-integrable (key&datum-ephemeral-entry-valid? entry)
  (not (ephemeron-broken? (car entry))))

(define-integrable (key&datum-ephemeral-entry-key entry)
  (ephemeron-key (car entry)))

(define-integrable (key&datum-ephemeral-entry-datum entry)
  (ephemeron-datum (car entry)))

(define (set-key&datum-ephemeral-entry-datum! entry object)
  (declare (no-type-checks) (no-range-checks))
  ;; Careful!  Don't use this with interrupts enabled, or it won't be
  ;; atomic.
  (set-ephemeron-datum! (car entry) object)
  (set-ephemeron-key! (cdr entry) object))

(define-integrable (call-with-key&datum-ephemeral-entry-key entry
		     if-valid
		     if-not)
  (let ((k (key&datum-ephemeral-entry-key entry)))
    (if (key&datum-ephemeral-entry-valid? entry)
	(if-valid k (lambda () (reference-barrier k)))
	(if-not))))

(define-integrable (call-with-key&datum-ephemeral-entry-key&datum entry
		     if-valid
		     if-not)
  (let ((k (key&datum-ephemeral-entry-key entry))
	(d (key&datum-ephemeral-entry-datum entry)))
    (if (key&datum-ephemeral-entry-valid? entry)
	;; The reference barrier need use only K (or only D), because
	;; as long as the entry and one of the key or datum is live,
	;; the other of the key or datum will be live too.
	(if-valid k d (lambda () (reference-barrier k)))
	(if-not))))

(declare (integrate-operator hash-table-entry-type:key&datum-ephemeral))
(define hash-table-entry-type:key&datum-ephemeral
  (make-entry-type make-key&datum-ephemeral-entry
		   key&datum-ephemeral-entry-valid?
		   call-with-key&datum-ephemeral-entry-key
		   call-with-key&datum-ephemeral-entry-key&datum
		   set-key&datum-ephemeral-entry-datum!))
(register-entry-type! 'key&datum-ephemeral
		      hash-table-entry-type:key&datum-ephemeral)

;;; key-list-weak -- if any element of the key is GC'd, the entry is dropped,
;;; but the datum may be retained arbitrarily long.

(define-integrable (klwe-valid? entry)
  (not (weak-list-reclaimed? (car entry))))

(define (call-with-klwe-key entry if-valid if-not)
  (if (klwe-valid? entry)
      (if-valid (car entry) (lambda () unspecific))
      (if-not)))

(define (call-with-klwe-key&datum entry if-valid if-not)
  (if (klwe-valid? entry)
      (if-valid (car entry) (cdr entry) (lambda () unspecific))
      (if-not)))

(define hash-table-entry-type:key-list-weak
  (make-entry-type cons
		   klwe-valid?
		   call-with-klwe-key
		   call-with-klwe-key&datum
		   set-cdr!))
(register-entry-type! 'key-list-weak hash-table-entry-type:key-list-weak)

;;; weak-key-list&datum -- if any element of the key is GC'd, or the datum is
;;; GC'd, then the entry is dropped.

(define-integrable (klwde-valid? entry)
  (not (weak-list-reclaimed? entry)))

(define (call-with-klwde-key entry if-valid if-not)
  (if (klwde-valid? entry)
      (if-valid (weak-cdr entry) (lambda () unspecific))
      (if-not)))

(define (call-with-klwde-key&datum entry if-valid if-not)
  (if (klwde-valid? entry)
      (if-valid (weak-cdr entry) (weak-car entry) (lambda () unspecific))
      (if-not)))

(define hash-table-entry-type:key-list&datum-weak
  (make-entry-type (lambda (key datum) (weak-cons datum key))
		   klwde-valid?
		   call-with-klwde-key
		   call-with-klwde-key&datum
		   weak-set-car!))
(register-entry-type! 'key-list&datum-weak
		      hash-table-entry-type:key-list&datum-weak)

;;;; Methods

(define (make-method:get compute-hash! key=? entry-type)
  (declare (integrate-operator compute-hash! key=? entry-type))
  (define (method:get table key fail succeed)
    (let ((hash (compute-hash! table key)))
      ;; Call COMPUTE-HASH! before TABLE-BUCKETS, because computing the
      ;; hash might trigger rehashing which replaces the bucket vector.
      (let loop ((p (vector-ref (table-buckets table) hash)))
	(if (pair? p)
	    (call-with-entry-key&datum entry-type (car p)
	      (lambda (key* datum barrier)
		(declare (integrate key* datum) (ignore barrier))
		(if (key=? key* key)
		    (if succeed
			(succeed datum)
			datum)
		    (loop (cdr p))))
	      (lambda () (loop (cdr p))))
	    (fail)))))
  method:get)

(define (make-method:put! compute-hash! key=? entry-type)
  (declare (integrate-operator compute-hash! key=? entry-type))
  (define (method:put! table key datum)
    (let ((hash (compute-hash! table key)))
      (let loop ((p (vector-ref (table-buckets table) hash)) (q #f))
	(if (pair? p)
	    (call-with-entry-key entry-type (car p)
	      (lambda (key* barrier)
		(declare (integrate key* barrier))
		(if (key=? key* key)
		    (begin
		      (without-interruption
			(lambda ()
			  (set-entry-datum! entry-type (car p) datum)))
		      (barrier))
		    (loop (cdr p) p)))
	      (lambda () (loop (cdr p) p)))
	    (without-interruption
	      (lambda ()
		(let ((r (cons (make-entry entry-type key datum) '())))
		  (if q
		      (set-cdr! q r)
		      (vector-set! (table-buckets table) hash r)))
		(increment-table-count! table)
		(maybe-grow-table! table)))))))
  method:put!)

(define (make-method:remove! compute-hash! key=? entry-type)
  (declare (integrate-operator compute-hash! key=? entry-type))
  (define (method:remove! table key)
    (let ((hash (compute-hash! table key)))
      (let loop ((p (vector-ref (table-buckets table) hash)) (q #f))
	(and (pair? p)
	     (call-with-entry-key entry-type (car p)
	       (lambda (key* barrier)
		 (declare (integrate key*) (ignore barrier))
		 (if (key=? key* key)
		     (without-interruption
		       (lambda ()
			 (if q
			     (set-cdr! q (cdr p))
			     (vector-set! (table-buckets table) hash (cdr p)))
			 (decrement-table-count! table)
			 (maybe-shrink-table! table)
			 #t))
		     (loop (cdr p) p)))
	       (lambda () (loop (cdr p) p)))))))
  method:remove!)

(define (make-method:clean! entry-type)
  (declare (integrate-operator entry-type))
  (define (method:clean! table)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n-buckets)))
	  (let ()
	    (define (scan-head p)
	      (if (pair? p)
		  (if (entry-valid? entry-type (car p))
		      (begin
			(vector-set! buckets i p)
			(scan-tail (cdr p) p))
		      (begin
			(decrement-table-count! table)
			(scan-head (cdr p))))
		  (vector-set! buckets i p)))
	    (define (scan-tail p q)
	      (if (pair? p)
		  (if (entry-valid? entry-type (car p))
		      (scan-tail (cdr p) p)
		      (begin
			(decrement-table-count! table)
			(let loop ((p (cdr p)))
			  (if (pair? p)
			      (if (entry-valid? entry-type (car p))
				  (begin
				    (set-cdr! q p)
				    (scan-tail (cdr p) p))
				  (begin
				    (decrement-table-count! table)
				    (loop (cdr p))))
			      (set-cdr! q p)))))))
	    (scan-head (vector-ref buckets i)))))))
  method:clean!)

(define (make-method:rehash! key-hash entry-type)
  (declare (integrate-operator key-hash entry-type))
  (define (method:rehash! table entries)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
	(let loop ((p entries))
	  (if (pair? p)
	      (let ((q (cdr p)))
		(call-with-entry-key entry-type (car p)
		  (lambda (key barrier)
		    (declare (integrate key) (ignore barrier))
		    (let ((hash (key-hash key n-buckets)))
		      (set-cdr! p (vector-ref buckets hash))
		      (vector-set! buckets hash p)))
		  (lambda () (decrement-table-count! table)))
		(loop q)))))))
  method:rehash!)

(define (make-method:map! entry-type)
  (declare (integrate-operator entry-type))
  (define (method:map! procedure table)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n-buckets)))
	  (let loop ((this (vector-ref buckets i)) (prev #f))
	    (if (pair? this)
		(call-with-entry-key&datum entry-type (car this)
		  (lambda (key datum barrier)
		    (declare (integrate key datum))
		    (declare (ignore barrier))
		    (set-entry-datum! entry-type
				      (car this)
				      (procedure key datum))
		    (loop (cdr this) this))
		  (lambda ()
		    (decrement-table-count! table)
		    (if prev
			(set-cdr! prev (cdr this))
			(vector-set! buckets i (cdr this)))
		    (loop (cdr this) prev)))))))))
  method:map!)

(define (make-method:prune! entry-type)
  (declare (integrate-operator entry-type))
  (define (method:prune! predicate table)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n-buckets)))
	  (let ()

	    (define (scan this prev)
	      (if (pair? this)
		  (call-with-entry-key&datum entry-type (car this)
		    (lambda (key datum barrier)
		      (declare (integrate key datum))
		      (declare (ignore barrier))
		      (if (predicate key datum)
			  (delete this prev)
			  (scan (cdr this) this)))
		    (lambda ()
		      (delete this prev)))))

	    (define (delete this prev)
	      (decrement-table-count! table)
	      (if prev
		  (set-cdr! prev (cdr this))
		  (vector-set! buckets i (cdr this)))
	      (scan (cdr this) prev))

	    (scan (vector-ref buckets i) #f))))))
  method:prune!)

(define (make-method:fold entry-type)
  (declare (integrate-operator entry-type))
  (define (method:fold kons knil table)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
	(let per-bucket ((i 0) (acc knil))
	  (if (fix:< i n-buckets)
	      (let per-entry ((p (vector-ref buckets i)) (acc acc))
		(if (pair? p)
		    (per-entry (cdr p)
			       (call-with-entry-key&datum entry-type (car p)
				 (lambda (key datum barrier)
				   (declare (integrate key datum))
				   (declare (ignore barrier))
				   (kons key datum acc))
				 (lambda () acc)))
		    (per-bucket (fix:+ i 1) acc)))
	      acc)))))
  method:fold)

(define (make-method:find entry-type)
  (declare (integrate-operator entry-type))
  (define (method:find procedure table fail)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
	(let per-bucket ((i 0))
	  (if (fix:< i n-buckets)
	      (let per-entry ((p (vector-ref buckets i)))
		(if (pair? p)
		    (call-with-entry-key&datum entry-type (car p)
		      (lambda (key datum barrier)
			(declare (integrate key datum))
			(declare (ignore barrier))
			(or (procedure key datum)
			    (per-entry (cdr p))))
		      (lambda ()
			(per-entry (cdr p))))
		    (per-bucket (fix:+ i 1))))
	      (fail))))))
  method:find)

(define (make-method:every entry-type)
  (declare (integrate-operator entry-type))
  (define (method:every predicate table)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
	(let per-bucket ((i 0) (result #t))
	  (if (fix:< i n-buckets)
	      (let per-entry ((p (vector-ref buckets i)) (result result))
		(if (pair? p)
		    (call-with-entry-key&datum entry-type (car p)
		      (lambda (key datum barrier)
			(declare (integrate key datum))
			(declare (ignore barrier))
			(let ((value (predicate key datum)))
			  (and value
			       (per-entry (cdr p) value))))
		      (lambda ()
			(per-entry (cdr p) result)))
		    (per-bucket (fix:+ i 1) result)))
	      result)))))
  method:every)

(define (make-method:copy-bucket entry-type)
  (declare (integrate-operator entry-type))
  (define (method:copy-bucket bucket)
    (let find-head ((p bucket))
      (if (pair? p)
	  (call-with-entry-key&datum entry-type (car p)
	    (lambda (key datum barrier)
	      (declare (integrate key datum) (ignore barrier))
	      (let ((head (cons (make-entry entry-type key datum) '())))
		(let loop ((p (cdr p)) (previous head))
		  (if (pair? p)
		      (loop (cdr p)
			    (call-with-entry-key&datum entry-type (car p)
			      (lambda (key datum barrier)
				(declare (integrate key datum))
				(declare (ignore barrier))
				(let ((p*
				       (cons (make-entry entry-type key datum)
					     '())))
				  (set-cdr! previous p*)
				  p*))
			      (lambda () previous)))))
		head))
	    (lambda () (find-head (cdr p))))
	  p)))
  method:copy-bucket)

;;;; Resizing

(define (maybe-grow-table! table)
  (if (> (table-count table) (hash-table-grow-size table))
      (begin
	(let loop ((size (hash-table-grow-size table)))
	  (if (> (table-count table) size)
	      (loop (increment-size table size))
	      (new-size! table size)))
	(set-table-initial-size-in-effect?! table #f))))

(define (maybe-shrink-table! table)
  (if (and (< (table-count table) (hash-table-shrink-size table))
	   (not (table-initial-size-in-effect? table)))
      (let loop ((size (hash-table-grow-size table)))
	(cond ((<= size minimum-size)
	       (new-size! table minimum-size))
	      ((< (table-count table) (compute-shrink-size table size))
	       (loop (decrement-size table size)))
	      (else
	       (new-size! table size))))))

(define (new-size! table size)
  (set-table-grow-size! table size)
  (let ((old-buckets (table-buckets table)))
    (reset-table! table)
    (let ((n-buckets (vector-length old-buckets))
	  (method (table-type-method:rehash! (hash-table-type table))))
      (set-table-needs-rehash?! table #f)
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i n-buckets)))
	(method table (vector-ref old-buckets i))))
    (maybe-shrink-table! table)))

(define (reset-table! table)
  (reset-shrink-size! table)
  (let ((primes
	 (let ((size
		(round->exact (/ (hash-table-grow-size table)
				 (hash-table-rehash-threshold table)))))
	   (let loop
	       ((primes
		 (if (< size (stream-car (table-primes table)))
		     prime-numbers-stream
		     (table-primes table))))
	     (if (<= size (stream-car primes))
		 primes
		 (loop (stream-cdr primes)))))))
    (set-table-primes! table primes)
    (set-table-buckets! table (make-vector (stream-car primes) '()))))

(define (reset-shrink-size! table)
  (set-table-shrink-size! table
			  (compute-shrink-size table
					       (hash-table-grow-size table))))

(define (compute-shrink-size table size)
  (if (<= size minimum-size)
      0
      (max 0 (decrement-size table (decrement-size table size)))))

(define (increment-size table size)
  (let ((rehash-size (hash-table-rehash-size table)))
    (if (exact-integer? rehash-size)
	(+ size rehash-size)
	(let ((size* (round->exact (* size rehash-size))))
	  (if (> size* size)
	      size*
	      (+ size 1))))))

(define (decrement-size table size)
  (let ((rehash-size (hash-table-rehash-size table)))
    (if (exact-integer? rehash-size)
	(- size rehash-size)
	(let ((size* (round->exact (/ size rehash-size))))
	  (if (< size* size)
	      size*
	      (- size 1))))))

;;;; Address hashing

;;; Address-hash tables compute their hash number from the address of
;;; the key.  Because the address is changed by the garbage collector,
;;; it is necessary to rehash the table after a garbage collection.

;;; Rehashing the table during the garbage collection is undesirable
;;; for these reasons:
;;; 1. The time required to rehash the table is proportional to the
;;;    number of items in the table, which can be quite large.  It's
;;;    undesirable for the garbage collection time to be extended this
;;;    way.
;;; 2. If the garbage collector rearranges the internals of the table,
;;;    then nearly every operation on the table must be locked to
;;;    prevent garbage collection from occurring while it runs.  This
;;;    means long periods with interrupts disabled, plus the overhead
;;;    of interrupt locking that is otherwise unnecessary.
;;; 3. If the table isn't used in between two garbage collections,
;;;    then the effort to rehash it during the first garbage
;;;    collection is wasted.

;;; For these reasons, rehashing of the table is performed lazily.
;;; When the garbage collector runs, it sets the table's NEEDS-REHASH?
;;; flag.  This flag is examined by all of the hash-table operations
;;; to see if it is necessary to rehash the table before performing
;;; the operation.  Since the only reason for rehashing the table is
;;; to ensure consistency between the table's contents and the result
;;; of the address hashing operation, it is sufficient to check this
;;; flag whenever the address hashing is performed.  This means that
;;; the rehashing of the table and the computing of the corresponding
;;; address hash must occur atomically with respect to the garbage
;;; collector.

;;; The only tricky part about this algorithm is that the garbage
;;; collector might run while the table is being resized.  If this
;;; occurs, part of the table might be hashed correctly, while the
;;; rest would be incorrect.  This is not a problem because resizing
;;; (with one exception) is always the last thing done by an
;;; operation.  If the garbage collection occurs during a resizing,
;;; the NEEDS-REHASH? flag will be true after the resizing is
;;; completed, and the next operation will rehash the table.

;;; The exception to this rule is COMPUTE-ADDRESS-HASH, which might
;;; have to shrink the table due to keys which have been reclaimed by
;;; the garbage collector.  REHASH-TABLE! explicitly checks for this
;;; possibility, and rehashes the table again if necessary.

(define (compute-non-address-hash key-hash)
  (declare (integrate-operator key-hash))
  (lambda (table key)
    (declare (integrate table key))
    (key-hash key (vector-length (table-buckets table)))))

(define (compute-address-hash key-hash)
  (declare (integrate-operator key-hash))
  (lambda (table key)
    (declare (integrate table key))
    (let loop ()
      (let ((hash (key-hash key (vector-length (table-buckets table)))))
	(if (table-needs-rehash? table)
	    (begin
	      (rehash-table! table)
	      (loop))
	    hash)))))

(define (rehash-table! table)
  (without-interruption
    (lambda ()
      (let ((entries (extract-table-entries! table)))
	(set-table-needs-rehash?! table #f)
	((table-type-method:rehash! (hash-table-type table)) table entries))
      (maybe-shrink-table! table))))

(define (extract-table-entries! table)
  (let ((buckets (table-buckets table)))
    (let ((n-buckets (vector-length buckets)))
      (do ((i 0 (fix:+ i 1))
	   (entries '()
		    (append! (let ((p (vector-ref buckets i)))
			       (vector-set! buckets i '())
			       p)
			     entries)))
	  ((not (fix:< i n-buckets)) entries)))))

;;;; EQ/EQV/EQUAL Hash functions

(define-integrable (eq-hash-mod key modulus)
  (fix:remainder (eq-hash key) modulus))

(define-integrable (eq-hash object)
  (fix:and ((ucode-primitive primitive-object-set-type)
	    (ucode-type positive-fixnum)
	    object)
	   (hash-mask)))

(define-integrable (eqv-hash-mod key modulus)
  (fix:remainder (eqv-hash key) modulus))

(define (eqv-hash key)
  (if (or (object-type? (ucode-type bignum) key)
	  (object-type? (ucode-type flonum) key)
	  (object-type? (ucode-type ratnum) key)
	  (object-type? (ucode-type recnum) key))
      (primitive-object-hash key)
      (eq-hash key)))

(define-integrable (equal-hash-mod key modulus)
  (fix:remainder (equal-hash key) modulus))

(define (comparator-binary-hash-function comparator)
  (let ((hash-fn (comparator-hash-function comparator)))
    (cond ((eqv? eq-hash hash-fn) eq-hash-mod)
	  ((eqv? eqv-hash hash-fn) eqv-hash-mod)
	  ((eqv? equal-hash hash-fn) equal-hash-mod)
	  (else
	   (lambda (key modulus)
	     (fix:remainder (hash-fn key) modulus))))))

;;;; Constructing and Open-Coding Types and Constructors

(define (comparator->hash-table-type comparator . args)
  (if (not (comparator-hashable? comparator))
      (error:bad-range-argument comparator))
  (make-hash-table-type (comparator-binary-hash-function comparator)
			(comparator-equality-predicate comparator)
			(comparator-rehash-after-gc? comparator)
			(comparator-entry-type comparator args)))

(define (comparator-entry-type comparator args)
  (cond ((weak-list-comparator? comparator)
	 (if (memq 'weak-values args)
	     hash-table-entry-type:key-list&datum-weak
	     hash-table-entry-type:key-list-weak))
	((and (memq 'weak-keys args) (memq 'weak-values args))
	 hash-table-entry-type:key&datum-weak)
	((memq 'weak-keys args) hash-table-entry-type:key-weak)
	((memq 'weak-values args) hash-table-entry-type:datum-weak)
	((and (memq 'ephemeral-keys args) (memq 'ephemeral-values args))
	 hash-table-entry-type:key&datum-ephemeral)
	((memq 'ephemeral-keys args) hash-table-entry-type:key-ephemeral)
	((memq 'ephemeral-values args) hash-table-entry-type:datum-ephemeral)
	(else hash-table-entry-type:strong)))

(define (make-hash-table-type* key=? . options)
  (receive (key-hash rehash-after-gc? entry-type-name)
      (hash-table-type-options options 'make-hash-table-type*)
    (make-hash-table-type (if (default-object? key-hash)
			      (comparator-binary-hash-function
			       (key=->comparator key=? 'make-hash-table-type*))
			      key-hash)
			  key=?
			  (if (default-object? rehash-after-gc?)
			      (comparator-rehash-after-gc?
			       (key=->comparator key=? 'make-hash-table-type*))
			      rehash-after-gc?)
			  (get-entry-type entry-type-name))))

(define-deferred hash-table-type-options
  (keyword-option-parser
   (list (list 'hash-function binary-procedure? default-object)
	 (list 'rehash-after-gc? boolean? default-object)
	 (list 'entry-type entry-type-name? (lambda () 'strong)))))

(define (make-hash-table-type key-hash key=? rehash-after-gc? entry-type)
  (hash-table-intern! (hash-metadata key=? key-hash rehash-after-gc?)
		      entry-type
    (lambda ()
      (let ((constructor
	     (hash-table-ref/default hash-table-type-constructors
				     entry-type
				     #f)))
	(if constructor
	    (constructor key-hash key=? rehash-after-gc?)
	    (%make-hash-table-type key-hash key=? rehash-after-gc?
				   entry-type))))))

(define (hash-metadata key-hash key=? rehash-after-gc?)
  (let ((lookup
	 (lambda (get set)
	   (let ((pair
		  (hash-table-intern!
		   (hash-table-intern! hash-metadata-table
				       key-hash
				       make-key-ephemeral-eq-hash-table)
		   key=?
		   (lambda () (cons #f #f)))))
	     (or (get pair)
		 (let ((v (make-key-ephemeral-eq-hash-table)))
		   (set pair v)
		   v))))))
    (if rehash-after-gc?
	(lookup car set-car!)
	(lookup cdr set-cdr!))))

(define (memoize-hash-table-type! key-hash key=? rehash-after-gc? entry-type
				  type)
  (let ((crap (hash-metadata key-hash key=? rehash-after-gc?)))
    (cond ((hash-table-ref/default crap entry-type #f)
	   => (lambda (type*)
		(warn "Replacing memoized hash table type:" type type*))))
    (%hash-table-set! crap entry-type type)))

(define (%make-hash-table-type key-hash key=? rehash-after-gc? entry-type)
  (let ((compute-hash!
	 ((if rehash-after-gc?
	      compute-address-hash
	      compute-non-address-hash)
	  (protected-hash-function key-hash))))
    ;; Don't integrate COMPUTE-HASH!.
    (make-table-type key-hash key=? rehash-after-gc? compute-hash!
		     entry-type)))

;;; Using syntax rather than integrable procedures avoids some
;;; unnecessary duplicates of the great pile of hash table code.  This
;;; is a non-hygienic kludge, so don't use it outside this file.  It's
;;; non-hygienic because of a collection of bugs: SYNTAX-RULES and
;;; DECLARE don't mix, hygienically macro-generated macro definitions
;;; aren't fasdumpable...

(define-syntax define-integrableish
  (sc-macro-transformer
   (lambda (form environment)
     (declare (ignore environment))
     (let ((name (caadr form))
	   (parameters (cdadr form))
	   (body (cddr form)))
       `(define-syntax ,name
	  (sc-macro-transformer
	   (lambda (form environment)
	     (cons '(named-lambda (,name ,@parameters)
		      (declare (integrate ,@parameters))
		      ,@body)
		   (map (lambda (subform)
			  (close-syntax subform environment))
			(cdr form))))))))))

(define-integrableish (open-type-constructor entry-type)
  (declare (integrate-operator %make-hash-table-type make-table-type))
  (declare (integrate-operator make-method:get make-method:put!))
  (declare (integrate-operator make-method:remove! make-method:clean!))
  (declare (integrate-operator make-method:rehash! make-method:fold))
  (declare (integrate-operator make-method:copy-bucket make-method:prune!))
  (declare (integrate-operator make-method:find make-method:every))
  (lambda (key-hash key=? rehash-after-gc?)
    (let ((compute-hash!
	   ((if rehash-after-gc?
		compute-address-hash
		compute-non-address-hash)
	    (protected-hash-function key-hash))))
      ;; Don't integrate COMPUTE-HASH!.
      (make-table-type key-hash key=? rehash-after-gc? compute-hash!
		       entry-type))))

(define-integrableish (open-type-constructor! entry-type)
  (%hash-table-set! hash-table-type-constructors
		    entry-type
		    (open-type-constructor entry-type)))

(define-integrableish (open-type key-hash key=? rehash-after-gc? entry-type)
  (declare (integrate-operator %make-hash-table-type make-table-type))
  (declare (integrate-operator compute-address-hash compute-non-address-hash))
  (declare (integrate-operator make-method:get make-method:put!))
  (declare (integrate-operator make-method:remove! make-method:clean!))
  (declare (integrate-operator make-method:rehash! make-method:fold))
  (declare (integrate-operator make-method:copy-bucket make-method:prune!))
  (declare (integrate-operator make-method:find make-method:every))
  (make-table-type key-hash key=? rehash-after-gc?
		   (if rehash-after-gc?
		       (compute-address-hash key-hash)
		       (compute-non-address-hash key-hash))
		   entry-type))

(define-integrableish (open-type! key-hash key=? rehash-after-gc? entry-type)
  (let ((hash-table-type
	 (open-type key-hash key=? rehash-after-gc? entry-type)))
    (memoize-hash-table-type! key-hash key=? rehash-after-gc? entry-type
			      hash-table-type)
    hash-table-type))

(define equal-hash-table-type)
(define key-ephemeral-eq-hash-table-type)
(define key-weak-eq-hash-table-type)
(define datum-weak-eq-hash-table-type)
(define key-ephemeral-eqv-hash-table-type)
(define key-weak-eqv-hash-table-type)
(define datum-weak-eqv-hash-table-type)
(define non-pointer-hash-table-type)
(define string-ci-hash-table-type)
(define string-hash-table-type)
(define strong-eq-hash-table-type)
(define strong-eqv-hash-table-type)
(define hash-table-type-constructors)
(define hash-metadata-table)
(add-boot-init!
 (lambda ()
   (set! key-ephemeral-eq-hash-table-type
	 (open-type eq-hash-mod eq? #t hash-table-entry-type:key-ephemeral))
   (set! make-key-ephemeral-eq-hash-table
	 (hash-table-constructor key-ephemeral-eq-hash-table-type))
   (set! hash-table-type-constructors (make-key-ephemeral-eq-hash-table))
   (set! hash-metadata-table (make-key-ephemeral-eq-hash-table))
   (memoize-hash-table-type! eq-hash-mod eq? #t
			     hash-table-entry-type:key-ephemeral
			     key-ephemeral-eq-hash-table-type)
   (open-type-constructor! hash-table-entry-type:strong)
   (open-type-constructor! hash-table-entry-type:key-weak)
   (open-type-constructor! hash-table-entry-type:datum-weak)
   (open-type-constructor! hash-table-entry-type:key&datum-weak)
   (open-type-constructor! hash-table-entry-type:key-ephemeral)
   (open-type-constructor! hash-table-entry-type:datum-ephemeral)
   (open-type-constructor! hash-table-entry-type:key&datum-ephemeral)
   (let ((make make-hash-table-type))	;For brevity...
     (set! equal-hash-table-type
	   (make equal-hash-mod equal? #t hash-table-entry-type:strong))
     (set! key-weak-eq-hash-table-type	;Open-coded
	   (open-type! eq-hash-mod eq? #t hash-table-entry-type:key-weak))
     (set! datum-weak-eq-hash-table-type ;Open-coded
	   (open-type! eq-hash-mod eq? #t hash-table-entry-type:datum-weak))
     (set! key-ephemeral-eqv-hash-table-type
	   (make eqv-hash-mod eqv? #t hash-table-entry-type:key-ephemeral))
     (set! key-weak-eqv-hash-table-type
	   (make eqv-hash-mod eqv? #t hash-table-entry-type:key-weak))
     (set! datum-weak-eqv-hash-table-type
	   (make eqv-hash-mod eqv? #t hash-table-entry-type:datum-weak))
     (set! non-pointer-hash-table-type	;Open-coded
	   (open-type! eq-hash-mod eq? #f hash-table-entry-type:strong))
     (set! string-ci-hash-table-type
	   (make string-ci-hash string-ci=? #t hash-table-entry-type:strong))
     (set! string-hash-table-type
	   (make string-hash string=? #t hash-table-entry-type:strong))
     (set! strong-eq-hash-table-type	;Open-coded
	   (open-type! eq-hash-mod eq? #t hash-table-entry-type:strong))
     (set! strong-eqv-hash-table-type
	   (make eqv-hash-mod eqv? #t hash-table-entry-type:strong)))
   unspecific))

(define make-equal-hash-table)
(define make-key-ephemeral-eq-hash-table)
(define make-key-weak-eq-hash-table)
(define make-datum-weak-eq-hash-table)
(define make-key-ephemeral-eqv-hash-table)
(define make-key-weak-eqv-hash-table)
(define make-datum-weak-eqv-hash-table)
(define make-non-pointer-hash-table)
(define make-string-ci-hash-table)
(define make-string-hash-table)
(define make-strong-eq-hash-table)
(define make-strong-eqv-hash-table)
(add-boot-init!
 (lambda ()
   (let-syntax ((init
		 (syntax-rules ()
		   ((init constructor type)
		    (set! constructor (hash-table-constructor type))))))
     (init make-equal-hash-table equal-hash-table-type)
     ;; This is done above.
     ;; (init make-key-ephemeral-eq-hash-table key-ephemeral-eq-hash-table-type)
     (init make-key-weak-eq-hash-table key-weak-eq-hash-table-type)
     (init make-datum-weak-eq-hash-table datum-weak-eq-hash-table-type)
     (init make-key-ephemeral-eqv-hash-table key-ephemeral-eqv-hash-table-type)
     (init make-key-weak-eqv-hash-table key-weak-eqv-hash-table-type)
     (init make-datum-weak-eqv-hash-table datum-weak-eqv-hash-table-type)
     (init make-non-pointer-hash-table non-pointer-hash-table-type)
     (init make-string-ci-hash-table string-ci-hash-table-type)
     (init make-string-hash-table string-hash-table-type)
     (init make-strong-eq-hash-table strong-eq-hash-table-type)
     (init make-strong-eqv-hash-table strong-eqv-hash-table-type))
   unspecific))

;;;; Compatibility with SRFI 69 and older MIT Scheme

(define (hash-table/constructor key-hash key=? rehash-after-gc? entry-type)
  (hash-table-constructor
   (make-hash-table-type key-hash key=? rehash-after-gc? entry-type)))

(define (strong-hash-table/constructor key-hash key=?
				       #!optional rehash-after-gc?)
  (hash-table/constructor key-hash
			  key=?
			  (if (default-object? rehash-after-gc?)
			      #f
			      rehash-after-gc?)
			  hash-table-entry-type:strong))

(define (weak-hash-table/constructor key-hash key=?
				     #!optional rehash-after-gc?)
  (hash-table/constructor key-hash
			  key=?
			  (if (default-object? rehash-after-gc?)
			      #f
			      rehash-after-gc?)
			  hash-table-entry-type:key-weak))

(define (srfi-69-hash-table-type #!optional key=? key-hash . args)
  (apply make-hash-table-type*
	 (if (default-object? key=?) equal? key=?)
	 (if (default-object? key-hash)
	     args
	     (cons* 'hash-function key-hash args))))

(define (hash-by-identity key #!optional modulus)
  (if (default-object? modulus)
      (eq-hash key)
      (begin
	(guarantee positive-fixnum? modulus)
	(eq-hash-mod key modulus))))

(define (hash-by-eqv key #!optional modulus)
  (if (default-object? modulus)
      (eqv-hash key)
      (begin
	(guarantee positive-fixnum? modulus)
	(eqv-hash-mod key modulus))))

(define (hash-by-equal key #!optional modulus)
  (if (default-object? modulus)
      (equal-hash key)
      (begin
	(guarantee positive-fixnum? modulus)
	(equal-hash-mod key modulus))))

(define (hash-table/lookup table key if-found if-not-found)
  (hash-table-ref table key if-not-found if-found))

(define (hash-table/modify! table key default procedure)
  (let ((datum (procedure (hash-table-ref/default table key default))))
    (%hash-table-set! table key datum)
    datum))

(define (hash-table-walk table procedure)
  (hash-table-for-each procedure table))

(define (hash-table-hash-function table)
  (table-type-key-hash (hash-table-type table)))

(define (hash-table-equivalence-function table)
  (table-type-key=? (hash-table-type table)))

(define (make-hash-table* type #!optional initial-size)
  (guarantee hash-table-type? type 'make-hash-table*)
  (%make-hash-table type initial-size 'make-hash-table*))

;;;; Miscellany

(define (check-arg object default predicate description procedure)
  (cond ((predicate object) object)
	((not object) default)
	(else (error:wrong-type-argument object description procedure))))

(define-integrable without-interruption with-thread-events-blocked)

(define (key=->comparator key= caller)
  (cond ((eqv? key= eq?) (make-eq-comparator))
	((eqv? key= eqv?) (make-eqv-comparator))
	((eqv? key= equal?) (make-equal-comparator))
	((eqv? key= string=?) (string-comparator))
	((eqv? key= string-ci=?) (string-ci-comparator))
	((eqv? key= int:=) (exact-integer-comparator))
	((eqv? key= char-set=) (char-set-comparator))
	(else (error:bad-range-argument key= caller))))