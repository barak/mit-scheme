#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; SVM assembler: rules compiler

(declare (usual-integrations))

(define (compile-assembler-rules pathname)
  (let ((dir (directory-pathname pathname)))
    (receive (coding-types key-abbrevs defns)
	(classify-records
	 (parse-inputs (read-file (pathname-default-type pathname "scm"))))
      (for-each (lambda (procedure)
		  (procedure coding-types))
		defns)
      (check-coding-types coding-types)
      (expand-implicit-coding-types coding-types)
      (let ((explicit
	     (keep-matching-items coding-types coding-type-explicit?)))
	(check-coding-types explicit)
	(check-code-allocations explicit)
	(for-each (lambda (coding-type)
		    (assign-defn-names coding-type key-abbrevs))
		  explicit)
	(write-scheme-header explicit
			     key-abbrevs
			     (merge-pathnames "svm1-opcodes.scm" dir))
	(write-c-header explicit
			key-abbrevs
			(merge-pathnames "svm1-defns.h" dir))
	(if #t				;debugging
	    (write-coding-types explicit (pathname-new-type pathname "exp")))
	(write-rt-coding-types explicit
			       (merge-pathnames "assembler-db.scm" dir))))))

(define (classify-records records)
  (let ((coding-types (make-queue))
	(key-abbrevs (make-queue))
	(defns (make-queue)))
    (for-each (lambda (record)
		(enqueue! (cond ((coding-type? record) coding-types)
				((key-abbrev? record) key-abbrevs)
				((procedure? record) defns)
				(else (error "Unknown record type:" record)))
			  record))
	      records)
    (values (queue->list coding-types)
	    (queue->list key-abbrevs)
	    (queue->list defns))))

(define (find-coding-type name coding-types #!optional error?)
  (or (find-matching-item coding-types
	(lambda (type)
	  (eq? (coding-type-name type) name)))
      (and (if (default-object? error?) #t error?)
	   (error "Unknown coding-type name:" name))))

;;;; Datatypes

(define-record-type <coding-type>
    (make-coding-type name n-bits start-index)
    coding-type?
  (name coding-type-name)
  (n-bits coding-type-n-bits)
  (start-index coding-type-start-index)
  (defns coding-type-defns set-coding-type-defns!))

(define (coding-type-code-limit type)
  (expt 2 (coding-type-n-bits type)))

(define (coding-type-explicit? type)
  (if (coding-type-n-bits type) #t #f))

(define-record-type <defn>
    (%make-defn coding-type pattern pvars has-code? coding)
    defn?
  (coding-type defn-coding-type set-defn-coding-type!)
  (pattern defn-pattern)
  (pvars defn-pvars)
  (has-code? defn-has-code?)
  (coding defn-coding)
  (code defn-code set-defn-code!)
  (name defn-name set-defn-name!))

(define (make-defn type-name pattern has-code? coding)
  (%make-defn type-name
	      pattern
	      (parse-pattern pattern)
	      has-code?
	      coding))

(define-record-type <key-abbrev>
    (make-key-abbrev keyword abbreviation)
    key-abbrev?
  (keyword key-abbrev-keyword)
  (abbreviation key-abbrev-abbreviation))

(define (check-coding-types coding-types)
  ;; Check that all coding-type names are unique.
  (do ((coding-types coding-types (cdr coding-types)))
      ((not (pair? coding-types)))
    (let ((name (coding-type-name (car coding-types))))
      (if (find-coding-type name (cdr coding-types) #f)
	  (error "Duplicate coding-type name:" name))))
  ;; Check that each definition is well-formed.
  (for-each (lambda (coding-type)
	      (for-each (lambda (defn) (check-defn defn coding-types))
			(coding-type-defns coding-type)))
	    coding-types)
  ;; Check that the coding-type references form an acyclic graph with
  ;; a single root.
  (check-coding-type-graph coding-types))

(define (check-defn defn coding-types)
  coding-types
  ;; Check for duplicate pattern variables.
  (do ((pvars (defn-pvars defn) (cdr pvars)))
      ((not (pair? pvars)))
    (if (there-exists? (cdr pvars)
	  (lambda (pv)
	    (eq? (pvar-name pv) (pvar-name (car pvars)))))
	(error "Duplicate pattern variable:" (car pvars))))
  ;; Check for missing or extra variable references in coding.
  (let ((pvars1 (defn-pvars defn))
	(pvars2 (defn-coding defn)))
    (if (not (and (fix:= (length pvars1) (length pvars2))
		  (for-all? pvars1 (lambda (pv1) (memq pv1 pvars2)))
		  (for-all? pvars2 (lambda (pv2) (memq pv2 pvars1)))))
	(error "Pattern/coding mismatch:" pvars1 pvars2)))
  ;; Check for incorrect use of code marker.
  (if (and (defn-has-code? defn)
	   (not (coding-type-explicit? (defn-coding-type defn))))
      (error "Code marker not allowed in implicit type.")))

(define (check-coding-type-graph coding-types)
  (let ((nodes
	 (map (lambda (coding-type)
		(vector coding-type '() '()))
	      coding-types))
	(queue (make-queue)))
    ;; Compute initial references.
    (let ((find-node
	   (lambda (coding-type)
	     (find-matching-item nodes
	       (lambda (node)
		 (eq? (vector-ref node 0) coding-type))))))
      (for-each (lambda (coding-type from)
		  (for-each (lambda (to)
			      (enqueue! queue (cons from (find-node to))))
			    (compute-references coding-type coding-types)))
		coding-types
		nodes))
    ;; Transitively close the reference graph.
    (queue-map! queue
      (lambda (p)
	(let loop ((from (car p)) (to (cdr p)))
	  (if (not (memq to (vector-ref from 1)))
	      (begin
		(vector-set! from 1 (cons to (vector-ref from 1)))
		(vector-set! to 2 (cons from (vector-ref to 2)))
		(for-each (lambda (to)
			    (enqueue! queue (cons from to)))
			  (vector-ref to 1))
		(for-each (lambda (to) (loop from to))
			  (vector-ref to 1))
		(for-each (lambda (from) (loop from to))
			  (vector-ref from 2)))))))
    ;; Check for cycles.
    (for-each (lambda (node)
		(if (memq node (vector-ref node 1))
		    (error "Cycle in coding-type graph:" node)))
	      nodes)
    ;; Check for single root.
    (let ((roots
	   (keep-matching-items nodes
	     (lambda (node)
	       (null? (vector-ref node 2))))))
      (if (not (pair? roots))
	  (error "No roots in coding-type graph."))
      (if (pair? (cdr roots))
	  (error "Multiple roots in coding-type graph:" roots)))))

(define (compute-references coding-type coding-types)
  ;; Check that all pvar types are bound, and return references.
  (do ((defns (coding-type-defns coding-type) (cdr defns))
       (refs '()
	     (do ((pvars (defn-pvars (car defns)) (cdr pvars))
		  (refs refs
			(let ((name (pvar-type (car pvars))))
			  (if (lookup-pvar-type name)
			      refs
			      (let ((ref (find-coding-type name coding-types)))
				(if (memq ref refs)
				    refs
				    (cons ref refs)))))))
		 ((not (pair? pvars)) refs))))
      ((not (pair? defns)) refs)))

(define (check-code-allocations coding-types)
  (for-each (lambda (coding-type)
	      (for-each (lambda (defn)
			  (if (defn-has-code? defn)
			      (if (not (defn-code defn))
				  (error "Missing code:" defn))
			      (if (defn-code defn)
				  (error "Unwanted code:" defn))))
			(coding-type-defns coding-type)))
	    coding-types))

;;;; Parsing

(define (parse-inputs inputs)
  (receive (inputs abbrevs)
      (expand-abbrevs inputs '())
    (map (lambda (input)
	   (let loop ((alist parser-alist))
	     (if (not (pair? alist))
		 (error "Unknown input:" input))
	     (if (and (pair? input)
		      (eq? (car input) (caaar alist))
		      (syntax-match? (cdaar alist) (cdr input)))
		 ((cdar alist) input abbrevs)
		 (loop (cdr alist)))))
	 inputs)))

(define (expand-abbrevs inputs abbrevs)
  (receive (abbrev-defs inputs) (split-list inputs abbrev-def?)
    (let ((abbrevs
	   (map* abbrevs
		 (lambda (abbrev-def)
		   (cons `(',(caadr abbrev-def) ,@(cdadr abbrev-def))
			 (eval (caddr abbrev-def)
			       (make-top-level-environment))))
		 abbrev-defs))
	  (any-expansions? #f))
      (let ((outputs
	     (append-map (lambda (input)
			   (let ((abbrev
				  (find-matching-item abbrevs
				    (lambda (abbrev)
				      (syntax-match? (car abbrev) input)))))
			     (if abbrev
				 (begin
				   (set! any-expansions? #t)
				   ((cdr abbrev) input))
				 (list input))))
			 inputs)))
	(if any-expansions?
	    (expand-abbrevs outputs abbrevs)
	    (values outputs abbrevs))))))

(define (split-list items predicate)
  (let loop ((items items) (true '()) (false '()))
    (if (pair? items)
	(if (predicate (car items))
	    (loop (cdr items) (cons (car items) true) false)
	    (loop (cdr items) true (cons (car items) false)))
	(values (reverse! true) (reverse! false)))))

(define (abbrev-def? input)
  (syntax-match? '('DEFINE-ABBREVIATION (SYMBOL * DATUM) EXPRESSION)
		 input))

(define (define-parser keyword pattern parser)
  (let loop ((ps parser-alist))
    (if (pair? ps)
	(if (eq? (caaar ps) keyword)
	    (begin
	      (set-cdr! (caar ps) pattern)
	      (set-cdr! (car ps) parser))
	    (loop (cdr ps)))
	(begin
	  (set! parser-alist
		(cons (cons (cons keyword pattern) parser)
		      parser-alist))
	  unspecific))))

(define parser-alist
  '())

(define-parser 'DEFINE-KEYWORD-ABBREVIATION '(SYMBOL SYMBOL)
  (lambda (input abbrevs)
    abbrevs
    (make-key-abbrev (cadr input) (caddr input))))

(define-parser 'DEFINE-IMPLICIT-CODING-TYPE '(SYMBOL * DATUM)
  (lambda (input abbrevs)
    (let ((coding-type (make-coding-type (cadr input) #f 0)))
      (parse-coding-type-body coding-type (cddr input) abbrevs)
      coding-type)))

(define-parser 'DEFINE-EXPLICIT-CODING-TYPE
  `(SYMBOL (,exact-positive-integer? ? ,exact-nonnegative-integer?) * DATUM)
  (lambda (input abbrevs)
    (let ((name (cadr input))
	  (n-bits (caaddr input))
	  (start-index (if (pair? (cdaddr input)) (car (cdaddr input)) 0))
	  (body (cdddr input)))
      (let ((code-limit (expt 2 n-bits)))
	(if (not (< start-index code-limit))
	    (error:bad-range-argument start-index
				      'DEFINE-EXPLICIT-CODING-TYPE)))
      (let ((coding-type (make-coding-type name n-bits start-index)))
	(parse-coding-type-body coding-type body abbrevs)
	coding-type))))

(define (parse-coding-type-body coding-type body abbrevs)
  (set-coding-type-defns!
   coding-type
   (map (lambda (input)
	  (if (not (syntax-match? '('DEFINE-CODE-SEQUENCE DATUM * DATUM)
				  input))
	      (error "Illegal sequence definition:" input))
	  (parse-code-sequence coding-type (cadr input) (cddr input)))
	(receive (body abbrevs) (expand-abbrevs body abbrevs)
	  abbrevs
	  body))))

(define-parser 'DEFINE-CODE-SEQUENCE '(SYMBOL DATUM * DATUM)
  (lambda (input abbrevs)
    abbrevs
    (lambda (coding-types)
      (let ((coding-type (find-coding-type (cadr input) coding-types)))
	(let ((defn
		(parse-code-sequence coding-type (caddr input) (cdddr input))))
	  (set-coding-type-defns!
	   coding-type
	   (append! (coding-type-defns coding-type) (list defn)))
	  defn)))))

(define (parse-code-sequence coding-type pattern coding)
  (let ((pvars (parse-pattern pattern)))
    (receive (has-code? coding)
	(if (coding-type-explicit? coding-type)
	    (if (and (pair? coding)
		     (equal? (car coding) '(NO-CODE)))
		(values #f (cdr coding))
		(values #t coding))
	    (values #f coding))
      (%make-defn coding-type
		  pattern
		  pvars
		  has-code?
		  (map (lambda (item)
			 (guarantee-symbol item #f)
			 (or (find-matching-item pvars
			       (lambda (pv)
				 (eq? (pvar-name pv) item)))
			     (error "Missing name reference:" item)))
		       coding)))))

;;;; Expansion

(define (expand-implicit-coding-types coding-types)
  (let ((to-substitute (make-queue))
	(to-expand (make-queue))
	(next-pass (make-queue)))
    (let ((queue-ct
	   (lambda (type)
	     (if (independent-coding-type? type coding-types)
		 (if (coding-type-explicit? type)
		     (assign-defn-codes type)
		     (enqueue! to-substitute type))
		 (enqueue! to-expand type)))))
      (for-each queue-ct coding-types)
      (queue-map! to-substitute
	(lambda (type1)
	  (queue-map! to-expand
	    (lambda (type2)
	      (expand-coding-type type1 type2)
	      (enqueue! next-pass type2)))
	  (queue-map! next-pass queue-ct))))
    (queue-map! to-expand
      (lambda (type)
	(if (not (coding-type-explicit? type))
	    (error "Unexpanded coding type:" type))
	(assign-defn-codes type)))))

(define (independent-coding-type? type coding-types)
  (let ((implicit-types
	 (delete-matching-items coding-types coding-type-explicit?)))
    (for-all? (coding-type-defns type)
      (lambda (defn)
	(not (there-exists? (defn-pvars defn)
	       (lambda (pv)
		 (find-coding-type (pvar-type pv) implicit-types #f))))))))

(define (expand-coding-type to-substitute to-expand)
  (let ((type-name (coding-type-name to-substitute)))
    (let loop ()
      (let ((any-changes? #f))
	(set-coding-type-defns!
	 to-expand
	 (append-map! (lambda (defn)
			(let ((pv
			       (find-matching-item (defn-pvars defn)
				 (lambda (pv)
				   (eq? (pvar-type pv) type-name)))))
			  (if pv
			      (begin
				(set! any-changes? #t)
				(map (lambda (defn*)
				       (specialize-defn defn pv defn*))
				     (coding-type-defns to-substitute)))
			      (list defn))))
		      (coding-type-defns to-expand)))
	(if any-changes?
	    (loop))))))

(define (assign-defn-codes coding-type)
  (let ((code-limit (coding-type-code-limit coding-type)))
    (do ((defns (coding-type-defns coding-type) (cdr defns))
	 (code (coding-type-start-index coding-type)
	       (let ((defn (car defns)))
		 (if (defn-has-code? defn)
		     (begin
		       (if (not (< code code-limit))
			   (error "Too many codes assigned:" coding-type))
		       (set-defn-code! defn code)
		       (+ code 1))
		     (begin
		       (set-defn-code! defn #f)
		       code)))))
	((not (pair? defns)) unspecific))))

(define (coding-type-end-index coding-type)
  (+ (coding-type-start-index coding-type)
     (count-matching-items (coding-type-defns coding-type) defn-has-code?)))

(define (specialize-defn defn pv defn*)
  (let ((defn* (maybe-rename defn* defn)))
    (make-defn (defn-coding-type defn)
	       (rewrite-pattern (lambda (pv*)
				  (if (eq? (pvar-name pv*) (pvar-name pv))
				      (defn-pattern defn*)
				      pv*))
				(defn-pattern defn))
	       (defn-has-code? defn)
	       (append-map (lambda (item)
			     (if (and (pvar? item)
				      (eq? (pvar-name item) (pvar-name pv)))
				 (defn-coding defn*)
				 (list item)))
			   (defn-coding defn)))))

(define (maybe-rename defn defn*)
  (let ((alist
	 (let loop
	     ((pvars (defn-pvars defn))
	      (pvars* (defn-pvars defn*)))
	   (if (pair? pvars)
	       (let ((pv (car pvars))
		     (clash?
		      (lambda (name)
			(there-exists? pvars*
			  (lambda (pv)
			    (eq? (pvar-name pv) name)))))
		     (k
		      (lambda (pv)
			(loop (cdr pvars) (cons pv pvars*)))))
		 (if (clash? (pvar-name pv))
		     (let find-rename ((n 1))
		       (let ((rename (symbol (pvar-name pv) '- n)))
			 (if (clash? rename)
			     (find-rename (+ n 1))
			     (let ((pv* (make-pvar rename (pvar-type pv))))
			       (cons (cons pv pv*)
				     (k pv*))))))
		     (k pv)))
	       '()))))
    (if (null? alist)
	defn
	(let ((rename-pv
	       (lambda (item)
		 (let ((p
			(and (pvar? item)
			     (assq item alist))))
		   (if p
		       (cdr p)
		       item)))))
	  (make-defn (defn-coding-type defn)
		     (rewrite-pattern rename-pv (defn-pattern defn))
		     (defn-has-code? defn)
		     (map rename-pv (defn-coding defn)))))))

(define (rewrite-pattern procedure pattern)
  (let loop ((pattern pattern))
    (cond ((pvar? pattern) (procedure pattern))
	  ((pair? pattern) (map loop pattern))
	  (else pattern))))

;;;; Name assignment

(define (assign-defn-names coding-type key-abbrevs)
  (let ((defns (coding-type-defns coding-type)))
    ;; Generate names as lists of symbols.
    (for-each (lambda (defn)
		(set-defn-name! defn
				(pattern->defn-name (defn-pattern defn)
						    key-abbrevs)))
	      defns)
    ;; Eliminate redundant items in names.
    (for-each (lambda (defns)
		(if (pair? (cdr defns))
		    (begin
		      (delete-shared-prefixes defns)
		      (for-each (lambda (item)
				  (delete-corresponding-name-items defns item))
				(deleteable-name-items)))
		    (let ((defn (car defns)))
		      (set-defn-name!
		       defn
		       (delete-matching-items! (defn-name defn)
			 deleteable-name-item?)))))
	      (group-defns-by-prefix defns))
    ;; Join name items into hyphen-separated symbols.
    (for-each (lambda (defn)
		(set-defn-name! defn (defn-name->symbol (defn-name defn))))
	      defns)))

(define (pattern->defn-name pattern key-abbrevs)
  (let ((items
	 (let loop ((pattern pattern))
	   (cond ((symbol? pattern)
		  (list (map-key-abbrevs pattern key-abbrevs)))
		 ((pvar? pattern)
		  (list (let ((pvt (lookup-pvar-type (pvar-type pattern))))
			  (if pvt
			      (pvt-abbreviation pvt)
			      (map-key-abbrevs (pvar-type pattern)
					       key-abbrevs)))))
		 ((pair? pattern) (append-map! loop pattern))
		 (else '())))))
    (let trim-tail ((items items))
      (if (and (fix:>= (length items) 3)
	       (let ((l (reverse items)))
		 (eq? (car l) (cadr l))))
	  (trim-tail (except-last-pair! items))
	  items))))

(define (map-key-abbrevs keyword key-abbrevs)
  (let ((key-abbrev
	 (find-matching-item key-abbrevs
	   (lambda (key-abbrev)
	     (eq? (key-abbrev-keyword key-abbrev) keyword)))))
    (if key-abbrev
	(key-abbrev-abbreviation key-abbrev)
	keyword)))

(define (defn-name->symbol items)
  (if (pair? items)
      (apply symbol
	     (car items)
	     (append-map (lambda (item) (list '- item))
			 (cdr items)))
      '||))

(define (group-defns-by-prefix defns)
  (let ((groups '()))
    (for-each (lambda (defn)
		(let ((name (defn-name defn)))
		  (let ((p (assq (car name) groups)))
		    (if p
			(set-cdr! p (cons defn (cdr p)))
			(begin
			  (set! groups (cons (list (car name) defn) groups))
			  unspecific)))))
	      defns)
    (reverse! (map (lambda (group) (reverse! (cdr group))) groups))))

(define (delete-shared-prefixes defns)
  (let ((names (map defn-name defns)))
    (let ((n
	   (length
	    (fold-left (lambda (a b)
			 (let join ((a a) (b b))
			   (if (and (pair? a) (pair? b) (eqv? (car a) (car b)))
			       (cons (car a) (join (cdr a) (cdr b)))
			       '())))
		       (car names)
		       (cdr names)))))
      (for-each (lambda (defn name)
		  (set-defn-name! defn (cons (car name) (list-tail name n))))
		defns
		names))))

(define (delete-corresponding-name-items defns to-delete)
  (let loop ((lower-limit 1))
    (let ((indices
	   (map (lambda (defn)
		  (index-of-deleted-name-item to-delete
					      (defn-name defn)
					      lower-limit))
		defns)))
      (if (for-all? indices (lambda (i) i))
	  (loop (if (apply = indices)
		    (let ((index (car indices)))
		      (let ((names
			     (map (lambda (defn)
				    (delete-name-item (defn-name defn) index))
				  defns)))
			(if (distinct-names? names)
			    (begin
			      (for-each set-defn-name! defns names)
			      index)
			    (fix:+ index 1))))
		    (fix:+ (apply min indices) 1)))))))

(define (index-of-deleted-name-item to-delete items lower-limit)
  (let loop ((items items) (index 0))
    (and (pair? items)
	 (if (and (fix:>= index lower-limit)
		  (eq? (car items) to-delete))
	     index
	     (loop (cdr items) (fix:+ index 1))))))

(define (delete-name-item items index)
  (let loop ((items items) (i 0))
    (if (fix:< i index)
	(cons (car items) (loop (cdr items) (fix:+ i 1)))
	(cdr items))))

(define (distinct-names? names)
  (if (pair? names)
      (if (member (car names) (cdr names))
	  #f
	  (distinct-names? (cdr names)))
      #t))

(define (deleteable-name-item? item)
  (there-exists? (pvar-types)
    (lambda (pvt)
      (eq? (pvt-abbreviation pvt) item))))

(define (deleteable-name-items)
  (map pvt-abbreviation (pvar-types)))

;;;; Output

(define (write-scheme-header coding-types key-abbrevs pathname)
  (wrap-scheme-output "Opcodes for SVM version 1" pathname
    (lambda (port)
      (for-each
       (lambda (coding-type)
	 (let ((limit
		(apply max
		       (map defn-name-length
			    (coding-type-defns coding-type)))))
	   (for-each
	    (lambda (defn)
	      (let ((opcode (defn-code defn)))
		(if opcode
		    (begin
		      (write-string "(define-integrable svm1-" port)
		      (write (map-key-abbrevs (coding-type-name coding-type)
					      key-abbrevs)
			     port)
		      (write-string ":" port)
		      (write (defn-name defn) port)
		      (write-spaces (fix:- limit (defn-name-length defn))
				    port)
		      (write-string " #x" port)
		      (if (fix:< opcode #x10)
			  (write-char #\0 port))
		      (write-string (number->string opcode 16) port)
		      (write-string ")" port)
		      (newline port)))))
	    (coding-type-defns coding-type))))
       coding-types))))

(define (defn-name-length defn)
  (string-length (symbol-name (defn-name defn))))

(define (wrap-scheme-output title pathname generator)
  (call-with-output-file pathname
    (lambda (port)
      (write-string "#| -*-Scheme-*-\n\n" port)
      (write-copyright+license pathname port)
      (newline port)
      (write-string "|#\n\n" port)
      (write-string ";;;; " port)
      (write-string title port)
      (write-string "\n\n" port)
      (write-string "(declare (usual-integrations))\n" port)
      (write-string "\f\n" port)
      (generator port))))

(define (write-c-header coding-types key-abbrevs pathname)
  (wrap-c-header "Instructions for SVM version 1" pathname
    (lambda (port)
      (for-each (lambda (p)
		  (write-string "#define SVM1_REG_" port)
		  (write-c-name (car p) #t port)
		  (write-string " " port)
		  (write (cdr p) port)
		  (newline port))
		fixed-registers)
      (newline port)
      (for-each
       (lambda (coding-type)
	 (let ((prefix
		(string-append
		 "SVM1_"
		 (name->c-string (map-key-abbrevs (coding-type-name coding-type)
						  key-abbrevs)
				 #t)
		 "_"))
	       (long-form?
		(there-exists? (coding-type-defns coding-type)
		  (lambda (defn)
		    (pair? (defn-coding defn))))))
	   (write-c-code-macro prefix
			       "START_CODE"
			       (coding-type-start-index coding-type)
			       port)
	   (write-c-code-macro prefix
			       "END_CODE"
			       (coding-type-end-index coding-type)
			       port)
	   (newline port)
	   (write-c-type-bindings prefix coding-type port)
	   (newline port)
	   (for-each (let ((proc
			    (if long-form?
				write-c-opcode+decoder
				write-c-opcode)))
		       (lambda (defn)
			 (if (defn-has-code? defn)
			     (proc prefix defn port))))
		     (coding-type-defns coding-type))
	   (if (not long-form?)
	       (newline port))))
       coding-types))))

(define (wrap-c-header title pathname generator)
  (call-with-output-file pathname
    (lambda (port)
      (let ((cs
	     (string-append "SCM_"
			    (name-string->c-string (pathname-name pathname) #t)
			    "_H")))
	(write-string "/* -*-C-*-\n\n" port)
	(write-copyright+license pathname port)
	(newline port)
	(write-string "*/\n\n" port)
	(write-string "/* " port)
	(write-string title port)
	(write-string " */\n\n" port)
	(write-string "#ifndef " port)
	(write-string cs port)
	(write-string "\n" port)
	(write-string "#define " port)
	(write-string cs port)
	(write-string " 1\n\n" port)
	(generator port)
	(write-string "#endif /* not " port)
	(write-string cs port)
	(write-string " */\n" port)))))

(define (write-c-type-bindings prefix coding-type port)
  (write-string "#define " port)
  (write-string prefix port)
  (write-string "BINDINGS(binder) \\" port)
  (newline port)
  (write-c-macro-body (lambda (defn port)
			(write-string "  " port)
			(write-string "binder (" port)
			(write-string prefix port)
			(write-c-name (defn-name defn) #t port)
			(write-string ", " port)
			(write-c-name (defn-name defn) #f port)
			(write-string ")" port))
		      (keep-matching-items (coding-type-defns coding-type)
			defn-has-code?)
		      port))

(define (write-c-opcode+decoder prefix defn port)
  (write-c-opcode prefix defn port)
  (let ((coding (defn-coding defn)))
    (if (pair? coding)
	(begin
	  (write-string "#define DECODE_" port)
	  (write-string prefix port)
	  (write-c-name (defn-name defn) #t port)
	  (write-string "(" port)
	  (write-c-name (pvar-name (car coding)) #f port)
	  (for-each (lambda (pv)
		      (write-string ", " port)
		      (write-c-name (pvar-name pv) #f port))
		    (cdr coding))
	  (write-string ") \\" port)
	  (newline port)
	  (write-c-macro-body (lambda (pv port)
				(write-string "  DECODE_" port)
				(write-c-name (pvar-type pv) #t port)
				(write-string " (" port)
				(write-c-name (pvar-name pv) #f port)
				(write-string ")" port))
			      coding
			      port))))
  (newline port))

(define (write-c-opcode prefix defn port)
  (write-c-code-macro prefix
		      (name->c-string (defn-name defn) #t)
		      (defn-code defn)
		      port))

(define (write-c-code-macro prefix name code port)
  (write-string "#define " port)
  (write-string prefix port)
  (write-string name port)
  (write-string " " port)
  (write-c-hex-code code port)
  (newline port))

(define (write-c-hex-code n port)
  (write-string "0x" port)
  (write-string (string-pad-left (number->string n 16) 2 #\0) port))

(define (write-c-macro-body write-item items port)
  (for-each (lambda (item)
	      (write-item item port)
	      (write-string "; \\" port)
	      (newline port))
	    (except-last-pair items))
  (write-item (last items) port)
  (newline port))

(define (write-copyright+license pathname port)
  pathname
  (write-string "DO NOT EDIT: this file was generated by a program." port)
  (newline port)
  (newline port)
  (write-mit-scheme-copyright port)
  (newline port)
  (newline port)
  (write-mit-scheme-license port)
  (newline port))

(define (name->c-string name upcase?)
  (name-string->c-string (symbol-name name) upcase?))

(define (name-string->c-string name upcase?)
  (call-with-output-string
    (lambda (port)
      (write-c-name-string name upcase? port))))

(define (write-c-name name upcase? port)
  (write-c-name-string (symbol-name name) upcase? port))

(define (write-c-name-string name upcase? port)
  (let ((e (string-length name))
	(recase
	 (lambda (s)
	   (if upcase? s (string-downcase s)))))
    (let loop ((i 0))
      (if (fix:< i e)
	  (let ((c (string-ref name i)))
	    (cond ((char-alphanumeric? c)
		   (write-char (if upcase?
				   (char-upcase c)
				   (char-downcase c))
			       port)
		   (loop (fix:+ i 1)))
		  ((fix:= i 0)
		   (write-string (recase (case c
					   ((#\+) "ADD")
					   ((#\-) "SUBTRACT")
					   ((#\*) "MULTIPLY")
					   ((#\/) "DIVIDE")
					   (else (error "Unknown char:" c))))
				 port)
		   (loop (fix:+ i 1)))
		  ((char=? c #\-)
		   (if (and (fix:< (fix:+ i 1) e)
			    (char=? (string-ref name (fix:+ i 1)) #\>))
		       (begin
			 (write-string (recase "_TO_") port)
			 (loop (fix:+ i 2)))
		       (begin
			 (write-char #\_ port)
			 (loop (fix:+ i 1)))))
		  ((char=? c #\?)
		   (write-string (recase "_P") port)
		   (loop (fix:+ i 1)))
		  ((char=? c #\!)
		   (write-string (recase "_X") port)
		   (loop (fix:+ i 1)))
		  (else
		   (error "Unknown char:" c))))))))

(define (write-spaces n port)
  (do ((i 0 (fix:+ i 1)))
      ((not (fix:< i n)))
    (write-char #\space port)))

(define (write-coding-types coding-types pathname)
  (wrap-scheme-output "Expanded coding types" pathname
    (lambda (port)
      (if (pair? coding-types)
	  (begin
	    (write-coding-type (car coding-types) port)
	    (for-each (lambda (coding-type)
			(newline port)
			(newline port)
			(write-coding-type coding-type port))
		      (cdr coding-types)))))))

(define (write-coding-type coding-type port)
  (let ((indentation 2))
    (write-string "(" port)
    (write (if (coding-type-explicit? coding-type)
	       'DEFINE-EXPLICIT-CODING-TYPE
	       'DEFINE-IMPLICIT-CODING-TYPE)
	   port)
    (write-string " " port)
    (write (coding-type-name coding-type) port)
    (if (coding-type-explicit? coding-type)
	(begin
	  (write-string " " port)
	  (write (coding-type-start-index coding-type) port)))
    (newline port)
    (for-each (lambda (defn)
		(newline port)
		(write-defn defn indentation port))
	      (coding-type-defns coding-type))
    (write-spaces indentation port)
    (write-string ")" port)))

(define (write-defn defn indentation port)
  (write-spaces indentation port)
  (write-string ";; " port)
  (write (defn-name defn) port)
  (newline port)
  (write-spaces indentation port)
  (write-string "(define-code-sequence " port)
  (newline port)
  (let ((indentation (+ indentation 2)))
    (pretty-print (defn-pattern defn) port #t indentation)
    (if (defn-has-code? defn)
	(begin
	  (newline port)
	  (write-spaces indentation port)
	  (write (defn-code defn) port)))
    (for-each (lambda (pv)
		(newline port)
		(write-spaces indentation port)
		(write (pvar-name pv) port))
	      (defn-coding defn)))
  (write-char #\) port)
  (newline port))

(define (write-rt-coding-types coding-types pathname)
  (wrap-scheme-output "Opcodes for SVM version 1" pathname
    (lambda (port)
      (if (pair? coding-types)
	  (begin
	    (pretty-print (rt-coding-type-constructor (car coding-types)) port)
	    (for-each (lambda (coding-type)
			(newline port)
			(newline port)
			(pretty-print (rt-coding-type-constructor coding-type)
				      port))
		      (cdr coding-types)))))))

(define (rt-coding-type-constructor coding-type)
  `(MAKE-RT-CODING-TYPE
    ',(coding-type-name coding-type)
    (LIST
     ,@(map (lambda (defn)
	      `(MAKE-RT-DEFN ',(defn-name defn)
			     ,(if (defn-has-code? defn) `,(defn-code defn) `#F)
			     ',(defn-pattern defn)
			     ,(rt-defn-encoder-constructor defn)
			     ,(rt-defn-decoder-constructor defn)))
	    (coding-type-defns coding-type)))))

(define (rt-defn-encoder-constructor defn)
  `(LAMBDA (INSTANCE WRITE-BYTE)
   ,@(if (null? (defn-coding defn))
       '(INSTANCE WRITE-BYTE UNSPECIFIC)
       (map (lambda (item)
	      (let ((pval `(RT-INSTANCE-PVAL ',(pvar-name item) INSTANCE))
		    (pvt (lookup-pvar-type (pvar-type item))))
		(if pvt
		    `(,(pvt-encoder pvt) ,pval WRITE-BYTE)
		    `(LET ((PVAL ,pval))
		       ((RT-INSTANCE-ENCODER PVAL) PVAL WRITE-BYTE)))))
	    (defn-coding defn)))))

(define (rt-defn-decoder-constructor defn)
  (let ((pvars (defn-pvars defn)))
    (let ((n-pvars (length pvars))
	  (body
	   (lambda (pv)
	     (let ((pvt (lookup-pvar-type (pvar-type pv))))
	       (if pvt
		   `(,(pvt-decoder pvt) READ-BYTE)
		   `(DECODE-RT-CODING-TYPE ',(pvar-type pv)
					   READ-BYTE
					   CODING-TYPES))))))
      `(LAMBDA (READ-BYTE)
      ,@(cond((fix:= n-pvars 0)
	      `(READ-BYTE CODING-TYPES '()))
	     ((fix:= n-pvars 1)
	      `((LIST ,(body (car pvars)))))
	     (else
	      `((LET* ,(map (lambda (pv)
			      `(,(symbol 'V (pvar-index pv pvars)) ,(body pv)))
			    (defn-coding defn))
		 (LIST ,@(let loop ((i 0))
			   (if (fix:< i n-pvars)
			       (cons (symbol 'V i) (loop (fix:+ i 1)))
			       '())))))))))))

(define (pvar-index pv pvars)
  (let loop ((pvars pvars) (index 0))
    (if (not (pair? pvars))
	(error:bad-range-argument pv 'PVAR-INDEX))
    (if (eq? (car pvars) pv)
	index
	(loop (cdr pvars) (fix:+ index 1)))))