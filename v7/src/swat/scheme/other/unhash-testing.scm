;;; -*- Scheme -*-

(declare (usual-integrations)
	 (integrate-external "/scheme/700/runtime/hash"))

(define foo-1 'invalid-rehash)
(define foo-2 'invalid-bucket)
(define foo-3 'unhash-table)
(define (foo)
  (set! foo-1 3)
  (set! foo-2 3)
  (set! foo-3 3))

(define *do-validation?* #T)

  (define (count-unhash-table uht)
    (let ((count 0))
      (do ((i 0 (+ i 1)))
	  ((= i (vector-length uht)) count)
	(set! count (+ count (length (cdr (vector-ref uht i))))))))

  (define (valid-bucket-contents? table when y full?)
    (define (valid? x)
      (and (list? x)
	   (or (null? x)
	       (and (pair? x)
		    (weak-pair? (car x))
		    (let ((hash-number (weak-cdr (car x))))
		      (and (number? hash-number)
			   (or (not full?)
			       (not (weak-car (car x)))
			       (= hash-number (hash (weak-car (car x)) table))
			       (let ((table (car (cons table #f)))
				     (x (car (cons x #f)))
				     (y (car (cons y #f)))
				     (forty-two (car (cons 42 #f))))
				 (write-line
				  (list "invalid rehash" when table hash-number (car x)))
				 (table)
				 (list table x y forty-two)))))
		    (valid? (cdr x))))))
    (or (valid? y)
	(begin 
	  (write-line (list "Invalid unhash bucket" table when))
	  (+ 2 foo-2))))

  (define (validate table when full?)
    (if *Do-Validation?*
	(fluid-let ((*Do-Validation?* #F))
	  (let ((uht (hash-table/unhash-table table)))
	    (for-each
	     (lambda (bucket)
	       (valid-bucket-contents? table when (cdr bucket) full?))
	     (vector->list uht))))
	'OK))

  (define (show-unhash-table uht)
    (for-each
     (lambda (x) (if (not (number? x))
		     (begin
		       (write-line (list "show-unhash-table: not a number" x))
		       (+ 3 foo-3)))
	     (display x)
	     (display #\space))
     (reduce append '()
	     (map (lambda (bucket)
		    (map weak-cdr (cdr bucket)))
		  (vector->list uht))))
    (newline))

  (define (our-rehash-all-gc-daemon)
    (let loop ((l all-hash-tables)
	       (n (weak-cdr all-hash-tables)))
      (cond ((null? n)
	     (weak-set-cdr! l n))
	    ((not (weak-pair/car? n))
	     (loop l (weak-cdr n)))
	    (else
	     (weak-set-cdr! l n)
	     (let* ((table (weak-car n)))
	       (validate table 'before #F)
	       ; (write-line (list 'before (count-unhash-table uht)))
	       (hash-table/rehash table)
	       (validate table 'after #T)
	       ; (write-line (list 'after (count-unhash-table uht)))
	       ; (show-unhash-table uht)
	       (loop n (weak-cdr n)))))))

  (delq! rehash-all-gc-daemon
	 (access gc-daemons (->environment '(runtime gc-daemons))))
  (add-gc-daemon! our-rehash-all-gc-daemon)

  (let ((old-hash object-hash))
    (set! object-hash
	  (lambda (object #!optional table insert?)
	    (let ((table
		   (if (default-object? table)
		       default-hash-table
		       (begin
			 (if (not (hash-table? table))
			     (error:wrong-type-argument table
							"object-hash table"
							'OBJECT-HASH))
			 table)))
		  (insert? (or (default-object? insert?) insert?)))
	      (let ((result (old-hash object table insert?)))
		(validate table 'hash #F)
		result)))))

  (let ((old-unhash object-unhash))
    (set! object-unhash
	  (lambda (number #!optional table)
	    (let ((table
		   (if (default-object? table)
		       default-hash-table
		       (begin
			 (if (not (hash-table? table))
			     (error:wrong-type-argument table
							"object-hash table"
							'OBJECT-UNHASH))
			 table))))
	      (let ((result (old-unhash number table)))
		(validate table 'unhash #T)
		result)))))