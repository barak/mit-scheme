(declare (usual-integrations))

;;; Library routines.  Mostly copied from the MIT Scheme runtime library.

(define-macro (ucode-type t) (microcode-type/name->code t))
(define-macro (ucode-primitive . p) (apply make-primitive-procedure p))

(define (append . lists)
  (let ((lists (reverse! lists)))
    (if (null? lists)
	'()
	(let loop ((accum (car lists)) (rest (cdr lists)))
	  (if (null? rest)
	      accum
	      (loop (let ((l1 (car rest)))
		      (cond ((pair? l1)
			     (let ((root (cons (car l1) #f)))
			       (let loop ((cell root) (next (cdr l1)))
				 (cond ((pair? next)
					(let ((cell* (cons (car next) #f)))
					  (set-cdr! cell cell*)
					  (loop cell* (cdr next))))
				       ((null? next)
					(set-cdr! cell accum))
				       (else
					(error:wrong-type-argument (car rest)
								   "list"
								   'APPEND))))
			       root))
			    ((null? l1)
			     accum)
			    (else
			     (error:wrong-type-argument (car rest) "list"
							'APPEND))))
		    (cdr rest)))))))


(define (assq key alist)
  (let loop ((alist* alist))
    (if (pair? alist*)
	(begin
	  (if (not (pair? (car alist*)))
	      (error:wrong-type-argument alist "alist" 'assq))
	  (if (eq? (car (car alist*)) key)
	      (car alist*)
	      (loop (cdr alist*))))
	(begin
	  (if (not (null? alist*))
	      (error:wrong-type-argument alist "alist" 'assq))
	  #F))))

(define (assv key alist)
  (let loop ((alist* alist))
    (if (pair? alist*)
	(begin
	  (if (not (pair? (car alist*)))
	      (error:wrong-type-argument alist "alist" 'assv))
	  (if (eqv? (car (car alist*)) key)
	      (car alist*)
	      (loop (cdr alist*))))
	(begin
	  (if (not (null? alist*))
	      (error:wrong-type-argument alist "alist" 'assv))
	  #F))))

(define (list . items)
  items)

(define (list-ref list index)
  (let ((tail (list-tail list index)))
    (if (not (pair? tail))
	(error:bad-range-argument index 'LIST-REF))
    (car tail)))

(define (list-tail list index)
  (guarantee-index/list index 'LIST-TAIL)
  (let loop ((list list) (index* index))
    (if (fix:zero? index*)
	list
	(begin
	  (if (not (pair? list))
	      (error:bad-range-argument index 'LIST-TAIL))
	  (loop (cdr list) (fix:- index* 1))))))


(define get #F)
(define put #F)

(let ((properties '()))
  (define (our-get x y)
    (let ((x-cut (assq x properties)))
      (if x-cut
	  (let ((value (assq y (cdr x-cut))))
	    (if value (cdr value) '()))
	  '())))
  (define (our-put x y z)
    (let ((x-cut (assq x properties)))
      (if x-cut
	  (let ((value (assq y (cdr x-cut))))
	    (if value
		(set-cdr! value z)
		(set-cdr! x-cut (cons (cons y z) (cdr x-cut)))))
	  (set! properties `((,x . ((,y . ,z))) ,@properties))))
    'OK)
  (set! get our-get)
  (set! put our-put))


(define (gensym)
  (system-pair-cons (ucode-type uninterned-symbol)
		    "G"
		    #F))

(define (reverse l)
  (let loop ((rest l) (so-far '()))
    (if (pair? rest)
	(loop (cdr rest) (cons (car rest) so-far))
	(begin
	  (if (not (null? rest))
	      (error:wrong-type-argument l "list" 'REVERSE))
	  so-far))))

(define (reverse! l)
  (let loop ((current l) (new-cdr '()))
    (if (pair? current)
	(let ((next (cdr current)))
	  (set-cdr! current new-cdr)
	  (loop next current))
	(begin
	  (if (not (null? current))
	      (error:wrong-type-argument l "list" 'REVERSE!))
	  new-cdr))))

(let-syntax
    ((mapping-procedure
      (macro (name combiner initial-value procedure first rest)
	(let ((name (string-upcase (symbol->string name))))
	  `(COND ((NULL? ,rest)
		  (LET 1-LOOP ((LIST ,first))
		    (IF (PAIR? LIST)
			(,combiner (,procedure (CAR LIST))
				   (1-LOOP (CDR LIST)))
			(BEGIN
			  (IF (NOT (NULL? LIST))
			      (ERROR:WRONG-TYPE-ARGUMENT ,first "list" ',name))
			  ,initial-value))))
		 ((NULL? (CDR ,rest))
		  (LET 2-LOOP ((LIST1 ,first) (LIST2 (CAR ,rest)))
		    (IF (AND (PAIR? LIST1) (PAIR? LIST2))
			(,combiner (,procedure (CAR LIST1) (CAR LIST2))
				   (2-LOOP (CDR LIST1) (CDR LIST2)))
			(BEGIN
			  (IF (AND (NOT (PAIR? LIST1))
				   (NOT (NULL? LIST1)))
			      (ERROR:WRONG-TYPE-ARGUMENT ,first "list" ',name))
			  (IF (AND (NOT (PAIR? LIST2))
				   (NOT (NULL? LIST2)))
			      (ERROR:WRONG-TYPE-ARGUMENT (CAR ,rest)
							 "list" ',name))
			  ,initial-value))))
		 (ELSE
		  (LET ((LISTS (CONS ,first ,rest)))
		    (LET N-LOOP ((LISTS* LISTS))
		      (LET PARSE-CARS
			  ((LISTS LISTS)
			   (LISTS* LISTS*)
			   (CARS '())
			   (CDRS '()))
			(COND ((NULL? LISTS*)
			       (,combiner (APPLY ,procedure (REVERSE! CARS))
					  (N-LOOP (REVERSE! CDRS))))
			      ((PAIR? (CAR LISTS*))
			       (PARSE-CARS (CDR LISTS)
					   (CDR LISTS*)
					   (CONS (CAR (CAR LISTS*)) CARS)
					   (CONS (CDR (CAR LISTS*)) CDRS)))
			      (ELSE
			       (IF (NOT (NULL? (CAR LISTS*)))
				   (ERROR:WRONG-TYPE-ARGUMENT (CAR LISTS) "list"
							      ',name))
			       ,initial-value)))))))))))

  (define (for-each procedure first . rest)
    (mapping-procedure for-each begin unspecific procedure first rest))

  (define (map procedure first . rest)
    (mapping-procedure map cons '() procedure first rest))

  ;;(define (map* initial-value procedure first . rest)
  ;;  (mapping-procedure map* cons initial-value procedure first rest))
  ;;
  ;;(define (append-map procedure first . rest)
  ;;  (mapping-procedure append-map append '() procedure first rest))
  ;;
  ;;(define (append-map* initial-value procedure first . rest)
  ;;  (mapping-procedure append-map* append initial-value procedure first rest))
  ;;
  ;;(define (append-map! procedure first . rest)
  ;;  (mapping-procedure append-map! append! '() procedure first rest))
  ;;
  ;;(define (append-map*! initial-value procedure first . rest)
  ;;  (mapping-procedure append-map*! append! initial-value procedure first rest))

;;; end LET-SYNTAX
  )



(define-integrable (guarantee-index/list object procedure)
  (if (not (index-fixnum? object))
      (guarantee-index/list/fail object procedure)))

(define (guarantee-index/list/fail object procedure)
  (error:wrong-type-argument object "valid list index"
			     procedure))

(define (eqv? x y)
  ;; EQV? is officially supposed to work on booleans, characters, and
  ;; numbers specially, but it turns out that EQ? does the right thing
  ;; for everything but numbers, so we take advantage of that.
  (or (eq? x y)
      (if (object-type? (object-type x) y)
	  (and (not (fix:fixnum? x))
	       (if (number? y)
		   (and (= x y)
			(boolean=? (exact? x) (exact? y)))
		   (and (object-type? (ucode-type vector) y)
			(fix:zero? (vector-length x))
			(fix:zero? (vector-length y)))))
	  (and (number? x)
	       (number? y)
	       (= x y)
	       (boolean=? (exact? x) (exact? y))))))

(define (equal? x y)
  (or (eq? x y)
      (if (object-type? (object-type x) y)
	  (cond ((pair? y)
		 (and (equal? (car x) (car y))
		      (equal? (cdr x) (cdr y))))
		((vector? y)
		 (let ((size (vector-length x)))
		   (and (fix:= size (vector-length y))
			(let loop ((index 0))
			  (or (fix:= index size)
			      (and (equal? (vector-ref x index)
					   (vector-ref y index))
				   (loop (fix:+ index 1))))))))
		((string? y)
		 (string=? x y))
		((number? y)
		 (and (= x y)
		      (boolean=? (exact? x) (exact? y))))
		;;((cell? y)
		;; (equal? (cell-contents x) (cell-contents y)))
		;;((bit-string? y)
		;; (bit-string=? x y))
		;;((pathname? x)
		;; (and (pathname? y)
		;;      (pathname=? x y)))
		(else false))
	  (and (number? x)
	       (number? y)
	       (= x y)
	       (boolean=? (exact? x) (exact? y))))))

(define (memq item items)
  (let loop ((items* items))
    (if (pair? items*)
	(if (eq? (car items*) item)
	    items*
	    (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list" 'MEMQ))
	  #f))))

(define-integrable (boolean=? x y)
  (let ((y y))
    (if x y (not y))))

;; string.scm

(define (string-copy string)
  (guarantee-string string 'string-copy)
  (let ((size (string-length string)))
    (let ((result (string-allocate size)))
      (substring-move-right! string 0 size result 0)
      result)))

(define (%string-append strings)
  (let ((result
	 (string-allocate
	  (let loop ((strings strings) (length 0))
	    (if (null? strings)
		length
		(begin
		  (guarantee-string (car strings) 'string-append)
		  (loop (cdr strings)
			(fix:+ (string-length (car strings)) length))))))))

    (let loop ((strings strings) (index 0))
      (if (null? strings)
	  result
	  (let ((size (string-length (car strings))))
	    (substring-move-right! (car strings) 0 size result index)
	    (loop (cdr strings) (fix:+ index size)))))))

(define (string-append . strings)
  (%string-append strings))

(define-integrable (guarantee-string object procedure)
  (if (not (string? object))
      (error:wrong-type-argument object "string" procedure)))

(define-integrable (guarantee-index/string object procedure)
  (if (not (index-fixnum? object))
      (guarantee-index/string/fail object procedure)))

(define (guarantee-index/string/fail object procedure)
  (error:wrong-type-argument object "valid string index"
			     procedure))

;; symbol.scm

(define (symbol-name symbol)
  (if (not (symbol? symbol))
      (error:wrong-type-argument symbol "symbol" 'SYMBOL-NAME))
  (system-pair-car symbol))

(define-integrable (symbol->string symbol)
  (string-copy (symbol-name symbol)))


;; apply.scm


(define (apply-2 f a0)
  (define (fail)
    (error "apply: Improper argument list" a0))

  (let-syntax ((apply-dispatch&bind
		(macro (var clause . clauses)
		  (if (null? clauses)
		      (cadr clause)
		      (let walk ((lv var)
				 (clause clause)
				 (clauses clauses))
			`(if (not (pair? ,lv))
			     (if (null? ,lv)
				 ,(cadr clause)
				 (fail))
			     ,(if (null? (cdr clauses))
				  (cadr (car clauses))
				  (let ((lv* (generate-uninterned-symbol))
					(av* (car clause)))
				    `(let ((,lv* (cdr ,lv))
					   (,av* (car ,lv)))
				       ,(walk lv* (car clauses)
					      (cdr clauses)))))))))))

    (apply-dispatch&bind a0
			 (v0 (f))
			 (v1 (f v0))
			 (v2 (f v0 v1))
			 (v3 (f v0 v1 v2))
			 (v4 (f v0 v1 v2 v3))
			 (v5 (f v0 v1 v2 v3 v4))
			 #|
			 (v6 (f v0 v1 v2 v3 v4 v5))
			 (v7 (f v0 v1 v2 v3 v4 v5 v6))
			 |#
			 (else
			  ((ucode-primitive apply) f a0)))))
  
(define (apply-entity-procedure self f . args)
  ;; This is safe because args is a newly-consed list
  ;; shared with no other code (modulo debugging).

  (define (splice! last next)
    (if (null? (cdr next))
	(set-cdr! last (car next))
	(splice! next (cdr next))))

  self					; ignored
  (apply-2 f
	   (cond ((null? args) '())
		 ((null? (cdr args))
		  (car args))
		 (else
		  (splice! args (cdr args))
		  args))))

(define apply
  (make-entity
   apply-entity-procedure
   (vector (fixed-objects-item 'ARITY-DISPATCHER-TAG)
	   (lambda ()
	     (error "apply needs at least one argument"))
	   (lambda (f)
	     (f))
	   apply-2)))