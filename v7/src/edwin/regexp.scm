;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Regular Expressions

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define char-search-forward)
(define search-forward)
(define re-search-forward)
(define char-search-backward)
(define search-backward)
(define re-search-backward)
(define char-match-forward)
(define match-forward)
(define re-match-forward)
(define char-match-backward)
(define match-backward)
(define re-match-start)
(define re-match-end)

(define regular-expression-package
  (make-environment
    (let-syntax ()

(define-macro (define-search name key-name searcher compile-key
		mark-limit mark-compare)
  `(SET! ,name
	 (NAMED-LAMBDA (,name ,key-name #!OPTIONAL START END LIMIT?)
	   (COND ((UNASSIGNED? START)
		  (SET! START (CURRENT-POINT))
		  (SET! END (,mark-limit START))
		  (SET! LIMIT? #!FALSE))
		 ((UNASSIGNED? END)
		  (SET! END (,mark-limit START))
		  (SET! LIMIT? #!FALSE))
		 (ELSE
		  (IF (NOT (,mark-compare START END))
		      (ERROR ,(string-append (symbol->string name)
					     ": Marks incorrectly related")
			     START END))
		  (IF (UNASSIGNED? LIMIT?) (SET! LIMIT? #!FALSE))))
	   (OR (,searcher (MARK-GROUP START)
			  (MARK-INDEX START)
			  (MARK-INDEX END)
			  (,compile-key ,key-name))
	       (LIMIT-MARK-MOTION LIMIT? END)))))

(define-macro (make-primitive name)
  (make-primitive-procedure name))

(define match-group)
(define registers (make-vector 20))

(set! re-match-start
(named-lambda (re-match-start i)
  (if (or (negative? i) (> i 9))
      (error "RE-MATCH-START: No such register" i)
      (let ((group (unhash match-group)))
	(if (not group)
	    (error "RE-MATCH-START: No match registers" i)
	    (make-mark group (vector-ref registers i)))))))

(set! re-match-end
(named-lambda (re-match-end i)
  (if (or (negative? i) (> i 9))
      (error "RE-MATCH-END: No such register" i)
      (let ((group (unhash match-group)))
	(if (not group)
	    (error "RE-MATCH-END: No match registers" i)
	    (make-mark group (vector-ref registers (+ i 10))))))))

(define (%re-finish group index)
  (if index
      (begin (set! match-group (hash group))
	     (make-mark group index))
      (begin (set! match-group (hash #!FALSE))
	     #!FALSE)))

(define pattern-cache
  (make-list 32 '(foo . bar)))

(define (compile-pattern regexp)
  ;; Incredible hair here to prevent excessive consing.
  ((if (ref-variable "Case Fold Search") cdr car)
   (cdr (or (assq regexp pattern-cache)
	    (begin (set! pattern-cache
			 (cons (cons regexp
				     (cons (re-compile-pattern regexp #!FALSE)
					   (re-compile-pattern regexp #!TRUE)))
			       (except-last-pair! pattern-cache)))
		   (car pattern-cache))))))

(define (compile-char char)
  (re-compile-char char (ref-variable "Case Fold Search")))

(define (compile-string string)
  (re-compile-string string (ref-variable "Case Fold Search")))

;;;; Search

(define-search char-search-forward char
  %re-search-forward compile-char group-end mark<=)

(define-search search-forward string
  %re-search-forward compile-string group-end mark<=)

(define-search re-search-forward regexp
  %re-search-forward compile-pattern group-end mark<=)

(define (%re-search-forward group start end pattern)
  (%re-finish group
	      (%%re-search-forward pattern
				   (re-translation-table
				    (ref-variable "Case Fold Search"))
				   (ref-variable "Syntax Table")
				   registers
				   group start end)))


(define %%re-search-forward
  (make-primitive re-search-buffer-forward))

(define-search char-search-backward char
  %re-search-backward compile-char group-start mark>=)

(define-search search-backward string
  %re-search-backward compile-string group-start mark>=)

(define-search re-search-backward regexp
  %re-search-backward compile-pattern group-start mark>=)

(define (%re-search-backward group start end pattern)
  (%re-finish group
	      (%%re-search-backward pattern
				    (re-translation-table
				     (ref-variable "Case Fold Search"))
				    (ref-variable "Syntax Table")
				    registers
				    group end start)))


(define %%re-search-backward
  (make-primitive re-search-buffer-backward))


;;;; Match

(define-macro (define-forward-match name key-name compile-key)
  `(SET! ,name
	 (NAMED-LAMBDA (,name ,key-name #!OPTIONAL START END)
	   (COND ((UNASSIGNED? START)
		  (SET! START (CURRENT-POINT))
		  (SET! END (GROUP-END START)))
		 ((UNASSIGNED? END)
		  (SET! END (GROUP-END START)))
		 ((NOT (MARK<= START END))
		  (ERROR ,(string-append (symbol->string name)
					 ": Marks incorrectly related")
			 START END)))
	   (%RE-MATCH-FORWARD (MARK-GROUP START)
			      (MARK-INDEX START)
			      (MARK-INDEX END)
			      (,compile-key ,key-name)))))

(define-forward-match char-match-forward char compile-char)
(define-forward-match match-forward string compile-string)
(define-forward-match re-match-forward regexp compile-pattern)

(define (%re-match-forward group start end pattern)
  (%re-finish group
	      (%%re-match-forward pattern
				  (re-translation-table
				   (ref-variable "Case Fold Search"))
				  (ref-variable "Syntax Table")
				  registers
				  group start end)))


(define %%re-match-forward
  (make-primitive re-match-buffer))


(set! char-match-backward
(named-lambda (char-match-backward char #!optional start end)
  (cond ((unassigned? start)
	 (set! start (current-point))
	 (set! end (group-start start)))
	((unassigned? end)
	 (set! end (group-start start)))
	((not (mark>= start end))
	 (error "CHAR-MATCH-BACKWARD: Marks incorrectly related" start end)))
  (%re-match-backward (mark-group start)
		      (mark-index start)
		      (-1+ (mark-index start))
		      (mark-index end)
		      (compile-char char))))

(set! match-backward
(named-lambda (match-backward string #!optional start end)
  (cond ((unassigned? start)
	 (set! start (current-point))
	 (set! end (group-start start)))
	((unassigned? end)
	 (set! end (group-start start)))
	((not (mark>= start end))
	 (error "MATCH-BACKWARD: Marks incorrectly related" start end)))
  (%re-match-backward (mark-group start)
		      (mark-index start)
		      (- (mark-index start) (string-length string))
		      (mark-index end)
		      (compile-string string))))

(define (%re-match-backward group start mark end pattern)
  (and (<= end mark)
       (%re-match-forward group mark start pattern)
       mark))

;;; end REGULAR-EXPRESSION-PACKAGE
)))

;;;; Quote

(define re-quote-string
  (let ((special (char-set #\[ #\] #\* #\. #\\ #\? #\+ #\^ #\$)))
    (named-lambda (re-quote-string string)
      (let ((end (string-length string)))
	(define (count start n)
	  (let ((index (substring-find-next-char-in-set string start end
							special)))
	    (if index
		(count (1+ index) (1+ n))
		n)))
	(let ((n (count 0 0)))
	  (if (zero? n)
	      string
	      (let ((result (string-allocate (+ end n))))
		(define (loop start i)
		  (let ((index
			 (substring-find-next-char-in-set string start end
							  special)))
		    (if index
			(begin (substring-move-right! string start index
						      result i)
			       (let ((i (+ i (- index start))))
				 (string-set! result i #\\)
				 (string-set! result (1+ i)
					      (string-ref string index))
				 (loop (1+ index) (+ i 2))))
			(substring-move-right! string start end result i))))
		(loop 0 0)
		result)))))))

;;;; Char Skip

(define (skip-chars-forward pattern #!optional start end limit?)
  (cond ((unassigned? start)
	 (set! start (current-point))
	 (set! end (group-end start))
	 (set! limit? 'LIMIT))
	((unassigned? end)
	 (set! end (group-end start))
	 (set! limit? 'LIMIT))
	(else
	 (if (not (mark<= start end))
	     (error "SKIP-CHARS-FORWARD: Marks incorrectly related" start end))
	 (if (unassigned? limit?) (set! limit? 'LIMIT))))
  (let ((index
	 (%find-next-char-in-set (mark-group start)
				 (mark-index start)
				 (mark-index end)
				 (re-compile-char-set pattern #!TRUE))))
    (if index
	(make-mark (mark-group start) index)
	(limit-mark-motion limit? end))))

(define (skip-chars-backward pattern #!optional start end limit?)
  (cond ((unassigned? start)
	 (set! start (current-point))
	 (set! end (group-start start))
	 (set! limit? 'LIMIT))
	((unassigned? end)
	 (set! end (group-start start))
	 (set! limit? 'LIMIT))
	(else
	 (if (not (mark>= start end))
	     (error "SKIP-CHARS-FORWARD: Marks incorrectly related" start end))
	 (if (unassigned? limit?) (set! limit? 'LIMIT))))
  (let ((index
	 (%find-previous-char-in-set (mark-group start)
				     (mark-index start)
				     (mark-index end)
				     (re-compile-char-set pattern #!TRUE))))
    (if index
	(make-mark (mark-group start) index)
	(limit-mark-motion limit? end))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
