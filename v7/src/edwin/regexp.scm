;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/regexp.scm,v 1.46 1989/03/14 08:02:02 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

(define match-group)
(define registers (make-vector 20))

(define (re-match-start i)
  (if (or (negative? i) (> i 9))
      (error "RE-MATCH-START: No such register" i)
      (let ((group (unhash match-group)))
	(if (not group)
	    (error "RE-MATCH-START: No match registers" i)
	    (make-mark group (vector-ref registers i))))))

(define (re-match-end i)
  (if (or (negative? i) (> i 9))
      (error "RE-MATCH-END: No such register" i)
      (let ((group (unhash match-group)))
	(if (not group)
	    (error "RE-MATCH-END: No match registers" i)
	    (make-mark group (vector-ref registers (+ i 10)))))))

(define (%re-finish group index)
  (if index
      (begin
	(set! match-group (hash group))
	(make-mark group index))
      (begin
	(set! match-group (hash false))
	false)))

(define pattern-cache
  (make-list 32 (cons* "" "" "")))

(define (compile-pattern regexp)
  ;; Incredible hair here to prevent excessive consing.
  ((if (ref-variable "Case Fold Search") cdr car)
   (cdr (or (assq regexp pattern-cache)
	    (let ((entry
		   (cons regexp
			 (cons (re-compile-pattern regexp false)
			       (re-compile-pattern regexp true)))))
	      (set! pattern-cache
		    (cons entry
			  (except-last-pair! pattern-cache)))
	      entry)))))

(define (compile-char char)
  (re-compile-char char (ref-variable "Case Fold Search")))

(define (compile-string string)
  (re-compile-string string (ref-variable "Case Fold Search")))

;;;; Search

(define-macro (define-search name key-name searcher compile-key
		mark-limit mark-compare)
  `(DEFINE (,name ,key-name #!OPTIONAL START END LIMIT?)
     (LET ((START (IF (DEFAULT-OBJECT? START) (CURRENT-POINT) START)))
       (LET ((END (IF (DEFAULT-OBJECT? END) (,mark-limit START) END)))
	 (LET ((LIMIT? (AND (NOT (DEFAULT-OBJECT? LIMIT?)) LIMIT?)))
	   (IF (NOT (,mark-compare START END))
	       (ERROR ,(string-append (symbol->string name)
				      ": Marks incorrectly related")
		      START END))
	   (OR (,searcher (MARK-GROUP START)
			  (MARK-INDEX START)
			  (MARK-INDEX END)
			  (,compile-key ,key-name))
	       (LIMIT-MARK-MOTION LIMIT? END)))))))

(define-search char-search-forward char
  %re-search-forward compile-char group-end mark<=)

(define-search search-forward string
  %re-search-forward compile-string group-end mark<=)

(define-search re-search-forward regexp
  %re-search-forward compile-pattern group-end mark<=)

(define (%re-search-forward group start end pattern)
  (%re-finish group
	      ((ucode-primitive re-search-buffer-forward)
	       pattern
	       (re-translation-table (ref-variable "Case Fold Search"))
	       (syntax-table/entries (ref-variable "Syntax Table"))
	       registers
	       group start end)))

(define-search char-search-backward char
  %re-search-backward compile-char group-start mark>=)

(define-search search-backward string
  %re-search-backward compile-string group-start mark>=)

(define-search re-search-backward regexp
  %re-search-backward compile-pattern group-start mark>=)

(define (%re-search-backward group start end pattern)
  (%re-finish group
	      ((ucode-primitive re-search-buffer-backward)
	       pattern
	       (re-translation-table (ref-variable "Case Fold Search"))
	       (syntax-table/entries (ref-variable "Syntax Table"))
	       registers
	       group end start)))

;;;; Match

(define-macro (define-forward-match name key-name compile-key)
  `(DEFINE (,name ,key-name #!OPTIONAL START END)
     (LET ((START (IF (DEFAULT-OBJECT? START) (CURRENT-POINT) START)))
       (LET ((END (IF (DEFAULT-OBJECT? END) (GROUP-END START) END)))
	 (IF (NOT (MARK<= START END))
	     (ERROR ,(string-append (symbol->string name)
				    ": Marks incorrectly related")
		    START END))
	 (%RE-MATCH-FORWARD (MARK-GROUP START)
			    (MARK-INDEX START)
			    (MARK-INDEX END)
			    (,compile-key ,key-name))))))

(define-forward-match char-match-forward char compile-char)
(define-forward-match match-forward string compile-string)
(define-forward-match re-match-forward regexp compile-pattern)

(define-macro (define-backward-match name key-name key-length compile-key)
  `(DEFINE (,name ,key-name #!OPTIONAL START END)
     (LET ((START (IF (DEFAULT-OBJECT? START) (CURRENT-POINT) START)))
       (LET ((END (IF (DEFAULT-OBJECT? END) (GROUP-START START) END)))
	 (IF (NOT (MARK>= START END))
	     (ERROR ,(string-append (symbol->string name)
				    ": Marks incorrectly related")
		    START END))
	 (LET ((GROUP (MARK-GROUP START))
	       (START-INDEX (MARK-INDEX START))
	       (END-INDEX (MARK-INDEX END)))
	   (LET ((INDEX (- START-INDEX ,key-length)))
	     (AND (<= END-INDEX INDEX)
		  (%RE-MATCH-FORWARD GROUP
				     INDEX
				     START-INDEX
				     (,compile-key ,key-name))
		  (MAKE-MARK GROUP INDEX))))))))

(define-backward-match char-match-backward
  char
  1
  compile-char)

(define-backward-match match-backward
  string
  (string-length string)
  compile-string)

(define (%re-match-forward group start end pattern)
  (%re-finish group
	      ((ucode-primitive re-match-buffer)
	       pattern
	       (re-translation-table (ref-variable "Case Fold Search"))
	       (syntax-table/entries (ref-variable "Syntax Table"))
	       registers
	       group start end)))

;;;; Quote

(define re-quote-string
  (let ((special (char-set #\[ #\] #\* #\. #\\ #\? #\+ #\^ #\$)))
    (lambda (string)
      (let ((end (string-length string)))
	(let ((n
	       (let loop ((start 0) (n 0))
		 (let ((index
			(substring-find-next-char-in-set string start end
							 special)))
		   (if index
		       (loop (1+ index) (1+ n))
		       n)))))
	  (if (zero? n)
	      string
	      (let ((result (string-allocate (+ end n))))
		(let loop ((start 0) (i 0))
		  (let ((index
			 (substring-find-next-char-in-set string start end
							  special)))
		    (if index
			(begin
			  (substring-move-right! string start index result i)
			  (let ((i (+ i (- index start))))
			    (string-set! result i #\\)
			    (string-set! result
					 (1+ i)
					 (string-ref string index))
			    (loop (1+ index) (+ i 2))))
			(substring-move-right! string start end result i))))
		result)))))))

;;;; Char Skip

(define (skip-chars-forward pattern #!optional start end limit?)
  (let ((start (if (default-object? start) (current-point) start)))
    (let ((end (if (default-object? end) (group-end start) end)))
      (let ((limit? (if (default-object? limit?) 'LIMIT limit?)))
	(if (not (mark<= start end))
	    (error "SKIP-CHARS-FORWARD: Marks incorrectly related" start end))
	(let ((index
	       (%find-next-char-in-set (mark-group start)
				       (mark-index start)
				       (mark-index end)
				       (re-compile-char-set pattern true))))
	  (if index
	      (make-mark (mark-group start) index)
	      (limit-mark-motion limit? end)))))))

(define (skip-chars-backward pattern #!optional start end limit?)
  (let ((start (if (default-object? start) (current-point) start)))
    (let ((end (if (default-object? end) (group-start start) end)))
      (let ((limit? (if (default-object? limit?) 'LIMIT limit?)))
	(if (not (mark>= start end))
	    (error "SKIP-CHARS-BACKWARD: Marks incorrectly related" start end))
	(let ((index
	       (%find-previous-char-in-set (mark-group start)
					   (mark-index start)
					   (mark-index end)
					   (re-compile-char-set pattern
								true))))
	  (if index
	      (make-mark (mark-group start) index)
	      (limit-mark-motion limit? end)))))))