#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;;; Regular s-expressions
;;; package: (runtime regular-sexpression)

;;; The compiler takes a regular sexpression and returns an
;;; instruction.  An instruction is a procedure that accepts a success
;;; continuation, and returns a "linked instruction".  But success
;;; continuations and linked instructions have the same signature,
;;; which encourages the use of a combinator language.

(declare (usual-integrations))

(define (compile-regsexp regsexp)
  (%link-insn
   (bind-condition-handler (list condition-type:error)
       (lambda (condition)
	 (signal-compile-error regsexp condition))
     (lambda ()
       (%compile-regsexp regsexp)))))

(define (%link-insn insn)
  (%make-compiled-regsexp
   (insn
    (lambda (position groups fail)
      fail
      (cons (get-index position)
	    (%convert-groups groups))))))

(define-record-type <compiled-regsexp>
    (%make-compiled-regsexp impl)
    compiled-regsexp?
  (impl %compiled-regsexp-impl))

(define-guarantee compiled-regsexp "compiled regular s-expression")

(define (%top-level-match crsexp start-position)
  ((%compiled-regsexp-impl crsexp) start-position '() (lambda () #f)))

(define (%compile-regsexp regsexp)
  (cond ((unicode-char? regsexp)
	 (insn:char regsexp))
	((string? regsexp)
	 (insn:string regsexp))
	((and (pair? regsexp)
	      (symbol? (car regsexp))
	      (find (lambda (rule)
		      (and (eq? (caar rule) (car regsexp))
			   (syntax-match? (cdar rule) (cdr regsexp))))
		    %compile-regsexp-rules))
	 => (lambda (rule)
	      (apply (cdr rule) (cdr regsexp))))
	(else
	 (error "Ill-formed regular s-expression:" regsexp))))

(define (%compile-char-set items)
  (scalar-values->char-set
   (append-map (lambda (item)
		 (cond ((well-formed-scalar-value-range? item)
			(list item))
		       ((unicode-char? item)
			(list (char->integer item)))
		       ((char-set? item)
			(char-set->scalar-values item))
		       ((string? item)
			(map char->integer (string->list item)))
		       (else
			(error "Ill-formed char-set item:" item))))
	       items)))

(define (%compile-group-key key)
  (if (not (or (fix:fixnum? key)
	       (unicode-char? key)
	       (symbol? key)))
      (error "Ill-formed regsexp group key:" key))
  key)

(define condition-type:compile-regsexp)
(define signal-compile-error)
(define (initialize-conditions!)
  (set! condition-type:compile-regsexp
	(make-condition-type 'COMPILE-REGSEXP condition-type:error
	    '(PATTERN CAUSE)
	  (lambda (condition port)
	    (write (access-condition condition 'PATTERN) port)
	    (write-string ": " port)
	    (write-condition-report (access-condition condition 'CAUSE) port))))
  (set! signal-compile-error
	(condition-signaller condition-type:compile-regsexp
			     '(PATTERN CAUSE)
			     standard-error-handler))
  unspecific)

;;;; Compiler rules

(define (define-rule pattern compiler)
  (add-boot-init!
   (lambda ()
     (if (not (and (pair? pattern)
		   (symbol? (car pattern))))
	 (error:bad-range-argument pattern 'DEFINE-RULE))
     (let ((p
	    (find (lambda (p)
		    (eq? (car p) (car pattern)))
		  %compile-regsexp-rules)))
       (if p
	   (set-cdr! p compiler)
	   (begin
	     (set! %compile-regsexp-rules
		   (cons (cons pattern compiler)
			 %compile-regsexp-rules))
	     unspecific))))))

(define %compile-regsexp-rules '())

(define-rule '(ANY-CHAR)
  (lambda ()
    (%compile-regsexp '(INVERSE-CHAR-SET "\n"))))

(define-rule '(+ FORM)
  (lambda (regsexp)
    (%compile-regsexp `(** 1 #F ,regsexp))))

(define-rule '(+? FORM)
  (lambda (regsexp)
    (%compile-regsexp `(**? 1 #F ,regsexp))))

(define-rule '(CHAR-SET * DATUM)
  (lambda items
    (insn:char-set (%compile-char-set items))))

(define-rule '(INVERSE-CHAR-SET * DATUM)
  (lambda items
    (insn:inverse-char-set (%compile-char-set items))))

(define-rule '(LINE-START) (lambda () (insn:line-start)))
(define-rule '(LINE-END) (lambda () (insn:line-end)))
(define-rule '(STRING-START) (lambda () (insn:string-start)))
(define-rule '(STRING-END) (lambda () (insn:string-end)))

(define-rule '(? FORM)
  (lambda (regsexp)
    (insn:? (%compile-regsexp regsexp))))

(define-rule '(* FORM)
  (lambda (regsexp)
    (insn:* (%compile-regsexp regsexp))))

(define-rule '(?? FORM)
  (lambda (regsexp)
    (insn:?? (%compile-regsexp regsexp))))

(define-rule '(*? FORM)
  (lambda (regsexp)
    (insn:*? (%compile-regsexp regsexp))))

(define-rule '(** DATUM FORM)
  (lambda (n regsexp)
    (check-repeat-1-arg n)
    (insn:** n n (%compile-regsexp regsexp))))

(define-rule '(**? DATUM FORM)
  (lambda (n regsexp)
    (check-repeat-1-arg n)
    (insn:**? n n (%compile-regsexp regsexp))))

(define (check-repeat-1-arg n)
  (if (not (exact-nonnegative-integer? n))
      (error "Repeat limit must be non-negative integer:" n)))

(define-rule '(** DATUM DATUM FORM)
  (lambda (n m regsexp)
    (check-repeat-2-args n m)
    (insn:** n m (%compile-regsexp regsexp))))

(define-rule '(**? DATUM DATUM FORM)
  (lambda (n m regsexp)
    (check-repeat-2-args n m)
    (insn:**? n m (%compile-regsexp regsexp))))

(define (check-repeat-2-args n m)
  (if (not (exact-nonnegative-integer? n))
      (error "Repeat limit must be non-negative integer:" n))
  (if m
      (begin
	(if (not (exact-nonnegative-integer? m))
	    (error "Repeat limit must be non-negative integer:" m))
	(if (not (<= n m))
	    (error "Repeat lower limit greater than upper limit:" n m)))))

(define-rule '(ALT * FORM)
  (lambda regsexps
    (insn:alt (map %compile-regsexp regsexps))))

(define-rule '(SEQ * FORM)
  (lambda regsexps
    (insn:seq (map %compile-regsexp regsexps))))

(define-rule '(GROUP DATUM FORM)
  (lambda (key regsexp)
    (insn:group (%compile-group-key key)
		(%compile-regsexp regsexp))))

(define-rule '(GROUP-REF DATUM)
  (lambda (key)
    (insn:group-ref (%compile-group-key key))))

;;;; Instructions

(define (insn:always-succeed)
  (lambda (succeed)
    succeed))

(define (insn:always-fail)
  (lambda (succeed)
    succeed
    (lambda (position groups fail)
      position groups
      (fail))))

(define (insn:string-start)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (not (prev-char position))
	  (succeed position groups fail)
	  (fail)))))

(define (insn:string-end)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (not (next-char position))
	  (succeed position groups fail)
	  (fail)))))

(define (insn:line-start)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (let ((char (prev-char position)))
	    (or (not char)
		(char=? char #\newline)))
	  (succeed position groups fail)
	  (fail)))))

(define (insn:line-end)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (let ((char (next-char position)))
	    (or (not char)
		(char=? char #\newline)))
	  (succeed position groups fail)
	  (fail)))))

(define (insn:char char)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (eqv? (next-char position) char)
	  (succeed (next-position position) groups fail)
	  (fail)))))

(define (insn:chars chars)
  (lambda (succeed)
    (lambda (position groups fail)
      (let loop ((chars chars) (position position))
	(if (pair? chars)
	    (if (eqv? (next-char position) (car chars))
		(loop (cdr chars) (next-position position))
		(fail))
	    (succeed position groups fail))))))

(define (insn:string string)
  (let ((end (string-length string)))
    (cond ((fix:= end 0)
	   (insn:always-succeed))
	  ((fix:= end 1)
	   (insn:char (string-ref string 0)))
	  (else
	   (lambda (succeed)
	     (lambda (position groups fail)
	       (let loop ((i 0) (position position))
		 (if (fix:< i end)
		     (let ((char (string-ref string i)))
		       (if (eqv? (next-char position) char)
			   (loop (fix:+ i 1) (next-position position))
			   (fail)))
		     (succeed position groups fail)))))))))

(define (insn:char-set char-set)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (let ((char (next-char position)))
	    (and char
		 (char-set-member? char-set char)))
	  (succeed (next-position position) groups fail)
	  (fail)))))

(define (insn:inverse-char-set char-set)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (let ((char (next-char position)))
	    (and char
		 (not (char-set-member? char-set char))))
	  (succeed (next-position position) groups fail)
	  (fail)))))

(define (insn:group key insn)
  (insn:seq (list (%insn:start-group key)
		  insn
		  (%insn:end-group key))))

(define (%insn:start-group key)
  (lambda (succeed)
    (lambda (position groups fail)
      (succeed position
	       (%start-group key position groups)
	       fail))))

(define (%insn:end-group key)
  (lambda (succeed)
    (lambda (position groups fail)
      (succeed position
	       (%end-group key position groups)
	       fail))))

(define (insn:group-ref key)
  (lambda (succeed)
    (lambda (position groups fail)
      (((%find-group key groups) succeed) position groups fail))))

(define (insn:seq insns)
  (lambda (succeed)
    (fold-right (lambda (insn next)
		  (insn next))
		succeed
		insns)))

(define (insn:alt insns)
  (reduce-right (lambda (insn1 insn2)
		  (lambda (succeed)
		    (%failure-chain (insn1 succeed)
				    (insn2 succeed))))
		(insn:always-fail)
		insns))

(define (insn:? insn)
  (lambda (succeed)
    (%failure-chain (insn succeed) succeed)))

(define (insn:?? insn)
  (lambda (succeed)
    (%failure-chain succeed (insn succeed))))

;;; The next two operations must fail when the instruction makes no
;;; progress in a given iteration.  Otherwise patterns like (* (SEQ))
;;; will loop forever.

(define (insn:* insn)
  (lambda (succeed)
    (define (loop position groups fail)
      ((%failure-chain (insn
			(lambda (position* groups* fail*)
			  (if (same-positions? position* position)
			      (fail*)
			      (loop position* groups* fail*))))
		       succeed)
       position groups fail))
    loop))

(define (insn:*? insn)
  (lambda (succeed)
    (define (loop position groups fail)
      ((%failure-chain succeed
		       (insn
			(lambda (position* groups* fail*)
			  (if (same-positions? position* position)
			      (fail*)
			      (loop position* groups* fail*)))))
       position groups fail))
    loop))

(define (%failure-chain s1 s2)
  (lambda (position groups fail)
    (s1 position
	groups
	(lambda () (s2 position groups fail)))))

(define (insn:** n m insn)
  (%repeat n m insn
	   (lambda (limit insn)
	     (%hybrid-chain limit
			    (lambda (succeed)
			      (lambda (continue)
				(%failure-chain (insn continue) succeed)))))
	   insn:*))

(define (insn:**? n m insn)
  (%repeat n m insn
	   (lambda (limit insn)
	     (%hybrid-chain limit
			    (lambda (succeed)
			      (lambda (continue)
				(%failure-chain succeed (insn continue))))))
	   insn:*?))

(define (%repeat n m insn repeat-limited repeat-unlimited)
  (let ((insn1 (%repeat-exactly n insn))
	(insn2
	 (if m
	     (repeat-limited (- m n) insn)
	     (repeat-unlimited insn))))
    (lambda (succeed)
      (insn1 (insn2 succeed)))))

(define (%repeat-exactly n insn)
  (%hybrid-chain n
		 (lambda (succeed)
		   succeed
		   insn)))

(define (%hybrid-chain limit linker)
  (if (<= limit 8)
      (%immediate-chain limit linker)
      (%delayed-chain limit linker)))

(define (%immediate-chain limit pre-linker)
  (lambda (succeed)
    (let ((linker (pre-linker succeed)))
      (let loop ((i 0))
	(if (< i limit)
	    (linker (loop (+ i 1)))
	    succeed)))))

(define (%delayed-chain limit pre-linker)
  (lambda (succeed)
    (let ((linker (pre-linker succeed)))
      (let loop ((i 0))
	(if (< i limit)
	    (lambda (position groups fail)
	      ((linker (loop (+ i 1))) position groups fail))
	    succeed)))))

;;;; Positions

(define (get-index position)
  ((%position-type-get-index (%get-position-type position)) position))

(define (next-char position)
  ((%position-type-next-char (%get-position-type position)) position))

(define (prev-char position)
  ((%position-type-prev-char (%get-position-type position)) position))

(define (next-position position)
  ((%position-type-next-position (%get-position-type position)) position))

(define (same-positions? p1 p2)
  ((%position-type-same? (%get-position-type p1)) p1 p2))

(define (%get-position-type position)
  (or (find (lambda (type)
	      ((%position-type-predicate type) position))
	    %all-position-types)
      (error:wrong-type-datum position "position")))

(define-structure (%position-type (constructor %make-position-type))
  (predicate #f read-only #t)
  (get-index #f read-only #t)
  (next-char #f read-only #t)
  (prev-char #f read-only #t)
  (next-position #f read-only #t)
  (same? #f read-only #t))

(define (define-position-type predicate . args)
  (add-boot-init!
   (lambda ()
     (let ((type (apply %make-position-type predicate args)))
       (let ((tail
	      (find-tail (lambda (type)
			   (eq? (%position-type-predicate type) predicate))
			 %all-position-types)))
	 (if tail
	     (set-car! tail type)
	     (begin
	       (set! %all-position-types (cons type %all-position-types))
	       unspecific)))))))

(define %all-position-types '())

;;;; Groups

(define (%start-group key position groups)
  (cons (list key position)
	groups))

(define (%end-group key position groups)
  ;; Kind of slow, but it's functional.  Could speed up with side
  ;; effects.
  (let ((p (assq key groups)))
    (if (not (and p (null? (cddr p))))
	(error "%END-GROUP called with no %START-GROUP:" key))
    (cons (list key (cadr p) position)
	  (delq p groups))))

(define (%find-group key groups)
  (let ((p (assq key groups)))
    (if (not p)
	;; This can happen with (* (GROUP ...)), but in other cases it
	;; would be an error.
	(insn:always-succeed)
	(begin
	  (if (null? (cddr p))
	      (error "Reference to group appears before group's end:" key))
	  (insn:chars (%group-chars (cadr p) (caddr p)))))))

(define (%group-chars start-position end-position)
  (let ((same? (%position-type-same? (%get-position-type start-position))))
    (let loop ((position start-position) (chars '()))
      (if (same? position end-position)
	  (reverse! chars)
	  (let ((char (next-char position)))
	    (if (not char)
		(error "Failure of SAME? predicate"))
	    (loop (next-position position)
		  (cons char chars)))))))

(define (%convert-groups groups)
  (map (lambda (g)
	 (list (car g)
	       (get-index (cadr g))
	       (get-index (caddr g))))
       (remove (lambda (g)
		 (null? (cddr g)))
	       groups)))

;;;; Match input port

(define (regsexp-match-input-port crsexp port)
  (let ((caller 'REGSEXP-MATCH-INPUT-PORT))
    (guarantee-compiled-regsexp crsexp caller)
    (guarantee-input-port port caller)
    (%top-level-match crsexp
		      (%char-source->position
		       (lambda ()
			 (let ((char (read-char port)))
			   (if (eof-object? char)
			       #f
			       char)))))))

(define (%char-source->position source)
  (%make-source-position 0 (source) #f source))

(define-structure (%source-position (constructor %make-source-position))
  (index #f read-only #t)
  (next-char #f read-only #t)
  (prev-char #f read-only #t)
  (source #f read-only #t))

(define-position-type %source-position?
  (lambda (position)
    (%source-position-index position))
  (lambda (position)
    (%source-position-next-char position))
  (lambda (position)
    (%source-position-prev-char position))
  (lambda (position)
    (%make-source-position (fix:+ (%source-position-index position) 1)
			   ((%source-position-source position))
			   (%source-position-next-char position)
			   (%source-position-source position)))
  (lambda (p1 p2)
    (and (eq? (%source-position-source p1)
	      (%source-position-source p2))
	 (fix:= (%source-position-index p1)
		(%source-position-index p2)))))

;;;; Match string

(define (regsexp-match-string crsexp string #!optional start end)
  (let ((caller 'REGSEXP-MATCH-STRING))
    (guarantee-compiled-regsexp crsexp caller)
    (guarantee-string string caller)
    (let* ((end
	    (let ((length (string-length string)))
	      (if (default-object? end)
		  length
		  (begin
		    (guarantee-substring-end-index end length caller)
		    end))))
	   (start
	    (if (default-object? start)
		0
		(begin
		  (guarantee-substring-start-index start end caller)
		  start))))
      (%top-level-match crsexp
			(cons start (%make-substring string start end))))))

(define-structure (%substring (constructor %make-substring))
  (string #f read-only #t)
  (start #f read-only #t)
  (end #f read-only #t))

(define (%string-position? object)
  (declare (no-type-checks))
  (and (pair? object)
       (%substring? (cdr object))))

(define-integrable (%string-position-index position)
  (declare (no-type-checks))
  (car position))

(define-integrable (%string-position-string position)
  (declare (no-type-checks))
  (%substring-string (cdr position)))

(define-integrable (%string-position-start position)
  (declare (no-type-checks))
  (%substring-start (cdr position)))

(define-integrable (%string-position-end position)
  (declare (no-type-checks))
  (%substring-end (cdr position)))

(define-position-type %string-position?
  (lambda (position)
    (%string-position-index position))
  (lambda (position)
    (if (fix:< (%string-position-index position)
	       (%string-position-end position))
	(string-ref (%string-position-string position)
		    (%string-position-index position))
	#f))
  (lambda (position)
    (if (fix:> (%string-position-index position)
	       (%string-position-start position))
	(string-ref (%string-position-string position)
		    (fix:- (%string-position-index position) 1))
	#f))
  (lambda (position)
    (declare (no-type-checks))
    (cons (fix:+ (car position) 1)
	  (cdr position)))
  (lambda (p1 p2)
    (declare (no-type-checks))
    (and (eq? (cdr p1) (cdr p2))
	 (fix:= (car p1) (car p2)))))