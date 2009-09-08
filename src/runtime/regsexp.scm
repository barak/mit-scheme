#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

(declare (usual-integrations))

(define (compile-regsexp regsexp)
  (%make-compiled-regsexp (%compile-regsexp regsexp)))

(define-record-type <compiled-regsexp>
    (%make-compiled-regsexp insn)
    compiled-regsexp?
  (insn %compiled-regsexp-insn))

(define-guarantee compiled-regsexp "compiled regular s-expression")

(define (%compile-regsexp regsexp)
  (cond ((unicode-char? regsexp)
	 (insn:char regsexp))
	((string? regsexp)
	 (insn:string regsexp))
	((and (pair? regsexp)
	      (symbol? (car regsexp))
	      (list? (cdr regsexp))
	      (find (lambda (rule)
		      (syntax-match? (car rule) regsexp))
		    %compile-regsexp-rules))
	 => (lambda (rule)
	      (apply (cdr rule) (cdr regsexp))))
	(else
	 (error:wrong-type-argument regsexp "regular s-expression"
				    'COMPILE-REGSEXP))))

(define (%compile-char-set items)
  (scalar-values->alphabet
   (map (lambda (item)
	  (cond ((or (unicode-scalar-value? item)
		     (and (pair? item)
			  (pair? (cdr item))
			  (null? (cddr item))
			  (unicode-scalar-value? (car item))
			  (unicode-scalar-value? (cadr item))
			  (< (car item) (cadr item))))
		 (list item))
		((unicode-char? item)
		 (list (char->integer item)))
		((alphabet? item)
		 (alphabet->scalar-values item))
		((string? item)
		 (map char->integer (string->list item)))
		(else
		 (error:wrong-type-argument item "char-set item"
					    'COMPILE-REGSEXP))))
	items)))

(define (%compile-group-key key)
  (if (not (or (fix:fixnum? key)
	       (unicode-char? key)
	       (symbol? key)))
      (error:wrong-type-argument key "regsexp group key" 'COMPILE-REGSEXP))
  key)

(define (define-rule pattern compiler)
  (add-boot-init!
   (lambda ()
     (let ((p (assoc pattern %compile-regsexp-rules)))
       (if p
	   (set-cdr! p compiler)
	   (begin
	     (set! %compile-regsexp-rules
		   (cons (cons pattern compiler)
			 %compile-regsexp-rules))
	     unspecific))))))

(define %compile-regsexp-rules '())

;;;; Compiler rules

(define-rule '('ANY-CHAR)
  (lambda ()
    (%compile-regsexp '(INVERSE-CHAR-SET "\n"))))

(define-rule '('* FORM)
  (lambda (regsexp)
    (%compile-regsexp `(REPEAT> 0 #F ,regsexp))))

(define-rule '('+ FORM)
  (lambda (regsexp)
    (%compile-regsexp `(REPEAT> 1 #F ,regsexp))))

(define-rule '('? FORM)
  (lambda (regsexp)
    (%compile-regsexp `(REPEAT> 0 1 ,regsexp))))

(define-rule '('*? FORM)
  (lambda (regsexp)
    (%compile-regsexp `(REPEAT< 0 #F ,regsexp))))

(define-rule '('+? FORM)
  (lambda (regsexp)
    (%compile-regsexp `(REPEAT< 1 #F ,regsexp))))

(define-rule '('?? FORM)
  (lambda (regsexp)
    (%compile-regsexp `(REPEAT< 0 1 ,regsexp))))

(define-rule '('CHAR-SET * DATUM)
  (lambda items
    (insn:char-set (%compile-char-set items))))

(define-rule '('INVERSE-CHAR-SET * DATUM)
  (lambda items
    (insn:inverse-char-set (%compile-char-set items))))

(define-rule '('LINE-START) (lambda () (insn:line-start)))
(define-rule '('LINE-END) (lambda () (insn:line-end)))
(define-rule '('STRING-START) (lambda () (insn:string-start)))
(define-rule '('STRING-END) (lambda () (insn:string-end)))

(define-rule '('REPEAT> DATUM DATUM FORM)
  (lambda (n m regsexp)
    (check-repeat-args n m)
    (insn:repeat> n m (%compile-regsexp regsexp))))

(define-rule '('REPEAT< DATUM DATUM FORM)
  (lambda (n m regsexp)
    (check-repeat-args n m)
    (insn:repeat< n m (%compile-regsexp regsexp))))

(define (check-repeat-args n m)
  (guarantee-exact-nonnegative-integer n 'COMPILE-REGSEXP)
  (if m
      (guarantee-exact-nonnegative-integer m 'COMPILE-REGSEXP)
      (if (not (<= n m))
	  (error:bad-range-argument m 'COMPILE-REGSEXP))))

(define-rule '('ALT * FORM)
  (lambda regsexps
    (insn:alt (map %compile-regsexp regsexps))))

(define-rule '('SEQ * FORM)
  (lambda regsexps
    (insn:seq (map %compile-regsexp regsexps))))

(define-rule '('GROUP DATUM FORM)
  (lambda (key regsexp)
    (insn:group (%compile-group-key key)
		(%compile-regsexp regsexp))))

(define-rule '('GROUP-REF DATUM)
  (lambda (key)
    (insn:group-ref (%compile-group-key key))))

;;;; Instructions

(define (insn:always-succeed)
  (lambda (position groups succeed fail)
    (succeed position groups fail)))

(define (insn:always-fail)
  (lambda (position groups succeed fail)
    position groups succeed
    (fail)))

(define (insn:string-start)
  (lambda (position groups succeed fail)
    (if (not (prev-char position))
	(succeed position groups fail)
	(fail))))

(define (insn:string-end)
  (lambda (position groups succeed fail)
    (if (not (next-char position))
	(succeed position groups fail)
	(fail))))

(define (insn:line-start)
  (lambda (position groups succeed fail)
    (if (let ((char (prev-char position)))
	  (or (not char)
	      (char=? char #\newline)))
	(succeed position groups fail)
	(fail))))

(define (insn:line-end)
  (lambda (position groups succeed fail)
    (if (let ((char (next-char position)))
	  (or (not char)
	      (char=? char #\newline)))
	(succeed position groups fail)
	(fail))))

(define (insn:char char)
  (lambda (position groups succeed fail)
    (if (eqv? (next-char position) char)
	(succeed (next-position position) groups fail)
	(fail))))

(define (insn:string string)
  (let ((end (string-length string)))
    (cond ((fix:= end 0)
	   (insn:always-succeed))
	  ((fix:= end 1)
	   (insn:char (string-ref string 0)))
	  (else
	   (lambda (position groups succeed fail)
	     (let loop ((i 0) (position position))
	       (if (fix:< i end)
		   (let ((char (string-ref string i)))
		     (if (eqv? (next-char position) char)
			 (loop (fix:+ i 1) (next-position position))
			 (fail)))
		   (succeed position groups fail))))))))

(define (insn:char-set alphabet)
  (lambda (position groups succeed fail)
    (if (let ((char (next-char position)))
	  (and char
	       (char-in-alphabet? char alphabet)))
	(succeed (next-position position) groups fail)
	(fail))))

(define (insn:inverse-char-set alphabet)
  (lambda (position groups succeed fail)
    (if (let ((char (next-char position)))
	  (and char
	       (not (char-in-alphabet? char alphabet))))
	(succeed (next-position position) groups fail)
	(fail))))

(define (insn:group key insn)
  (lambda (position groups succeed fail)
    (insn position
	  (lambda (position* fail*)
	    (succeed position*
		     (new-group key position position* groups)
		     fail*))
	  fail)))

(define (insn:group-ref key)
  (lambda (position groups succeed fail)
    ((find-group key groups) position groups succeed fail)))

(define (insn:seq insns)
  (if (pair? insns)
      (let loop ((insn (car insns)) (insns (cdr insns)))
	(if (pair? insns)
	    (insn:seq2 insn (loop (car insns) (cdr insns)))
	    insn))
      (insn:always-succeed)))

(define (insn:seq2 insn1 insn2)
  (lambda (position groups succeed fail)
    (insn1 position
	   groups
	   (lambda (position* groups* fail*)
	     (insn2 position* groups* succeed fail*))
	   fail)))

(define (insn:alt insns)
  (if (pair? insns)
      (let loop ((insn (car insns)) (insns (cdr insns)))
	(if (pair? insns)
	    (insn:alt2 insn (loop (car insns) (cdr insns)))
	    insn))
      (insn:always-fail)))

(define (insn:alt2 insn1 insn2)
  (lambda (position groups succeed fail)
    (insn1 position
	   succeed
	   (lambda ()
	     (insn2 position groups succeed fail)))))

(define (insn:repeat> n m insn)
  (%insn:repeat n m insn insn:repeat>-limited insn:*))

(define (insn:repeat< n m insn)
  (%insn:repeat n m insn insn:repeat<-limited insn:*?))

(define (insn:repeat>-limited limit insn)
  (lambda (position groups succeed fail)
    (let loop ((i 0) (position position) (groups groups) (fail fail))
      (if (< i limit)
	  (insn position
		groups
		(lambda (position* groups* fail*)
		  (loop (+ i 1) position* groups* fail*))
		(lambda ()
		  (succeed position groups fail)))
	  (succeed position groups fail)))))

(define (insn:* insn)
  (lambda (position groups succeed fail)
    (let loop ((position position) (groups groups) (fail fail))
      (insn position
	    groups
	    loop
	    (lambda ()
	      (succeed position groups fail))))))

(define (insn:repeat<-limited limit insn)
  (lambda (position groups succeed fail)
    (let loop ((i 0) (position position) (groups groups) (fail fail))
      (if (< i limit)
	  (succeed position
		   groups
		   (lambda ()
		     (insn position
			   groups
			   (lambda (position* groups* fail*)
			     (loop (+ i 1) position* groups* fail*))
			   fail)))
	  (fail)))))

(define (insn:*? insn)
  (lambda (position groups succeed fail)
    (let loop ((position position) (groups groups) (fail fail))
      (succeed position
	       groups
	       (lambda ()
		 (insn position groups loop fail))))))

(define (%insn:repeat n m insn repeat-limited repeat-unlimited)
  (if (eqv? n m)
      (if (> n 0)
	  (insn:repeat-exactly n insn)
	  (insn:always-succeed))
      (let ((tail
	     (if m
		 (repeat-limited (- m n) insn)
		 (repeat-unlimited insn))))
	(if (> n 0)
	    (insn:seq2 (insn:repeat-exactly n insn) tail)
	    tail))))

(define (insn:repeat-exactly n insn)
  (if (<= n 8)
      (let loop ((i 0))
	(if (< i n)
	    (insn:seq2 insn (loop (+ i 1)))
	    insn))
      (lambda (position groups succeed fail)
	(let loop ((i 0) (position position) (groups groups) (fail fail))
	  (if (< i n)
	      (insn position
		    groups
		    (lambda (position* groups* fail*)
		      (loop (+ i 1) position* groups* fail*))
		    fail)
	      (succeed position groups fail))))))

;;;; Positions and groups

(define (next-char position)
  ((%position-type-next-char (%get-position-type position))))

(define (prev-char position)
  ((%position-type-prev-char (%get-position-type position))))

(define (next-position position)
  ((%position-type-next-position (%get-position-type position))))

(define (%get-position-type position)
  (or (find (lambda (type)
	      ((%position-type-predicate type) position))
	    %all-position-types)
      (error:wrong-type-datum position "position")))

(define-structure (%position-type (constructor %make-position-type))
  (predicate #f read-only #t)
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

(define (new-group key start-position end-position groups)
  (cons (cons key (%make-group-insn start-position end-position))
	groups))

(define (find-group key groups)
  (let ((p (assq key groups)))
    (if (not p)
	(error "No group with this key:" key))
    (cdr p)))

(define (%make-group-insn start-position end-position)
  (let ((same? (%position-type-same? (%get-position-type start-position))))
    (let loop ((position start-position) (chars '()))
      (if (same? start-position end-position)
	  (insn:chars (reverse! chars))
	  (loop (next-position position)
		(cons (next-char position) chars))))))

(define (insn:chars chars)
  (lambda (position groups succeed fail)
    (let loop ((chars chars) (position position))
      (if (pair? chars)
	  (if (eqv? (next-char position) (car chars))
	      (loop (cdr chars) (next-position position))
	      (fail))
	  (succeed position groups fail)))))

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

(define (%top-level-match crsexp position)
  ((%compiled-regsexp-insn crsexp) position
				   '()
				   (lambda (position groups fail)
				     position fail
				     groups)
				   (lambda () #f)))

(define (%char-source->position source)
  (%make-source-position 0 (source) #f source))

(define-structure (%source-position (constructor %make-source-position))
  (index #f read-only #t)
  (next-char #f read-only #t)
  (prev-char #f read-only #t)
  (source #f read-only #t))

(define-position-type %source-position?
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
	    (let ((length (string-length end)))
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