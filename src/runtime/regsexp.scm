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

(define (%compile-regsexp regsexp)
  (cond ((unicode-char? regsexp)
	 (insn:char regsexp #f))
	((string? regsexp)
	 (insn:string regsexp #f))
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

(define (%link-insn insn)
  (make-compiled-regsexp
   (insn
    (lambda (position groups fail)
      fail
      (cons (get-index position)
	    ((groups 'get-all)))))))

(define-record-type <compiled-regsexp>
    (make-compiled-regsexp impl)
    compiled-regsexp?
  (impl compiled-regsexp-impl))

(define (top-level-match crsexp start-position)
  (let ((result
	 ((compiled-regsexp-impl crsexp)
	  start-position (make-groups) (lambda () #f))))
    (and result
	 (cons (get-index start-position)
	       result))))

(define (group-key? object)
  (or (fix:fixnum? object)
      (unicode-char? object)
      (symbol? object)))

(define condition-type:compile-regsexp)
(define signal-compile-error)
(define (initialize-conditions!)
  (set! condition-type:compile-regsexp
	(make-condition-type 'compile-regsexp condition-type:error
	    '(pattern cause)
	  (lambda (condition port)
	    (write (access-condition condition 'pattern) port)
	    (write-string ": " port)
	    (write-condition-report (access-condition condition 'cause) port))))
  (set! signal-compile-error
	(condition-signaller condition-type:compile-regsexp
			     '(pattern cause)
			     standard-error-handler))
  unspecific)

;;;; Compiler rules

(define (define-rule pattern compiler)
  (add-boot-init!
   (lambda ()
     (if (not (and (pair? pattern)
		   (symbol? (car pattern))))
	 (error:bad-range-argument pattern 'define-rule))
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

(define-rule '(any-char)
  (lambda ()
    (insn:char-matching (negate (char=-predicate #\newline)))))

(define-rule `(char-ci datum)
  (lambda (char)
    (guarantee unicode-char? char)
    (insn:char char #t)))

(define-rule `(string-ci datum)
  (lambda (string)
    (guarantee string? string)
    (insn:string string #t)))

(define-rule '(char-matching expression)
  (lambda (predicate)
    (insn:char-matching
     (cond ((unary-procedure? predicate)
	    predicate)
	   ((and (syntax-match? '('not expression) predicate)
		 (unary-procedure? (cadr predicate)))
	    (cadr predicate))
	   (else
	    (error:not-a unary-procedure? predicate))))))

(define-rule '(char-in * datum)
  (lambda items
    (insn:char-set (char-set* items))))

(define-rule '(char-not-in * datum)
  (lambda items
    (insn:inverse-char-set (char-set* items))))

(define-rule '(legacy-char-syntax datum)
  (lambda (code)
    (insn:char-matching
     (if (or (char=? code #\-) (char=? code #\space))
	 char-whitespace?
	 (syntax-code-predicate code)))))

(define-rule '(inverse-legacy-char-syntax datum)
  (lambda (code)
    (insn:char-matching
     (negate
      (if (or (char=? code #\-) (char=? code #\space))
	  char-whitespace?
	  (syntax-code-predicate code))))))

(define (negate predicate)
  (lambda (object)
    (not (predicate object))))

(define-rule '(line-start) (lambda () (insn:line-start)))
(define-rule '(line-end) (lambda () (insn:line-end)))
(define-rule '(string-start) (lambda () (insn:string-start)))
(define-rule '(string-end) (lambda () (insn:string-end)))

(define-rule '(? form)			;greedy 0 or 1
  (lambda (regsexp)
    (insn:? (%compile-regsexp regsexp))))

(define-rule '(* form)			;greedy 0 or more
  (lambda (regsexp)
    (insn:* (%compile-regsexp regsexp))))

(define-rule '(+ form)			;greedy 1 or more
  (lambda (regsexp)
    (%compile-regsexp `(** 1 #f ,regsexp))))

(define-rule '(?? form)			;shy 0 or 1
  (lambda (regsexp)
    (insn:?? (%compile-regsexp regsexp))))

(define-rule '(*? form)			;shy 0 or more
  (lambda (regsexp)
    (insn:*? (%compile-regsexp regsexp))))

(define-rule '(+? form)			;shy 1 or more
  (lambda (regsexp)
    (%compile-regsexp `(**? 1 #f ,regsexp))))

(define-rule '(** datum form)		;greedy exactly N
  (lambda (n regsexp)
    (guarantee exact-nonnegative-integer? n)
    (insn:** n n (%compile-regsexp regsexp))))

(define-rule '(**? datum form)		;shy exactly N
  (lambda (n regsexp)
    (guarantee exact-nonnegative-integer? n)
    (insn:**? n n (%compile-regsexp regsexp))))

(define-rule '(** datum datum form)	;greedy between N and M
  (lambda (n m regsexp)
    (check-repeat-2-args n m)
    (insn:** n m (%compile-regsexp regsexp))))

(define-rule '(**? datum datum form)	;shy begin N and M
  (lambda (n m regsexp)
    (check-repeat-2-args n m)
    (insn:**? n m (%compile-regsexp regsexp))))

(define (check-repeat-2-args n m)
  (guarantee exact-nonnegative-integer? n)
  (if m
      (begin
	(guarantee exact-nonnegative-integer? m)
	(if (not (<= n m))
	    (error "Repeat lower limit greater than upper limit:" n m)))))

(define-rule '(alt * form)
  (lambda regsexps
    (insn:alt (map %compile-regsexp regsexps))))

(define-rule '(seq * form)
  (lambda regsexps
    (insn:seq (map %compile-regsexp regsexps))))

(define-rule `(group datum form)
  (lambda (key regsexp)
    (guarantee group-key? key)
    (insn:group key (%compile-regsexp regsexp))))

(define-rule `(group-ref datum)
  (lambda (key)
    (guarantee group-key? key)
    (insn:group-ref key)))

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

(define (insn:char-matching predicate)
  (lambda (succeed)
    (lambda (position groups fail)
      (if (let ((char (next-char position)))
	    (and char
		 (predicate char)))
	  (succeed (next-position position) groups fail)
	  (fail)))))

(define (insn:char char fold-case?)
  (insn:char-matching
   ((if fold-case? char-ci=-predicate char=-predicate) char)))

(define (insn:char-set char-set)
  (insn:char-matching (char-set-predicate char-set)))

(define (insn:inverse-char-set char-set)
  (insn:char-matching (negate (char-set-predicate char-set))))

(define (insn:string string fold-case?)
  (let ((end (string-length string)))
    (cond ((fix:= end 0)
	   (insn:always-succeed))
	  ((fix:= end 1)
	   (insn:char (string-ref string 0) fold-case?))
	  (else
	   (let ((c= (if fold-case? char-ci=? char=?)))
	     (lambda (succeed)
	       (lambda (position groups fail)
		 (let loop ((i 0) (position position))
		   (if (fix:< i end)
		       (if (let ((char (next-char position)))
			     (and char
				  (c= char (string-ref string i))))
			   (loop (fix:+ i 1) (next-position position))
			   (fail))
		       (succeed position groups fail))))))))))

(define (insn:group key insn)
  (let ((start
	 (lambda (succeed)
	   (lambda (position groups fail)
	     (succeed position
		      ((groups 'start) key position)
		      fail))))
	(end
	 (lambda (succeed)
	   (lambda (position groups fail)
	     (succeed position
		      ((groups 'end) key position)
		      fail)))))
    (lambda (succeed)
      (start (insn (end succeed))))))

(define (insn:group-ref key)
  (lambda (succeed)
    (lambda (position groups fail)
      ((let ((value ((groups 'get-value) key)))
	 (if value
	     ((insn:string value #f) succeed)
	     ;; This can happen with (* (GROUP ...)), but in other cases it
	     ;; would be an error.
	     succeed))
       position groups fail))))

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

(define (%hybrid-chain limit pre-linker)
  (if (<= limit 8)
      (%immediate-chain limit pre-linker)
      (%delayed-chain limit pre-linker)))

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
  ((position 'get-index)))

(define (next-char position)
  ((position 'next-char)))

(define (next-position position)
  ((position 'next-position)))

(define (prev-char position)
  ((position 'prev-char)))

(define (prev-position position)
  ((position 'prev-position)))

(define (same-positions? p1 p2)
  (and (eq? ((p1 'get-marker)) ((p2 'get-marker)))
       (fix:= ((p1 'get-index)) ((p2 'get-index)))))

(define (make-source-position source)
  (let ((marker (list 'source-position)))

    (define (at-index index next-char prev-char prev-position)

      (define (next-position)
	(at-index (fix:+ index 1) (source) next-char this))

      (define (this operator)
	(case operator
	  ((get-marker) (lambda () marker))
	  ((get-index) (lambda () index))
	  ((next-char) (lambda () next-char))
	  ((next-position) next-position)
	  ((prev-char) (lambda () prev-char))
	  ((prev-position) (lambda () prev-position))
	  (else (error "Unknown operator:" operator))))

      this)

    (at-index 0 (source) #f #f)))

(define (make-string-position string start end)
  (let ((marker (list 'string-position)))

    (define (at-index index)

      (define (next-char)
	(and (fix:< index end)
	     (string-ref string index)))

      (define (next-position)
	(at-index (fix:+ index 1)))

      (define (prev-char)
	(and (fix:> index start)
	     (string-ref string (fix:- index 1))))

      (define (prev-position)
	(at-index (fix:- index 1)))

      (lambda (operator)
	(case operator
	  ((get-marker) (lambda () marker))
	  ((get-index) (lambda () index))
	  ((next-char) next-char)
	  ((next-position) next-position)
	  ((prev-char) prev-char)
	  ((prev-position) prev-position)
	  (else (error "Unknown operator:" operator)))))

    (at-index start)))

;;;; Groups

(define (make-groups)

  (define (state started-groups ended-groups)

    (define (start key position)
      (if (assv key started-groups)
	  (error "Incorrectly nested group:" key))
      (state (cons (cons key position) started-groups)
	     ended-groups))

    (define (end key position)
      (if (not (and (pair? started-groups)
		    (eqv? (caar started-groups) key)))
	  (error "Incorrectly nested group:" key))
      (state (cdr started-groups)
	     (cons (finish-group key
				 (cdar started-groups)
				 position)
		   ended-groups)))

    (define (finish-group key start-position end-position)
      (cons key
	    (let loop ((position end-position) (chars '()))
	      (if (same-positions? position start-position)
		  (list->string chars)
		  (let ((char (prev-char position)))
		    (loop (prev-position position)
			  (cons char chars)))))))

    (define (get-value key)
      (if (assv key started-groups)
	  (error "Can't refer to unfinished group:" key))
      (let ((p (assv key ended-groups)))
	(and p
	     (cdr p))))

    (lambda (operator)
      (case operator
	((start) start)
	((end) end)
	((get-value) get-value)
	((get-all) (lambda () (reverse ended-groups)))
	(else (error "Unknown operator:" operator)))))

  (state '() '()))

;;;; Match and search

(define (regsexp-match-string crsexp string #!optional start end)
  (let* ((caller 'regsexp-match-string)
	 (end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller)))
    (guarantee nfc-string? string caller)
    (top-level-match crsexp (make-string-position string start end))))

(define (regsexp-search-string-forward crsexp string #!optional start end)
  (let* ((caller 'regsexp-search-string-forward)
	 (end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller)))
    (guarantee nfc-string? string caller)
    (let loop ((position (make-string-position string start end)))
      (or (top-level-match crsexp position)
	  (and (next-char position)
	       (loop (next-position position)))))))

(define (regsexp-match-input-port crsexp port)
  (top-level-match crsexp
		   (make-source-position
		    (lambda ()
		      (let ((char (read-char port)))
			(if (eof-object? char)
			    #f
			    char))))))

;;;; Convert regexp pattern to regsexp

(define (re-pattern->regsexp pattern)
  (let ((end (string-length pattern)))
    (let ((index 0)
	  (this-alt '())
	  (prev-alts '())
	  (group-number 0)
	  (pending-groups '()))

      (define (have-next?)
	(fix:< index end))

      (define (get-next)
	(let ((char (string-ref pattern index)))
	  (set! index (fix:+ index 1))
	  char))

      (define (next-is? char)
	(and (char=? (string-ref pattern index) char)
	     (begin
	       (set! index (fix:+ index 1))
	       #t)))

      (define (get-expr)
	(let ((alt (get-alt)))
	  (if (pair? prev-alts)
	      `(alt ,@(reverse (cons alt prev-alts)))
	      alt)))

      (define (get-alt)
        (let ((exprs (optimize-alt (reverse this-alt) #f)))
          (if (= (length exprs) 1)
              (car exprs)
              `(seq ,@exprs))))

      (define (optimize-alt exprs builder)
        (if (pair? exprs)
            (if (char? (car exprs))
                (let ((builder (or builder (string-builder))))
                  (builder (car exprs))
                  (optimize-alt (cdr exprs) builder))
                (if builder
                    (cons (builder)
                          (cons (car exprs)
                                (optimize-alt (cdr exprs) #f)))
                    (cons (car exprs)
                          (optimize-alt (cdr exprs) #f))))
            (if builder
                (list (builder))
                '())))

      (define (dispatch)
	(if (have-next?)
	    (let ((char (get-next)))
	      (case char
		((#\\) (dispatch-backslash))
		((#\$) (output-expr '(line-end)))
		((#\^) (output-expr '(line-start)))
		((#\.) (output-expr '(any-char)))
		((#\[) (parse-char-set))
		((#\*) (replace-last-expr (lambda (expr) `(* ,expr))))
		((#\+) (replace-last-expr (lambda (expr) `(+ ,expr))))
		((#\?) (replace-last-expr (lambda (expr) `(? ,expr))))
		(else (output-expr char))))
	    (get-expr)))

      (define (dispatch-backslash)
	(let ((char (get-next)))
	  (case char
	    ((#\<) (output-expr '(word-start)))
	    ((#\>) (output-expr '(word-end)))
	    ((#\b) (output-expr '(word-bound)))
	    ((#\B) (output-expr '(not-word-bound)))
	    ((#\`) (output-expr '(string-start)))
	    ((#\') (output-expr '(string-end)))
	    ((#\w) (output-expr '(char-in whitespace)))
	    ((#\W) (output-expr '(char-not-in whitespace)))
	    ((#\s) (output-expr `(legacy-char-syntax ,(get-next))))
	    ((#\S) (output-expr `(inverse-legacy-char-syntax ,(get-next))))
	    ((#\() (start-group))
	    ((#\)) (end-group))
	    ((#\|) (push-alt))
	    (else (error "Unsupported regexp:" (string #\\ char))))))

      (define (output-expr expr)
	(set! this-alt (cons expr this-alt))
        (dispatch))

      (define (replace-last-expr transform)
	(set-car! this-alt (transform (car this-alt)))
        (dispatch))

      (define (start-group)
	(set! group-number (fix:+ group-number 1))
	(set! pending-groups
	      (cons (vector group-number this-alt prev-alts)
		    pending-groups))
	(set! this-alt '())
	(set! prev-alts '())
        (dispatch))

      (define (end-group)
	(let ((expr `(group ,(vector-ref (car pending-groups) 0) ,(get-expr))))
	  (set! this-alt (vector-ref (car pending-groups) 1))
	  (set! prev-alts (vector-ref (car pending-groups) 2))
	  (set! pending-groups (cdr pending-groups))
	  (output-expr expr)))

      (define (push-alt)
	(set! prev-alts (cons (get-alt) prev-alts))
	(set! this-alt '())
        (dispatch))

      (define (parse-char-set)
        (let loop
            ((chars
              (append (if (next-is? #\^)
                          (list #\^)
                          '())
                      (if (next-is? #\])
                          (list #\])
                          '()))))
          (let ((char (get-next)))
            (if (char=? char #\])
                (output-expr
                 (receive (ranges invert?)
                     (re-char-pattern->code-points
                      (list->string (reverse chars)))
                   (cons (if invert? 'char-not-in 'char-in)
			 (normalize-ranges ranges))))
                (loop (cons char chars))))))

      (dispatch))))