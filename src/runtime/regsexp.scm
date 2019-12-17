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
;;; package: (runtime regexp regsexp)

(declare (usual-integrations))

(define (regsexp? object)
  (and (match-rule object) #t))
(register-predicate! regsexp? 'regular-sexpression)

(define (compile-regsexp regsexp)
  (%link-insn
   (parameterize ((%input-pattern regsexp))
     (%compile-regsexp regsexp))))

(define %input-pattern
  (make-unsettable-parameter #f))

(define (%compile-regsexp regsexp)
  (let ((thunk (match-rule regsexp)))
    (if (not thunk)
	(compile-error (%input-pattern) regsexp))
    (thunk)))

(define (%link-insn insn)
  (make-compiled-regsexp
   (insn
    (lambda (position groups fail)
      (declare (ignore fail))
      (cons position (all-groups groups))))))

(define-record-type <compiled-regsexp>
    (make-compiled-regsexp impl)
    compiled-regsexp?
  (impl compiled-regsexp-impl))

(define (top-level-match crsexp start-position)
  (let ((result
	 ((compiled-regsexp-impl crsexp)
	  start-position (make-groups) (lambda () #f))))
    (and result
	 (cons* (pos-index start-position)
		(pos-index (car result))
		(map (lambda (group)
		       (cons (group-key group)
			     (group-value group)))
		     (cdr result))))))

(define (regsexp-group-key? object)
  (or (fix:fixnum? object)
      (unicode-char? object)
      (symbol? object)))

(define condition-type:compile-regsexp)
(define compile-error)
(define (initialize-conditions!)
  (set! condition-type:compile-regsexp
	(make-condition-type 'compile-regsexp condition-type:error
	    '(pattern element)
	  (lambda (condition port)
	    (write-string "Ill-formed regular s-expression: " port)
	    (write (access-condition condition 'element) port)
	    (write-string " from pattern: " port)
	    (write (access-condition condition 'pattern) port))))
  (set! compile-error
	(condition-signaller condition-type:compile-regsexp
			     '(pattern element)
			     standard-error-handler))
  unspecific)

;;;; Compiler rules

(define regsexp-rules)
(defer-boot-action 'regexp-rules
  (lambda ()
    (set! regsexp-rules (make-rules 'regsexp))
    unspecific))

(define-deferred-procedure match-rule 'regexp-rules
  (rules-matcher regsexp-rules))

(define-deferred-procedure define-rule 'regexp-rules
  (rules-definer regsexp-rules))

(define (any-char? object)
  (unicode-char? object))

(define (min-arity? object)
  (exact-nonnegative-integer? object))

(define (max-arity? object)
  (or (not object)
      (exact-nonnegative-integer? object)))

(define-rule "char"
  (lambda (char) (insn:char char #f))
  any-char?)

(define-rule "string"
  (lambda (string) (insn:string string #f))
  string?)

(define-rule '(any-char)
  (lambda ()
    (insn:char-matching (complement (char=-predicate #\newline)))))

(define-rule `(char-ci ,any-char?)
  (lambda (char) (insn:char char #t)))

(define-rule `(string-ci ,string?)
  (lambda (string) (insn:string string #t)))

(define-rule `(char-matching ,unary-procedure?)
  (lambda (predicate) (insn:char-matching predicate)))

(define-rule `(char-matching (not ,unary-procedure?))
  (lambda (expr) (insn:char-matching (complement (cadr expr)))))

(define-rule `(char-in . ,cpl-element?)
  (lambda items (insn:char-set (char-set* items))))

(define-rule `(char-not-in . ,cpl-element?)
  (lambda items (insn:inverse-char-set (char-set* items))))

(define (syntax-code? object)
  (or (eqv? object #\-)
      (eqv? object #\space)
      (char-syntax-code? object)))

(define-rule `(legacy-char-syntax ,syntax-code?)
  (lambda (code)
    (insn:char-matching (syntax-code-predicate code))))

(define-rule `(inverse-legacy-char-syntax ,syntax-code?)
  (lambda (code)
    (insn:char-matching (complement (syntax-code-predicate code)))))

(define-rule '(line-start) (lambda () (insn:line-start)))
(define-rule '(line-end) (lambda () (insn:line-end)))
(define-rule '(string-start) (lambda () (insn:string-start)))
(define-rule '(string-end) (lambda () (insn:string-end)))

(define-rule `(? ,regsexp?)		;greedy 0 or 1
  (lambda (regsexp)
    (insn:? (%compile-regsexp regsexp))))

(define-rule `(* ,regsexp?)		;greedy 0 or more
  (lambda (regsexp)
    (insn:* (%compile-regsexp regsexp))))

(define-rule `(+ ,regsexp?)		;greedy 1 or more
  (lambda (regsexp)
    (insn:** 1 #f (%compile-regsexp regsexp))))

(define-rule `(?? ,regsexp?)		;shy 0 or 1
  (lambda (regsexp)
    (insn:?? (%compile-regsexp regsexp))))

(define-rule `(*? ,regsexp?)		;shy 0 or more
  (lambda (regsexp)
    (insn:*? (%compile-regsexp regsexp))))

(define-rule `(+? ,regsexp?)		;shy 1 or more
  (lambda (regsexp)
    (insn:**? 1 #f (%compile-regsexp regsexp))))

(define-rule `(** ,min-arity? ,regsexp?) ;greedy exactly N
  (lambda (n regsexp)
    (insn:** n n (%compile-regsexp regsexp))))

(define-rule `(**? ,min-arity? ,regsexp?) ;shy exactly N
  (lambda (n regsexp)
    (insn:**? n n (%compile-regsexp regsexp))))

(define-rule `(** ,min-arity? ,max-arity? ,regsexp?) ;greedy between N and M
  (lambda (n m regsexp) (insn:** n m (%compile-regsexp regsexp)))
  (lambda (n m regsexp)
    (declare (ignore regsexp))
    (or (not m)
	(<= n m))))

(define-rule `(**? ,min-arity? ,max-arity? ,regsexp?) ;shy between N and M
  (lambda (n m regsexp) (insn:**? n m (%compile-regsexp regsexp)))
  (lambda (n m regsexp)
    (declare (ignore regsexp))
    (or (not m)
	(<= n m))))

(define-rule `(alt . ,regsexp?)
  (lambda regsexps
    (insn:alt (map-in-order %compile-regsexp regsexps))))

(define-rule `(seq . ,regsexp?)
  (lambda regsexps
    (insn:seq (map-in-order %compile-regsexp regsexps))))

(define-rule `(group ,regsexp-group-key? ,regsexp?)
  (lambda (key regsexp)
    (insn:group key (%compile-regsexp regsexp))))

(define-rule `(group-ref ,regsexp-group-key?)
  (lambda (key)
    (insn:group-ref key)))

;;;; Match, search, and replace

(define (regsexp-match-string crsexp string #!optional start end)
  (let ((caller 'regsexp-match-string))
    (guarantee nfc-string? string caller)
    (let* ((end (fix:end-index end (string-length string) caller))
	   (start (fix:start-index start end caller)))
      (top-level-match crsexp (make-string-position string start end)))))

(define (regsexp-search-string-forward crsexp string #!optional start end)
  (let ((caller 'regsexp-search-string-forward))
    (guarantee nfc-string? string caller)
    (let* ((end (fix:end-index end (string-length string) caller))
	   (start (fix:start-index start end caller)))
      (let loop ((position (make-string-position string start end)))
	(or (top-level-match crsexp position)
	    (and (next-char position)
		 (loop (next-position position))))))))

(define (regsexp-match-input-port crsexp port)
  (guarantee textual-input-port? port 'regsexp-match-input-port)
  (top-level-match crsexp
		   (make-source-position
		    (lambda ()
		      (let ((char (read-char port)))
			(if (eof-object? char)
			    #f
			    char))))))

(define (regsexp-match? object)
  (and (pair? object)
       (exact-nonnegative-integer? (car object))
       (pair? (cdr object))
       (exact-nonnegative-integer? (cadr object))
       (<= (car object) (cadr object))
       (list? (cddr object))
       (every (lambda (elt)
		(and (pair? elt)
		     (regsexp-group-key? (car elt))
		     (string? (cdr elt))))
	      (cddr object))))
(register-predicate! regsexp-match? 'regsexp-match)

(define (match-value key match caller)
  (let ((p (assv key (cddr match))))
    (if (not p)
	(error:bad-range-argument key caller))
    (cdr p)))

(define (regsexp-replacement? object)
  (or (string? object)
      (regsexp-group-key? object)
      (and (list? object)
	   (every regsexp-replacement? object))))
(register-predicate! regsexp-replacement? 'regsexp-replacement)

(define (regsexp-replacer replacement)
  (let ((replacer (%regsexp-replacer replacement 'regsexp-replacer)))
    (lambda (match)
      (guarantee regsexp-match? match 'regsexp-replacer)
      (let ((builder (string-builder)))
	(replacer builder match)
	(builder)))))

(define (%regsexp-replacer replacement caller)
  (let loop ((r replacement))
    (cond ((string? r)
	   (lambda (builder match)
	     (declare (ignore match))
	     (builder r)))
	  ((regsexp-group-key? r)
	   (lambda (builder match)
	     (builder (match-value r match caller))))
	  ((list? r)
	   (let ((elts (map loop r)))
	     (lambda (builder match)
	       (for-each (lambda (elt) (elt builder match)) elts))))
	  (else
	   (error:not-a regsexp-replacement? r caller)))))

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