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

;;;; SRFI 115: Scheme Regular Expressions
;;; package: (runtime regexp srfi-115)

(declare (usual-integrations))

(define (sre? object)
  (and (or (find-cset-sre-rule object)
	   (find-sre-rule object))
       #t))
(register-predicate! sre? 'source-regexp)

(define (cset-sre? object)
  (and (find-cset-sre-rule object)
       #t))
(register-predicate! cset-sre? 'char-set-regexp)

(define (compile-sre-top-level sre)
  (make-regexp
   (parameterize ((%input-pattern sre)
		  (%submatch-next 1))
     (generate-matcher
      (lambda ()
	(compile-sre sre))))))

(define %input-pattern (make-unsettable-parameter #f))
(define %submatch-next (make-settable-parameter #f))

(define (next-submatch-number)
  (let ((n (%submatch-next)))
    (%submatch-next (+ n 1))
    n))

(define-record-type <regexp>
    (make-regexp impl)
    regexp?
  (impl regexp-impl))

(define condition-type:compile-regexp)
(define compile-error)
(define (initialize-conditions!)
  (set! condition-type:compile-regexp
	(make-condition-type 'compile-sre condition-type:error
	    '(pattern element)
	  (lambda (condition port)
	    (write-string "Ill-formed regular s-expression: " port)
	    (write (access-condition condition 'element) port)
	    (write-string " from pattern: " port)
	    (write (access-condition condition 'pattern) port))))
  (set! compile-error
	(condition-signaller condition-type:compile-regexp
			     '(pattern element)
			     standard-error-handler))
  unspecific)

;;;; Procedures

(define (regexp-matches re string #!optional start end)
  (guarantee nfc-string? string 'regexp-matches)
  (let* ((end (fix:end-index end (string-length string) 'regexp-matches))
	 (start (fix:start-index start end 'regexp-matches)))
    (%regexp-match (regexp re) string start end)))

(define (regexp-matches? re string #!optional start end)
  (guarantee nfc-string? string 'regexp-matches?)
  (let* ((end (fix:end-index end (string-length string) 'regexp-matches?))
	 (start (fix:start-index start end 'regexp-matches?)))
    (%regexp-match (regexp re) string start end)))

(define (regexp-search re string #!optional start end)
  (guarantee nfc-string? string 'regexp-search)
  (let* ((end (fix:end-index end (string-length string) 'regexp-search))
	 (start (fix:start-index start end 'regexp-search)))
    (let ((regexp (regexp re)))
      (let loop ((index start))
	(if (fix:< index end)
	    (or (%regexp-match regexp string index end)
		(loop (fix:+ index 1)))
	    (%regexp-match regexp string index end))))))

(define (regexp re)
  (if (regexp? re)
      re
      (compile-sre-top-level re)))

(define (regexp->nfa regexp)
  (matcher->nfa (regexp-impl regexp)))

(define (print-regexp regexp #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (fresh-line port)
    (for-each (lambda (object)
		(write-line object port))
	      (regexp->nfa regexp))))

(define (%regexp-match regexp string start end)
  (let ((groups (run-matcher (regexp-impl regexp) string start end)))
    (and groups
	 (make-regexp-match (car groups) (cdr groups)))))

(define-record-type <regexp-match>
    (make-regexp-match group0 groups)
    regexp-match?
  (group0 %regexp-match-group0)
  (groups %regexp-match-groups))

(define-print-method regexp-match?
  (standard-print-method 'regexp-match
    (lambda (match)
      (list (group-value (%regexp-match-group0 match))))))

(define (regexp-match-count match)
  (length (%regexp-match-groups match)))

(define (%match-group match key caller)
  (if (eqv? key 0)
      (%regexp-match-group0 match)
      (let ((group
	     (find (lambda (group)
		     (eqv? key (group-key group)))
		   (%regexp-match-groups match))))
	(if (not group)
	    (error:bad-range-argument key caller))
	group)))

(define (regexp-match-submatch match key)
  (group-value (%match-group match key 'regexp-match-submatch)))

(define (regexp-match-submatch-start match key)
  (group-start (%match-group match key 'regexp-match-submatch-start)))

(define (regexp-match-submatch-end match key)
  (group-end (%match-group match key 'regexp-match-submatch-end)))

(define (regexp-match->list match)
  (cons (group-value (%regexp-match-group0 match))
	(map group-value (%regexp-match-groups match))))

(define (regexp-match-keys match)
  (cons (group-key (%regexp-match-group0 match))
	(map group-key (%regexp-match-groups match))))

;;;; Compiler rules

(define sre-rules)
(define sre-rewrite-rules)
(define cset-sre-rules)
(define cset-sre-rewrite-rules)
(defer-boot-action 'regexp-rules
  (lambda ()
    (set! sre-rules (make-rules 'sre))
    (set! sre-rewrite-rules (make-rules 'sre-rewrite))
    (set! cset-sre-rules (make-rules 'cset-sre))
    (set! cset-sre-rewrite-rules (make-rules 'cset-sre-rewrite))
    unspecific))

(define (rule-finder rules rewrite-rules)
  (let ((matcher (rules-matcher rules))
	(rewriter (rules-rewriter rewrite-rules)))
    (lambda (object)
      (matcher (rewriter object)))))

(define-deferred-procedure find-sre-rule 'regexp-rules
  (rule-finder sre-rules sre-rewrite-rules))

(define-deferred-procedure find-cset-sre-rule 'regexp-rules
  (rule-finder cset-sre-rules cset-sre-rewrite-rules))

(define (pattern-rule-definer rules)
  (let ((adder (rules-adder rules)))
    (lambda (pattern operation #!optional predicate)
      (adder
       (if (pattern? pattern)
	   (pattern-rule pattern operation predicate)
	   (general-rule pattern predicate operation))))))

(define-deferred-procedure define-sre-rule 'regexp-rules
  (pattern-rule-definer sre-rules))

(define-deferred-procedure define-sre-rewriter 'regexp-rules
  (pattern-rule-definer sre-rewrite-rules))

(define-deferred-procedure define-cset-sre-rule 'regexp-rules
  (pattern-rule-definer cset-sre-rules))

(define-deferred-procedure define-cset-sre-rewriter 'regexp-rules
  (pattern-rule-definer cset-sre-rewrite-rules))

(define (alias-rule-definer rules)
  (let ((adder (rules-adder rules)))
    (lambda (from to)
      (guarantee interned-symbol? from)
      (guarantee interned-symbol? to)
      (adder
       (pattern-rule `(,from . ,any-object?)
		     (lambda args (cons to args)))))))

(define-deferred-procedure define-sre-alias 'regexp-rules
  (alias-rule-definer sre-rewrite-rules))

(define-deferred-procedure define-cset-sre-alias 'regexp-rules
  (alias-rule-definer cset-sre-rewrite-rules))

(define (compile-sre sre)
  (cond ((find-cset-sre-rule sre)
	 => (lambda (rule)
	      (insn:char-set ((rule-operation rule) sre) #f)))
	((find-sre-rule sre)
	 => (lambda (rule)
	      ((rule-operation rule) sre)))
	(else
	 (compile-error (%input-pattern) sre))))

(define (compile-sres sres)
  (insn:seq (map-in-order compile-sre sres)))

(define (compile-cset-sre cset-sre)
  (cond ((find-cset-sre-rule cset-sre)
	 => (lambda (rule)
	      ((rule-operation rule) cset-sre)))
	(else
	 (compile-error (%input-pattern) cset-sre))))

(define (compile-cset-sres cset-sres)
  (map-in-order compile-cset-sre cset-sres))

(define (any-char? object)
  (unicode-char? object))

(define (min-arity? object)
  (exact-nonnegative-integer? object))

(define (max-arity? object)
  (exact-nonnegative-integer? object))

(define (backref-key? object)
  (or (exact-positive-integer? object)
      (interned-symbol? object)))

;;;; <sre>

(define-sre-rule "char"
  (lambda (char) (insn:char char #f))
  any-char?)

(define-sre-rule "string"
  (lambda (string) (insn:string string #f))
  string?)

(define-sre-rule `(* . ,sre?)
  (lambda sres (insn:* (compile-sres sres))))
(define-sre-alias 'zero-or-more '*)

(define-sre-rule `(+ . ,sre?)
  (lambda sres (insn:>= 1 (compile-sres sres))))
(define-sre-alias 'one-or-more '+)

(define-sre-rule `(? . ,sre?)
  (lambda sres (insn:? (compile-sres sres))))
(define-sre-alias 'optional '?)

(define-sre-rule `(= ,min-arity? . ,sre?)
  (lambda (n . sres) (insn:= n (compile-sres sres))))
(define-sre-alias 'exactly '=)

(define-sre-rule `(>= ,min-arity? . ,sre?)
  (lambda (n . sres) (insn:>= n (compile-sres sres))))
(define-sre-alias 'at-least '>=)

(define-sre-rule `(** ,min-arity? ,max-arity? . ,sre?)
  (lambda (n m . sres) (insn:** n m (compile-sres sres)))
  (lambda (n m . sres) (declare (ignore sres)) (<= n m)))
(define-sre-alias 'repeated '**)

(define-sre-rule `(|\|| . ,sre?)
  (lambda sres (insn:alt (map-in-order compile-sre sres))))
(define-sre-alias 'or '|\||)

(define-sre-rule `(: . ,sre?)
  (lambda sres (compile-sres sres)))
(define-sre-alias 'seq ':)

(define-sre-rule `($ . ,sre?)
  (lambda sres
    (insn:group (next-submatch-number)
		(compile-sres sres))))
(define-sre-alias 'submatch '$)

(define-sre-rule `(-> ,interned-symbol? . ,sre?)
  (lambda (key . sres)
    (insn:group key (compile-sres sres))))
(define-sre-alias 'submatch-named '->)

(define-sre-rule 'bos (lambda () (insn:string-start)))
(define-sre-rule 'eos (lambda () (insn:string-end)))
(define-sre-rule 'bol (lambda () (insn:line-start)))
(define-sre-rule 'eol (lambda () (insn:line-end)))

(define-sre-rule `(?? . ,sre?)
  (lambda sres (insn:?? (compile-sres sres))))
(define-sre-alias 'non-greedy-optional '??)

(define-sre-rule `(*? . ,sre?)
  (lambda sres (insn:*? (compile-sres sres))))
(define-sre-alias 'non-greedy-zero-or-more '*?)

(define-sre-rule `(**? ,min-arity? ,max-arity? . ,sre?)
  (lambda (n m . sres) (insn:**? n m (compile-sres sres)))
  (lambda (n m . sres) (declare (ignore sres)) (<= n m)))
(define-sre-alias 'non-greedy-repeated '**?)

;;;; <cset-sre>

(define-cset-sre-rule "char"
  (lambda (char) (char-set char))
  any-char?)

(define-cset-sre-rule "string"
  (lambda (string) (char-set string))
  (lambda (object)
    (and (string? object)
	 (fix:= 1 (string-length object)))))

(define-cset-sre-rule "char-set"
  (lambda (cs) cs)
  (lambda (object) (char-set? object)))

(define-cset-sre-rule `(,string?)
  (lambda (s) (char-set s)))

(define-cset-sre-rule `(char-set ,string?)
  (lambda (s) (char-set s)))

(define (range-spec? object)
  (or (unicode-char? object)
      (and (string? object)
	   (let ((end (string-length object)))
	     (and (even? end)
		  (let loop ((i 0))
		    (if (fix:< i end)
			(and (char<=? (string-ref object i)
				      (string-ref object (fix:+ i 1)))
			     (loop (fix:+ i 2)))
			#t)))))))

(define (range-spec->ranges rs)
  (if (unicode-char? rs)
      (list rs)
      (let ((end (string-length rs)))
	(let loop ((i 0) (ranges '()))
	  (if (fix:< i end)
	      (loop (fix:+ i 2)
		    (cons (list (char->integer (string-ref rs i))
				(char->integer (string-ref rs (fix:+ i 1))))
			  ranges))
	      ranges)))))

(define-cset-sre-rule `(/ . ,range-spec?)
  (lambda rs (char-set* (append-map range-spec->ranges rs))))
(define-cset-sre-alias 'char-range '/)

(define-cset-sre-rule `(or  . ,cset-sre?)
  (lambda cset-sres (char-set-union* (compile-cset-sres cset-sres))))
(define-cset-sre-alias '|\|| 'or)

(define-cset-sre-rule `(and  . ,cset-sre?)
  (lambda cset-sres (char-set-intersection* (compile-cset-sres cset-sres))))
(define-cset-sre-alias '& 'and)

(define-cset-sre-rule `(-  . ,cset-sre?)
  (lambda cset-sres (apply char-set-difference (compile-cset-sres cset-sres))))
(define-cset-sre-alias 'difference '-)

(define-cset-sre-rule `(~  . ,cset-sre?)
  (lambda cset-sres
    (char-set-difference char-set:unicode
			 (char-set-union* (compile-cset-sres cset-sres)))))
(define-cset-sre-alias 'complement '~)

(for-each (lambda (names)
	    (let ((operation (lambda () (char-set (car names)))))
	      (for-each (lambda (name)
			  (define-cset-sre-rule name operation))
			names)))
	  '((any)
	    (nonl)
	    (ascii)
	    (lower-case lower)
	    (upper-case upper)
	    (title-case title)
	    (alphabetic alpha)
	    (numeric num)
	    (alphanumeric alphanum alnum)
	    (punctuation punct)
	    (symbol)
	    (graphic graph)
	    (whitespace white space)
	    (printing print)
	    (control cntrl)
	    (hex-digit xdigit)))