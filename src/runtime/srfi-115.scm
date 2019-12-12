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

(define (valid-sre? object)
  (and (or (find-cset-sre-rule initial-ctx object)
	   (find-sre-rule initial-ctx object))
       #t))
(register-predicate! valid-sre? 'source-regexp)

(define (valid-cset-sre? object)
  (and (find-cset-sre-rule initial-ctx object)
       #t))
(register-predicate! valid-cset-sre? 'char-set-regexp)

(define (compile-sre-top-level sre)
  (parameterize ((input-pattern sre)
		 (submatch-next (make-index-generator 1))
		 (submatch-keys (make-submatch-keys)))
    (make-regexp sre
		 (compile-matcher
		  (lambda ()
		    (compile-sre initial-ctx sre)))
		 (submatch-keys->list (submatch-keys)))))

(define input-pattern (make-parameter #f))
(define submatch-next (make-parameter #f))

(define (next-submatch-number)
  ((submatch-next)))

(define-record-type <regexp>
    (make-regexp sre impl submatch-keys)
    regexp?
  (sre regexp->sre)
  (impl regexp-impl)
  (submatch-keys regexp-submatch-keys))

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

;;;; Match and search

(define (regexp-matches? re string #!optional start end)
  (guarantee nfc-string? string 'regexp-matches?)
  (let* ((end (fix:end-index end (string-length string) 'regexp-matches?))
	 (start (fix:start-index start end 'regexp-matches?)))
    (and (run-matcher (regexp-impl
		       (if (regexp? re)
			   re
			   ;; Disable captures to speed up match.
			   (compile-sre-top-level `(w/nocapture ,re))))
		      string start end)
	 #t)))

(define (regexp-matches re string #!optional start end)
  (guarantee nfc-string? string 'regexp-matches)
  (let* ((end (fix:end-index end (string-length string) 'regexp-matches))
	 (start (fix:start-index start end 'regexp-matches)))
    (%regexp-match (regexp re) string start end)))

(define (%regexp-match regexp string start end)
  (let ((groups (run-matcher (regexp-impl regexp) string start end)))
    (and groups
	 (make-regexp-match (car groups)
			    (cdr groups)
			    (regexp-submatch-keys regexp)))))

(define (regexp-search re string #!optional start end)
  (guarantee nfc-string? string 'regexp-search)
  (let* ((end (fix:end-index end (string-length string) 'regexp-search))
	 (start (fix:start-index start end 'regexp-search)))
    (%regexp-search (regexp re) string start end)))

(define (%regexp-search regexp string start end)
  (let loop ((index start))
    (if (fix:< index end)
	(or (%regexp-match regexp string index end)
	    (loop (fix:+ index 1)))
	(%regexp-match regexp string index end))))

(define (regexp-search-all re string #!optional start end)
  (guarantee nfc-string? string 'regexp-search)
  (let* ((end (fix:end-index end (string-length string) 'regexp-search))
	 (start (fix:start-index start end 'regexp-search)))
    (%regexp-search-all (regexp re) string start end)))

(define (%regexp-search-all regexp string start end)
  (let loop ((index start))
    (let ((match (%regexp-search regexp string index end)))
      (if match
	  (cons match (loop (regexp-match-start match)))
	  '()))))

;;;; Match datatype

(define-record-type <regexp-match>
    (make-regexp-match group0 groups keys)
    regexp-match?
  (group0 regexp-match-group)
  (groups regexp-submatch-groups)
  (keys regexp-match-submatch-keys))

(define (regexp-match-value match)
  (group-value (regexp-match-group match)))

(define (regexp-match-start match)
  (group-start (regexp-match-group match)))

(define (regexp-match-end match)
  (group-end (regexp-match-group match)))

(define (regexp-match-access proc match key caller)
  (if (eqv? key 0)
      (proc (regexp-match-group match))
      (begin
	(guarantee regexp-match-key? key caller)
	(if (not (memv key (regexp-match-submatch-keys match)))
	    (error:bad-range-argument key caller))
	(%regexp-match-access proc match key))))

(define (%regexp-match-access proc match key)
  (let ((group
	 (find (lambda (group)
		 (eq? key (group-key group)))
	       (regexp-submatch-groups match))))
    (and group
	 (proc group))))

(define (regexp-match-submatch match key)
  (regexp-match-access group-value match key 'regexp-match-submatch))

(define (regexp-match-submatch-start match key)
  (regexp-match-access group-start match key 'regexp-match-submatch-start))

(define (regexp-match-submatch-end match key)
  (regexp-match-access group-end match key 'regexp-match-submatch-end))

(define (regexp-match-count match)
  (length (regexp-submatch-groups match)))

(define (regexp-match-keys match)
  (cons 0 (regexp-match-submatch-keys match)))

(define (regexp-match->list match)
  (cons (regexp-match-value match)
	(map (lambda (key)
	       (%regexp-match-access group-value match key))
	     (regexp-match-submatch-keys match))))

(define-print-method regexp-match?
  (standard-print-method 'regexp-match
    (lambda (match)
      (list (group-value (regexp-match-group match))))))

;;;; Replacement

(define (regexp-replace re string subst #!optional start end count)
  (guarantee regexp-replace-subst? subst 'regexp-replace)
  (let* ((len (string-length string))
	 (end (if end (fix:end-index end len 'regexp-replace) len))
	 (start (fix:start-index start end 'regexp-replace))
	 (regexp (regexp re))
	 (count
	  (if (default-object? count)
	      0
	      (guarantee exact-nonnegative-integer? count 'regexp-replace))))

    (define (find-match index n)
      (let ((match (%regexp-search regexp string index end)))
	(if match
	    (if (< n count)
		(find-match (regexp-match-start match)
			    (- n 1))
		(string-append (subst-match 'pre match string start end)
			       (subst-match subst match string start end)
			       (subst-match 'post match string start end)))
	    (substring string start end))))

    (find-match start 0)))

(define (regexp-replace-all re string subst #!optional start end)
  (guarantee regexp-replace-subst? subst 'regexp-replace-all)
  (let* ((len (string-length string))
	 (end (if end (fix:end-index end len 'regexp-replace-all) len))
	 (start (fix:start-index start end 'regexp-replace-all))
	 (regexp (regexp re)))

    (define (subst-matches matches start)
      (if (pair? matches)
	  (let ((match (car matches))
		(matches (cdr matches)))
	    (cons* (subst-match 'pre match string start end)
		   (subst-match subst match string start
				(if (pair? matches)
				    (regexp-match-start (car matches))
				    end))
		   (subst-matches matches (regexp-match-end match))))
	  '()))

    (let ((matches (%regexp-search regexp string start end)))
      (if (pair? matches)
	  (string-append* (subst-matches matches start))
	  (substring string start end)))))

(define (regexp-replace-subst? object)
  (or (string? object)
      (regexp-match-key? object)
      (eq? 'pre object)
      (eq? 'post object)))
(register-predicate! regexp-replace-subst? 'regexp-replace-subst)

(define (subst-match match subst string start end)
  (cond ((string? subst)
	 subst)
	((eq? 'pre subst)
	 (string-slice string start (regexp-match-start match)))
	((eq? 'post subst)
	 (string-slice string (regexp-match-end match) end))
	(else
	 (or (regexp-match-submatch match subst) ""))))

(define (regexp-match-key? object)
  (or (exact-nonnegative-integer? object)
      (interned-symbol? object)))
(register-predicate! regexp-match-key? 'regexp-match-key)

(define (regexp-match-replacement? object)
  (or (string? object)
      (regexp-match-key? object)
      (and (list? object)
	   (every regexp-match-replacement? object))))
(register-predicate! regexp-match-replacement? 'regexp-match-replacement)

(define (regexp-match-replace match repl)
  (guarantee regexp-match? match 'regexp-match-replace)
  (let ((builder (string-builder)))
    (let loop ((repl repl))
      (cond ((string? repl)
	     (builder repl))
	    ((regexp-match-key? repl)
	     (builder (or (regexp-match-submatch match repl) "")))
	    ((list? repl)
	     (for-each loop repl))
	    (else
	     (error:not-a regexp-match-replacement? repl
			  'regexp-match-replace))))
    (builder)))

;;;; Fold

(define (regexp-fold re kons knil string #!optional finish start end)
  (guarantee nfc-string? string 'regexp-fold)
  (let* ((end (fix:end-index end (string-length string) 'regexp-fold))
	 (start (fix:start-index start end 'regexp-fold)))
    (%regexp-fold kons knil finish re string start end)))

(define (%regexp-fold kons knil finish re string start end)
  (let ((regexp (regexp re)))
    (let loop ((index start) (acc knil))
      (let ((match (%regexp-search regexp string index end)))
	(cond (match (loop (regexp-match-end match) (kons index match acc)))
	      ((default-object? finish) acc)
	      (else (finish index #f acc)))))))

(define (regexp-fold-right re kons knil string #!optional finish start end)
  (guarantee nfc-string? string 'regexp-fold-right)
  (let* ((end (fix:end-index end (string-length string) 'regexp-fold-right))
	 (start (fix:start-index start end 'regexp-fold-right)))
    (if (default-object? finish)
	(%regexp-fold-right-1 kons knil re string start end)
	(%regexp-fold-right kons knil finish re string start end))))

(define (%regexp-fold-right-1 kons knil re string start end)
  ;; No need to propagate the final index, making the loop simpler and faster.
  (let ((regexp (regexp re)))
    (let loop ((index start))
      (let ((match (%regexp-search regexp string index end)))
	(if match
	    (kons index match (loop (regexp-match-end match)))
	    knil)))))

(define (%regexp-fold-right kons knil finish re string start end)
  (let ((regexp (regexp re)))
    (let loop ((index start) (k (lambda (index acc) (finish index #f acc))))
      (let ((match (%regexp-search regexp string index end)))
	(if match
	    (loop (regexp-match-end match)
		  (lambda (final-index acc)
		    (k final-index (kons index match acc))))
	    (k index knil))))))

;;;; Cut

(define (regexp-extract re string #!optional start end)
  (guarantee nfc-string? string 'regexp-extract)
  (let* ((end (fix:end-index end (string-length string) 'regexp-extract))
	 (start (fix:start-index start end 'regexp-extract)))
    (%regexp-fold-right-1 (lambda (index match strings)
			    (declare (ignore index))
			    (cons (regexp-match-value match) strings))
			  '()
			  re string start end)))

(define (regexp-split re string #!optional start end)
  (guarantee nfc-string? string 'regexp-split)
  (let* ((end (fix:end-index end (string-length string) 'regexp-split))
	 (start (fix:start-index start end 'regexp-split)))
    (%regexp-fold-right (lambda (index match strings)
			  (cons (substring string index
					   (regexp-match-start match))
				strings))
			'()
			(lambda (index match strings)
			  (declare (ignore match))
			  (cons (substring string index end)
				strings))
			re string start end)))

(define (regexp-partition re string #!optional start end)
  (guarantee nfc-string? string 'regexp-partition)
  (let* ((end (fix:end-index end (string-length string) 'regexp-partition))
	 (start (fix:start-index start end 'regexp-partition)))
    (%regexp-fold-right (lambda (index match strings)
			  (cons* (substring string index
					    (regexp-match-start match))
				 (regexp-match-value match)
				 strings))
			'()
			(lambda (index match strings)
			  (declare (ignore match))
			  (if (fix:< index end)
			      (cons (substring string index end)
				    strings)
			      strings))
			re string start end)))

;;;; Compiler rules

(define sre-rules)
(define sre-rewrite-rules)
(define cset-sre-rules)
(define cset-sre-rewrite-rules)
(defer-boot-action 'regexp-rules
  (lambda ()
    (set! sre-rules (make-rules 'sre 1))
    (set! sre-rewrite-rules (make-rules 'sre-rewrite 1))
    (set! cset-sre-rules (make-rules 'cset-sre 1))
    (set! cset-sre-rewrite-rules (make-rules 'cset-sre-rewrite 1))
    unspecific))

(define (rule-finder match-rules rewrite-rules)
  (rules-rewriter rewrite-rules (rules-matcher match-rules)))

(define-deferred-procedure find-sre-rule 'regexp-rules
  (rule-finder sre-rules sre-rewrite-rules))

(define-deferred-procedure find-cset-sre-rule 'regexp-rules
  (rule-finder cset-sre-rules cset-sre-rewrite-rules))

(define-deferred-procedure define-sre-rule 'regexp-rules
  (rules-definer sre-rules))

(define-deferred-procedure define-sre-rewriter 'regexp-rules
  (rules-definer sre-rewrite-rules))

(define-deferred-procedure define-cset-sre-rule 'regexp-rules
  (rules-definer cset-sre-rules))

(define-deferred-procedure define-cset-sre-rewriter 'regexp-rules
  (rules-definer cset-sre-rewrite-rules))

(define (alias-rule-definer rules)
  (let ((definer (rules-definer rules)))
    (lambda (from to)
      (guarantee interned-symbol? from)
      (guarantee interned-symbol? to)
      (definer `(,from . ,any-object?)
	(lambda (ctx . args)
	  (declare (ignore ctx))
	  (cons to args))))))

(define-deferred-procedure define-sre-alias 'regexp-rules
  (alias-rule-definer sre-rewrite-rules))

(define-deferred-procedure define-cset-sre-alias 'regexp-rules
  (alias-rule-definer cset-sre-rewrite-rules))

(define (compile-sre ctx sre)
  (cond ((find-cset-sre-rule ctx sre)
	 => (lambda (rule)
	      (insn:char-set
	       (maybe-xform ctx ((rule-operation rule) ctx sre)))))
	((find-sre-rule ctx sre)
	 => (lambda (rule)
	      ((rule-operation rule) ctx sre)))
	(else
	 (compile-error (input-pattern) sre))))

(define (compile-sres ctx sres)
  (insn:seq
   (map-in-order (lambda (sre)
		   (compile-sre ctx sre))
		 sres)))

(define (compile-cset-sre ctx cset-sre)
  (cond ((find-cset-sre-rule ctx cset-sre)
	 => (lambda (rule)
	      ((rule-operation rule) ctx cset-sre)))
	(else
	 (compile-error (input-pattern) cset-sre))))

(define (compile-cset-sres ctx cset-sres)
  (map-in-order (lambda (cset-sre)
		  (compile-cset-sre ctx cset-sre))
		cset-sres))

(define-record-type <ctx>
    (make-ctx fold? ascii? no-capture?)
    ctx?
  (fold? ctx-fold?)
  (ascii? ctx-ascii?)
  (no-capture? ctx-no-capture?))

(define initial-ctx
  (make-ctx #f #f #f))

(define (fold-ctx fold? ctx)
  (make-ctx fold? (ctx-ascii? ctx) (ctx-no-capture? ctx)))

(define (ascii-ctx ascii? ctx)
  (make-ctx (ctx-fold? ctx) ascii? (ctx-no-capture? ctx)))

(define (no-capture-ctx no-capture? ctx)
  (make-ctx (ctx-fold? ctx) (ctx-ascii? ctx) no-capture?))

(define (any-char? object)
  (unicode-char? object))

(define (min-arity? object)
  (exact-nonnegative-integer? object))

(define (max-arity? object)
  (exact-nonnegative-integer? object))

(define (submatch key insn)
  (hash-table-set! (submatch-keys) key #t)
  (insn:group key insn))

(define submatch-keys
  (make-parameter #f))

(define (make-submatch-keys)
  (make-strong-eqv-hash-table))

(define (submatch-keys->list table)
  (receive (numbered named)
      (partition exact-nonnegative-integer? (hash-table-keys table))
    (append (sort numbered <) named)))

(define char-set:word)
(defer-boot-action 'ucd
  (lambda ()
    (set! char-set:word (char-set-adjoin char-set:alphabetic #\_))
    unspecific))

;;;; <sre>

(define-sre-rule "char"
  (lambda (ctx char) (insn:char char (ctx-fold? ctx)))
  (lambda (ctx object) (declare (ignore ctx)) (unicode-char? object)))

(define-sre-rule "string"
  (lambda (ctx string) (insn:string string (ctx-fold? ctx)))
  (lambda (ctx object) (declare (ignore ctx)) (string? object)))

(define-sre-rule `(* . ,valid-sre?)
  (lambda (ctx . sres) (insn:* (compile-sres ctx sres))))
(define-sre-alias 'zero-or-more '*)

(define-sre-rule `(+ . ,valid-sre?)
  (lambda (ctx . sres) (insn:>= 1 (compile-sres ctx sres))))
(define-sre-alias 'one-or-more '+)

(define-sre-rule `(? . ,valid-sre?)
  (lambda (ctx . sres) (insn:? (compile-sres ctx sres))))
(define-sre-alias 'optional '?)

(define-sre-rule `(= ,min-arity? . ,valid-sre?)
  (lambda (ctx n . sres) (insn:= n (compile-sres ctx sres))))
(define-sre-alias 'exactly '=)

(define-sre-rule `(>= ,min-arity? . ,valid-sre?)
  (lambda (ctx n . sres) (insn:>= n (compile-sres ctx sres))))
(define-sre-alias 'at-least '>=)

(define-sre-rule `(** ,min-arity? ,max-arity? . ,valid-sre?)
  (lambda (ctx n m . sres) (insn:** n m (compile-sres ctx sres)))
  (lambda (ctx n m . sres) (declare (ignore ctx sres)) (<= n m)))
(define-sre-alias 'repeated '**)

(define-sre-rule `(|\|| . ,valid-sre?)
  (lambda (ctx . sres)
    (insn:alt
     (map-in-order (lambda (sre)
		     (compile-sre ctx sre))
		   sres))))
(define-sre-alias 'or '|\||)

(define-sre-rule `(: . ,valid-sre?)
  (lambda (ctx . sres) (compile-sres ctx sres)))
(define-sre-alias 'seq ':)

(define-sre-rule `($ . ,valid-sre?)
  (lambda (ctx . sres)
    (let ((insn (compile-sres ctx sres)))
      (if (ctx-no-capture? ctx)
	  insn
	  (submatch (next-submatch-number) insn)))))
(define-sre-alias 'submatch '$)

(define-sre-rule `(-> ,interned-symbol? . ,valid-sre?)
  (lambda (ctx key . sres)
    (let ((insn (compile-sres ctx sres)))
      (if (ctx-no-capture? ctx)
	  insn
	  (submatch key insn)))))
(define-sre-alias 'submatch-named '->)

(define-sre-rule 'bos
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:string-start)))

(define-sre-rule 'eos
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:string-end)))

(define-sre-rule 'bol
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:start-boundary char-set:newline)))

(define-sre-rule 'eol
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:end-boundary char-set:newline)))

(define-sre-rule 'bow
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:start-boundary char-set:word)))

(define-sre-rule 'eow
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:end-boundary char-set:word)))

(define-sre-rule 'nwb
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:non-boundary char-set:word)))

(define-sre-rewriter `(word . ,valid-sre?)
  (lambda (ctx . sres)
    (declare (ignore ctx))
    `(: bow ,@sres eow)))

(define-sre-rewriter `(word+ . ,valid-cset-sre?)
  (lambda (ctx . cset-sres)
    (declare (ignore ctx))
    `(: bow (+ (and ,char-set:word (or ,@cset-sres))) eow)))

(define-sre-rewriter 'word
  (lambda (ctx)
    (declare (ignore ctx))
    `(: bow (+ ,char-set:word) eow)))

(define-sre-rule 'bog
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:bog)))

(define-sre-rule 'eog
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:eog)))

(define-sre-rewriter 'grapheme
  (lambda (ctx)
    (declare (ignore ctx))
    `(: bog (* any) any eog)))

(define-sre-rule `(?? . ,valid-sre?)
  (lambda (ctx . sres) (insn:?? (compile-sres ctx sres))))
(define-sre-alias 'non-greedy-optional '??)

(define-sre-rule `(*? . ,valid-sre?)
  (lambda (ctx . sres) (insn:*? (compile-sres ctx sres))))
(define-sre-alias 'non-greedy-zero-or-more '*?)

(define-sre-rule `(**? ,min-arity? ,max-arity? . ,valid-sre?)
  (lambda (ctx n m . sres) (insn:**? n m (compile-sres ctx sres)))
  (lambda (ctx n m . sres) (declare (ignore ctx sres)) (<= n m)))
(define-sre-alias 'non-greedy-repeated '**?)

(let ((proc
       (lambda (keyword proc value)
	 (define-sre-rule `(,keyword . ,valid-sre?)
	   (lambda (ctx . sres)
	     (compile-sres (proc value ctx) sres))))))
  (proc 'w/case fold-ctx #f)
  (proc 'w/nocase fold-ctx #t)
  (proc 'w/unicode ascii-ctx #f)
  (proc 'w/ascii ascii-ctx #t)
  (proc 'w/nocapture no-capture-ctx #t))

;;;; <cset-sre>

(define (maybe-xform ctx cset)
  (let ((cset
	 (if (ctx-ascii? ctx)
	     (char-set-intersection char-set:ascii cset)
	     cset)))
    (if (ctx-fold? ctx)
	(char-set-union cset
			(char-set-upcase cset)
			(char-set-downcase cset))
	cset)))

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

(define (char-set->sre char-set)
  (receive (matched char-set*) (pull-out-names char-set)
    (if char-set*
	(let ((ranges
	       (cons '/
		     (char-set-range-fold-right
		      (lambda (start end tail)
			(let ((last (fix:- end 1)))
			  (if (fix:= last start)
			      (cons (integer->char start) tail)
			      (let ((s
				     (string (integer->char start)
					     (integer->char last))))
				(if (and (pair? tail)
					 (string? (car tail)))
				    (cons (string-append s (car tail))
					  (cdr tail))
				    (cons s tail))))))
		      '()
		      char-set*))))
	  (if (pair? matched)
	      `(or ,@matched ,ranges)
	      ranges))
	matched)))

(define-cset-sre-rule "char"
  (lambda (ctx char) (maybe-xform ctx (char-set char)))
  (lambda (ctx object) (declare (ignore ctx)) (unicode-char? object)))

(define-cset-sre-rule "string"
  (lambda (ctx string) (maybe-xform ctx (char-set string)))
  (lambda (ctx object)
    (declare (ignore ctx))
    (and (string? object)
	 (fix:= 1 (string-length object)))))

(define-cset-sre-rule "char-set"
  (lambda (ctx cs) (maybe-xform ctx cs))
  (lambda (ctx object) (declare (ignore ctx)) (char-set? object)))

(define-cset-sre-rule `(,string?)
  (lambda (ctx s) (maybe-xform ctx (char-set s))))

(define-cset-sre-rule `(char-set ,string?)
  (lambda (ctx s) (maybe-xform ctx (char-set s))))

(define-cset-sre-rule `(/ . ,range-spec?)
  (lambda (ctx . rs)
    (maybe-xform ctx (char-set* (append-map range-spec->ranges rs)))))
(define-cset-sre-alias 'char-range '/)

(define-cset-sre-rule `(or  . ,valid-cset-sre?)
  (lambda (ctx . cset-sres)
    (char-set-union* (compile-cset-sres ctx cset-sres))))
(define-cset-sre-alias '|\|| 'or)

(define-cset-sre-rule `(and  . ,valid-cset-sre?)
  (lambda (ctx . cset-sres)
    (char-set-intersection* (compile-cset-sres ctx cset-sres))))
(define-cset-sre-alias '& 'and)

(define-cset-sre-rule `(-  . ,valid-cset-sre?)
  (lambda (ctx . cset-sres)
    (apply char-set-difference (compile-cset-sres ctx cset-sres))))
(define-cset-sre-alias 'difference '-)

(define-cset-sre-rule `(~  . ,valid-cset-sre?)
  (lambda (ctx . cset-sres)
    (apply char-set-difference
	   (if (ctx-ascii? ctx) char-set:ascii char-set:full)
	   (compile-cset-sres ctx cset-sres))))
(define-cset-sre-alias 'complement '~)

(define char-set-names
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

(for-each (lambda (names)
	    (let ((operation
		   (lambda (ctx)
		     (maybe-xform ctx (name->char-set (car names))))))
	      (for-each (lambda (name)
			  (define-cset-sre-rule name operation))
			names)))
	  char-set-names)

(define (pull-out-names char-set)
  (let ((name (char-set->name char-set)))
    (if (and name
	     (any (lambda (names)
		    (memq name names))
		  char-set-names))
	(values name #f)
	(let loop
	    ((names '(alphanumeric alphabetic lower-case upper-case numeric
				   punctuation symbol whitespace control))
	     (matched '())
	     (char-set char-set))
	  (if (pair? names)
	      (let ((char-set* (name->char-set (car names))))
		(if (char-set<= char-set* char-set)
		    (loop (cdr names)
			  (cons (car names) matched)
			  (char-set-difference char-set char-set*))
		    (loop (cdr names) matched char-set)))
	      (values matched char-set))))))

(let ((proc
       (lambda (keyword proc value)
	 (define-cset-sre-rule `(,keyword . ,valid-cset-sre?)
	   (lambda (ctx . cset-sres)
	     (compile-cset-sres (proc value ctx) cset-sres))))))
  (proc 'w/case fold-ctx #f)
  (proc 'w/nocase fold-ctx #t)
  (proc 'w/unicode ascii-ctx #f)
  (proc 'w/ascii ascii-ctx #t))