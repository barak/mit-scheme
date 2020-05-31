#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

(add-boot-deps! '(runtime regexp rules)
		'(runtime error-handler)
		'(runtime ucd-glue))

(define (valid-sre? object)
  (and (or (match-cset-sre-rule initial-ctx object)
	   (match-sre-rule initial-ctx object))
       #t))
(register-predicate! valid-sre? 'source-regexp)

(define (valid-cset-sre? object)
  (and (match-cset-sre-rule initial-ctx object)
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

(define (print-regexp re #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (fresh-line port)
    (for-each (lambda (object)
		(write-line object port))
	      (regexp->nfa (regexp re)))))

(define-deferred condition-type:compile-regexp
  (make-condition-type 'compile-sre condition-type:error
      '(pattern element)
    (lambda (condition port)
      (write-string "Ill-formed regular s-expression: " port)
      (write (access-condition condition 'element) port)
      (write-string " from pattern: " port)
      (write (access-condition condition 'pattern) port))))

(define-deferred compile-error
  (condition-signaller condition-type:compile-regexp
		       '(pattern element)
		       standard-error-handler))

;;;; Match and search

(define (regexp-matches-all? re string #!optional start end)
  (%regexp-matches? re #t string start end 'regexp-matches-all?))

(define (regexp-matches-some? re string #!optional start end)
  (%regexp-matches? re #f string start end 'regexp-matches-some?))

(define-integrable (%regexp-matches? re match-all? string start end caller)
  (guarantee nfc-string? string caller)
  (let* ((end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller)))
    (and (run-matcher (regexp-impl (regexp re)) match-all? #f start
		      string start end)
	 #t)))

(define (regexp-matches-all re string #!optional start end)
  (%regexp-matches re #t string start end 'regexp-matches-all))

(define (regexp-matches-some re string #!optional start end)
  (%regexp-matches re #f string start end 'regexp-matches-some))

(define-integrable (%regexp-matches re match-all? string start end caller)
  (guarantee nfc-string? string caller)
  (let* ((end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller)))
    (%regexp-match (regexp re) match-all? #t start string start end)))

(define (%regexp-match regexp match-all? capture? index string start end)
  (let ((groups
	 (run-matcher (regexp-impl regexp) match-all? capture? index
		      string start end)))
    (and groups
	 (make-regexp-match (car groups)
			    (cdr groups)
			    (regexp-submatch-keys regexp)))))

(define (regexp-search re string #!optional start end)
  (guarantee nfc-string? string 'regexp-search)
  (let* ((end (fix:end-index end (string-length string) 'regexp-search))
	 (start (fix:start-index start end 'regexp-search)))
    (%regexp-search (regexp re) start string start end)))

(define (%regexp-search regexp index string start end)
  (let loop ((index index))
    (if (fix:< index end)
	(or (%regexp-match regexp #f #t index string start end)
	    (loop (fix:+ index 1)))
	(and (fix:= index end)
	     (%regexp-match regexp #f #t index string start end)))))

(define (regexp-search-all re string #!optional start end)
  (regexp-fold-right re
		     (lambda (index match string acc)
		       (declare (ignore index string))
		       (cons match acc))
		     '()
		     string (default-object) start end))

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

(define (regexp-match-empty? match)
  (group-empty? (regexp-match-group match)))

(define (regexp-match-length match)
  (group-length (regexp-match-group match)))

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
      (let ((match (%regexp-search regexp index string index end)))
	(if match
	    (if (< n count)
		(find-match (fix:max (regexp-match-end match)
				     (fix:+ index 1))
			    (+ n 1))
		(string-append (subst-match 'pre match string start end)
			       (subst-match subst match string start end)
			       (subst-match 'post match string start end)))
	    (substring string start end))))

    (find-match start 0)))

(define (regexp-replace-all re string subst #!optional start end)
  (guarantee regexp-replace-subst? subst 'regexp-replace-all)
  (let ((matches (regexp-search-all re string start end)))
    (if (pair? matches)
	(string-append*
	 (let ((start (if (default-object? start) 0 start))
	       (end (if (default-object? end) (string-length string) end)))
	   (let loop ((matches matches) (start start))
	     (if (pair? matches)
		 (let ((match (car matches))
		       (matches (cdr matches)))
		   (cons* (subst-match 'pre match string start end)
			  (subst-match subst match string start
				       (if (pair? matches)
					   (regexp-match-start (car matches))
					   end))
			  (loop matches (regexp-match-end match))))
		 '()))))
	(substring string start end))))

(define (regexp-replace-subst? object)
  (or (string? object)
      (regexp-match-key? object)
      (eq? 'pre object)
      (eq? 'post object)))
(register-predicate! regexp-replace-subst? 'regexp-replace-subst)

(define (subst-match subst match string start end)
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
  (and (list? object)
       (every (lambda (elt)
		(or (string? elt)
		    (regexp-match-key? elt)))
	      object)))
(register-predicate! regexp-match-replacement? 'regexp-match-replacement)

(define (regexp-match-replace match repl)
  (guarantee regexp-match? match 'regexp-match-replace)
  (guarantee regexp-match-replacement? repl 'regexp-match-replace)
  (string-append*
   (map (lambda (item)
	  (if (string? item)
	      item
	      (or (regexp-match-submatch match item) "")))
	repl)))

;;;; Fold

(define (regexp-fold re kons knil string #!optional finish start end ignore?)
  (guarantee nfc-string? string 'regexp-fold)
  (let ((regexp (regexp re))
	(end (fix:end-index end (string-length string) 'regexp-fold))
	(ignore? (if (default-object? ignore?) #f ignore?)))
    (let ((start (fix:start-index start end 'regexp-fold)))

      (define (loop index last-match-end acc)
	(let ((match (%regexp-search regexp index string start end)))
	  (if match
	      (if (and ignore? (ignore? match))
		  (skip index last-match-end match acc)
		  (continue index match (kons last-match-end match string acc)))
	      (done last-match-end acc))))

      (define (skip index last-match-end match acc)
	(loop (fix:max (regexp-match-end match) (fix:+ index 1))
	      last-match-end
	      acc))

      (define (continue index match acc)
	(let ((last-match-end (regexp-match-end match)))
	  (loop (fix:max last-match-end (fix:+ index 1))
		last-match-end
		acc)))

      (define (done last-match-end acc)
	(if (default-object? finish)
	    acc
	    (finish last-match-end #f string acc)))

      (loop start start knil))))

(define (regexp-fold-right re kons knil string
			   #!optional finish start end ignore?)
  (guarantee nfc-string? string 'regexp-fold-right)
  (let ((regexp (regexp re))
	(end (fix:end-index end (string-length string) 'regexp-fold-right))
	(ignore? (if (default-object? ignore?) #f ignore?)))
    (let ((start (fix:start-index start end 'regexp-fold-right)))

      (define (loop index last-match-end)
	(let ((match (%regexp-search regexp index string start end)))
	  (if match
	      (if (and ignore? (ignore? match))
		  (skip index last-match-end match)
		  (kons last-match-end match string (continue index match)))
	      (done last-match-end))))

      (define (skip index last-match-end match)
	(loop (fix:max (regexp-match-end match) (fix:+ index 1))
	      last-match-end))

      (define (continue index match)
	(let ((last-match-end (regexp-match-end match)))
	  (loop (fix:max last-match-end (fix:+ index 1))
		last-match-end)))

      (define (done last-match-end)
	(if (default-object? finish)
	    knil
	    (finish last-match-end #f string knil)))

      (loop start start))))

;;;; Cut

(define (regexp-extract re string #!optional start end)
  (regexp-fold-right re
		     (lambda (last-match-end match string strings)
		       (declare (ignore last-match-end string))
		       (if (regexp-match-empty? match)
			   strings
			   (cons (regexp-match-value match) strings)))
		     '()
		     string (default-object) start end))

(define (regexp-split re string #!optional start end)
  (regexp-fold-right re
		     (lambda (last-match-end match string strings)
		       (cons (string-slice string last-match-end
					   (regexp-match-start match))
			     strings))
		     '()
		     string
		     (lambda (last-match-end match string strings)
		       (declare (ignore match))
		       (cons (string-slice string last-match-end end)
			     strings))
		     start end regexp-match-empty?))

(define (regexp-partition re string #!optional start end)
  (let ((start (if (default-object? start) 0 start))
	(end (if (default-object? end) (string-length string) end)))
    (if (fix:< start end)
	(regexp-fold-right re
			   (lambda (last-match-end match string strings)
			     (cons* (string-slice string last-match-end
						  (regexp-match-start match))
				    (regexp-match-value match)
				    strings))
			   '()
			   string
			   (lambda (last-match-end match string strings)
			     (declare (ignore match))
			     (let ((s (string-slice string last-match-end end)))
			       (if (string-null? s)
				   strings
				   (cons s strings))))
			   start end regexp-match-empty?)
	(list ""))))

;;;; Compiler rules

(define-deferred sre-rules
  (make-rules 'sre 1))

(define-deferred sre-rewrite-rules
  (make-rules 'sre-rewrite 1))

(define-deferred cset-sre-rules
  (make-rules 'cset-sre 1))

(define-deferred cset-sre-rewrite-rules
  (make-rules 'cset-sre-rewrite 1))

(define (rule-finder match-rules rewrite-rules)
  (rules-rewriter rewrite-rules (rules-matcher match-rules)))

(define-deferred match-sre-rule
  (rule-finder sre-rules sre-rewrite-rules))

(define-deferred match-cset-sre-rule
  (rule-finder cset-sre-rules cset-sre-rewrite-rules))

(define-sequenced-procedure define-sre-rule (current-package-sequencer)
  (rules-definer sre-rules))

(define-sequenced-procedure define-sre-rewriter (current-package-sequencer)
  (rules-definer sre-rewrite-rules))

(define-sequenced-procedure define-cset-sre-rule (current-package-sequencer)
  (rules-definer cset-sre-rules))

(define-sequenced-procedure define-cset-sre-rewriter (current-package-sequencer)
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

(define-sequenced-procedure define-sre-alias (current-package-sequencer)
  (alias-rule-definer sre-rewrite-rules))

(define-sequenced-procedure define-cset-sre-alias (current-package-sequencer)
  (alias-rule-definer cset-sre-rewrite-rules))

(define (compile-sre ctx sre)
  (cond ((match-cset-sre-rule ctx sre)
	 => (lambda (thunk)
	      (insn:char-set (maybe-xform ctx (thunk))
			     (ctx-fold? ctx))))
	((match-sre-rule ctx sre)
	 => (lambda (thunk) (thunk)))
	(else
	 (compile-error (input-pattern) sre))))

(define (compile-sres ctx sres)
  (insn:seq
   (map-in-order (lambda (sre)
		   (compile-sre ctx sre))
		 sres)))

(define (compile-cset-sre ctx cset-sre)
  (cond ((match-cset-sre-rule ctx cset-sre)
	 => (lambda (thunk) (thunk)))
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

(define-deferred char-set:word
  (char-set-adjoin char-set:alphabetic #\_))

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
    (insn:start-boundary (char-set-invert char-set:newline))))

(define-sre-rule 'eol
  (lambda (ctx)
    (declare (ignore ctx))
    (insn:end-boundary (char-set-invert char-set:newline))))

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
    `(: bog (*? any) any eog)))

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
		    (cons (cons (char->integer (string-ref rs i))
				(fix:+
				 (char->integer (string-ref rs (fix:+ i 1)))
				 1))
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