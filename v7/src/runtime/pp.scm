#| -*-Scheme-*-

$Id: pp.scm,v 14.44 2002/11/20 19:46:22 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Pretty Printer
;;; package: (runtime pretty-printer)

(declare (usual-integrations))

(define (initialize-package!)
  (set! pp-description (make-generic-procedure 1 'PP-DESCRIPTION))
  (set-generic-procedure-default-generator! pp-description
    (lambda (generic tags)
      generic tags
      pp-description/default))
  (set! forced-indentation (special-printer kernel/forced-indentation))
  (set! pressured-indentation (special-printer kernel/pressured-indentation))
  (set! print-procedure (special-printer kernel/print-procedure))
  (set! print-let-expression (special-printer kernel/print-let-expression))
  (set! print-case-expression (special-printer kernel/print-case-expression))
  (set! code-dispatch-list
	`((COND . ,forced-indentation)
	  (CASE . ,print-case-expression)
	  (IF . ,forced-indentation)
	  (OR . ,forced-indentation)
	  (AND . ,forced-indentation)
	  (LET . ,print-let-expression)
	  (LET* . ,print-let-expression)
	  (LETREC . ,print-let-expression)
	  (FLUID-LET . ,print-let-expression)
	  (DEFINE . ,print-procedure)
	  (DEFINE-INTEGRABLE . ,print-procedure)
	  (LAMBDA . ,print-procedure)
	  (NAMED-LAMBDA . ,print-procedure)))
  (set! dispatch-list code-dispatch-list)
  (set! dispatch-default print-combination)
  (set! cocked-object (generate-uninterned-symbol))
  unspecific)

(define *pp-named-lambda->define?* #f)
(define *pp-primitives-by-name* #t)
(define *pp-uninterned-symbols-by-name* #t)
(define *pp-no-highlights?* #t)
(define *pp-save-vertical-space?* #f)
(define *pp-lists-as-tables?* #t)
(define *pp-forced-x-size* #f)
(define *pp-avoid-circularity?* #f)
(define *pp-default-as-code?* #t)
(define *pp-auto-highlighter* #f)

(define (pp object #!optional port . rest)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (let ((pretty-print
	   (lambda (object)
	     (apply pretty-print object port rest)
	     (newline port))))
      (cond ((pp-description object)
	     => (lambda (description)
		  (pretty-print object)
		  (for-each pretty-print description)))
	    ((arity-dispatched-procedure? object)
	     (pretty-print (unsyntax-entity object)))
	    ((and (procedure? object) (procedure-lambda object))
	     => pretty-print)
	    (else
	     (pretty-print object))))))

(define pp-description)

(define (pp-description/default object)
  (cond ((named-structure? object)
	 (named-structure/description object))
	((%record? object)		; unnamed record
	 (let loop ((i (- (%record-length object) 1)) (d '()))
	   (if (< i 0)
	       d
	       (loop (- i 1)
		     (cons (list i (%record-ref object i)) d)))))
	((weak-pair? object)
	 `((WEAK-CAR ,(weak-car object))
	   (WEAK-CDR ,(weak-cdr object))))
	((cell? object)
	 `((CONTENTS ,(cell-contents object))))
	(else
	 #f)))

;;; Controls the appearance of procedures in the CASE statement used
;;; to describe an arity dispatched procedure:
;;;  FULL:  full bodies of procedures
;;;  NAMED: just name if the procedure is a named lambda, like FULL if unnamed
;;;  SHORT: procedures appear in #[...] unparser syntax
(define *pp-arity-dispatched-procedure-style* 'FULL)

(define (unsyntax-entity object)
  (define (unsyntax-entry procedure)
    (case *pp-arity-dispatched-procedure-style*
      ((FULL)  (unsyntax-entity procedure))
      ((NAMED)
       (let ((text (unsyntax-entity procedure)))
	 (if (and (pair? text)
		  (eq? (car text) 'named-lambda)
		  (pair? (cdr text))
		  (pair? (cadr text)))
	     (caadr text)
	     text)))
      ((SHORT) procedure)
      (else procedure)))
  (cond ((arity-dispatched-procedure? object)
	 (let* ((default  (entity-procedure  object))
		(cases    (cdr (vector->list (entity-extra object))))
		(cases*
		 (let loop ((i 0) (tests '()) (cases cases))
		   (cond ((null? cases) (reverse tests))
			 ((car cases)
			  (loop (+ i 1)
				(cons `((,i) ,(unsyntax-entry (car cases)))
				      tests)
				(cdr cases)))
			 (else
			  (loop (+ i 1) tests (cdr cases)))))))
	   `(CASE NUMBER-OF-ARGUMENTS
	      ,@cases*
	      (ELSE
	       ,(unsyntax-entry default)))))
	((and (procedure? object) (procedure-lambda object))
	 => unsyntax)
	(else
	 object)))

(define (pretty-print object #!optional port as-code? indentation)
  (let ((as-code?
	 (if (default-object? as-code?)
	     (let ((default *pp-default-as-code?*))
	       (if (boolean? default)
		   default
		   (not (scode-constant? object))))
	     as-code?)))
    (pp-top-level (let ((sexp
			 (if (scode-constant? object)
			     object
			     (unsyntax object))))
		    (if (and as-code?
			     (pair? sexp)
			     (eq? (car sexp) 'NAMED-LAMBDA)
			     *pp-named-lambda->define?*)
			(if (and (eq? 'LAMBDA *pp-named-lambda->define?*)
				 (pair? (cdr sexp))
				 (pair? (cadr sexp)))
			    `(LAMBDA ,(cdadr sexp) ,@(cddr sexp))
			    `(DEFINE ,@(cdr sexp)))
			sexp))
		  (if (default-object? port) (current-output-port) port)
		  as-code?
		  (if (default-object? indentation) 0 indentation)
		  0)
    unspecific))

(define-structure (pretty-printer-highlight
		   (conc-name pph/)
		   (constructor
		    make-pretty-printer-highlight
		    (object #!optional
			    start-string end-string
			    as-code? depth-limit
			    breadth-limit)))
  (object #f read-only #t)
  (start-string "*=>" read-only #t)
  (end-string   "<=*" read-only #t)
  (as-code? 'DEFAULT read-only #t)
  (depth-limit 'DEFAULT read-only #t)
  (breadth-limit 'DEFAULT read-only #t))

(define (with-highlight-strings-printed pph thunk)
  (let ((print-string
	 (lambda (s)
	   (if (string? s)
	       (*unparse-string s)
	       (s output-port)))))
    (print-string (pph/start-string pph))
    (thunk)
    (print-string (pph/end-string pph))))

(define (pph/start-string-length pph)
  (let ((start (pph/start-string pph)))
    (if (string? start)
	(string-length start)
	0)))

(define (pph/end-string-length pph)
  (let ((end (pph/end-string pph)))
    (if (string? end)
	(string-length end)
	0)))

(define (pp-top-level expression port as-code? indentation list-depth)
  (fluid-let ((x-size (or *pp-forced-x-size* (output-port/x-size port)))
	      (output-port port))
    (let* ((numerical-walk
	    (if *pp-avoid-circularity?*
		numerical-walk-avoid-circularities
		numerical-walk))
	   (node (numerical-walk expression list-depth)))
      (if (positive? indentation)
	  (*unparse-string (make-string indentation #\space)))
      (if as-code?
	  (print-node node indentation list-depth)
	  (print-non-code-node node indentation list-depth))
      (output-port/discretionary-flush port))))

(define x-size)
(define output-port)

(define-integrable (*unparse-char char)
  (output-port/write-char output-port char))

(define-integrable (*unparse-string string)
  (output-port/write-string output-port string))

(define-integrable (*unparse-open)
  (*unparse-char #\())

(define-integrable (*unparse-close)
  (*unparse-char #\)))

(define-integrable (*unparse-space)
  (*unparse-char #\space))

(define-integrable (*unparse-newline)
  (*unparse-char #\newline))

(define (print-non-code-node node column depth)
  (fluid-let ((dispatch-list '())
	      (dispatch-default
	       (if *pp-lists-as-tables?*
		   print-data-table
		   print-data-column)))
    (print-node node column depth)))

(define (print-code-node node column depth)
  (fluid-let ((dispatch-list code-dispatch-list)
	      (dispatch-default print-combination))
    (print-node node column depth)))

(define (print-data-column nodes column depth)
  (*unparse-open)
  (print-column nodes (+ column 1) (+ depth 1))
  (*unparse-close))

(define (print-data-table nodes column depth)
  (*unparse-open)
  (maybe-print-table nodes (+ column 1) (+ depth 1))
  (*unparse-close))

(define (print-node node column depth)
  (cond ((list-node? node)
	 (print-list-node node column depth))
	((symbol? node)
	 (*unparse-symbol node))
	((prefix-node? node)
	 (*unparse-string (prefix-node-prefix node))
	 (let ((new-column
		(+ column (string-length (prefix-node-prefix node))))
	       (subnode (prefix-node-subnode node)))
	   (if (null? dispatch-list)
	       (print-node subnode new-column depth)
	       (print-non-code-node subnode new-column depth))))
	((highlighted-node? node)
	 (let ((highlight (highlighted-node/highlight node)))
	   (with-highlight-strings-printed highlight
	     (lambda ()
	       (let ((handler
		      (let ((as-code? (pph/as-code? highlight))
			    (currently-as-code? (not (null? dispatch-list))))
			(cond ((or (eq? as-code? 'DEFAULT)
				   (eq? as-code? currently-as-code?))
			       print-node)
			      (as-code?
			       print-code-node)
			      (else
			       print-non-code-node)))))
		 (handler (highlighted-node/subnode node)
			  (+ column (pph/start-string-length highlight))
			  (+ depth (pph/end-string-length highlight))))))))
	(else
	 (*unparse-string node))))

(define (print-list-node node column depth)
  (if (and *pp-save-vertical-space?*
	   (fits-within? node column depth))
      (print-guaranteed-list-node node)
      (let* ((subnodes (node-subnodes node))
	     (association
	      (and (not (null? (cdr subnodes)))
		   (assq (unhighlight (car subnodes)) dispatch-list))))
	(if (and (not association)
		 (fits-within? node column depth))
	    (print-guaranteed-list-node node)
	    ((if association
		 (cdr association)
		 dispatch-default)
	     subnodes column depth)))))

(define (print-guaranteed-node node)
  (cond ((list-node? node)
	 (print-guaranteed-list-node node))
	((symbol? node)
	 (*unparse-symbol node))
	((highlighted-node? node)
	 (with-highlight-strings-printed (highlighted-node/highlight node)
	   (lambda ()
	     (print-guaranteed-node (highlighted-node/subnode node)))))
	((prefix-node? node)
	 (*unparse-string (prefix-node-prefix node))
	 (print-guaranteed-node (prefix-node-subnode node)))
	(else
	 (*unparse-string node))))

(define (print-guaranteed-list-node node)
  (*unparse-open)
  (let loop ((nodes (node-subnodes node)))
    (print-guaranteed-node (car nodes))
    (if (not (null? (cdr nodes)))
	(begin
	  (*unparse-space)
	  (loop (cdr nodes)))))
  (*unparse-close))

(define (print-column nodes column depth)
  (let loop ((nodes nodes))
    (if (null? (cdr nodes))
	(print-node (car nodes) column depth)
	(begin
	  (print-node (car nodes) column 0)
	  (tab-to column)
	  (loop (cdr nodes))))))

(define (print-guaranteed-column nodes column)
  (let loop ((nodes nodes))
    (print-guaranteed-node (car nodes))
    (if (not (null? (cdr nodes)))
	(begin
	  (tab-to column)
	  (loop (cdr nodes))))))

(define (print-guaranteed-table nodes column all-widths)
  (define (print-row row widths spaces)
    (cond ((null? row)
	   unspecific)
	  ((null? widths)
	   (tab-to column)
	   (print-row row all-widths 0))
	  (else
	   (let ((next (car row)))
	     (pad-with-spaces spaces)
	     (print-guaranteed-node next)
	     (print-row (cdr row)
			(cdr widths)
			(1+ (- (car widths)
			       (node-size next))))))))
  (print-row nodes all-widths 0))

(define (maybe-print-table nodes column depth)
  (define (default)
    (print-column nodes column depth))

  (let* ((available-space (- x-size column))
	 (n-nodes (length nodes))
	 (max-cols (quotient (+ n-nodes 1) 2)))

    (define (try-columns n-columns)
      (let* ((nodev (list->vector nodes))
	     (last-size (node-size (vector-ref nodev (-1+ n-nodes)))))

	(define (fit? n-cols widths)
	  ;; This must check that all rows fit.
	  ;; The last one must be treated specially because it is
	  ;; followed by depth tokens (close parens).
	  (and (>= available-space (+ (-1+ n-cols) (reduce + 0 widths)))
	       (let ((last-n-1 (remainder (-1+ n-nodes) n-cols)))
		 (>= available-space
		     (+ (+ last-n-1 (reduce + 0 (list-head widths last-n-1)))
			(+ last-size depth))))))

	(define (find-max-width posn step)
	  (let loop ((posn posn)
		     (width 0))
	    (if (>= posn n-nodes)
		width
		(let ((next (node-size (vector-ref nodev posn))))
		  (loop (+ posn step)
			(if (> next width) next width))))))

	(define (find-widths n)
	  (let recur ((start 0))
	    (if (= start n)
		'()
		(cons (find-max-width start n) (recur (1+ start))))))

	(define (try n)
	  (if (< n 2)
	      (default)
	      (let ((widths (find-widths n)))
		(if (not (fit? n widths))
		    (try (- n 1))
		    (print-guaranteed-table
		     nodes column
		     ;; Try to make it look pretty.
		     (let ((next-n (-1+ n)))
		       (if (or (= n 2)
			       (not (= (quotient (+ n-nodes next-n) n)
				       (quotient (+ n-nodes (-1+ next-n))
						 next-n))))
			   widths
			   (let ((nwidths (find-widths next-n)))
			     (if (fit? (-1+ n) nwidths)
				 nwidths
				 widths)))))))))

	(try n-columns)))

    (if (< n-nodes 4)
	;; It's silly to tabulate 3 or less things.
	(default)
	(let loop ((n 1)
		   (nodes (cdr nodes))
		   (space (- available-space
			     (node-size (car nodes)))))
	  (cond ((> n max-cols)
		 ;; Make sure there are at least two relatively full rows.
		 ;; This also guarantees that nodes is not NULL?.
		 (try-columns max-cols))
		((>= space 0)
		 (loop (1+ n)
		       (cdr nodes)
		       (- space (1+ (node-size (car nodes))))))
		((<= n 2)
		 (default))
		(else
		 (try-columns (-1+ n))))))))

;;;; Printers

(define (print-combination nodes column depth)
  (*unparse-open)
  (let ((column (+ column 1))
	(depth (+ depth 1)))
    (cond ((null? (cdr nodes))
	   (print-node (car nodes) column depth))
	  ((two-on-first-line? nodes column depth)
	   (print-guaranteed-node (car nodes))
	   (*unparse-space)
	   (print-guaranteed-column (cdr nodes)
				    (+ column 1 (node-size (car nodes)))))
	  (else
	   (print-column nodes column depth))))
  (*unparse-close))

(define dispatch-list)
(define dispatch-default)
(define code-dispatch-list)

(define ((special-printer procedure) nodes column depth)
  (*unparse-open)
  (print-guaranteed-node (car nodes))	;(*unparse-symbol (car nodes))
  (*unparse-space)
  (if (not (null? (cdr nodes)))
      (procedure (cdr nodes)
		 (+ column 2 (node-size (car nodes)))
		 (+ column 2)
		 (+ depth 1)))
  (*unparse-close))

;;; Force the indentation to be an optimistic column.

(define forced-indentation)
(define (kernel/forced-indentation nodes optimistic pessimistic depth)
  pessimistic
  (print-column nodes optimistic depth))

;;; Pressure the indentation to be an optimistic column; no matter
;;; what happens, insist on a column, but accept a pessimistic one if
;;; necessary.

(define pressured-indentation)
(define (kernel/pressured-indentation nodes optimistic pessimistic depth)
  (if (fits-as-column? nodes optimistic depth)
      (print-guaranteed-column nodes optimistic)
      (begin
	(tab-to pessimistic)
	(print-column nodes pessimistic depth))))

;;; Print a procedure definition.  The bound variable pattern goes on
;;; the same line as the keyword, while everything else gets indented
;;; pessimistically.  We may later want to modify this to make higher
;;; order procedure patterns be printed more carefully.

(define print-procedure)
(define (kernel/print-procedure nodes optimistic pessimistic depth)
  (if (and *unparse-disambiguate-null-lambda-list?*
	   (member (car nodes) '("#f" "#F")))
      (*unparse-string "()")
      (print-node (car nodes) optimistic 0))
  (let ((rest (cdr nodes)))
    (if (not (null? rest))
	(begin
	  (tab-to pessimistic)
	  (print-column (cdr nodes) pessimistic depth)))))

;;; Print a binding form.  There is a great deal of complication here,
;;; some of which is to gracefully handle the case of a badly-formed
;;; binder.  But most important is the code that handles the name when
;;; we encounter a named let; it must go on the same line as the
;;; keyword.  In that case, the bindings try to fit on that line or
;;; start on that line if possible; otherwise they line up under the
;;; name.  The body, of course, is always indented pessimistically.

(define print-let-expression)
(define (kernel/print-let-expression nodes optimistic pessimistic depth)
  (let ((print-body
	 (lambda (nodes)
	   (if (not (null? nodes))
	       (begin
		 (tab-to pessimistic)
		 (print-column nodes pessimistic depth))))))
    (cond ((null? (cdr nodes))
	   ;; screw case
	   (print-node (car nodes) optimistic depth))
	  ((symbol? (car nodes))
	   ;; named let
	   (*unparse-symbol (car nodes))
	   (let ((new-optimistic
		  (+ optimistic (+ 1 (symbol-length (car nodes))))))
	     (cond ((fits-within? (cadr nodes) new-optimistic 0)
		    (*unparse-space)
		    (print-guaranteed-node (cadr nodes))
		    (print-body (cddr nodes)))
		   ((and (list-node? (cadr nodes))
			 (fits-as-column? (node-subnodes (cadr nodes))
					  (+ new-optimistic 2)
					  0))
		    (*unparse-space)
		    (*unparse-open)
		    (print-guaranteed-column (node-subnodes (cadr nodes))
					     (+ new-optimistic 1))
		    (*unparse-close)
		    (print-body (cddr nodes)))
		   (else
		    (tab-to optimistic)
		    (print-node (cadr nodes) optimistic 0)
		    (print-body (cddr nodes))))))
	  (else
	   ;; ordinary let
	   (print-node (car nodes) optimistic 0)
	   (print-body (cdr nodes))))))

(define print-case-expression)
(define (kernel/print-case-expression nodes optimistic pessimistic depth)
  (define (print-cases nodes)
    (if (not (null? nodes))
	(begin
	  (tab-to pessimistic)
	  (print-column nodes pessimistic depth))))
  (cond ((null? (cdr nodes))
	 (print-node (car nodes) optimistic depth))
	((fits-within? (car nodes) optimistic 0)
	 (print-guaranteed-node (car nodes))
	 (print-cases (cdr nodes)))
	(else
	 (tab-to (+ pessimistic 2))
	 (print-node (car nodes) optimistic 0)
	 (print-cases (cdr nodes)))))

;;;; Alignment

(define-integrable (fits-within? node column depth)
  (> (- x-size depth)
     (+ column (node-size node))))

;;; Fits if each node fits when stacked vertically at the given column.

(define (fits-as-column? nodes column depth)
  (let loop ((nodes nodes))
    (if (null? (cdr nodes))
	(fits-within? (car nodes) column depth)
	(and (> x-size
		(+ column (node-size (car nodes))))
	     (loop (cdr nodes))))))

;;; Fits if first two nodes fit on same line, and rest fit under the
;;; second node.  Assumes at least two nodes are given.

(define (two-on-first-line? nodes column depth)
  (let ((column (+ column (+ 1 (node-size (car nodes))))))
    (and (> x-size column)
	 (fits-as-column? (cdr nodes) column depth))))

;;; Starts a new line with the specified indentation.

(define (tab-to column)
  (*unparse-newline)
  (pad-with-spaces column))

(define-integrable (pad-with-spaces n-spaces)
  (*unparse-string (make-string n-spaces #\space)))

;;;; Numerical Walk

(define (numerical-walk object list-depth)
  (define (numerical-walk-no-auto-highlight object list-depth)
    (cond ((pair? object)
	   (let ((prefix (unparse-list/prefix-pair? object)))
	     (if prefix
		 (make-prefix-node prefix
				   (numerical-walk (cadr object)
						   list-depth))
		 (let ((unparser (unparse-list/unparser object)))
		   (if unparser
		       (walk-custom unparser object list-depth)
		       (walk-pair object list-depth))))))
	  ((symbol? object)
	   (if (or *pp-uninterned-symbols-by-name*
		   (interned-symbol? object))
	       object
	       (walk-custom unparse-object object list-depth)))
	  ((pretty-printer-highlight? object)
	   ;; (1) see note below.
	   (let ((rest (walk-highlighted-object
			object list-depth
			numerical-walk-no-auto-highlight)))
	     (make-highlighted-node (+ (pph/start-string-length object)
				       (pph/end-string-length object)
				       (node-size rest))
				    object
				    rest)))
	  ((vector? object)
	   (if (zero? (vector-length object))
	       (walk-custom unparse-object object list-depth)
	       (let ((unparser (unparse-vector/unparser object)))
		 (if unparser
		     (walk-custom unparser object list-depth)
		     (make-prefix-node "#"
				       (walk-pair (vector->list object)
						  list-depth))))))
	  ((primitive-procedure? object)
	   (if *pp-primitives-by-name*
	       (primitive-procedure-name object)
	       (walk-custom unparse-object object list-depth)))
	  (else
	   (walk-custom unparse-object object list-depth))))

  ;; We do the following test first and the test above at (1) for a
  ;; PRETTY-PRINTER-HIGHLIGHT because the highlighted object may
  ;; itself be a PRETTY-PRINTER-HIGHLIGHT.  It is also important that
  ;; the case (1) above uses NUMERICAL-WALK-NO-AUTO-HIGHLIGHT
  ;; otherwise we would get infinite recursion when the `unwrapped'
  ;; object REST is re-auto-highlighted by the test below.

  (cond ((and *pp-auto-highlighter*
	      (not (pretty-printer-highlight? object))
	      (*pp-auto-highlighter* object))
	 => (lambda (highlighted)
	      (numerical-walk-no-auto-highlight highlighted list-depth)))
	(else
	 (numerical-walk-no-auto-highlight object list-depth))))

(define (walk-custom unparser object list-depth)
  (with-string-output-port
   (lambda (port)
     (unparser (make-unparser-state port
				    list-depth
				    #t
				    (current-unparser-table))
	       object))))

(define (walk-pair pair list-depth)
  (if (and *unparser-list-depth-limit*
	   (>= list-depth *unparser-list-depth-limit*)
	   (no-highlights? pair))
      "..."
      (let ((list-depth (+ list-depth 1)))
	(let loop ((pair pair) (list-breadth 0))
	  (cond ((and *unparser-list-breadth-limit*
		      (>= list-breadth *unparser-list-breadth-limit*)
		      (no-highlights? pair))
		 (make-singleton-list-node "..."))
		((null? (cdr pair))
		 (make-singleton-list-node
		  (numerical-walk (car pair) list-depth)))
		(else
		 (make-list-node
		  (numerical-walk (car pair) list-depth)
		  (let ((list-breadth (+ list-breadth 1)))
		    (if (and (pair? (cdr pair))
			     (not (unparse-list/unparser (cdr pair))))
			(loop (cdr pair) list-breadth)
			(make-list-node
			 "."
			 (make-singleton-list-node
			  (if (and *unparser-list-breadth-limit*
				   (>= list-breadth
				       *unparser-list-breadth-limit*)
				   (no-highlights? pair))
			      "..."
			      (numerical-walk (cdr pair)
					      list-depth)))))))))))))

(define-integrable (no-highlights? object)
  (or *pp-no-highlights?*
      (not (partially-highlighted? object))))

(define (partially-highlighted? object)
  (cond ((pair? object)
	 (or (partially-highlighted? (car object))
	     (partially-highlighted? (cdr object))))
	((pretty-printer-highlight? object)
	 #t)
	((vector? object)
	 (partially-highlighted? (vector->list object)))
	(else
	 #f)))

(define (walk-highlighted-object object list-depth numerical-walk)
  (let ((dl (pph/depth-limit object)))
    (fluid-let ((*unparser-list-breadth-limit*
		 (let ((bl (pph/breadth-limit object)))
		   (if (eq? bl 'DEFAULT)
		       *unparser-list-breadth-limit*
		       bl)))
		(*unparser-list-depth-limit*
		 (if (eq? dl 'DEFAULT)
		     *unparser-list-depth-limit*
		     dl)))
      (numerical-walk (pph/object object)
		      (if (eq? dl 'DEFAULT)
			  list-depth
			  0)))))


;;;     The following are circular list/vector handing procedures.  They allow
;;;  arbitary circular constructions made from pairs and vectors to be printed
;;;  in closed form.  The term "current parenthetical level" means the lowest
;;;  parethetical level which contains the circularity object.  Expressions
;;;  like "up 1 parenthetical level" refer to the object which is one
;;;  parenthetical level above the lowest parenthetical level which contains
;;;  the circularity object--i.e., the second lowest parenthetical level
;;;  which contains the circularity object.
;;;     Finally, the expression, "up 1 parenthetical level, downstream 1 cdr,"
;;;  means that to find the object being referred to, you should go to the
;;;  parenthetical level one level above the lowest parenthetical level which
;;;  contains the circularity object, and then take the cdr of that list.
;;;  This notation must be used because while a new parenthetical level is
;;;  generated for each car and each vector-ref, a new parenthetical level
;;;  obtains from cdring iff the result of said cdring is not a pair.

;; This is the master procedure which all circularity-proof printing
;; goes through.

(define (numerical-walk-avoid-circularities exp list-depth)
  (numerical-walk-terminating exp (cons exp (make-queue)) list-depth))

;; This numerical walker has special pair and vector walkers to guarantee
;; proper termination.

(define (numerical-walk-terminating object half-pointer/queue list-depth)
  (define queue (cdr half-pointer/queue))
  (define half-pointer (car half-pointer/queue))
  (cond ((pair? object)
	 (let ((prefix (unparse-list/prefix-pair? object)))
	   (if prefix
	       (make-prefix-node
		prefix
		(numerical-walk-terminating
		 (cadr object)
		 (advance half-pointer (update-queue queue '(CDR CAR)))
		 list-depth))
	       (let ((unparser (unparse-list/unparser object)))
		 (if unparser
		     (walk-custom unparser object list-depth)
		     (walk-pair-terminating object half-pointer/queue
					    list-depth))))))
	((symbol? object)
	 (if (or *pp-uninterned-symbols-by-name*
		 (interned-symbol? object))
	     object
	     (walk-custom unparse-object object list-depth)))
	((pretty-printer-highlight? object)
	 (let ((rest (walk-highlighted-object object list-depth)))
	   (make-highlighted-node (+ (pph/start-string-length object)
				     (pph/end-string-length object)
				     (node-size rest))
				  object
				  rest)))
	((vector? object)
	 (if (zero? (vector-length object))
	     (walk-custom unparse-object object list-depth)
	     (let ((unparser (unparse-vector/unparser object)))
	       (if unparser
		   (walk-custom unparser object list-depth)
		   (make-prefix-node
		    "#"
		    (walk-vector-terminating
		     (vector->list object)
		     half-pointer/queue list-depth))))))
	((primitive-procedure? object)
	 (if *pp-primitives-by-name*
	     (primitive-procedure-name object)
	     (walk-custom unparse-object object list-depth)))
	(else
	 (walk-custom unparse-object object list-depth))))

;;; The following two procedures walk lists and vectors, respectively.

(define (walk-pair-terminating pair half-pointer/queue list-depth)
	(if (and *unparser-list-depth-limit*
	   (>= list-depth *unparser-list-depth-limit*)
	   (no-highlights? pair))
      "..."
      (let ((list-depth (+ list-depth 1)))
	(let loop ((pair pair) (list-breadth 0)
			       (half-pointer/queue half-pointer/queue))
	  (cond ((and *unparser-list-breadth-limit*
		      (>= list-breadth *unparser-list-breadth-limit*)
		      (no-highlights? pair))
		 (make-singleton-list-node "..."))
		((null? (cdr pair))
		 (make-singleton-list-node
		  (let ((half-pointer/queue
			 (advance
			  (car half-pointer/queue)
			  (update-queue (cdr half-pointer/queue) '(CAR)))))
		    (if (eq? (car half-pointer/queue) (car pair))
			(circularity-string (cdr half-pointer/queue))
			(numerical-walk-terminating
			 (car pair) half-pointer/queue list-depth)))))
		(else
		 (make-list-node
		  (let ((half-pointer/queue
			 (advance
			  (car half-pointer/queue)
			  (update-queue (cdr half-pointer/queue) '(CAR)))))
		    (if (eq? (car half-pointer/queue) (car pair))
			(circularity-string (cdr half-pointer/queue))
			(numerical-walk-terminating
			 (car pair) half-pointer/queue list-depth)))
		  (let ((list-breadth (+ list-breadth 1)))
		    (if
		     (and (pair? (cdr pair))
			  (not (unparse-list/unparser (cdr pair))))
		     (let ((half-pointer/queue
			    (advance
			     (car half-pointer/queue)
			     (update-queue (cdr half-pointer/queue) '(CDR)))))
		       (if (eq? (car half-pointer/queue) (cdr pair))
			   (make-singleton-list-node
			    (string-append
			     ". "
			     (circularity-string (cdr half-pointer/queue))))
			   (loop (cdr pair) list-breadth half-pointer/queue)))
		     (make-list-node
		      "."
		      (make-singleton-list-node
		       (if
			(and *unparser-list-breadth-limit*
			     (>= list-breadth
				 *unparser-list-breadth-limit*)
			     (no-highlights? pair))
			"..."
			(let ((half-pointer/queue
			       (advance
				(car half-pointer/queue)
				(update-queue
				 (cdr half-pointer/queue) '(CDR)))))
			  (if (eq? (car half-pointer/queue) (cdr pair))
			      (circularity-string (cdr half-pointer/queue))
			      (numerical-walk-terminating
			       (cdr pair)
			       half-pointer/queue list-depth)))))))))))))))

(define (walk-vector-terminating pair half-pointer/queue list-depth)
  (if (and *unparser-list-depth-limit*
	   (>= list-depth *unparser-list-depth-limit*)
	   (no-highlights? pair))
      "..."
      (let ((list-depth (+ list-depth 1)))
	(let loop ((pair pair) (list-breadth 0))
	  (cond ((and *unparser-list-breadth-limit*
		      (>= list-breadth *unparser-list-breadth-limit*)
		      (no-highlights? pair))
		 (make-singleton-list-node "..."))
		((null? (cdr pair))
		 (make-singleton-list-node
		  (let ((half-pointer/queue
			 (advance
			  (car half-pointer/queue)
			  (update-queue
			   (cdr half-pointer/queue) (list list-breadth)))))
		    (if (eq? (car half-pointer/queue) (car pair))
			(circularity-string (cdr half-pointer/queue))
			(numerical-walk-terminating
			 (car pair) half-pointer/queue list-depth)))))
		(else
		 (make-list-node
		  (let ((half-pointer/queue
			 (advance (car half-pointer/queue)
				  (update-queue (cdr half-pointer/queue)
						(list list-breadth)))))
		    (if (eq? (car half-pointer/queue) (car pair))
			(circularity-string (cdr half-pointer/queue))
			(numerical-walk-terminating
			 (car pair) half-pointer/queue list-depth)))
		  (let ((list-breadth (+ list-breadth 1)))
		    (if (not (unparse-list/unparser (cdr pair)))
			(loop (cdr pair) list-breadth)
			(make-list-node
			 "."
			 (make-singleton-list-node
			  (if (and *unparser-list-breadth-limit*
				   (>= list-breadth
				       *unparser-list-breadth-limit*)
				   (no-highlights? pair))
			      "..."
			      (numerical-walk-terminating
			       (cdr pair)
			       half-pointer/queue list-depth)))))))))))))

;;;; These procedures allow the walkers to interact with the queue.

(define cocked-object)

(define (advance half-object queue)
  (cond ((vector? half-object)
	 (cons (cons cocked-object half-object) queue))
	((not (pair? half-object))
	 (cons half-object queue))
	((eq? (car half-object) cocked-object)
	 (cons (let ((directive (queue-car queue)))
		 (cond ((>= directive 0)
			(vector-ref (cdr half-object) directive))
		       ((= directive -1)
			(cadr half-object))
		       (else
			(cddr half-object))))
	       (queue-cdr queue)))
	(else
	 (cons (cons cocked-object half-object) queue))))

(define (update-queue queue command-list)
  (define (uq-iter queue command-list)
    (cond ((null? command-list) queue)
	  ((eq? (car command-list) 'CAR)
	   (uq-iter (add-car queue) (cdr command-list)))
	  ((eq? (car command-list) 'CDR)
	   (uq-iter (add-cdr queue) (cdr command-list)))
	  (else
	   (uq-iter (add-vector-ref (car command-list) queue)
		    (cdr command-list)))))
  (uq-iter queue command-list))

(define (add-car queue)
  (queue-cons queue -1))

(define (add-cdr queue)
  (queue-cons queue -2))

(define (add-vector-ref n queue)
  (queue-cons queue n))


;;;; The Queue Abstraction.  Queues are data structures allowing fifo
;;;  access without mutation.  The following procedures implement them.

(define-structure (queue
		   (conc-name queue/)
		   (constructor
		    make-queue
		    (#!optional cons-cell past-cdrs)))
  (cons-cell (let* ((new-vector (make-fluid-vector))
		    (pointer (cons 0 new-vector)))
	       (cons pointer pointer)))
  (past-cdrs 0))

;;; Fluid Vectors.
;;  Queues are built on a subabstraction, "fluid-vectors," which
;;  are actually nested vectors of a default length.

(define default-fluid-vector-length 10)
(define virtual-fluid-vector-length (-1+ default-fluid-vector-length))

(define (fluid-vector-extend fluid-vector)
  (define new-fluid-vector (make-fluid-vector))
  (vector-set! fluid-vector virtual-fluid-vector-length new-fluid-vector)
  new-fluid-vector)

(define (fluid-vector-set! fluid-vector index object)
  (define tail (vector-ref fluid-vector virtual-fluid-vector-length))
  (if (< index virtual-fluid-vector-length)
      (vector-set! fluid-vector index object)
      (fluid-vector-set! tail (- index virtual-fluid-vector-length) object)))

(define (make-fluid-vector)
  (make-vector default-fluid-vector-length #f))

;;; The actual queue constructors/extractors

(define (queue-cons queue object)
  (let* ((old-cell (queue/cons-cell queue))
	 (head (car old-cell))
	 (tail (cdr old-cell)))
    (if (eq? head tail)
	(begin
	  (fluid-vector-set! (cdr tail) 0 object)
	  (make-queue (cons head (cons 1 (cdr tail))) (queue/past-cdrs queue)))
	(begin
	  (fluid-vector-set! (cdr tail) (car tail) object)
	  (make-queue (cons
		       head
		       (if (= (car tail) (-1+ virtual-fluid-vector-length))
				(cons 0 (fluid-vector-extend (cdr tail)))
				(cons (1+ (car tail)) (cdr tail))))
		      (queue/past-cdrs queue))))))

(define (queue-car queue)
  (define head (car (queue/cons-cell queue)))
  (vector-ref (cdr head) (car head)))

(define (queue-cdr queue)
  (define head (car (queue/cons-cell queue)))
  (define tail (cdr (queue/cons-cell queue)))
  (make-queue
   (cons
    (if (= (car head) (-1+ virtual-fluid-vector-length))
	(cons 0 (vector-ref (cdr head) virtual-fluid-vector-length))
	(cons (1+ (car head)) (cdr head)))
    tail)
   (if (= (queue-car queue) -2)
       (1+ (queue/past-cdrs queue))
       0)))

;;; Auxilary queue handlers.

(define (null-queue? queue)
  (define cell (queue/cons-cell queue))
  (eq? (car cell) (cdr cell)))

(define (queue-depth queue)
  (define (flatten starting-vector starting-n ending-vector ending-n)
    (if (eq? starting-vector ending-vector)
	(vector->list (subvector starting-vector starting-n ending-n))
	(append
	 (vector->list
	  (subvector starting-vector starting-n virtual-fluid-vector-length))
	 (flatten
	  (vector-ref starting-vector virtual-fluid-vector-length) 0
	  ending-vector ending-n))))
  (define (proc-list-iter list code-cache)
    (cond ((null? list) (if (eq? code-cache -2) 1 0))
	  ((>= (car list) 0)
	   (+ (if (eq? code-cache -2) 2 1)
	      (proc-list-iter (cdr list) (car list))))
	  ((= (car list) -1)
	   (1+ (proc-list-iter (cdr list) (car list))))
	  (else
	   (proc-list-iter (cdr list) (car list)))))
  (let* ((cell (queue/cons-cell queue))
	 (head (car cell))
	 (tail (cdr cell))
	 (operating-list
	  (flatten (cdr head) (car head) (cdr tail) (car tail))))
    (proc-list-iter operating-list #f)))


;;; This procedure creates the circularity object which is printed
;;; within circular structures.

(define (circularity-string queue)
  (let ((depth (queue-depth queue))
	(cdrs (queue/past-cdrs queue)))
    (string-append
     (cond ((= depth 1) "#[circularity (current parenthetical level")
	   ((= depth 2) "#[circularity (up 1 parenthetical level")
	   (else
	    (string-append "#[circularity (up "
			   (number->string (-1+ depth))
			   " parenthetical levels")))
     (cond ((= cdrs 0) ")]")
	   ((= cdrs 1) ", downstream 1 cdr.)]")
	   (else
	    (string-append ", downstream "
			   (number->string cdrs) " cdrs.)]"))))))


;;;; Node Model
;;;  Carefully crafted to use the least amount of memory, while at the
;;;  same time being as fast as possible.  The only concession to
;;;  space was in the implementation of atomic nodes, in which it was
;;;  decided that the extra space needed to cache the size of a string
;;;  or the print-name of a symbol wasn't worth the speed that would
;;;  be gained by keeping it around.

(define-integrable (symbol-length symbol)
  (string-length (symbol-name symbol)))

(define-integrable (*unparse-symbol symbol)
  (*unparse-string (symbol-name symbol)))

(define-structure (prefix-node
		   (conc-name prefix-node-)
		   (constructor %make-prefix-node))
  (size #f read-only #t)
  (prefix #f read-only #t)
  (subnode #f read-only #t))

(define (make-prefix-node prefix subnode)
  (cond ((string? subnode)
	 (string-append prefix subnode))
	((prefix-node? subnode)
	 (make-prefix-node (string-append prefix (prefix-node-prefix subnode))
			   (prefix-node-subnode subnode)))
	(else
	 (%make-prefix-node (+ (string-length prefix) (node-size subnode))
			    prefix
			    subnode))))


(define (make-list-node car-node cdr-node)
  (cons (+ 1 (node-size car-node) (list-node-size cdr-node)) ;+1 space.
	(cons car-node (node-subnodes cdr-node))))

(define (make-singleton-list-node car-node)
  (cons (+ 2 (node-size car-node))			;+1 each parenthesis.
	(list car-node)))

(define-integrable (list-node? object)
  (pair? object))

(define-integrable (list-node-size node)
  (car node))

(define-integrable (node-subnodes node)
  (cdr node))

(define (node-size node)
  (cond ((list-node? node) (list-node-size node))
	((symbol? node) (symbol-length node))
	((prefix-node? node) (prefix-node-size node))
	((highlighted-node? node)
	 (highlighted-node/size node))
	(else (string-length node))))

(define-structure (highlighted-node
		   (conc-name highlighted-node/)
		   (constructor make-highlighted-node))
  (size #f read-only #t)
  (highlight #f read-only #t)
  (subnode #f read-only #t))

(define (unhighlight node)
  (if (highlighted-node? node)
      (unhighlight (highlighted-node/subnode node))
      node))