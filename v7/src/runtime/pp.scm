#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/pp.scm,v 14.11 1990/09/13 23:46:06 cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Pretty Printer
;;; package: (runtime pretty-printer)

(declare (usual-integrations))

(define (initialize-package!)
  (set! forced-indentation (special-printer kernel/forced-indentation))
  (set! pressured-indentation (special-printer kernel/pressured-indentation))
  (set! print-procedure (special-printer kernel/print-procedure))
  (set! print-let-expression (special-printer kernel/print-let-expression))
  (set! dispatch-list
	`((COND . ,forced-indentation)
	  (IF . ,forced-indentation)
	  (OR . ,forced-indentation)
	  (AND . ,forced-indentation)
	  (LET . ,print-let-expression)
	  (FLUID-LET . ,print-let-expression)
	  (DEFINE . ,print-procedure)
	  (LAMBDA . ,print-procedure)
	  (NAMED-LAMBDA . ,print-procedure)))
  (set! dispatch-default print-combination)
  unspecific)

(define *named-lambda->define?* true)
(define *pp-primitives-by-name* true)
(define *pp-uninterned-symbols-by-name* true)
(define *forced-x-size* false)

(define (pp object #!optional port . rest)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (let ((pretty-print
	   (lambda (object) (apply pretty-print object port rest))))
      (newline port)
      (if (named-structure? object)
	  (begin
	    (pretty-print object)
	    (for-each (lambda (element)
			(newline port)
			(pretty-print element))
		      (named-structure/description object)))
	  (pretty-print
	   (or (and (procedure? object) (procedure-lambda object))
	       object))))))

(define (pretty-print object #!optional port as-code? indentation)
  (pp-top-level (if (scode-constant? object)
		    object
		    (let ((sexp (unsyntax object)))
		      (if (and *named-lambda->define?*
			       (pair? sexp)
			       (eq? (car sexp) 'NAMED-LAMBDA))
			  `(DEFINE ,@(cdr sexp))
			  sexp)))
		(if (default-object? port) (current-output-port) port)
		(if (default-object? as-code?)
		    (not (scode-constant? object))
		    as-code?)
		(if (default-object? indentation) 0 indentation)
		0)
  unspecific)

(define (pp-top-level expression port as-code? indentation list-depth)
  (fluid-let ((x-size (or *forced-x-size* (output-port/x-size port)))
	      (output-port port))
    (let ((node (numerical-walk expression list-depth)))
      (if (positive? indentation)
	  (*unparse-string (make-string indentation #\space)))
      (if as-code?
	  (print-node node indentation list-depth)
	  (print-non-code-node node indentation list-depth))
      (output-port/flush-output port))))

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
	      (dispatch-default print-data-column))
    (print-node node column depth)))

(define (print-data-column nodes column depth)
  (*unparse-open)
  (print-column nodes (+ column 1) (+ depth 1))
  (*unparse-close))

(define (print-node node column depth)
  (cond ((list-node? node)
	 (print-list-node node column depth))
	((symbol? node)
	 (*unparse-symbol node))
	((prefix-node? node)
	 (*unparse-string (node-prefix node))
	 (print-node (node-subnode node) 
		     (+ column (string-length (node-prefix node)))
		     depth))
	(else
	 (*unparse-string node))))

(define (print-list-node node column depth)
  (if (fits-within? node column depth)
      (print-guaranteed-list-node node)
      (let ((subnodes (node-subnodes node)))
	((or (let ((association (assq (car subnodes) dispatch-list)))
	       (and association (cdr association)))
	     dispatch-default)
	 subnodes column depth))))

(define (print-guaranteed-node node)
  (cond ((list-node? node)
	 (print-guaranteed-list-node node))
	((symbol? node)
	 (*unparse-symbol node))
	((prefix-node? node)
	 (*unparse-string (node-prefix node))
	 (print-guaranteed-node (node-subnode node)))
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

(define ((special-printer procedure) nodes column depth)
  (*unparse-open)
  (*unparse-symbol (car nodes))
  (*unparse-space)
  (if (not (null? (cdr nodes)))
      (procedure (cdr nodes)
		 (+ column 2 (symbol-length (car nodes)))
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
  (print-node (car nodes) optimistic 0)
  (tab-to pessimistic)
  (print-column (cdr nodes) pessimistic depth))

;;; Print a binding form.  There is a great deal of complication here,
;;; some of which is to gracefully handle the case of a badly-formed
;;; binder.  But most important is the code that handles the name when
;;; we encounter a named LET; it must go on the same line as the
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
	   ;; named LET
	   (*unparse-symbol (car nodes))
	   (let ((new-optimistic
		  (+ optimistic (+ 1 (symbol-length (car nodes))))))
	     (cond ((fits-within? (cadr nodes) new-optimistic 0)
		    (*unparse-space)
		    (print-guaranteed-node (cadr nodes))
		    (print-body (cddr nodes)))
		   ((fits-as-column? (node-subnodes (cadr nodes))
				     (+ new-optimistic 2)
				     0)
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
	   ;; ordinary LET
	   (print-node (car nodes) optimistic 0)
	   (print-body (cdr nodes))))))

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
  (*unparse-string (make-string column #\space)))

;;;; Numerical Walk

(define (numerical-walk object list-depth)
  (cond ((pair? object)
	 (let ((unparser (unparse-list/unparser object)))
	   (if unparser
	       (let ((prefix (unparse-list/prefix-pair? object)))
		 (if prefix
		     (make-prefix-node prefix
				       (numerical-walk (cadr object)
						       list-depth))
		     (walk-custom unparser object list-depth)))
	       (walk-pair object list-depth))))
	((vector? object)
	 (let ((unparser
		(and (not (zero? (vector-length object)))
		     (unparse-vector/unparser object))))
	   (if unparser
	       (walk-custom unparser object list-depth)
	       (make-prefix-node "#"
				 (walk-pair (vector->list object)
					    list-depth)))))
	((symbol? object)
	 (if (or *pp-uninterned-symbols-by-name*
		 (object-type? (ucode-type interned-symbol) object))
	     object
	     (walk-custom unparse-object object list-depth)))
	((primitive-procedure? object)
	 (if *pp-primitives-by-name*
	     (primitive-procedure-name object)
	     (walk-custom unparse-object object list-depth)))
	(else
	 (walk-custom unparse-object object list-depth))))

(define (walk-custom unparser object list-depth)
  (with-string-output-port
   (lambda (port)
     (unparser (make-unparser-state port
				    list-depth
				    true
				    (current-unparser-table))
	       object))))

(define (walk-pair pair list-depth)
  (if (and *unparser-list-depth-limit*
	   (>= list-depth *unparser-list-depth-limit*))
      "..."
      (let ((list-depth (+ list-depth 1)))
	(let loop ((pair pair) (list-breadth 0))
	  (cond ((and *unparser-list-breadth-limit*
		      (>= list-breadth *unparser-list-breadth-limit*))
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
				       *unparser-list-breadth-limit*))
			      "..."
			      (numerical-walk (cdr pair)
					      list-depth)))))))))))))

;;;; Node Model
;;;  Carefully crafted to use the least amount of memory, while at the
;;;  same time being as fast as possible.  The only concession to
;;;  space was in the implementation of atomic nodes, in which it was
;;;  decided that the extra space needed to cache the size of a string
;;;  or the print-name of a symbol wasn't worth the speed that would
;;;  be gained by keeping it around.

(define-integrable (symbol-length symbol)
  (string-length (symbol->string symbol)))

(define-integrable (*unparse-symbol symbol)
  (*unparse-string (symbol->string symbol)))

(define (make-prefix-node prefix subnode)
  (cond ((or (list-node? subnode)
	     (symbol? subnode))
	 (vector (+ (string-length prefix) (node-size subnode))
		 prefix
		 subnode))
	((prefix-node? subnode)
	 (make-prefix-node (string-append prefix (node-prefix subnode))
			   (node-subnode subnode)))
	(else
	 (string-append prefix subnode))))

(define-integrable (prefix-node? object)
  (vector? object))

(define-integrable (prefix-node-size node)
  (vector-ref node 0))

(define-integrable (node-prefix node)
  (vector-ref node 1))

(define-integrable (node-subnode node)
  (vector-ref node 2))

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
	(else (string-length node))))