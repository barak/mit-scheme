;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;;; Pretty Printer

(declare (usual-integrations))

(define scheme-pretty-printer
  (make-environment

(define *pp-primitives-by-name* true)
(define *forced-x-size* false)
(define *default-x-size* 80)

(define x-size)
(define next-coords)
(define add-sc-entry!)
(define sc-relink!)

(declare (integrate *unparse-string *unparse-char))

(define (*unparse-string string)
  (declare (integrate string))
  ((access :write-string *current-output-port*) string))

(define (*unparse-char char)
  (declare (integrate char))
  ((access :write-char *current-output-port*) char))

(define (*unparse-open)
  (*unparse-char #\())

(define (*unparse-close)
  (*unparse-char #\)))

(define (*unparse-space)
  (*unparse-char #\Space))

(define (*unparse-newline)
  (*unparse-char char:newline))

;;;; Top Level

(define (pp expression as-code?)
  (fluid-let ((x-size (get-x-size)))
    (let ((node (numerical-walk expression)))
      (*unparse-newline)
      ((if as-code? print-node print-non-code-node) node 0 0)
      ((access :flush-output *current-output-port*)))))

(define (stepper-pp expression port p-wrapper table nc relink! sc! offset)
  (fluid-let ((x-size (get-x-size))
	      (walk-dispatcher table)
	      (next-coords nc)
	      (sc-relink! relink!)
	      (add-sc-entry! sc!)
	      (print-combination (p-wrapper print-combination))
	      (forced-indentation (p-wrapper forced-indentation))
	      (pressured-indentation (p-wrapper pressured-indentation))
	      (print-procedure (p-wrapper print-procedure))
	      (print-let-expression (p-wrapper print-let-expression))
	      (print-node (p-wrapper print-node))
	      (print-guaranteed-node (p-wrapper print-guaranteed-node)))
    (let ((node (numerical-walk expression)))
      (with-output-to-port port
	(lambda ()
	  (print-node node (car offset) 0)
	  ((access :flush-output *current-output-port*)))))))

(define (get-x-size)
  (or *forced-x-size*
      ((access :x-size *current-output-port*))
      *default-x-size*))

(define (print-non-code-node node column depth)
  (fluid-let ((dispatch-list '()))
    (print-node node column depth)))

(define (print-node node column depth)
  (cond ((list-node? node) (print-list-node node column depth))
	((symbol? node) (*unparse-symbol node))
	((prefix-node? node) (*unparse-string (node-prefix node))
	 (print-node (node-subnode node) 
		     (+ (string-length (node-prefix node)) column)
		     depth))
	(else (*unparse-string node))))

(define (print-list-node node column depth)
  (if (fits-within? node column depth)
      (print-guaranteed-list-node node)
      (let ((subnodes (node-subnodes node)))
	((or (let ((association (assq (car subnodes) dispatch-list)))
	       (and association (cdr association)))
	     print-combination)
	 subnodes column depth))))

(define (print-guaranteed-node node)
  (cond ((list-node? node) (print-guaranteed-list-node node))
	((symbol? node) (*unparse-symbol node))
	((prefix-node? node)
	 (*unparse-string (node-prefix node))
	 (print-guaranteed-node (node-subnode node)))
	(else (*unparse-string node))))

(define (print-guaranteed-list-node node)
  (define (loop nodes)
    (print-guaranteed-node (car nodes))
    (if (not (null? (cdr nodes)))
	(begin (*unparse-space)
	       (loop (cdr nodes)))))
  (*unparse-open)
  (loop (node-subnodes node))
  (*unparse-close))

(define (print-column nodes column depth)
  (define (loop nodes)
    (if (null? (cdr nodes))
	(print-node (car nodes) column depth)
	(begin (print-node (car nodes) column 0)
	       (tab-to column)
	       (loop (cdr nodes)))))
  (loop nodes))

(define (print-guaranteed-column nodes column)
  (define (loop nodes)
    (print-guaranteed-node (car nodes))
    (if (not (null? (cdr nodes)))
	(begin (tab-to column)
	       (loop (cdr nodes)))))
  (loop nodes))

;;;; Printers

(define (print-combination nodes column depth)
  (*unparse-open)
  (let ((column (1+ column)) (depth (1+ depth)))
    (cond ((null? (cdr nodes))
	   (print-node (car nodes) column depth))
	  ((two-on-first-line? nodes column depth)
	   (print-guaranteed-node (car nodes)) 
	   (*unparse-space)
	   (print-guaranteed-column (cdr nodes)
				    (1+ (+ column (node-size (car nodes))))))
	  (else
	   (print-column nodes column depth))))
  (*unparse-close))

(define ((special-printer procedure) nodes column depth)
  (*unparse-open)
  (*unparse-symbol (car nodes))
  (*unparse-space)
  (if (not (null? (cdr nodes)))
      (procedure (cdr nodes)
		 (+ 2 (+ column (symbol-length (car nodes))))
		 (+ 2 column)
		 (1+ depth)))
  (*unparse-close))

;;; Force the indentation to be an optimistic column.

(define forced-indentation
  (special-printer
   (lambda (nodes optimistic pessimistic depth)
     (print-column nodes optimistic depth))))

;;; Pressure the indentation to be an optimistic column; no matter
;;; what happens, insist on a column, but accept a pessimistic one if
;;; necessary.

(define pressured-indentation
  (special-printer
   (lambda (nodes optimistic pessimistic depth)
     (if (fits-as-column? nodes optimistic depth)
	 (print-guaranteed-column nodes optimistic)
	 (begin (tab-to pessimistic)
		(print-column nodes pessimistic depth))))))

;;; Print a procedure definition.  The bound variable pattern goes on
;;; the same line as the keyword, while everything else gets indented
;;; pessimistically.  We may later want to modify this to make higher
;;; order procedure patterns be printed more carefully.

(define print-procedure
  (special-printer
   (lambda (nodes optimistic pessimistic depth)
     (print-node (car nodes) optimistic 0)
     (tab-to pessimistic)
     (print-column (cdr nodes) pessimistic depth))))

;;; Print a binding form.  There is a great deal of complication here,
;;; some of which is to gracefully handle the case of a badly-formed
;;; binder.  But most important is the code that handles the name when
;;; we encounter a named LET; it must go on the same line as the
;;; keyword.  In that case, the bindings try to fit on that line or
;;; start on that line if possible; otherwise they line up under the
;;; name.  The body, of course, is always indented pessimistically.

(define print-let-expression
  (special-printer
   (lambda (nodes optimistic pessimistic depth)
     (define (print-body nodes)
       (if (not (null? nodes))
	   (begin (tab-to pessimistic)
		  (print-column nodes pessimistic depth))))
     (cond ((null? (cdr nodes))				;Screw case.
	    (print-node (car nodes) optimistic depth))
	   ((symbol? (car nodes))			;Named LET.
	    (*unparse-symbol (car nodes))
	    (let ((new-optimistic
		   (1+ (+ optimistic (symbol-length (car nodes))))))
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
					      (1+ new-optimistic))
		     (*unparse-close)
		     (print-body (cddr nodes)))
		    (else
		     (tab-to optimistic)
		     (print-node (cadr nodes) optimistic 0)
		     (print-body (cddr nodes))))))
	   (else					;Ordinary LET.
	    (print-node (car nodes) optimistic 0)
	    (print-body (cdr nodes)))))))

(define dispatch-list
  `((COND . ,forced-indentation)
    (IF . ,forced-indentation)
    (OR . ,forced-indentation)
    (AND . ,forced-indentation)
    (LET . ,print-let-expression)
    (FLUID-LET . ,print-let-expression)
    (DEFINE . ,print-procedure)
    (LAMBDA . ,print-procedure)
    (NAMED-LAMBDA . ,print-procedure)))

;;;; Alignment

(declare (integrate fits-within?))

(define (fits-within? node column depth)
  (declare (integrate node column depth))
  (> (- x-size depth)
     (+ column (node-size node))))

;;; Fits if each node fits when stacked vertically at the given column.

(define (fits-as-column? nodes column depth)
  (define (loop nodes)
    (if (null? (cdr nodes))
	(fits-within? (car nodes) column depth)
	(and (> x-size
		(+ column (node-size (car nodes))))
	     (loop (cdr nodes)))))
  (loop nodes))

;;; Fits if first two nodes fit on same line, and rest fit under the
;;; second node.  Assumes at least two nodes are given.

(define (two-on-first-line? nodes column depth)
  (let ((column (1+ (+ column (node-size (car nodes))))))
    (and (> x-size column)
	 (fits-as-column? (cdr nodes) column depth))))

;;; Starts a new line with the specified indentation.

(define (tab-to column)
  (*unparse-newline)
  (*unparse-string (make-string column #\Space)))

;;;; Numerical Walk

(define (numerical-walk object)
  ((walk-dispatcher object) object))

(define (walk-general object)
  (write-to-string object))

(define (walk-primitive primitive)
  (if *pp-primitives-by-name*
      (primitive-procedure-name primitive)
      (write-to-string primitive)))

(define (walk-pair pair)
  (if (and (eq? (car pair) 'QUOTE)
	   (pair? (cdr pair))
	   (null? (cddr pair)))
      (make-prefix-node "'" (numerical-walk (cadr pair)))
      (walk-unquoted-pair pair)))

(define (walk-unquoted-pair pair)
  (if (null? (cdr pair))
      (make-singleton-list-node (numerical-walk (car pair)))
      (make-list-node
       (numerical-walk (car pair))
       (if (pair? (cdr pair))
	   (walk-unquoted-pair (cdr pair))
	   (make-singleton-list-node
	    (make-prefix-node ". " (numerical-walk (cdr pair))))))))

(define (walk-vector vector)
  (if (zero? (vector-length vector))
      "#()"
      (make-prefix-node "#" (walk-unquoted-pair (vector->list vector)))))

(define walk-dispatcher
  (make-type-dispatcher
   `((,symbol-type ,identity-procedure)
     (,primitive-procedure-type ,walk-primitive)
     (,(microcode-type-object 'PAIR) ,walk-pair)
     (,(microcode-type-object 'VECTOR) ,walk-vector)
     (,unparser-special-object-type ,walk-general))
   walk-general))

;;;; Node Model
;;;  Carefully crafted to use the least amount of memory, while at the
;;;  same time being as fast as possible.  The only concession to
;;;  space was in the implementation of atomic nodes, in which it was
;;;  decided that the extra space needed to cache the size of a string
;;;  or the print-name of a symbol wasn't worth the speed that would
;;;  be gained by keeping it around.

(declare (integrate symbol-length *unparse-symbol))

(define (symbol-length symbol)
  (declare (integrate symbol))
  (string-length (symbol->string symbol)))

(define (*unparse-symbol symbol)
  (declare (integrate symbol))
  (*unparse-string (symbol->string symbol)))

(define (make-prefix-node prefix subnode)
  (cond ((or (list-node? subnode)
	     (symbol? subnode))
	 (vector (+ (string-length prefix)
		    (node-size subnode))
		 prefix
		 subnode))
	((prefix-node? subnode)
	 (make-prefix-node (string-append prefix (node-prefix subnode))
			   (node-subnode subnode)))
	(else
	 (string-append prefix subnode))))

(define prefix-node? vector?)
(define prefix-node-size vector-first)
(define node-prefix vector-second)
(define node-subnode vector-third)

(define (make-list-node car-node cdr-node)
  (cons (1+ (+ (node-size car-node) (list-node-size cdr-node)))	;+1 space.
	(cons car-node (node-subnodes cdr-node))))

(define (make-singleton-list-node car-node)
  (cons (+ 2 (node-size car-node))			;+1 each parenthesis.
	(list car-node)))

(declare (integrate list-node? list-node-size node-subnodes))

(define list-node? pair?)
(define list-node-size car)
(define node-subnodes cdr)

(define (node-size node)
  ((cond ((list-node? node) list-node-size)
	 ((symbol? node) symbol-length)
	 ((prefix-node? node) prefix-node-size)
	 (else string-length))
   node))

;;; end SCHEME-PRETTY-PRINTER package.
))

;;;; Exports

(define pp
  (let ()
    (define (prepare scode)
      (let ((s-expression (unsyntax scode)))
	(if (and (pair? s-expression)
		 (eq? (car s-expression) 'NAMED-LAMBDA))
	    `(DEFINE ,@(cdr s-expression))
	    s-expression)))

    (define (bad-arg argument)
      (error "Bad optional argument" 'PP argument))

    (lambda (scode . optionals)
      (define (kernel as-code?)
	(if (scode-constant? scode)
	    ((access pp scheme-pretty-printer) scode as-code?)
	    ((access pp scheme-pretty-printer) (prepare scode) #!TRUE)))

      (cond ((null? optionals)
	     (kernel #!FALSE))
	    ((null? (cdr optionals))
	     (cond ((eq? (car optionals) 'AS-CODE)
		    (kernel #!TRUE))
		   ((output-port? (car optionals))
		    (with-output-to-port (car optionals)
		      (lambda () (kernel #!FALSE))))
		   (else
		    (bad-arg (car optionals)))))
	    ((null? (cddr optionals))
	     (cond ((eq? (car optionals) 'AS-CODE)
		    (if (output-port? (cadr optionals))
			(with-output-to-port (cadr optionals)
			  (lambda () (kernel #!TRUE)))
			(bad-arg (cadr optionals))))
		   ((output-port? (car optionals))
		    (if (eq? (cadr optionals) 'AS-CODE)
			(with-output-to-port (car optionals)
			  (lambda () (kernel #!TRUE)))
			(bad-arg (cadr optionals))))
		   (else
		    (bad-arg (car optionals)))))
	    (else
	     (error "Too many optional arguments" 'PP optionals)))
      *the-non-printing-object*)))

(define (pa procedure)
  (if (not (compound-procedure? procedure))
      (error "Must be a compound procedure" procedure))
  (pp (unsyntax-lambda-list (procedure-lambda procedure))))