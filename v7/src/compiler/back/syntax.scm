#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/syntax.scm,v 1.22 1988/08/31 06:43:22 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; LAP Syntaxer

(declare (usual-integrations))

(define-integrable cons-syntax cons)
(define-integrable append-syntax! append!)

#|
(define (cons-syntax directive directives)
  (if (and (bit-string? directive)
	   (not (null? directives))
	   (bit-string? (car directives)))
      (begin (set-car! directives
		       (instruction-append directive (car directives)))
	     directives)
      (cons directive directives)))

(define (append-syntax! directives1 directives2)
  (cond ((null? directives1) directives2)
	((null? directives2) directives1)
	(else
	 (let ((tail (last-pair directives1)))
	   (if (and (bit-string? (car tail))
		    (bit-string? (car directives2)))
	       (begin
		 (set-car! tail
			   (instruction-append (car tail) (car directives2)))
		 (set-cdr! tail (cdr directives2)))
	       (set-cdr! tail directives2))
	   directives1))))
|#

(define (lap:syntax-instruction instruction)
  (if (memq (car instruction)
	    '(EQUATE SCHEME-OBJECT ENTRY-POINT LABEL BLOCK-OFFSET))
      (directive->instruction-sequence instruction)
      (let ((match-result (instruction-lookup instruction)))
	(if (not match-result)
	    (error "LAP:SYNTAX-INSTRUCTION: illegal instruction syntax"
		   instruction))
	(let ((directives (match-result)))
	  (if (null? directives)
	      (error "LAP:SYNTAX-INSTRUCTION: instruction generation error"
		     instruction))
	  (instruction->instruction-sequence directives)))))

(define (instruction-lookup instruction)
  (pattern-lookup
   (cdr (or (assq (car instruction) instructions)
	    (error "INSTRUCTION-LOOKUP: Unknown keyword" (car instruction))))
   (cdr instruction)))

(define (add-instruction! keyword lookup)
  (let ((entry (assq keyword instructions)))
    (if entry
	(set-cdr! entry lookup)
	(set! instructions (cons (cons keyword lookup) instructions))))
  keyword)

(define instructions
  '())

(define (integer-syntaxer expression coercion-type size)
  (let ((name (make-coercion-name coercion-type size)))
    (if (integer? expression)
	`',((lookup-coercion name) expression)
	`(SYNTAX-EVALUATION ,expression ,name))))

(define (syntax-evaluation expression coercion)
  (if (integer? expression)
      (coercion expression)
      `(EVALUATION ,expression ,(coercion-size coercion) ,coercion)))

(define (optimize-group . components)
  (optimize-group-internal components
    (lambda (result make-group?)
      (if make-group?
	  `(GROUP ,@result)
	  result))))

(define-integrable optimize-group-early
  optimize-group)

(define optimize-group-internal
  (let ()
    (define (loop1 components)
      (cond ((null? components) '())
	    ((bit-string? (car components))
	     (loop2 (car components) (cdr components)))
	    (else
	     (cons (car components)
		   (loop1 (cdr components))))))

    (define (loop2 bit-string components)
      (cond ((null? components)
	     (list bit-string))
	    ((bit-string? (car components))
	     (loop2 (instruction-append bit-string (car components))
		    (cdr components)))
	    (else
	     (cons bit-string
		   (cons (car components)
			 (loop1 (cdr components)))))))

    (lambda (components receiver)
      (let ((components (loop1 components)))
	(if (null? components)
	    (error "OPTIMIZE-GROUP: No components"))
	(if (null? (cdr components))
	    (receiver (car components) false)
	    (receiver components true))))))

;;;; Variable width expression processing

(define (choose-clause value clauses)
  (if (null? clauses)
      (error "CHOOSE-CLAUSE: value out of range" value))
  (if (let ((low (caddr (car clauses)))
	    (high (cadddr (car clauses))))
	(and (or (null? low)
		 (<= low value))
	     (or (null? high)
		 (<= value high))))
      (car clauses)
      (choose-clause value (cdr clauses))))

(define (variable-width-expression-syntaxer name expression clauses)
  (if (integer? expression)
      (let ((chosen (choose-clause expression clauses)))
	`(LET ((,name ,expression))
	   (DECLARE (INTEGRATE ,name))
	   ,name			;ignore if not referenced
	   (CAR ,(car chosen))))
      `(SYNTAX-VARIABLE-WIDTH-EXPRESSION
	,expression
	(LIST
	 ,@(map (LAMBDA (clause)
		  `(CONS (LAMBDA (,name)
			   ,name	;ignore if not referenced
			   ,(car clause))
			 ',(cdr clause)))
		clauses)))))

(define (syntax-variable-width-expression expression clauses)
  (if (integer? expression)      (let ((chosen (choose-clause expression clauses)))
	(car ((car chosen) expression)))
      `(VARIABLE-WIDTH-EXPRESSION
	,expression
	,@clauses)))

;;;; Coercion Machinery

(define (make-coercion-name coercion-type size)
  (string->symbol
   (string-append "COERCE-"
		  (number->string size)
		  "-BIT-"
		  (symbol->string coercion-type))))

(define coercion-property-tag
  "Coercion")

(define ((coercion-maker coercion-types) coercion-type size)
  (let ((coercion
	 ((cdr (or (assq coercion-type coercion-types)
		   (error "Unknown coercion type" coercion-type)))
	  size)))
    (2D-put! coercion coercion-property-tag (list coercion-type size))
    coercion))

(define (coercion-size coercion)
  (cadr (coercion-properties coercion)))

(define (unmake-coercion coercion receiver)
  (apply receiver (coercion-properties coercion)))

(define (coercion-properties coercion)
  (or (2D-get coercion coercion-property-tag)
      (error "COERCION-PROPERTIES: Not a known coercion" coercion)))

(define coercion-environment
  (the-environment))

(define-integrable (lookup-coercion name)
  (lexical-reference coercion-environment name))

(define ((coerce-unsigned-integer nbits) n)
  (unsigned-integer->bit-string nbits n))

(define (coerce-signed-integer nbits)
  (let* ((limit (expt 2 (-1+ nbits)))
	 (offset (+ limit limit)))
    (lambda (n)
      (unsigned-integer->bit-string
       nbits
       (cond ((negative? n) (+ n offset))
	     ((< n limit) n)
	     (else (error "Integer too large to be encoded" n)))))))

(define (standard-coercion kernel)
  (lambda (nbits)
    (lambda (n)
      (unsigned-integer->bit-string nbits (kernel n)))))