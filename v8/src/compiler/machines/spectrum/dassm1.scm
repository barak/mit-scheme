#| -*-Scheme-*-

$Id: dassm1.scm,v 1.2 1994/11/22 04:01:23 adams Exp $

Copyright (c) 1988-1994 Massachusetts Institute of Technology

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

;;;; Disassembler: User Level
;;; package: (compiler disassembler)

(declare (usual-integrations))

;;; Flags that control disassembler behavior

(define disassembler/symbolize-output? true)
(define disassembler/compiled-code-heuristics? true)
(define disassembler/write-offsets? true)
(define disassembler/write-addresses? false)

;;;; Top level entries

(define (compiler:write-lap-file filename #!optional symbol-table?)
  (let ((pathname (->pathname filename))
	(symbol-table?
	 (if (default-object? symbol-table?) true symbol-table?)))
    (with-output-to-file (pathname-new-type pathname "lap")
      (lambda ()
	(let ((com-file (pathname-new-type pathname "com")))
	  (let ((object (fasload com-file)))
	    (if (compiled-code-address? object)
		(let ((block (compiled-code-address->block object)))
		  (disassembler/write-compiled-code-block
		   block
		   (compiled-code-block/dbg-info block symbol-table?)))
		(begin
		  (if (not
		       (and (scode/comment? object)
			    (dbg-info-vector? (scode/comment-text object))))
		      (error "Not a compiled file" com-file))
		  (let ((blocks
			 (vector->list
			  (dbg-info-vector/blocks-vector
			   (scode/comment-text object)))))
		    (if (not (null? blocks))
			(do ((blocks blocks (cdr blocks)))
			    ((null? blocks) unspecific)
			  (disassembler/write-compiled-code-block
			   (car blocks)
			   (compiled-code-block/dbg-info (car blocks)
							 symbol-table?))
			  (if (not (null? (cdr blocks)))
			      (begin
				(write-char #\page)
				(newline))))))))))))))

(define disassembler/base-address)

(define (compiler:disassemble entry)
  (let ((block (compiled-entry/block entry)))
    (let ((info (compiled-code-block/dbg-info block true)))
      (fluid-let ((disassembler/write-offsets? true)
		  (disassembler/write-addresses? true)
		  (disassembler/base-address (object-datum block)))
	(newline)
	(newline)
	(disassembler/write-compiled-code-block block info)))))

(define (compiler:disassemble-memory start words)
  (fluid-let ((disassembler/write-offsets? false)
	      (disassembler/write-addresses? true)
	      (disassembler/base-address start))
    (newline)
    (newline)
    (disassembler/write-instruction-stream
     #F
     (disassembler/instructions/address start (+ start (* 4 words))))))

(define (disassembler/write-compiled-code-block block info)
  (let ((symbol-table (and info (dbg-info/labels info))))
    (write-string "Disassembly of ")
    (write block)
    (let loop ((info (compiled-code-block/debugging-info block)))
      (cond ((string? info)
	     (write-string " (")
	     (write-string info)
	     (write-string ")"))
	    ((not (pair? info)))
	    ((vector? (car info))
	     (loop (cdr info)))
	    (else
	       (write-string " (Block ")
	       (write (cdr info))
	       (write-string " in ")
	       (write-string (car info))
	       (write-string ")"))))
    (write-string ":\n")
    (write-string "Code:\n\n")
    (disassembler/write-instruction-stream
     symbol-table
     (disassembler/instructions/compiled-code-block block symbol-table))
    (write-string "\nConstants:\n\n")
    (disassembler/write-constants-block block symbol-table)
    (newline)))

(define (disassembler/instructions/compiled-code-block block symbol-table)
  (disassembler/instructions block
			     (compiled-code-block/code-start block)
			     (compiled-code-block/code-end block)
			     symbol-table))

(define (disassembler/instructions/address start-address end-address)
  (disassembler/instructions false start-address end-address false))

(define (disassembler/write-instruction-stream symbol-table instruction-stream)
  (fluid-let ((*unparser-radix* 16))
    (disassembler/for-each-instruction instruction-stream
      (lambda (offset instruction)
	(disassembler/write-instruction
	 symbol-table
	 offset
	 (lambda () (display-instruction offset instruction)))))))

(define (disassembler/for-each-instruction instruction-stream procedure)
  (let loop ((instruction-stream instruction-stream))
    (if (not (disassembler/instructions/null? instruction-stream))
	(disassembler/instructions/read instruction-stream
	  (lambda (offset instruction instruction-stream)
	    (procedure offset instruction)
	    (loop (instruction-stream)))))))

(define (disassembler/write-constants-block block symbol-table)
  (fluid-let ((*unparser-radix* 16))
    (let ((end (system-vector-length block)))
      (let loop ((index (compiled-code-block/constants-start block)))
	(cond ((not (< index end)) 'DONE)
	      ((object-type?
		(let-syntax ((ucode-type
			      (macro (name) (microcode-type name))))
		  (ucode-type linkage-section))
		(system-vector-ref block index))
	       (loop (disassembler/write-linkage-section block
							 symbol-table
							 index)))
	      ((object-type?
		(let-syntax ((ucode-type
			      (macro (name) (microcode-type name))))
		  (ucode-type manifest-closure))
		(system-vector-ref block index))
	       (loop (disassembler/write-manifest-closure-pattern block
								  symbol-table
								  index)))
	      (else
	       (disassembler/write-instruction
		symbol-table
		(compiled-code-block/index->offset index)
		(lambda ()
		  (write-constant block
				  symbol-table
				  (system-vector-ref block index))))
	       (loop (1+ index))))))))

(define (write-constant block symbol-table constant)
  (write-string (cdr (write-to-string constant 60)))
  (cond ((lambda? constant)
	 (let ((expression (lambda-body constant)))
	   (if (and (compiled-code-address? expression)
		    (eq? (compiled-code-address->block expression) block))
	       (begin
		 (write-string "  (")
		 (let ((offset (compiled-code-address->offset expression)))
		   (let ((label
			  (disassembler/lookup-symbol symbol-table offset)))
		     (if label
			 (write-string label)
			 (write offset))))
		 (write-string ")")))))
	((compiled-code-address? constant)
	 (write-string "  (offset ")
	 (write (compiled-code-address->offset constant))
	 (write-string " in ")
	 (write (compiled-code-address->block constant))
	 (write-string ")"))
	(else false)))

(define (disassembler/write-linkage-section block symbol-table index)
  (let* ((field (object-datum (system-vector-ref block index)))
	 (descriptor (integer-divide field #x10000)))
    (let ((kind (integer-divide-quotient descriptor))
	  (length (integer-divide-remainder descriptor)))

      (define (write-caches offset size writer)
	(let loop ((index (1+ (+ offset index)))
		   (how-many (quotient (- length offset) size)))
	  (if (zero? how-many)
	      'DONE
	      (begin
		(disassembler/write-instruction
		 symbol-table
		 (compiled-code-block/index->offset index)
		 (lambda ()
		   (writer block index)))
		(loop (+ size index) (-1+ how-many))))))

      (disassembler/write-instruction
       symbol-table
       (compiled-code-block/index->offset index)
       (lambda ()
	 (write-string "#[LINKAGE-SECTION ")
	 (write field)
	 (write-string "]")))
       (case kind
	 ((0 3)
	  (write-caches
	   compiled-code-block/procedure-cache-offset
	   compiled-code-block/objects-per-procedure-cache
	   disassembler/write-procedure-cache))
	 ((1)
	  (write-caches
	   0
	   compiled-code-block/objects-per-variable-cache
	   (lambda (block index)
	     (disassembler/write-variable-cache "Reference" block index))))
	 ((2)
	  (write-caches
	   0
	   compiled-code-block/objects-per-variable-cache
	   (lambda (block index)
	     (disassembler/write-variable-cache "Assignment" block index))))
	 ((4)
	  (disassembler/write-instruction
	   symbol-table
	   (compiled-code-block/index->offset (1+ index))
	   (lambda ()
	     (write-string "Closure linkage cache"))))
	 (else
	  (error "disassembler/write-linkage-section: Unknown section kind"
		 kind)))
      (1+ (+ index length)))))

(define-integrable (variable-cache-name cache)
  ((ucode-primitive primitive-object-ref 2) cache 1))

(define (disassembler/write-variable-cache kind block index)
  (write-string kind)
  (write-string " cache to ")
  (write (variable-cache-name (disassembler/read-variable-cache block index))))

(define (disassembler/write-procedure-cache block index)
  (let ((result (disassembler/read-procedure-cache block index)))
    (write (vector-ref result 2))
    (write-string " argument procedure cache to ")
    (case (vector-ref result 0)
      ((COMPILED INTERPRETED)
       (write (vector-ref result 1)))
      ((VARIABLE)
       (write-string "variable ")
       (write (vector-ref result 1)))
      (else
       (error "disassembler/write-procedure-cache: Unknown cache kind"
	      (vector-ref result 0))))))

(define closure-entry-size 4)

(define (disassembler/write-manifest-closure-pattern block symbol-table index)
  (let* ((descriptor    (integer-divide (system-vector-ref block (+ index 1))
					#x10000))
	 (offset        (integer-divide-remainder descriptor))
	 (multiclosure? (= offset 0))
	 (closures      (if multiclosure?
			    (integer-divide-quotient descriptor)
			    1))
	 (pattern-len   (if multiclosure?
			    (+ 1 (* closures closure-entry-size))
			    closure-entry-size))
	 (closure-len   (object-datum (system-vector-ref block index)))
	 (free-vars     (- closure-len pattern-len)))
    (disassembler/write-instruction
     symbol-table
     (compiled-code-block/index->offset index)
     (lambda ()
       (write-string "#[MANIFEST-CLOSURE-PATTERN ")
       (write closure-len)
       (if multiclosure?
	   (begin (write-string " ")
		  (write closures)
		  (write-string "-closure")))
       (write-string " with ")
       (write free-vars)
       (write-string " free variable")
       (if (not (= free-vars 1))
	   (write-string "s"))
       (write-string "]")))
    (+ index pattern-len 1)))    

(define (disassembler/write-instruction symbol-table offset write-instruction)
  (if symbol-table
      (let ((label (dbg-labels/find-offset symbol-table offset)))
	(if label
	    (begin
	      (write-char #\Tab)
	      (write-string (dbg-label/name label))
	      (write-char #\:)
	      (newline)))))

  (if disassembler/write-addresses?
      (begin
	(write-string
	 (number->string (+ offset disassembler/base-address) 16))
	(write-char #\Tab)))
  
  (if disassembler/write-offsets?
      (begin
	(write-string (number->string offset 16))
	(write-char #\Tab)))

  (if symbol-table
      (write-string "    "))
  (write-instruction)
  (newline))

(let-syntax ((define-codes
	       (macro (start . names)
		 (define (loop names index assocs)
		   (if (null? names)
		       `((DEFINE CODE:COMPILER-XXX-ALIST ',assocs))
		       (loop (cdr names) (1+ index)
			     (cons (cons index (car names)) assocs))))
		 `(BEGIN ,@(loop names start '())))))
  ;; Copied from lapgen.scm
  (define-codes #x012
    primitive-apply primitive-lexpr-apply
    apply error lexpr-apply link
    interrupt-closure interrupt-dlink interrupt-procedure 
    interrupt-continuation interrupt-ic-procedure
    assignment-trap cache-reference-apply
    reference-trap safe-reference-trap unassigned?-trap
    -1+ &/ &= &> 1+ &< &- &* negative? &+ positive? zero?
    access lookup safe-lookup unassigned? unbound?
    set! define lookup-apply primitive-error
    quotient remainder modulo
    reflect-to-interface interrupt-continuation-2
    compiled-code-bkpt compiled-closure-bkpt
    new-interrupt-procedure))

(let-syntax ((define-hooks
	       (macro (start . names)
		 (define (loop names index assocs)
		   (if (null? names)
		       `((DEFINE HOOK:COMPILER-XXX-ALIST ',assocs))
		       (loop (cdr names) (+ 8 index)
			     (cons (cons index (car names)) assocs))))
		 `(BEGIN ,@(loop names start '())))))
  ;; Copied from lapgen.scm
  (define-hooks 100
    store-closure-code
    store-closure-entry			; newer version of store-closure-code.
    multiply-fixnum
    fixnum-quotient
    fixnum-remainder
    fixnum-lsh
    &+
    &-
    &*
    &/
    &=
    &<
    &>
    1+
    -1+
    zero?
    positive?
    negative?
    shortcircuit-apply
    shortcircuit-apply-1
    shortcircuit-apply-2
    shortcircuit-apply-3
    shortcircuit-apply-4
    shortcircuit-apply-5
    shortcircuit-apply-6
    shortcircuit-apply-7
    shortcircuit-apply-8
    stack-and-interrupt-check
    invoke-primitive
    vector-cons
    string-allocate
    floating-vector-cons
    flonum-sin
    flonum-cos
    flonum-tan
    flonum-asin
    flonum-acos
    flonum-atan
    flonum-exp
    flonum-log
    flonum-truncate
    flonum-ceiling
    flonum-floor
    flonum-atan2
    compiled-code-bkpt
    compiled-closure-bkpt
    copy-closure-pattern
    copy-multiclosure-pattern
    closure-entry-bkpt-hook
    interrupt-procedure/new
    interrupt-continuation/new
    interrupt-closure/new
    quotient
    remainder
    interpreter-call
    profile-count
    profile-count/2))

(define display-instruction
  (let ((prev-instruction '())
	(prev-prev-instruction '()))
    (lambda (offset instruction)

      (define (unannotated) (display instruction))

      (define (annotated)
	(let ((s (with-output-to-string (lambda() (display instruction)))))
	  (write-string s)
	  (write-string (make-string (max 1 (- 40 (string-length s))) #\Space))
	  (write-string ";")))

      (define (annotate-with-name name)
	(annotated)
	(write-string " ")
	(display name))

      (define (annotate-with-target address)
	(annotated)
	(write-string " ")
	(write-string (number->string address 16)))

      (define (match? pat obj)
	(or (eq? pat '?)
	    (and (eq? pat '?n) (number? obj))
	    (and (pair? pat) (pair? obj)
		 (match? (car pat) (car obj))
		 (match? (cdr pat) (cdr obj)))
	    (equal? pat obj)))

      (define (code?)
	(match? '(ble ? (offset ? 4 3)) instruction))
      (define (code-name)
	(let ((id.name (assoc (second (third instruction))
			      hook:compiler-xxx-alist)))
	  (and id.name
	       (cdr id.name))))

      (define (hook?)
	(and (or (equal? '(ble () (offset 0 4 3)) prev-instruction)
		 (equal? '(ble () (offset 12 4 3)) prev-instruction))
	     (match? '(ldi () ? 28) instruction)))
      (define (hook-name)
	(let ((id.name  (assoc (third instruction) code:compiler-xxx-alist)))
	  (and id.name
	       (cdr id.name))))
    
      (define (external-label?)
	(match? '(external-label . ?) instruction))

      (define (offset->address field adjustment)
	(remainder
	 (+ (+ offset disassembler/base-address) field adjustment)
	 #x100000000))
      (define (offset-targets)
	(let ((res
	       (map (lambda (@pco.n)
		      (offset->address (second @pco.n) 8))
		    (list-transform-positive instruction
		      (lambda (part) (and (pair? part)
					  (eq? (car part) '@pco)
					  (not (equal? (cadr part) 0))))))))
	  (if (null? res) #f res)))

      (define (special-offset-target)
	(cond ((and (match? '(bl () ? (@pco 0))      prev-instruction)
		    (match? '(? ? (offset ?n 0 ?) ?) instruction)
		    (eqv? (third prev-instruction) (fourth (third instruction))))	      
	       (offset->address (second (third instruction)) (+ 8 -4 3)))
	      ((match? '(uword () ?n) instruction)
	       (offset->address (third instruction) 3))
	      (else #f)))

      (cond ((and (code?) (code-name)) => annotate-with-name)
	    ((and (hook?) (hook-name)) => annotate-with-name)
	    ((external-label?)  (unannotated))
	    ((special-offset-target)   => annotate-with-target)
	    ((offset-targets)          => (lambda (x)
					    (annotate-with-target (car x))))
	    (else  	     (unannotated)))

      (set! prev-prev-instruction prev-instruction)
      (set! prev-instruction instruction))))