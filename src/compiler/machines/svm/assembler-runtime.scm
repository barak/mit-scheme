#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; SVM assembler: runtime

(declare (usual-integrations))

;;;; Datatypes

;;; The model for an assembler consists of a group of "coding types",
;;; each of which represents a group of interchangeable assembler
;;; expressions that are distinguished by a unique code byte.  For
;;; example, the "instruction" coding type has a number of possible
;;; machine instructions, which are distinguished by an opcode byte.

(define-record-type <rt-coding-type>
    (%make-rt-coding-type name defns)
    rt-coding-type?
  (name rt-coding-type-name)
  (defns rt-coding-type-defns))

(define rt-coding-types '())

(define (make-rt-coding-type name defns)
  (if (find-matching-item rt-coding-types
	(lambda (rt-coding-type)
	  (eq? (rt-coding-type-name rt-coding-type) name)))
      (error "Coding type already exists" name)
      (set! rt-coding-types
	    (cons (%make-rt-coding-type name defns) rt-coding-types))))

;;; Each coding type has a number of definitions, each of which
;;; represents the code sequence associated with a particular value of
;;; the coding-type's code byte.  Each definition has a pattern (or
;;; template) that specifies the syntax of the sequence, and
;;; procedures to encode or decode the sequence.  There is also a
;;; unique name (a symbol) and a unique code (a byte value); the code
;;; is optional and may be #F.

(define-record-type <rt-defn>
    (%make-rt-defn name code pattern pvars encoder decoder)
    rt-defn?
  (name rt-defn-name)
  (code rt-defn-code)
  (pattern rt-defn-pattern)
  (pvars rt-defn-pvars)
  (encoder rt-defn-encoder)
  (decoder rt-defn-decoder))

(define (make-rt-defn name code pattern encoder decoder)
  (%make-rt-defn name code pattern (parse-pattern pattern) encoder decoder))

(define-record-type <rt-instance>
    (make-rt-instance defn pvals)
    rt-instance?
  (defn rt-instance-defn)
  (pvals rt-instance-pvals))

(define (rt-instance-pattern instance)
  (rt-defn-pattern (rt-instance-defn instance)))

(define (rt-instance-pvars instance)
  (rt-defn-pvars (rt-instance-defn instance)))

(define (rt-instance-encoder instance)
  (rt-defn-encoder (rt-instance-defn instance)))

(define (rt-instance-pval name instance)
  (let loop
      ((pvars (rt-instance-pvars instance))
       (pvals (rt-instance-pvals instance)))
    (if (not (pair? pvars))
	(error:bad-range-argument name 'RT-INSTANCE-PVAL))
    (if (not (pair? pvals))
	(error:bad-range-argument instance 'RT-INSTANCE-PVAL))
    (if (eq? (pvar-name (car pvars)) name)
	(car pvals)
	(loop (cdr pvars) (cdr pvals)))))

;;; The assembler maintains a symbol table that tracks the values of
;;; labels and defined constants.  The bindings in the symbol table
;;; are typed.

(define-record-type <symbol-binding>
    (make-symbol-binding name type value)
    symbol-binding?
  (name symbol-binding-name)
  (type symbol-binding-type)
  (value symbol-binding-value))

(define (make-typed-symbol-table)
  (make-strong-eq-hash-table))

(define (define-symbol name type value symbol-table)
  (hash-table/get symbol-table name (make-symbol-binding name type value)))

(define (lookup-symbol name symbol-table)
  (hash-table/get symbol-table name #f))

;;;; Top level

;;(define-import instructions (compiler lap-syntaxer))

(define (add-instruction! keyword assemblers)
  (hash-table/put! instructions keyword assemblers)
  keyword)

(define (add-instruction-assembler! keyword assembler)
  (let ((assemblers (hash-table/get instructions keyword #f)))
    (if assemblers
	(hash-table/put! instructions keyword (cons assembler assemblers))
	(hash-table/put! instructions keyword (list assembler)))))

(define (clear-instructions!)
  (hash-table/clear! instructions))

(define (init-assembler-instructions!)
  ;; Initialize the assembler's instruction database using the
  ;; patterns and encoders in the instruction coding type (the
  ;; "fixed-width instruction" assemblers) as well as special
  ;; assemblers that create variable-width-expressions.

  (clear-instructions!)

  ;; Create the fixed width instruction assemblers first.  They are
  ;; used to create the variable-width instruction assemblers.
  (for-each
    (lambda (keyword.defns)
      (add-instruction!
       (car keyword.defns)
       (map fixed-instruction-assembler
	    ;; Instruction-keywords reverses the definitions.
	    (reverse! (cdr keyword.defns)))))
    (instruction-keywords))

  ;; Create the variable width instruction assemblers.
  (add-instruction-assembler! 'STORE (store-assembler))
  (add-instruction-assembler! 'LOAD (load-assembler))
  (add-instruction-assembler! 'LOAD-ADDRESS (load-address-assembler))
  (add-instruction-assembler! 'JUMP (jump-assembler))
  (add-instruction-assembler! 'CONDITIONAL-JUMP (cjump1-assembler))
  (add-instruction-assembler! 'CONDITIONAL-JUMP (cjump2-assembler)))

(define (instruction-keywords)
  ;; An alist: instruction keyword X list of rt-defns.
  (let loop ((keywords '())
	     (defns (rt-coding-type-defns (rt-coding-type 'instruction))))
    (if (pair? defns)
	(let* ((defn (car defns))
	       (keyword (car (rt-defn-pattern defn)))
	       (entry (assq keyword keywords)))
	  (if entry
	      (begin
		(set-cdr! entry (cons defn (cdr entry)))
		(loop keywords (cdr defns)))
	      (loop (cons (list keyword defn) keywords)
		    (cdr defns))))
	keywords)))

(define (fixed-instruction-assembler defn)
  ;; Return a rule matching the exact instruction pattern in rt-DEFN.
  ;; It will match only appropriately-sized constants.
  (lambda (expression)	;without keyword
    (let ((pvals (match-pattern (cdr (rt-defn-pattern defn))
				expression
				(make-typed-symbol-table))))
      (and pvals
	   ;; The match result thunk.
	   (lambda ()
	     (let ((bytes '()))
	       ((rt-defn-encoder defn)
		(make-rt-instance defn pvals)
		(lambda (byte) (set! bytes (cons byte bytes))))
	       (map (lambda (byte)
		      (if (integer? byte)
			  (vector-ref bit-strings byte)
			  byte))
		    (reverse! bytes))))))))

(define bit-strings
  (let ((v (make-vector 256)))
    (let loop ((i 0))
      (if (fix:< i 256)
	  (begin
	    (vector-set! v i (unsigned-integer->bit-string 8 i))
	    (loop (fix:1+ i)))))
    v))

(define (fixed-instruction-width lap)
  (if (and (pair? lap) (pair? (car lap)) (null? (cdr lap)))
      (reduce-left + 0 (map bit-string-length
			    (lap:syntax-instruction (car lap))))
      (error "FIXED-INSTRUCTION-WIDTH: Multiple instructions in LAP" lap)))

(define (assemble-fixed-instruction width lap)
  (if (and (pair? lap) (pair? (car lap)) (null? (cdr lap)))
      (let* ((bits (lap:syntax-instruction (car lap)))
	     (len (reduce-left + 0 (map bit-string-length bits))))
	(if (not (= len width))
	    (error "Mis-sized fixed instruction" lap))
	bits)
      (error "ASSEMBLE-FIXED-INSTRUCTION: Multiple instructions in LAP" lap)))

(define (pc-relative-stats nbits make-sample)
  ;; Returns a list: the byte and bit widths for a class of
  ;; variable-width instructions (calculated by measuring a
  ;; representative assembled by MAKE-SAMPLE) and the range of offsets
  ;; encodable by each.
  ;; 
  ;; The variable-width expression refers to *PC*, which is the PC at
  ;; the beginning of this instruction.  The instruction will actually
  ;; use the PC at the beginning of the next instruction.  Thus the
  ;; actual range of the encoding is translated upward by this
  ;; instruction's width, and the actual offset translated back again
  ;; in the pc-relative-selector-handler.
  (let ((high (-1+ (expt 2 (-1+ nbits))))
	(low (- (expt 2 (-1+ nbits)))))
    (let* ((bit-width (fixed-instruction-width (make-sample high)))
	   (byte-width (/ bit-width 8)))
      (list nbits byte-width bit-width
	    (+ low byte-width) (+ high byte-width)))))

(define (pc-relative-selector stats make-inst)
  ;; Create a selector for a variable-width-expression using the stats
  ;; calculated earlier by pc-relative-stats.
  (let ((nbits (car stats))
	(byte-width (cadr stats))
	(bit-width (caddr stats)))
    (cons
     (named-lambda (pc-relative-selector-handler offset)
       (let ((operand (fix-offset (- offset byte-width) nbits)))
	 (assemble-fixed-instruction bit-width (make-inst operand))))
     (cddr stats))))

(define-integrable (fix-offset offset nbits)
  (if (or (and (= nbits 16)
	       (let ((low #x-80) (high #x7F))
		 (and (<= low offset) (<= offset high))))
	  (and (= nbits 32)
	       (let ((low #x-8000) (high #x7FFF))
		 (and (<= low offset) (<= offset high)))))
      (signed-integer->bit-string nbits offset)
      ;; Does not fit into a smaller number of bytes; no fixing necessary.
      offset))

(define (store-assembler)
  (let ((make-sample (lambda (offset)
		       (inst:store 'WORD rref:word-0
				   (ea:pc-relative offset)))))
    (let (( 8bit-stats (pc-relative-stats  8 make-sample))
	  (16bit-stats (pc-relative-stats 16 make-sample))
	  (32bit-stats (pc-relative-stats 32 make-sample)))
      (rule-matcher
       ((? scale) (? source) (PC-RELATIVE (- (? label) *PC*)))
       (let ((make-inst (lambda (offset)
			  (inst:store scale source
				      (ea:pc-relative offset)))))
	 `((VARIABLE-WIDTH-EXPRESSION
	    (- ,label *PC*)
	    ,(pc-relative-selector  8bit-stats make-inst)
	    ,(pc-relative-selector 16bit-stats make-inst)
	    ,(pc-relative-selector 32bit-stats make-inst))))))))

(define (load-assembler)
  (let ((make-sample (lambda (offset)
		       (inst:load 'WORD rref:word-0
				  (ea:pc-relative offset)))))
    (let (( 8bit-stats (pc-relative-stats  8 make-sample))
	  (16bit-stats (pc-relative-stats 16 make-sample))
	  (32bit-stats (pc-relative-stats 32 make-sample)))
      (rule-matcher
       ((? scale) (? target) (PC-RELATIVE (- (? label) *PC*)))
       (let ((make-inst (lambda (offset)
			  (inst:load scale target
				     (ea:pc-relative offset)))))
	 `((VARIABLE-WIDTH-EXPRESSION
	    (- ,label *PC*)
	    ,(pc-relative-selector  8bit-stats make-inst)
	    ,(pc-relative-selector 16bit-stats make-inst)
	    ,(pc-relative-selector 32bit-stats make-inst))))))))

(define (load-address-assembler)
  (let ((make-sample (lambda (offset)
		       (inst:load-address rref:word-0
					  (ea:pc-relative offset)))))
    (let (( 8bit-stats (pc-relative-stats  8 make-sample))
	  (16bit-stats (pc-relative-stats 16 make-sample))
	  (32bit-stats (pc-relative-stats 32 make-sample)))
      (rule-matcher
       ((? target) (PC-RELATIVE (- (? label) *PC*)))
       (let ((make-inst (lambda (offset)
			  (inst:load-address target
					     (ea:pc-relative offset)))))
	 `((VARIABLE-WIDTH-EXPRESSION
	    (- ,label *PC*)
	    ,(pc-relative-selector  8bit-stats make-inst)
	    ,(pc-relative-selector 16bit-stats make-inst)
	    ,(pc-relative-selector 32bit-stats make-inst))))))))

(define (jump-assembler)
  (let ((make-sample (lambda (offset)
		       (inst:jump (ea:pc-relative offset)))))
    (let (( 8bit-stats (pc-relative-stats  8 make-sample))
	  (16bit-stats (pc-relative-stats 16 make-sample))
	  (32bit-stats (pc-relative-stats 32 make-sample)))
      (rule-matcher
       ((PC-RELATIVE (- (? label) *PC*)))
       (let ((make-inst (lambda (offset)
			  (inst:jump (ea:pc-relative offset)))))
	 `((VARIABLE-WIDTH-EXPRESSION
	    (- ,label *PC*)
	    ,(pc-relative-selector  8bit-stats make-inst)
	    ,(pc-relative-selector 16bit-stats make-inst)
	    ,(pc-relative-selector 32bit-stats make-inst))))))))

(define (cjump2-assembler)
  (let ((make-sample (lambda (offset)
		       (inst:conditional-jump 'EQ rref:word-0 rref:word-1
					      (ea:pc-relative offset)))))
    (let (( 8bit-stats (pc-relative-stats  8 make-sample))
	  (16bit-stats (pc-relative-stats 16 make-sample))
	  (32bit-stats (pc-relative-stats 32 make-sample)))
      (rule-matcher
       ((? test) (? src1) (? src2) (PC-RELATIVE (- (? label) *PC*)))
       (let ((make-inst (lambda (offset)
			  (inst:conditional-jump test src1 src2
						 (ea:pc-relative offset)))))
	 `((VARIABLE-WIDTH-EXPRESSION
	    (- ,label *PC*)
	    ,(pc-relative-selector  8bit-stats make-inst)
	    ,(pc-relative-selector 16bit-stats make-inst)
	    ,(pc-relative-selector 32bit-stats make-inst))))))))

(define (cjump1-assembler)
  (let ((make-sample (lambda (offset)
		       (inst:conditional-jump 'EQ rref:word-0
					      (ea:pc-relative offset)))))
    (let (( 8bit-stats (pc-relative-stats  8 make-sample))
	  (16bit-stats (pc-relative-stats 16 make-sample))
	  (32bit-stats (pc-relative-stats 32 make-sample)))
      (rule-matcher
       ((? test) (? source) (PC-RELATIVE (- (? label) *PC*)))
       (let ((make-inst (lambda (offset)
			  (inst:conditional-jump test source
						 (ea:pc-relative offset)))))
	 `((VARIABLE-WIDTH-EXPRESSION
	    (- ,label *PC*)
	    ,(pc-relative-selector  8bit-stats make-inst)
	    ,(pc-relative-selector 16bit-stats make-inst)
	    ,(pc-relative-selector 32bit-stats make-inst))))))))

(define (match-rt-coding-type name expression symbol-table)
  (let loop ((defns (rt-coding-type-defns (rt-coding-type name))))
    (and (pair? defns)
	 (let ((pvals
		(match-pattern (rt-defn-pattern (car defns))
			       expression
			       symbol-table)))
	   (if pvals
	       (make-rt-instance (car defns) pvals)
	       (loop (cdr defns)))))))

(define (decode-rt-coding-type name read-byte)
  (let ((type (rt-coding-type name))
	(code (read-byte)))
    (let ((defn
	    (find-matching-item (rt-coding-type-defns type)
	     (lambda (defn)
	       (eqv? (rt-defn-code defn) code)))))
      (if defn
	  (cons (rt-defn-name defn)
		((rt-defn-decoder defn) read-byte))
	  (coding-error code type)))))

(define (rt-coding-type name)
  (or (find-matching-item rt-coding-types
	(lambda (rt-coding-type)
	  (eq? (rt-coding-type-name rt-coding-type) name)))
      (error:bad-range-argument name 'RT-CODING-TYPE)))

(define condition-type:coding-error
  (make-condition-type
   'rt-coding-error
   condition-type:error
   '(INVALID-CODE CODING-TYPE)
   (lambda (condition port)
     (write-string "Coding error: 0x" port)
     (write-string (number->string (access-condition condition 'INVALID-CODE)
				   16) port)
     (write-string " is not a valid " port)
     (write (access-condition condition 'CODING-TYPE) port)
     (write-string " rt-coding-type." port))))

(define coding-error
  (let ((signaller (condition-signaller condition-type:coding-error
					'(INVALID-CODE CODING-TYPE)
					standard-error-handler)))
    (named-lambda (coding-error code type)
      (call-with-current-continuation
       (lambda (continuation)
	 (with-restart 'CONTINUE "Continue with the next byte."
		       (lambda () (continuation `(WORD U ,code)))
		       values
		       (lambda () (signaller code type))))))))

;;;; Assembler Machine Dependencies

(define-integrable maximum-padding-length
  ;; Instructions can be any number of bytes long.
  ;; Thus the maximum padding is 7 bytes.
  56)

(define-integrable padding-string
  ;; Pad with zero, the distinguished invalid opcode.
  (unsigned-integer->bit-string 8 0))

(define-integrable block-offset-width
  ;; Block offsets are encoded words
  16)

(define maximum-block-offset
  (- (expt 2 (-1+ block-offset-width)) 1))

(define-integrable (block-offset->bit-string offset start?)
  (unsigned-integer->bit-string block-offset-width
				(+ (* 2 offset)
				   (if start? 0 1))))

;;; Machine dependent instruction order

(define (instruction-insert! bits block position receiver)
  (let ((l (bit-string-length bits)))
    (bit-substring-move-right! bits 0 l block position)
    (receiver (+ position l))))

(define-integrable (instruction-initial-position block)
  block					; ignored
  0)

(define-integrable instruction-append bit-string-append)

;;;; Patterns

(define (parse-pattern pattern)
  (reverse!
   (let ((lose (lambda () (error "Ill-formed pattern:" pattern))))
     (let loop ((pattern pattern) (pvars '()))
       (if (pair? pattern)
	   (if (eq? (car pattern) '_)
	       (begin
		 (if (not (and (pair? (cdr pattern))
				   (symbol? (cadr pattern))
				   (pair? (cddr pattern))
				   (symbol? (caddr pattern))
				   (null? (cdddr pattern))))
		     (lose))
		 (if (there-exists? pvars
		       (lambda (pv)
			 (eq? (pvar-name pv) (pvar-name pattern))))
		     ;; Don't add duplicate pvar.
		     pvars
		     (cons pattern pvars)))
	       (begin
		 (if (not (list? (cdr pattern)))
		     (lose))
		 (let traverse
		     ((items (cdr pattern))
		      (pvars (loop (car pattern) pvars)))
		   (if (pair? items)
		       (traverse (cdr items) (loop (car items) pvars))
		       pvars))))
	   (begin
	     (if (not (or (symbol? pattern)
			  (exact-integer? pattern)
			  (flo:flonum? pattern)
			  (boolean? pattern)
			  (null? pattern)))
		 (lose))
	     pvars))))))

(define (pvar? object) (and (pair? object) (eq? (car object) '_)))
(define-integrable (make-pvar name type) `(_ ,name ,type))
(define-integrable (pvar-name pv) (cadr pv))
(define-integrable (pvar-type pv) (caddr pv))

(define (match-pattern pattern expression symbol-table)
  (let ((pvals (match-pattern* pattern expression symbol-table)))
    (and pvals (reverse! pvals))))

(define (match-pattern* pattern expression symbol-table)
  (let loop ((pattern pattern) (expression expression) (pvals '()))
    (if (pair? pattern)
	(if (eq? (car pattern) '_)
	    (let ((pvt (lookup-pvar-type (pvar-type pattern))))
	      (if pvt
		  (and ((pvt-predicate pvt) expression)
		       (cons expression pvals))
		  (let ((instance
			 (match-rt-coding-type (pvar-type pattern)
					       expression
					       symbol-table)))
		    (and instance
			 (cons instance pvals)))))
	    (let traverse
		((patterns pattern)
		 (expressions expression)
		 (pvals pvals))
	      (if (pair? patterns)
		  (and (pair? expressions)
		       (let ((pvals
			      (loop (car patterns)
				    (car expressions)
				    pvals)))
			 (and pvals
			      (traverse (cdr patterns)
					(cdr expressions)
					pvals))))
		  (and (null? expressions)
		       pvals))))
	(and (eqv? pattern expression)
	     pvals))))

(define (rt-instance-expression instance)
  (let loop
      ((pattern (rt-instance-pattern instance))
       (pvals (rt-instance-pvals instance))
       (k
	(lambda (expression pvals)
	  pvals
	  expression)))
    (if (pair? pattern)
	(if (eq? (car pattern) '_)
	    (k (let ((pval (car pvals)))
		 (if (rt-instance? pval)
		     (rt-instance-expression pval)
		     pval))
	       (cdr pvals))
	    (let traverse ((patterns pattern) (pvals pvals) (expressions '()))
	      (if (pair? patterns)
		  (loop (car patterns)
			pvals
			(lambda (expression pvals)
			  (traverse (cdr patterns)
				    pvals
				    (cons expression expressions))))
		  (k (reverse! expressions) pvals))))
	(k pattern pvals))))

;;;; Registers

(define (word-register-reference? object)
  (and (register-reference? object)
       (fix:< (reference->register object) regnum:float-0)))

(define (float-register-reference? object)
  (and (register-reference? object)
       (let ((regnum (reference->register object)))
	 (and (fix:>= regnum regnum:float-0)
	      (fix:< regnum number-of-machine-registers)))))

;;;; Pattern-variable types

(define-record-type <pvt>
    (make-pvt name abbreviation sb-type predicate encoder decoder)
    pvt?
  (name pvt-name)
  (abbreviation pvt-abbreviation)
  (sb-type pvt-sb-type)
  (predicate pvt-predicate)
  (encoder pvt-encoder)
  (decoder pvt-decoder))

(define (lookup-pvar-type keyword)
  (hash-table/get pvar-type-table keyword #f))

(define (pvar-types)
  (hash-table/datum-list pvar-type-table))

(define pvar-type-table
  (make-strong-eq-hash-table))

(define (define-pvt name abbreviation sb-type predicate encoder decoder)
  (hash-table/put! pvar-type-table
		   name
		   (make-pvt name abbreviation sb-type
			     predicate encoder decoder))
  name)

(define (define-pvt-unsigned n-bytes)
  (let ((n-bits (* n-bytes 8)))
    (let ((limit (expt 2 n-bits)))
      (define-pvt (symbol 'UNSIGNED- n-bits) (symbol 'U n-bits) 'INTEGER
	(lambda (object)
	  (and (exact-nonnegative-integer? object)
	       (< object limit)))
	(symbol 'ENCODE-UNSIGNED-INTEGER- n-bits)
	(symbol 'DECODE-UNSIGNED-INTEGER- n-bits)))))

(define-pvt-unsigned 1)
(define-pvt-unsigned 2)
(define-pvt-unsigned 4)

(define (define-pvt-signed n-bytes)
  (let ((n-bits (* n-bytes 8)))
    (let ((limit (expt 2 (- n-bits 1))))
      (define-pvt (symbol 'SIGNED- n-bits) (symbol 'S n-bits) 'INTEGER
	(lambda (object)
	  (or (and (bit-string? object)
		   (= n-bits (bit-string-length object)))
	      (and (exact-integer? object)
		   (>= object (- limit))
		   (< object limit))))
	(symbol 'ENCODE-SIGNED-INTEGER- n-bits)
	(symbol 'DECODE-SIGNED-INTEGER- n-bits)))))

(define-pvt-signed 1)
(define-pvt-signed 2)
(define-pvt-signed 4)

(define-pvt 'TYPE-WORD 'TC 'INTEGER
  (lambda (object)
    (and (exact-nonnegative-integer? object) (< object #x40)))
  'ENCODE-UNSIGNED-INTEGER-8
  'DECODE-UNSIGNED-INTEGER-8)

(define-pvt 'FLOAT 'FLT 'FLOAT
  (lambda (object)
    (flo:flonum? object))
  'ENCODE-FLOAT
  'DECODE-FLOAT)

(define-pvt 'REGISTER 'R 'REGISTER
  register-reference?
  'ENCODE-RREF
  'DECODE-RREF)

(define-pvt 'WORD-REGISTER 'WR 'REGISTER
  word-register-reference?
  'ENCODE-RREF
  'DECODE-RREF)

(define-pvt 'FLOAT-REGISTER 'FR 'REGISTER
  float-register-reference?
  'ENCODE-RREF
  'DECODE-RREF)

;;;; Primitive codecs

(define (encode-unsigned-integer-8 n write-byte)
  (write-byte n))

(define (encode-unsigned-integer-16 n write-byte)
  (write-byte (remainder n #x100))
  (write-byte (quotient n #x100)))

(define (encode-unsigned-integer-32 n write-byte)
  (encode-unsigned-integer-16 (remainder n #x10000) write-byte)
  (encode-unsigned-integer-16 (quotient n #x10000) write-byte))

(define (encode-unsigned-integer-64 n write-byte)
  (encode-unsigned-integer-32 (remainder n #x100000000) write-byte)
  (encode-unsigned-integer-32 (quotient n #x100000000) write-byte))

(define (decode-unsigned-integer-8 read-byte)
  (read-byte))

(define (decode-unsigned-integer-16 read-byte)
  (let ((b0 (read-byte)))
    (fix:+ (fix:lsh (read-byte) 8) b0)))

(define (decode-unsigned-integer-32 read-byte)
  (let ((d0 (decode-unsigned-integer-16 read-byte)))
    (+ (* (decode-unsigned-integer-16 read-byte) #x10000) d0)))

(define (decode-unsigned-integer-64 read-byte)
  (let ((d0 (decode-unsigned-integer-32 read-byte)))
    (+ (* (decode-unsigned-integer-32 read-byte) #x100000000) d0)))

(define (encode-signed-integer-8 n write-byte)
  (if (bit-string? n)
      (write-bytes n 1 write-byte)
      (write-byte (if (fix:< n 0)
		      (fix:+ n #x100)
		      n))))

(define (encode-signed-integer-16 n write-byte)
  (if (bit-string? n)
      (write-bytes n 2 write-byte)
      (encode-unsigned-integer-16 (if (fix:< n 0)
				      (fix:+ n #x10000)
				      n)
				  write-byte)))

(define (encode-signed-integer-32 n write-byte)
  (if (bit-string? n)
      (write-bytes n 4 write-byte)
      (encode-unsigned-integer-32 (if (< n 0)
				      (+ n #x100000000)
				      n)
				  write-byte)))

(define (encode-signed-integer-64 n write-byte)
  (if (bit-string? n)
      (write-bytes n 8 write-byte)
      (encode-unsigned-integer-64 (if (< n 0)
				      (+ n #x10000000000000000)
				      n)
				  write-byte)))

(define (write-bytes bits bytes write-byte)
  (if (not (= (* bytes 8) (bit-string-length bits)))
      (error "Wrong number of bytes" bytes bits))
  (let loop ((start 0)
	     (end (bit-string-length bits)))
    (if (fix:< start end)
	(let ((next (fix:+ start 8)))
	  (write-byte (bit-string->unsigned-integer
		       (bit-substring bits start next)))
	  (loop next end)))))

(define (decode-signed-integer-8 read-byte)
  (let ((n (read-byte)))
    (if (fix:< n #x80)
	n
	(fix:- n #x100))))

(define (decode-signed-integer-16 read-byte)
  (let ((n (decode-unsigned-integer-16 read-byte)))
    (if (fix:< n #x8000)
	n
	(fix:- n #x10000))))

(define (decode-signed-integer-32 read-byte)
  (let ((n (decode-unsigned-integer-32 read-byte)))
    (if (< n #x80000000)
	n
	(- n #x100000000))))

(define (decode-signed-integer-64 read-byte)
  (let ((n (decode-unsigned-integer-64 read-byte)))
    (if (< n #x8000000000000000)
	n
	(- n #x10000000000000000))))

(define (encode-float x write-byte)
  (receive (n e) (float->integers x)
    (encode-signed-integer-64 n write-byte)
    (encode-signed-integer-16 e write-byte)))

(define (decode-float read-byte)
  (let ((n (decode-signed-integer-64 read-byte)))
    (integers->float n (decode-signed-integer-16 read-byte))))

(define (float->integers x)
  (let ((x>0
	 (let ((done
		(lambda (x e)
		  (let ((n (inexact->exact x)))
		    (if (not (exact-nonnegative-integer? n))
			(error "Flonum decode failed:" x))
		    (values n e)))))
	   (lambda (x)
	     (if (and (> x 1)
		      (= x (/ x 2)))
		 (error "Can't encode infinity:" x))
	     (let ((n (expt 2 flo:significand-digits-base-2)))
	       (let ((n/2 (/ n 2)))
		 (if (< x n/2)
		     (let loop ((x (* x 2)) (e -1))
		       (if (< x n/2)
			   (loop (* x 2) (- e 1))
			   (done x e)))
		     (let loop ((x x) (e 0))
		       (if (>= x n)
			   (loop (/ x 2) (+ e 1))
			   (done x e))))))))))
    (cond ((> x 0)
	   (x>0 x))
	  ((< x 0)
	   (receive (n e) (x>0 (- x))
	     (values (- n) e)))
	  ((= x 0)
	   (values 0 0))
	  (else
	   (error "Can't encode NaN:" x)))))

(define (integers->float n e)
  (if (= n 0)
      0.
      (let ((x (exact->inexact n)))
	(cond ((> e 0)
	       (let loop ((x (* x 2)) (e (- e 1)))
		 (if (> e 0)
		     (loop (* x 2) (- e 1))
		     x)))
	      ((< e 0)
	       (let loop ((x (/ x 2)) (e (+ e 1)))
		 (if (< e 0)
		     (loop (/ x 2) (+ e 1))
		     x)))
	      (else x)))))

(define (encode-rref rref write-byte)
  (let ((regnum (reference->register rref)))
    (encode-unsigned-integer-8
     (if (fix:< regnum regnum:float-0)
	 regnum
	 (fix:- regnum regnum:float-0))
     write-byte)))

(define (decode-rref read-byte)
  (let ((regnum (decode-unsigned-integer-8 read-byte)))
    (list 'R
	  (cond ((= regnum regnum:interpreter-register-block) 'IBLOCK)
		((= regnum regnum:stack-pointer) 'SP)
		((= regnum regnum:free-pointer) 'FREE)
		((= regnum regnum:value) 'VALUE)
		((= regnum regnum:dynamic-link) 'DLINK)
		(else regnum)))))