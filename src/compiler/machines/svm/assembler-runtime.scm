#| -*-Scheme-*-

$Id: 76e95c05c4790b731e4aba713646079f453548e1 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
    (make-rt-coding-type name defns)
    rt-coding-type?
  (name rt-coding-type-name)
  (defns rt-coding-type-defns))

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

(define (make-symbol-table)
  (make-eq-hash-table))

(define (define-symbol name type value symbol-table)
  (hash-table/get symbol-table name (make-symbol-binding name type value)))

(define (lookup-symbol name symbol-table)
  (hash-table/get symbol-table name #f))

;;;; Top level

;;; **** where are real top-level entries? ****

(define (match-rt-coding-type name expression coding-types symbol-table)
  (let loop ((defns (rt-coding-type-defns (rt-coding-type name coding-types))))
    (and (pair? defns)
	 (let ((pvals
		(match-pattern (rt-defn-pattern (car defns))
			       expression
			       coding-types
			       symbol-table)))
	   (if pvals
	       (make-rt-instance (car defns) pvals)
	       (loop (cdr defns)))))))

(define (decode-rt-coding-type name read-byte coding-types)
  (let ((type (rt-coding-type name coding-types))
	(code (read-byte)))
    (let ((rcd
	   (find-matching-item (rt-coding-type-defns type)
	     (lambda (rcd)
	       (eqv? (rt-defn-code rcd) code)))))
      (if (not rcd)
	  (error "No matching code:" code type))
      (make-rt-instance rcd ((rt-defn-decoder rcd) read-byte coding-types)))))

(define (rt-coding-type name coding-types)
  (or (find-matching-item coding-types
	(lambda (rt-coding-type)
	  (eq? (rt-coding-type-name rt-coding-type) name)))
      (error:bad-range-argument name 'RT-CODING-TYPE)))

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

(define (match-pattern pattern expression coding-types symbol-table)
  (let loop ((pattern pattern) (expression expression) (pvals '()))
    (if (pair? pattern)
	(if (eq? (car pattern) '_)
	    (let ((pvt (lookup-pvar-type (pvar-type pattern))))
	      (if pvt
		  (and (or ((pvt-predicate pvt) expression)
			   (eq? (match-symbolic-expression expression
							   symbol-table)
				(pvt-sb-type pvt)))
		       (cons expression pvals))
		  (let ((instance
			 (match-rt-coding-type (pvar-type pattern)
					       expression
					       coding-types
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

;;;; Instructions

(define-syntax define-inst
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL * SYMBOL) (cdr form))
	 (let ((tag (cadr form))
	       (params (cddr form)))
	   (let ((name (symbol-append 'INST: tag)))
	     `(BEGIN
		(DEFINE-INTEGRABLE (,name ,@params)
		  (LIST (LIST ',tag ,@params)))
		(DEFINE-INTEGRABLE (,(symbol-append name '?) INST)
		  (EQ? (CAR INST) ',tag)))))
	 (ill-formed-syntax form)))))

(define-syntax define-unary-operations
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(* SYMBOL) (cdr form))
	 `(BEGIN
	    ,@(let loop ((names (cdr form)))
		(if (pair? names)
		    (cons `(DEFINE-INST ,(car names) TARGET SOURCE)
			  (loop (cdr names)))
		    '())))
	 (ill-formed-syntax form)))))

(define-syntax define-generic-unary-operations
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(* SYMBOL) (cdr form))
	 `(BEGIN
	    ,@(let loop ((names (cdr form)))
		(if (pair? names)
		    (cons `(DEFINE-INST ,(car names) TYPE TARGET SOURCE)
			  (loop (cdr names)))
		    '())))
	 (ill-formed-syntax form)))))

(define-syntax define-binary-operations
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(* SYMBOL) (cdr form))
	 `(BEGIN
	    ,@(let loop ((names (cdr form)))
		(if (pair? names)
		    (cons `(DEFINE-INST ,(car names) TARGET SOURCE1 SOURCE2)
			  (loop (cdr names)))
		    '())))
	 (ill-formed-syntax form)))))

(define-syntax define-generic-binary-operations
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(* SYMBOL) (cdr form))
	 `(BEGIN
	    ,@(let loop ((names (cdr form)))
		(if (pair? names)
		    (cons `(DEFINE-INST ,(car names) TYPE
			     TARGET SOURCE1 SOURCE2)
			  (loop (cdr names)))
		    '())))
	 (ill-formed-syntax form)))))

(define-inst store size source address)
(define-inst load size target address)
(define-inst load-address target address)
(define-inst load-immediate target value)
(define-inst copy-block size size-type from to)

(define (load-immediate-operand? n)
  (or (and (exact-integer? n)
	   (<= #x80000000 n < #x100000000))
      (flo:flonum? n)))

;; TYPE and DATUM can be constants or registers; address is a register.
(define-inst load-pointer target type address)
(define-inst load-non-pointer target type datum)

(define-inst label label)
(define-inst entry-point label)

(define-inst jump address)

(define (inst:trap n . args)
  (list (cons* 'TRAP n args)))

(define (inst:conditional-jump condition source arg3 #!optional arg4)
  (list (cons* 'CONDITIONAL-JUMP
	       condition
	       source
	       arg3
	       (if (default-object? arg4) '() (list arg4)))))

(define (inst:conditional-jump? inst)
  (eq? (car inst) 'CONDITIONAL-JUMP))

;; N-ELTS is a constant or a register.
(define-inst flonum-header target n-elts)

(define-inst datum-u8 expression)
(define-inst datum-u16 expression)
(define-inst datum-u32 expression)
(define-inst datum-s8 expression)
(define-inst datum-s16 expression)
(define-inst datum-s32 expression)

(define-generic-unary-operations
  copy negate increment decrement abs)

(define-unary-operations
  object-type object-datum object-address
  fixnum->integer integer->fixnum address->integer integer->address
  not
  sqrt round ceiling floor truncate
  log exp cos sin tan acos asin atan
  flonum-align flonum-length)

(define-generic-binary-operations
  + - *)

(define-binary-operations
  quotient remainder
  lsh and andc or xor
  max-unsigned min-unsigned
  / atan2)

;;;; Memory addressing

(define-syntax define-ea
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL * SYMBOL) (cdr form))
	 (let ((tag (cadr form))
	       (params (cddr form)))
	   (let ((name (symbol-append 'EA: tag)))
	     `(BEGIN
		(DEFINE-INTEGRABLE (,name ,@params)
		  (INST-EA (,tag ,@(map (lambda (p) `(UNQUOTE p)) params))))
		(DEFINE-INTEGRABLE (,(symbol-append name '?) EA)
		  (AND (PAIR? EA)
		       (EQ? (CAR EA) ',tag))))))
	 (ill-formed-syntax form)))))

(define-ea indirect base)
(define-ea offset base offset scale)
(define-ea indexed base offset oscale index iscale)
(define-ea pre-decrement base scale)
(define-ea pre-increment base scale)
(define-ea post-decrement base scale)
(define-ea post-increment base scale)
(define-ea pc-relative offset)

(define (memory-reference? ea)
  (or (ea:indirect? ea)
      (ea:offset? ea)
      (ea:indexed? ea)
      (ea:pre-decrement? ea)
      (ea:pre-increment? ea)
      (ea:post-decrement? ea)
      (ea:post-increment? ea)
      (ea:pc-relative? ea)))

(define (ea:address label)
  (ea:pc-relative `(- ,label *PC*)))

(define (ea:stack-pop)
  (ea:post-increment regnum:stack-pointer 'WORD))

(define (ea:stack-push)
  (ea:pre-decrement regnum:stack-pointer 'WORD))

(define (ea:stack-ref index)
  (ea:offset regnum:stack-pointer index 'WORD))

(define (ea:alloc-word)
  (ea:post-increment regnum:free-pointer 'WORD))

(define (ea:alloc-byte)
  (ea:post-increment regnum:free-pointer 'BYTE))

(define (ea:alloc-float)
  (ea:post-increment regnum:free-pointer 'FLOAT))

;;;; Traps

(define-syntax define-traps
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(BEGIN
	,@(map (lambda (name)
		 `(DEFINE ,(symbol-append 'TRAP: name)
		    (INST:TRAP ',name)))
	       (cddr form))))))

(define-traps
  ;; This group doesn't return; don't push return address.
  apply lexpr-apply cache-reference-apply lookup-apply
  primitive-apply primitive-lexpr-apply
  error primitive-error
  &+ &- &* &/ 1+ -1+ quotient remainder modulo
  &= &< &> zero? positive? negative?

  ;; This group returns; push return address.
  link conditionally-serialize
  interrupt-closure interrupt-dlink interrupt-procedure
  interrupt-continuation interrupt-ic-procedure
  reference-trap safe-reference-trap assignment-trap unassigned?-trap
  lookup safe-lookup set! unassigned? define unbound? access)

;;;; Machine registers

(define-integrable number-of-machine-registers 512)
(define-integrable number-of-temporary-registers 512)

(define-syntax define-fixed-registers
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(* SYMBOL) (cdr form))
	 (let ((alist
		(let loop ((names (cdr form)) (index 0))
		  (if (pair? names)
		      (cons (cons (car names) index)
			    (loop (cdr names) (+ index 1)))
		      '()))))
	   `(BEGIN
	      ,@(map (lambda (p)
		       `(DEFINE-INTEGRABLE ,(symbol-append 'REGNUM: (car p))
			  ,(cdr p)))
		     alist)
	      (DEFINE FIXED-REGISTERS ',alist)))
	 (ill-formed-syntax form)))))

(define-fixed-registers
  stack-pointer
  dynamic-link
  free-pointer
  value
  environment)

(define-integrable regnum:float-0 256)

(define (any-register? object)
  (and (index-fixnum? object)
       (fix:< object number-of-machine-registers)
       object))

(define (word-register? object)
  (and (any-register? object)
       (fix:< object regnum:float-0)
       object))

(define (float-register? object)
  (and (any-register? object)
       (fix:>= object regnum:float-0)
       (fix:- object regnum:float-0)))

(define available-machine-registers
  (let loop ((r regnum:environment))
    (if (< r number-of-machine-registers)
	(cons r (loop (+ r 1)))
	'())))

;;;; Register references

(define register-reference
  (let ((references
	 (list->vector
	  (map (lambda (r) `(R ,r)) available-machine-registers))))
    (lambda (register)
      (guarantee-limited-index-fixnum register
				      number-of-machine-registers
				      'REGISTER-REFERENCE)
      (vector-ref references register))))

(define (register-reference? object)
  (and (pair? object)
       (eq? (car object) 'R)
       (pair? (cdr object))
       (index-fixnum? (cadr object))
       (fix:< (cadr object) number-of-machine-registers)
       (null? (cddr object))))

(define (word-register-reference? object)
  (and (pair? object)
       (eq? (car object) 'R)
       (pair? (cdr object))
       (index-fixnum? (cadr object))
       (fix:< (cadr object) regnum:float-0)
       (null? (cddr object))))

(define (float-register-reference? object)
  (and (pair? object)
       (eq? (car object) 'R)
       (pair? (cdr object))
       (index-fixnum? (cadr object))
       (fix:>= (cadr object) regnum:float-0)
       (fix:< (cadr object) number-of-machine-registers)
       (null? (cddr object))))

(define-guarantee register-reference "register reference")

(define (reference->register reference)
  (guarantee-register-reference reference 'REFERENCE->REGISTER)
  (cadr reference))

;;;; Symbolic expressions

(define (match-symbolic-expression expression symbol-table)
  (let loop ((expression expression))
    (cond ((symbol? expression)
	   (let ((binding (lookup-symbol expression symbol-table)))
	     (and binding
		  (symbol-binding-type binding))))
	  ((and (pair? expression)
		(symbol? (car expression))
		(list? (cdr expression))
		(lookup-symbolic-operator (car expression) #f))
	   => (lambda (op)
		(let ((types
		       (map (lambda (expression)
			      (cond ((se-integer? expression) 'INTEGER)
				    ((se-float? expression) 'FLOAT)
				    (else (loop expression))))
			    (cdr expression))))
		  (and (pair? types)
		       (for-all? types (lambda (type) type))
		       ((symbolic-operator-matcher op) types)))))
	  (else #f))))

(define (symbolic-pval? pval)
  (or (symbol? pval)
      (and (pair? pval)
	   (symbol? (car pval)))))

(define (sb-type:address? type) (eq? type 'ADDRESS))
(define (sb-type:integer? type) (eq? type 'INTEGER))
(define (sb-type:float? type) (eq? type 'FLOAT))

(define (define-symbolic-operator name matcher evaluator)
  (hash-table/put! symbolic-operators name (cons matcher evaluator)))

(define (symbolic-operator-matcher op)
  (car op))

(define (symbolic-operator-evaluator op)
  (cdr op))

(define (lookup-symbolic-operator name error?)
  (or (hash-table/get symbolic-operators name #f)
      (error:bad-range-argument name #f)))

(define symbolic-operators
  (make-eq-hash-table))

(define-integrable (se-integer? object)
  (exact-integer? object))

(define-integrable (se-float? object)
  (flo:flonum? object))

(define (se-address? object)
  ???)

(define (se-address:+ address offset)
  ???)

(define (se-address:- address1 address2)
  ???)

(define-symbolic-operator '+
  (lambda (types)
    (and (or (for-all? types sb-type:integer?)
	     (for-all? types sb-type:float?)
	     (and (sb-type:address? (car types))
		  (for-all? (cdr types) sb-type:integer?)))
	 (car types)))
  (lambda (pvals)
    (if (se-address? (car pvals))
	(se-address:+ (car pvals) (apply + (cdr pvals)))
	(apply + pvals))))

(define-symbolic-operator '-
  (lambda (types)
    (and (fix:= (length types) 2)
	 (let ((t1 (car types))
	       (t2 (cadr types)))
	   (cond ((and (sb-type:address? t1) (sb-type:integer? t2)) t1)
		 ((not (eq? t1 t2)) #f)
		 ((or (sb-type:integer? t1) (sb-type:float? t1)) t1)
		 ((sb-type:address? t1) 'INTEGER)
		 (else #f)))))
  (lambda (pvals)
    (let ((pv1 (car pvals))
	  (pv2 (cadr pvals)))
      (if (se-address? pv1)
	  (if (se-address? pv2)
	      (se-address:- pv1 pv2)
	      (se-address:+ pv1 (- pv2)))
	  (- pv1 pv2)))))

(define-symbolic-operator '*
  (lambda (types)
    (and (or (for-all? types sb-type:integer?)
	     (for-all? types sb-type:float?))
	 (car types)))
  (lambda (pvals)
    (apply * pvals)))

(define-symbolic-operator '/
  (lambda (types)
    (and (fix:= (length types) 2)
	 (let ((t1 (car types))
	       (t2 (cadr types)))
	   (and (eq? t1 t2)
		(or (sb-type:integer? t1)
		    (sb-type:float? t1))
		t1))))
  (lambda (pvals)
    (let ((pv1 (car pvals))
	  (pv2 (cadr pvals)))
      (if (exact-integer? pv1)
	  (quotient pv1 pv2)
	  (/ pv1 pv2)))))

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
  (make-eq-hash-table))

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
	  (and (exact-integer? object)
	       (>= object (- limit))
	       (< object limit)))
	(symbol 'ENCODE-SIGNED-INTEGER- n-bits)
	(symbol 'DECODE-SIGNED-INTEGER- n-bits)))))

(define-pvt-signed 1)
(define-pvt-signed 2)
(define-pvt-signed 4)

(define-pvt 'TYPE-WORD 'TC 'INTEGER
  (lambda (object)
    (and (se-integer? object)
	 (< object #x40)))
  'ENCODE-UNSIGNED-INTEGER-8
  'DECODE-UNSIGNED-INTEGER-8)

(define-pvt 'FLOAT 'FLT 'FLOAT
  (lambda (object)
    (se-float? object))
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
  (write-byte (remainder n #x100))
  (write-byte (quotient n #x100)))

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
  (write-byte (if (fix:< n 0)
		  (fix:+ n #x100)
		  n)))

(define (encode-signed-integer-16 n write-byte)
  (encode-unsigned-integer-16 (if (fix:< n 0)
				  (fix:+ n #x10000)
				  n)
			      write-byte))

(define (encode-signed-integer-32 n write-byte)
  (encode-unsigned-integer-32 (if (< n 0)
				  (+ n #x100000000)
				  n)
			      write-byte))

(define (encode-signed-integer-64 n write-byte)
  (encode-unsigned-integer-64 (if (< n 0)
				  (+ n #x10000000000000000)
				  n)
			      write-byte))

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
  (encode-unsigned-integer-8 (reference->register rref) write-byte))

(define (decode-rref read-byte)
  (register-reference (decode-unsigned-integer-8 read-byte)))