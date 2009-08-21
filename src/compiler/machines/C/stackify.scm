#| -*-Scheme-*-

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

;;;; C-output object pseudo-assembler that outputs a stack-based byte code
;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define debug? #f)

(define *record/1* #f)

(define (write-debug-record/1 depth prog)
  (set! *record/1*
	`(stack-depth ,depth
		      pc
		      ,(string-list/length (stackify-program/opcodes prog))
		      strtab-ptr
		      ,(string-list/length (stackify-program/strtab prog))))
  unspecific)

(define (write-debug-record/2 op)
  (write (append `(opcode ,(vector-ref *stackify/opcode-name* op)) *record/1*))
  (newline)
  (set! *record/1* #f)
  unspecific)


(define *stackify/table*)
(define *stackify/tag-base*)
(define *stackify/tag-next*)

(define-integrable (recnum? obj)
  (object-type? (object-type 3+4i) obj))

(define-integrable (ratnum? obj)
  (object-type? (object-type 3/4) obj))

(define-integrable (constant? obj)
  (object-type? (object-type #t) obj))

(define-integrable (fix:max x y)
  (if (fix:< x y)
      y
      x))

;; This version uses an eq hash table

(define-integrable (stackify/make-table)
  (make-eq-hash-table))

(define-integrable (stackify/table/lookup key)
  (hash-table/get *stackify/table* key #f))

(define-integrable (stackify/table/associate! key val)
  (hash-table/put! *stackify/table* key val))

;; An value in the table looks like
;;
;; #(max-count build-count recursive recursing?)
;;
;; where max-count is the number of times it is encountered in the walk
;; and build-count is the number of times it has been built
;; During walk, build-count remains at 0 while max-count increments
;; During build, max-count remains constant while build-count increments

(define (stackify/count/increment! obj)
  (let ((info (stackify/table/lookup obj)))
    (if (not info)
	(let ((new (vector 1 0 #f #t)))
	  (stackify/table/associate! obj new)
	  new)
	(let ((new (fix:+ (vector-ref info 0) 1)))
	  (if (vector-ref info 3)	;if recursing?, recursive
	      (vector-set! info 2 #t))
	  (vector-set! info 0 new)
	  info))))
    
(define (stackify/count/decrement! obj)
  (let ((info (stackify/table/lookup obj)))
    (cond ((not info)
	   (error "stackify/count/decrement!: Unknown object" obj))
	  ((fix:= (vector-ref info 1) (vector-ref info 0))
	   (error "stackify/count/decrement!: Seen too many times" obj))
	  (else
	   (let ((new (fix:+ (vector-ref info 1) 1)))
	     (vector-set! info 1 new)
	     info)))))

(define (stackify/shared? obj)
  (let ((info (stackify/table/lookup obj)))
    (and info
	 (not (fix:= (vector-ref info 0) 1))
	 (not (fix:= (vector-ref info 1) (vector-ref info 0))))))

(define (walk/trivial? obj)
  (or (boolean? obj)
      (null? obj)
      (reference-trap? obj)
      (constant? obj)
      (char? obj)
      (guaranteed-fixnum? obj)
      (stackify-escape? obj)))

;; Note: complex and ratnum are compound: Build components and then
;; aggregate

(define (walk/simple? obj)
  (or (exact-integer? obj)
      (flo:flonum? obj)
      (symbol? obj)
      (string? obj)
      (bit-string? obj)
      (scode/primitive-procedure? obj)
      ;; The runtime system needs the following
      (interpreter-return-address? obj)))

(define (walk/vector obj)
  (let ((len (vector-length obj)))
    (let loop ((i len))
      (and (fix:> i 0)
	   (let ((next-i (fix:- i 1)))
	     (walk (vector-ref obj next-i))
	     (loop next-i))))))

(define (walk/compound obj)
  (cond ((walk/simple? obj)
	 unspecific)
	((number? obj)
	 (cond ((recnum? obj)
		(walk (real-part obj))
		(walk (imag-part obj)))
	       ((ratnum? obj)
		(walk (numerator obj))
		(walk (denominator obj)))
	       (else
		(error "walk: Unknown kind of number"
		       obj))))
	((fake-compiled-block? obj)
	 ;; For now, fake compiled blocks are almost simple, as they
	 ;; are built separately.  We just need to remember them
	 ;; in walk order
	 (set! *subblocks* (cons obj *subblocks*))
	 (walk/vector (fake-block/proxy obj)))
	((pair? obj)
	 (walk (car obj))
	 (walk (cdr obj)))
	((%record? obj)
	 (let ((len (%record-length obj)))
	   (let loop ((i len))
	     (and (fix:> i 0)
		  (let ((next-i (fix:- i 1)))
		    (walk (%record-ref obj next-i))
		    (loop next-i))))))
	((vector? obj)
	 (walk/vector obj))
	(else
	 (error "walk/compound: Unknown kind of object" obj))))

(define (walk obj)
  (cond ((walk/trivial? obj) unspecific)
	((fake-compiled-procedure? obj)
	 ;; Pseudo-trivial: Walk the compiled code block instead
	 ;; so that it is encountered in walk order
	 (walk (fake-procedure/block obj)))
	(else
	 (let ((info (stackify/count/increment! obj)))
	   (and (fix:= (vector-ref info 0) 1)
		(begin
		  (walk/compound obj)
		  (vector-set! info 3 #f)
		  unspecific))))))

(define (regmap/empty)
  (list 'tag))

(define (regmap/lookup regmap obj)
  (let ((place (assq obj (cdr regmap))))
    (and place
	 (cdr place))))

;; These versions update regmap in place

(define (regmap/allocate regmap obj)
  ;; Returns <regmap place>
  (let ((place (assq obj (cdr regmap))))
    (if place
	(error "regmap/allocate: Doubly-allocated" regmap obj))
    (let ((place (assq #f (cdr regmap))))
      (cond (place
	     (set-car! place obj)
	     (values regmap (cdr place)))
	    ((null? (cdr regmap))
	     (set-cdr! regmap (list (cons obj 0)))
	     (values regmap 0))
	    (else
	     (let* ((last (cadr regmap))
		    (idx (fix:+ (cdr last) 1)))
	       (set-cdr! regmap (cons (cons obj idx) (cdr regmap)))
	       (values regmap idx)))))))

(define (regmap/forget regmap obj)
  (let ((place (assq obj (cdr regmap))))
    (if (not place)
	(error "regmap/forget: Not present" regmap obj))
    (set-car! place #f)
    regmap))

(define (regmap/max-entries regmap)
  (length (cdr regmap)))

;; Byte-coded back end

(define *string-list/quantum* 512)

(define-structure (string-list
		   (constructor string-list/make ())
		   (conc-name string-list/))
  (length 0)
  (pointer 0)
  (current (make-string *string-list/quantum*))
  (stack '()))

(define (string-list/add-byte! sl byte)
  (let ((ptr (string-list/pointer sl))
	(current (string-list/current sl))
	(length (string-list/length sl)))
    (if (fix:< ptr (string-length current))
	(begin
	  (vector-8b-set! current ptr byte)
	  (set-string-list/pointer! sl (fix:+ ptr 1))
	  (set-string-list/length! sl (fix:+ length 1))
	  sl)
	(let ((new (make-string *string-list/quantum*)))
	  (set-string-list/stack! sl
				  (cons (cons ptr current)
					(string-list/stack sl)))
	  (set-string-list/current! sl new)
	  (set-string-list/pointer! sl 0)
	  (string-list/add-byte! sl byte)))))

(define (%string-list/add-string! sl str)
  (let ((ptr (string-list/pointer sl))
	(current (string-list/current sl))
	(length (string-list/length sl))
	(str-len (string-length str)))
    (let ((new-ptr (fix:+ ptr str-len)))
      (cond ((not (fix:> new-ptr (string-length current)))
	     (substring-move! str 0 str-len current ptr)
	     (set-string-list/pointer! sl new-ptr)
	     (set-string-list/length! sl (fix:+ length str-len))
	     sl)
	    ((fix:= ptr 0)
	     (set-string-list/stack! sl
				     (cons (cons str-len str)
					   (string-list/stack sl)))
	     (set-string-list/length! sl (fix:+ length str-len))
	     sl)
	    (else
	     (let ((new (make-string *string-list/quantum*)))
	       (set-string-list/stack! sl
				       (cons (cons ptr current)
					     (string-list/stack sl)))
	       (set-string-list/current! sl new)
	       (set-string-list/pointer! sl 0)
	       (if (fix:< (fix:* 4 str-len) (fix:* 3 *string-list/quantum*))
		   (%string-list/add-string! sl str)
		   (begin
		     (set-string-list/stack! sl
					     (cons (cons str-len str)
						   (string-list/stack sl)))
		     (set-string-list/length! sl (fix:+ length str-len))
		     sl))))))))

;; We add 1 before encoding the value so that there
;; are no null characters in the encoding.
;; The decoder subtracts one from the decoded value.

(define (encode-nat nat)
  ;; result: <n-digits digits>
  (let loop ((length 0) (nat (+ nat 1)) (digits '()))
    (if (< nat 128)
	(values (fix:+ length 1) (reverse! (cons nat digits)))
	(loop (fix:+ length 1)
	      (quotient nat 128)
	      (cons (fix:+ 128 (remainder nat 128))
		    digits)))))

(define (string-list/add-nat! sl nat)
  (call-with-values
      (lambda ()
	(encode-nat nat))
    (lambda (n-digits digits)
      n-digits				; unused
      (let loop ((digits digits)
		 (sl sl))
	(if (null? digits)
	    sl
	    (loop (cdr digits)
		  (string-list/add-byte! sl (car digits))))))))

(define (string-list/add-string! sl str)
  (string-list/add-nat! sl (string-length str))
  (%string-list/add-string! sl str))

(define (string-list/write! result offset sl)
  (let loop ((stack (reverse (cons (cons (string-list/pointer sl)
					 (string-list/current sl))
				   (string-list/stack sl))))
	     (offset offset))
    (if (null? stack)
	offset
	(let ((comp (cdar stack))
	      (complen (caar stack)))
	  (substring-move! comp 0 complen result offset)
	  (loop (cdr stack)
		(fix:+ offset complen))))))

;; A program is a pair of string lists
;; The first string list is the opcode string list
;; The second string list is the string table

(define-structure (stackify-program
		   (constructor stackify-program/make ())
		   (conc-name stackify-program/))
  (opcodes (string-list/make))
  (strtab (string-list/make)))

(define (stackify/empty-program)
  (stackify-program/make))

(define (stackify/finalize-program prog sdepth rsize)
  (let ((header (string-list/make))
	(opcodes (stackify-program/opcodes prog))
	(strtab (stackify-program/strtab prog)))
    (string-list/add-nat! header sdepth)
    (string-list/add-nat! header rsize)
    (let ((oplen (string-list/length opcodes)))
      (string-list/add-nat! header oplen)
      (let* ((headlen (string-list/length header))
	     (preflen (fix:+ headlen oplen))
	     (totlen (fix:+ preflen (string-list/length strtab)))
	     (bytes (make-string totlen)))
	(let ((off (string-list/write! bytes 0 header)))
	  (if (not (fix:= off headlen))
	      (error "stackify/finalize-program Counter inconsistency 1")))
	(let ((off (string-list/write! bytes headlen opcodes)))
	  (if (not (fix:= off preflen))
	      (error "stackify/finalize-program Counter inconsistency 2")))
	(let ((off (string-list/write! bytes preflen strtab)))
	  (if (not (fix:= off totlen))
	      (error "stackify/finalize-program Counter inconsistency 3")))
	bytes))))

(define-integrable (build/push-opcode! opcode prog)
  (if debug?
      (write-debug-record/2 opcode))
  (string-list/add-byte! (stackify-program/opcodes prog) opcode)
  prog)

(define (build/single-opcode opcode prog)
  (build/push-opcode! opcode prog))

(define (build/natural opcode nat prog)
  (string-list/add-nat! (stackify-program/strtab prog) nat)
  (build/push-opcode! opcode prog))

(define (build/push-nat nat prog)
  (string-list/add-nat! (stackify-program/strtab prog) nat)
  prog)

(define (build/string opcode str prog)
  (string-list/add-string! (stackify-program/strtab prog) str)
  (build/push-opcode! opcode prog))

;; Push a trivial non-pointer object

(define (build/trivial obj prog)
  (cond ((eq? obj #f)
	 (build/single-opcode stackify-opcode/push-false prog))
	((eq? obj #t)
	 (build/single-opcode stackify-opcode/push-true prog))
	((eq? obj '())
	 (build/single-opcode stackify-opcode/push-nil prog))
	((reference-trap? obj)
	 (if (not (unassigned-reference-trap? obj))
	     (error "build/trivial: Can't build reference trap" obj))
	 (build/single-opcode stackify-opcode/push-unassigned prog))
	((constant? obj)
	 (build/natural stackify-opcode/push-constant
			(object-datum obj)
			prog))
	((char? obj)
	 (build/natural stackify-opcode/push-char
			(char-code obj)
			(build/push-nat (char-bits obj) prog)))
	((stackify-escape? obj)
	 (build/escape obj prog))
	((not (guaranteed-fixnum? obj))
	 (error "build/trivial: Not trivial" obj))
	((fix:< obj 0)
	 (if (fix:= obj -1)
	     (build/single-opcode stackify-opcode/push--1 prog)
	     (build/natural stackify-opcode/push--fixnum
			    (fix:- 0 obj)
			    prog)))
	((fix:< obj (vector-length stackify/fast-fixnum-opcodes))
	 (build/single-opcode (vector-ref stackify/fast-fixnum-opcodes obj)
			      prog))
	(else
	 (build/natural stackify-opcode/push-+fixnum
			obj
			prog))))

(define (build/escape obj prog)
  (let ((kind (stackify-escape/kind obj))
	(contents (stackify-escape/contents obj)))
    (case kind
      ((arity)
       (build/natural stackify-opcode/push-ulong
		      contents
		      prog))
      ((label-descriptor)
       (let ((code-word (car contents))
	     (offset (cdr contents)))
	 (build/natural stackify-opcode/push-label-descriptor
			code-word
			(build/push-nat offset prog))))
      ((label-relative-entry)
       (build/natural stackify-opcode/push-label-entry
		      (+ contents *stackify/tag-base*)
		      prog))
      ((nm-header)
       (build/natural stackify-opcode/push-nm-header
		      contents
		      prog))
      ((operator-linkage-kind)
       (build/natural stackify-opcode/push-linkage-header-operator
		      contents
		      prog))
      ((global-operator-linkage-kind)
       (build/natural stackify-opcode/push-linkage-header-global
		      contents
		      prog))
      ((assignment-linkage-kind)
       (build/natural stackify-opcode/push-linkage-header-assignment
		      contents
		      prog))
      ((reference-linkage-kind)
       (build/natural stackify-opcode/push-linkage-header-reference
		      contents
		      prog))
      (else
       (error "build/escape: Unknown kind" kind)))))

;; Pop two elements off the stack, make a pair of the type of obj

(define (build/simple-pair obj prog)
  (build/single-opcode
   (cond ((recnum? obj)
	  stackify-opcode/push-cons-recnum)
	 ((ratnum? obj)
	  stackify-opcode/push-cons-ratnum)
	 (else
	  (error "build/simple-pair: Unexpected object" obj)))
   prog))

;; Push a simple pointer object

(define (build/simple obj prog)
  (cond ((string? obj)
	 (build/string stackify-opcode/push-string obj prog))
	((symbol? obj)
	 (build/string
	  (if (uninterned-symbol? obj)
	      stackify-opcode/push-uninterned-symbol
	      stackify-opcode/push-symbol)
	  (symbol-name obj)
	  prog))
	((bit-string? obj)
	 (build/string stackify-opcode/push-bit-string
		       (reverse-string
			(number->string
			 (bit-string->unsigned-integer obj)
			 16))
		       (build/push-nat (bit-string-length obj) prog)))
	((scode/primitive-procedure? obj)
	 (let ((arity (primitive-procedure-arity obj))
	       (name (symbol-name (primitive-procedure-name obj))))
	   (cond ((fix:< arity 0)
		  (build/string stackify-opcode/push-primitive-lexpr
				name
				prog))
		 ((fix:< arity (vector-length stackify/fast-primitive-opcodes))
		  (build/string
		   (vector-ref stackify/fast-primitive-opcodes arity)
		   name
		   prog))
		 (else
		  (build/string
		   stackify-opcode/push-primitive
		   name
		   (build/push-nat arity prog))))))
	((exact-integer? obj)
	 (let ((val (if (< obj 0) (- 0 obj) obj))
	       (op (if (< obj 0)
		       stackify-opcode/push--integer
		       stackify-opcode/push-+integer)))
	   (build/string op
			 (number->string val 16)
			 prog)))
	((flo:flonum? obj)
	 (build/string stackify-opcode/push-flonum
		       (number->string obj)
		       prog))
	;; The runtime system needs the following
	((interpreter-return-address? obj)
	 (build/natural
	  stackify-opcode/push-return-code
	  (object-datum obj)
	  prog))
	(else
	 (error "build/simple: Not simple" obj))))

(define (build/fast index opcodes generic prog)
  (if (and (fix:< index (vector-length opcodes))
	   (vector-ref opcodes index))
      (build/single-opcode (vector-ref opcodes index)
			   prog)
      (build/natural generic
		     index
		     prog)))

(define (build/lookup obj prog regmap)
  (let ((place (regmap/lookup regmap obj)))
    (if (not place)
	(error "build/lookup: Can't find" obj))
    (build/fast place
		stackify/fast-lookup-opcodes
		stackify-opcode/push-lookup
		prog)))

;; Store top of stack to a regmap location, don't pop

(define (build/store place prog)
  (build/fast place
	      stackify/fast-store-opcodes
	      stackify-opcode/store
	      prog))

;; Pop N+1 elements off the stack and cons* them, push result
;; TOS is last cdr

(define (build/cons* n prog)
  (build/fast n
	      stackify/fast-cons*-opcodes
	      stackify-opcode/push-cons*
	      prog))

;; Build '(#f #f)

(define (build/empty-cons prog)
  (build/single-opcode stackify-opcode/push-empty-cons prog))

;; Pop top of stack and write as new car of pair at new top of stack

(define (build/set-car prog)
  (build/single-opcode stackify-opcode/pop-and-set-car prog))

;; Pop top of stack and write as new cdr of pair at new top of stack

(define (build/set-cdr prog)
  (build/single-opcode stackify-opcode/pop-and-set-cdr prog))

;; Pop N elements off the stack, and make an N-element vector with them
;; TOS is element 0

(define (build/make-vector n prog)
  (build/fast n
	      stackify/fast-vector-opcodes
	      stackify-opcode/push-vector
	      prog))

;; Push an N-element vector full of #f on the stack

(define (build/make-empty-vector n prog)
  (build/natural stackify-opcode/push-empty-vector
		 n
		 prog))

;; Pop top of stack and write at element N of vector at new top of stack

(define (build/vector-set n prog)
  (build/fast n
	      stackify/fast-vector-set-opcodes
	      stackify-opcode/pop-and-vector-set
	      prog))

;; Pop N elements off the stack, and make an N-element record with them
;; TOS is element 0

(define (build/make-record n prog)
  (build/fast n
	      stackify/fast-record-opcodes
	      stackify-opcode/push-record
	      prog))

;; Push an N-element record full of #f on the stack

(define (build/make-empty-record n prog)
  (build/natural stackify-opcode/push-empty-record
		 n
		 prog))

;; Pop top of stack and write at element N of record at new top of stack

(define (build/record-set n prog)
  (build/fast n
	      stackify/fast-record-set-opcodes
	      stackify-opcode/pop-and-record-set
	      prog))

(define (build/vector obj prog curr-depth max-depth regmap)
  (let ((len (vector-length obj)))
    (let loop ((i len)
	       (prog prog)
	       (curr-depth* curr-depth)
	       (max-depth max-depth)
	       (regmap regmap))
      (if (not (fix:> i 0))
	  (values (build/make-vector len prog)
		  (fix:max (fix:+ 1 curr-depth) max-depth)
		  regmap)
	  (let ((next-i (fix:- i 1)))
	    (call-with-values
		(lambda ()
		  (build (vector-ref obj next-i)
			 prog
			 curr-depth*
			 max-depth
			 regmap))
	      (lambda (prog* max-depth* regmap*)
		(loop next-i
		      prog*
		      (fix:+ curr-depth* 1)
		      max-depth*
		      regmap*))))))))
  

(define (build/unique obj prog curr-depth max-depth regmap)
  ;; Returns <program max-depth regmap>
  (define (simple-pair sel1 sel2)
    (call-with-values
	(lambda ()
	  (build (sel1 obj) prog curr-depth max-depth regmap))
      (lambda (prog* max-depth* regmap*)
	(call-with-values
	    (lambda ()
	      (build (sel2 obj) prog* (fix:+ curr-depth 1) max-depth* regmap*))
	  (lambda (prog** max-depth** regmap**)
	    (values (build/simple-pair obj prog**)
		    (fix:max (fix:+ 2 curr-depth) max-depth**)
		    regmap**))))))

  (cond ((walk/simple? obj)
	 (values (build/simple obj prog)
		 (fix:max (fix:+ curr-depth 1) max-depth)
		 regmap))
	((number? obj)
	 (cond ((recnum? obj)
		(simple-pair real-part imag-part))
	       ((ratnum? obj)
		(simple-pair numerator denominator))
	       (else
		(error "build/unique: Unknown kind of number" obj))))
	((fake-compiled-block? obj)
	 (call-with-values
	     (lambda ()
	       (fluid-let ((*stackify/tag-base* *stackify/tag-next*))
		 (set! *stackify/tag-next*
		       (+ *stackify/tag-next*
			  (fake-block/ntags obj)))
		 (build/vector (fake-block/proxy obj)
			       prog curr-depth max-depth regmap)))
	   (lambda (prog* max-depth* regmap*)
	     (values
	      (build/single-opcode stackify-opcode/retag-cc-block
				   prog*)
	      max-depth*
	      regmap*))))
	((pair? obj)
	 (let loop ((n 0)
		    (obj obj)
		    (prog prog)
		    (curr-depth curr-depth)
		    (max-depth max-depth)
		    (regmap regmap))
	   (call-with-values
	       (lambda ()
		 (build (car obj) prog curr-depth max-depth regmap))
	     (lambda (prog* max-depth* regmap*)
	       (let ((next (cdr obj)))
		 (if (or (not (pair? next))
			 (stackify/shared? next))
		     (call-with-values
			 (lambda ()
			   (build next prog* (fix:+ curr-depth 1)
				  max-depth* regmap*))
		       (lambda (prog** max-depth** regmap**)
			 (values (build/cons* n prog**)
				 (fix:max (fix:+ 2 curr-depth) max-depth**)
				 regmap**)))
		     (begin
		       (stackify/count/decrement! next)
		       (loop (fix:+ n 1)
			     next
			     prog*
			     (fix:+ curr-depth 1)
			     max-depth*
			     regmap*))))))))
	((%record? obj)
	 (let ((len (%record-length obj)))
	   (let loop ((i len)
		      (prog prog)
		      (curr-depth* curr-depth)
		      (max-depth max-depth)
		      (regmap regmap))
	     (if (not (fix:> i 0))
		 (values (build/make-record len prog)
			 (fix:max (fix:+ 1 curr-depth) max-depth)
			 regmap)
		 (let ((next-i (fix:- i 1)))
		   (call-with-values
		       (lambda ()
			 (build (%record-ref obj next-i)
				prog
				curr-depth*
				max-depth
				regmap))
		     (lambda (prog* max-depth* regmap*)
		       (loop next-i
			     prog*
			     (fix:+ curr-depth* 1)
			     max-depth*
			     regmap*))))))))
	((vector? obj)
	 (build/vector obj prog curr-depth max-depth regmap))
	(else
	 (error "build/unique: Unknown kind of object" obj))))

(define (build/cyclic obj prog curr-depth max-depth regmap)
  ;; Outer reference to cyclic object
  ;; Returns <program max-depth regmap>
  (call-with-values
      (lambda ()
	(regmap/allocate regmap obj))
    (lambda (regmap* place)
      (cond ((or (walk/simple? obj)
		 (number? obj)
		 (fake-compiled-block? obj))
	     (error "build/cyclic: Cyclic what?" obj))
	    ((pair? obj)
	     (call-with-values
		 (lambda ()
		   (build (car obj)
			  (build/store place (build/empty-cons prog))
			  (fix:+ curr-depth 1)
			  max-depth regmap*))
	       (lambda (prog* max-depth* regmap**)
		 (call-with-values
		     (lambda ()
		       (build (cdr obj)
			      (build/set-car prog*)
			      (fix:+ curr-depth 1)
			      max-depth* regmap**))
		   (lambda (prog** max-depth** regmap***)
		     (values (build/set-cdr prog**)
			     (fix:max (fix:+ curr-depth 1) max-depth**)
			     regmap***))))))
	    ((%record? obj)
	     (let ((len (%record-length obj))
		   (curr-depth (fix:+ curr-depth 1)))
	       (let loop ((i len)
			  (prog (build/store
				 place
				 (build/make-empty-record len prog)))
			  (max-depth max-depth)
			  (regmap regmap*))
		 (if (not (fix:> i 0))
		     (values prog
			     (fix:max curr-depth max-depth)
			     regmap)
		     (let ((next-i (fix:- i 1)))
		       (call-with-values
			   (lambda ()
			     (build (%record-ref obj next-i)
				    prog
				    curr-depth
				    max-depth
				    regmap))
			 (lambda (prog* max-depth* regmap*)
			   (loop next-i
				 (build/record-set next-i prog*)
				 max-depth*
				 regmap*))))))))
	    ((vector? obj)
	     (let ((len (vector-length obj))
		   (curr-depth (fix:+ curr-depth 1)))
	       (let loop ((i len)
			  (prog (build/store
				 place
				 (build/make-empty-vector len prog)))
			  (max-depth max-depth)
			  (regmap regmap*))
		 (if (not (fix:> i 0))
		     (values prog
			     (fix:max curr-depth max-depth)
			     regmap)
		     (let ((next-i (fix:- i 1)))
		       (call-with-values
			   (lambda ()
			     (build (vector-ref obj next-i)
				    prog
				    curr-depth
				    max-depth
				    regmap))
			 (lambda (prog* max-depth* regmap*)
			   (loop next-i
				 (build/vector-set next-i prog*)
				 max-depth*
				 regmap*))))))))
	    (else
	     (error "build/cyclic: Unknown kind of object" obj))))))

(define (build/shared obj prog curr-depth max-depth regmap)
  ;; First-reference to shared non-cyclic object
  ;; Returns <program max-depth regmap>
  (call-with-values
      (lambda ()
	(build/unique obj prog curr-depth max-depth regmap))
    (lambda (prog* max-depth* regmap*)
      (call-with-values
	  (lambda ()
	    (regmap/allocate regmap* obj))
	(lambda (regmap** place)
	  (values (build/store place prog*)
		  max-depth*
		  regmap**))))))

(define (build obj prog curr-depth max-depth regmap)
  ;; Returns <program max-depth regmap>
  (if debug?
      (write-debug-record/1 curr-depth prog))
  (cond ((walk/trivial? obj)
	 (values (build/trivial obj prog)
		 (fix:max (fix:+ curr-depth 1) max-depth)
		 regmap))
	((fake-compiled-procedure? obj)
	 (with-values (lambda ()
			(build (fake-procedure/block obj)
			       prog
			       curr-depth
			       max-depth
			       regmap))
	   (lambda (prog* max-depth* regmap*)
	     (values
	      (build/natural stackify-opcode/cc-block-to-entry
			     (fake-procedure/label-value obj)
			     prog*)
	      max-depth*
	      regmap*))))
	(else
	 (let ((info (stackify/count/decrement! obj)))
	   (cond ((not (fix:= (vector-ref info 1) 1))
		  ;; Nth reference to a previously-built object
		  ;; Note: We must sequence regmap correctly...
		  (let ((prog* (build/lookup obj prog regmap)))
		    (values prog*
			    (fix:max (fix:+ curr-depth 1) max-depth)
			    (if (fix:= (vector-ref info 1) (vector-ref info 0))
				;; last reference to shared object
				(regmap/forget regmap obj)
				regmap))))
		 ((fix:= (vector-ref info 0) 1)
		  ;; Singleton reference
		  (build/unique obj prog curr-depth max-depth regmap))
		 ((vector-ref info 2)
		  ;; Outer reference to a cyclic structure
		  (build/cyclic obj prog curr-depth max-depth regmap))
		 (else
		  ;; First reference to shared non-cyclic object
		  (build/shared obj prog curr-depth max-depth regmap)))))))

;;; Stackify escapes for construction of compiled code blocks
;;  Note that fake-compiled-procedure and fake-compiled-blocks are
;;  also escapes, but they take more work.

(define-structure (stackify-escape
		   (constructor stackify-escape/make)
		   (conc-name stackify-escape/))
  (kind false read-only true)
  (contents false read-only true))
		   
(define (stackify/make-uuo-arity arity)
  (stackify-escape/make 'arity arity))

(define (stackify/make-label-descriptor code-word offset)
  (stackify-escape/make 'label-descriptor (cons code-word offset)))

(define (stackify/make-label-relative-entry tagno)
  (stackify-escape/make 'label-relative-entry tagno))

(define (stackify/make-nm-header length)
  (stackify-escape/make 'nm-header length))

(define (stackify/make-linkage-header kind count)
  (stackify-escape/make kind count))

;; These two are truly the identity procedure

(define (stackify/make-uuo-name name)
  name)

(define (stackify/make-var-ref-entry name)
  name)

;;; Top level

(define (stackify ntags obj)
  (define (core)
    (fluid-let ((*stackify/table* (stackify/make-table))
		(*stackify/tag-base* 0)
		(*stackify/tag-next* ntags))
      (walk obj)
      (call-with-values
	  (lambda ()
	    (build obj (stackify/empty-program) 0 0 (regmap/empty)))
	(lambda (prog max-depth regmap)
	  (stackify/finalize-program prog
				     max-depth
				     (regmap/max-entries regmap))))))
  (if (not debug?)
      (core)
      (begin
	(stackify/setup-debug!)
	(with-output-to-file debug? core))))
