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

;;;; Disassembler: User Level
;;; package: (compiler disassembler)

(declare (usual-integrations))

;;; Flags that control disassembler behavior

(define disassembler/symbolize-output? #t)
(define disassembler/compiled-code-heuristics? #t)
(define disassembler/write-offsets? #t)
(define disassembler/write-addresses? #f)

;;;; Top level entries

(define (compiler:write-lap-file filename #!optional symbol-table?)
  (let ((pathname (->pathname filename))
	(symbol-table?
	 (if (default-object? symbol-table?) #t symbol-table?)))
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
    (let ((info (compiled-code-block/dbg-info block #t)))
      (fluid-let ((disassembler/write-offsets? #t)
		  (disassembler/write-addresses? #t)
		  (disassembler/base-address (object-datum block)))
	(newline)
	(newline)
	(disassembler/write-compiled-code-block block info)))))

(define (disassembler/write-compiled-code-block block info)
  (let ((symbol-table (and info (dbg-info/labels info))))
    (write-string "Disassembly of ")
    (write block)
    (call-with-values
	(lambda () (compiled-code-block/filename-and-index block))
      (lambda (filename index)
	(if filename
	    (begin
	      (write-string " (Block ")
	      (write index)
	      (write-string " in ")
	      (write-string filename)
	      (write-string ")")))))
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
  (disassembler/instructions #f start-address end-address #f))

(define (disassembler/write-instruction-stream symbol-table instruction-stream)
  (fluid-let ((*unparser-radix* 16))
    (disassembler/for-each-instruction instruction-stream
      (lambda (offset instruction comment)
	(disassembler/write-instruction
	 symbol-table
	 offset
	 (lambda ()
	   (if comment
	       (let ((s (with-output-to-string
			  (lambda () (display instruction)))))
		 (if (< (string-length s) 40)
		     (write-string (string-pad-right s 40))
		     (write-string s))
		 (write-string "; ")
		 (display comment))
	       (write instruction))))))))

(define (disassembler/for-each-instruction instruction-stream procedure)
  (let loop ((instruction-stream instruction-stream))
    (if (not (disassembler/instructions/null? instruction-stream))
	(disassembler/instructions/read instruction-stream
	  (lambda (offset instruction comment instruction-stream)
	    (procedure offset instruction comment)
	    (loop (instruction-stream)))))))

(define (disassembler/write-constants-block block symbol-table)
  (fluid-let ((*unparser-radix* 16))
    (let ((end (system-vector-length block)))
      (let loop ((index (compiled-code-block/marked-start block)))
	(cond ((not (< index end)) 'DONE)
	      ((object-type? (ucode-type linkage-section)
			     (system-vector-ref block index))
	       (loop (disassembler/write-linkage-section block
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
	(else #f)))

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
	 (write kind) (write-string " ") (write length)
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
    (write (-1+ (vector-ref result 2)))
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


;;;; i386/dassm2.scm

(define (disassembler/read-variable-cache block index)
  ((ucode-primitive primitive-object-set-type 2)
   (ucode-type quad)
   (system-vector-ref block index)))

(define (disassembler/read-procedure-cache block index)
  (fluid-let ((*block block))
    (let ((offset (compiled-code-block/index->offset index))
	  (word (system-vector-ref block index)))
      (if (object-type? (ucode-type fixnum) word)
	  ;; Unlinked.
	  (vector 'INTERPRETED (system-vector-ref block (1+ index)) word)
	  ;; Linked;
	  (let ((arity (read-unsigned-integer offset 16))
		(opcode (read-unsigned-integer (+ offset 2) 8))
		(operand (read-unsigned-integer (+ offset 3) 8)))
	    (if (and (= opcode svm1-inst:ijump-u8) (= operand 0))
		(vector 'COMPILED (read-procedure (+ offset 4)) arity)
		(error (string-append "disassembler/read-procedure-cache:"
				      " Unexpected instruction")
		       opcode operand)))))))

(define (disassembler/instructions block start-offset end-offset symbol-table)
  (let loop ((offset start-offset) (state (disassembler/initial-state)))
    (if (and end-offset (< offset end-offset))
	(disassemble-one-instruction
	 block offset symbol-table state
	 (lambda (offset* instruction comment state)
	   (make-instruction offset
			     instruction
			     comment
			     (lambda () (loop offset* state)))))
	'())))

(define-integrable (disassembler/instructions/null? obj)
  (null? obj))

(define (disassembler/instructions/read instruction-stream receiver)
  (receiver (instruction-offset instruction-stream)
	    (instruction-instruction instruction-stream)
	    (instruction-comment instruction-stream)
	    (instruction-next instruction-stream)))

(define-structure (instruction (type vector))
  (offset false read-only true)
  (instruction false read-only true)
  (comment false read-only true)
  (next false read-only true))

(define *block)
(define *current-offset)
(define *symbol-table)
(define *valid?)

(define (disassemble-one-instruction block offset symbol-table state receiver)
  (fluid-let ((*block block)
	      (*current-offset offset)
	      (*symbol-table symbol-table)
	      (*valid? true))
    (let ((start-offset *current-offset))
      ;; External label markers come in two parts:
      ;; An entry type descriptor, and a gc offset.
      (cond ((eq? state 'EXTERNAL-LABEL-OFFSET)
	     (let* ((word (next-unsigned-16-bit-word))
		    (label (find-label *current-offset)))
	       (receiver *current-offset
			 (if label
			     `(BLOCK-OFFSET ,label)
			     `(WORD U ,word))
			 #F
			 'INSTRUCTION)))
	    ((external-label-marker? symbol-table offset state)
	     (let ((word (next-unsigned-16-bit-word)))
	       (receiver *current-offset
			 `(ENTRY ,(decipher-entry-type-code word))
			 #F
			 'EXTERNAL-LABEL-OFFSET)))
	    (else
	     (let ((instruction (disassemble-next-instruction)))
	       (if (or *valid? (not (eq? 'BYTE (car instruction))))
		   (receiver *current-offset
			     instruction
			     (disassembler/guess-comment instruction state)
			     (disassembler/next-state instruction state))
		   (let ((inst `(BYTE U ,(caddr instruction))))
		     (receiver (1+ start-offset)
			       inst
			       #F
			       (disassembler/next-state inst state))))))))))

(define (decipher-entry-type-code code)
  (case code
    ((#xFFFE) 'EXPRESSION) ; aka CET_EXPRESSION via read_cc_entry_type
    ((#xFFFD) 'INTERNAL-PROCEDURE)	; aka CET_INTERNAL_PROCEDURE
    ((#xFFFC) 'INTERNAL-CONTINUATION)	; etc.
    ((#xFFFB) 'TRAMPOLINE)
    ((#xFFFA) 'RETURN-TO-INTERPRETER)
    ((#xFFFF #xFFF9 #xFFF8) code)	; invalid
    (else
     (if (fix:> code #x8000)
	 `(CONTINUATION ,(fix:- code #x8000))
	 (let ((n-required (fix:and code #x7F))
	       (n-optional (fix:and (fix:lsh code -7) #x7F))
	       (rest? (not (fix:zero? (fix:and code #x4000)))))
	   `(ARITY ,n-required ,n-optional ,rest?))))))

(define (disassembler/initial-state)
  'INSTRUCTION-NEXT)

(define (disassembler/next-state instruction state)
  state					; ignored
  (cond ((and disassembler/compiled-code-heuristics?
	      (memq (car instruction)
		    '(trap-trap-0
		      trap-trap-1-wr trap-trap-2-wr trap-trap-3-wr
		      jump-pcr-s8 jump-pcr-s16 jump-pcr-s32
		      jump-indir-wr)))
	 'EXTERNAL-LABEL)
	(else
	 'INSTRUCTION)))

(define (disassembler/guess-comment instruction state)
  state ; ignored
  (let loop ((insn instruction))
    (and (pair? insn)
	 (if (and (memq (car insn) '(PCR-S8 PCR-S16 PCR-S32))
		  (pair? (cdr insn))
		  (exact-integer? (cadr insn))
		  (not (zero? (cadr insn))))
	     (+ (cadr insn) *current-offset)
	     (or (loop (car insn))
		 (loop (cdr insn)))))))

(define (disassembler/lookup-symbol symbol-table offset)
  (and symbol-table
       (let ((label (dbg-labels/find-offset symbol-table offset)))
	 (and label 
	      (dbg-label/name label)))))

(define (external-label-marker? symbol-table offset state)
  (define-integrable (offset-word->offset word)
    (fix:quotient (bit-string->unsigned-integer word) 2))

  (if symbol-table
      (let ((label (dbg-labels/find-offset symbol-table (+ offset 4))))
	(and label
	     (dbg-label/external? label)))
      (and *block
	   (not (eq? state 'INSTRUCTION))
	   (let loop ((offset (+ offset 4)))
	     (let ((contents (read-bits (- offset 2) 16)))
	       (if (bit-string-clear! contents 0)
		   (let ((offset (- offset (offset-word->offset contents))))
		     (and (positive? offset)
			  (loop offset)))
		   (= offset (offset-word->offset contents))))))))

(define (read-procedure offset)
  (with-absolutely-no-interrupts
   (lambda ()
     ((ucode-primitive primitive-object-set-type 2)
      (ucode-type compiled-entry)
      ((ucode-primitive make-non-pointer-object 1)
       (read-unsigned-integer offset 32))))))

(define (read-unsigned-integer offset size)
  (bit-string->unsigned-integer (read-bits offset size)))

(define (read-signed-integer offset size)
  (bit-string->signed-integer (read-bits offset size)))

(define (read-bits offset size-in-bits)
  (let ((word (bit-string-allocate size-in-bits))
	(bit-offset (* offset addressing-granularity)))
    (with-absolutely-no-interrupts
     (lambda ()
       (if *block
	   (read-bits! *block bit-offset word)
	   (read-bits! offset 0 word))))
    word))

(define-integrable (make-unsigned-reader nbits)
  (let ((nbytes (fix:quotient nbits 8)))
    (lambda ()
      (let ((offset *current-offset))
	(let ((word (read-bits offset nbits)))
	  (set! *current-offset (+ offset nbytes))
	  (bit-string->unsigned-integer word))))))

(define next-unsigned-byte (make-unsigned-reader 8))
(define next-unsigned-16-bit-word (make-unsigned-reader 16))

(define (find-label offset)
  (and disassembler/symbolize-output?
       (disassembler/lookup-symbol *symbol-table offset)))

;; These are used by dassm1.scm

(define compiled-code-block/procedure-cache-offset 0)
(define compiled-code-block/objects-per-procedure-cache 2)
(define compiled-code-block/objects-per-variable-cache 1)

;; global variable used by runtime/udata.scm -- Moby yuck!

(set! compiled-code-block/bytes-per-object 4)


;;;; i386/dasm3.scm

(define (disassemble-next-instruction)
  (bind-condition-handler
   (list condition-type:coding-error)
   (lambda (condition)
     (continue))
   (lambda ()
     (decode-rt-coding-type 'instruction next-unsigned-byte))))