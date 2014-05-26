#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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
		(write-compiled-code-block (compiled-code-address->block object)
					   symbol-table?)
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
			  (write-compiled-code-block (car blocks) symbol-table?)
			  (if (not (null? (cdr blocks)))
			      (begin
				(write-char #\page)
				(newline))))))))))))))

(define disassembler/base-address)

(define (compiler:disassemble entry)
  (let ((block (compiled-entry/block entry)))
    (fluid-let ((disassembler/write-offsets? #t)
		(disassembler/write-addresses? #t)
		(disassembler/base-address (object-datum block)))
      (newline)
      (newline)
      (write-compiled-code-block block #t))))

(define (write-compiled-code-block block symbol-table?)
  (let ((cursor (block-cursor block symbol-table?)))
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
	      (write-string "):\n")))))
    (write-string "\nCode:\n")
    (write-instructions cursor)
    (write-string "\nConstants:\n")
    (write-constants cursor)
    (newline)))

(define-structure cursor
  (block false read-only true)
  (offset 0)
  (symbol-table false read-only true))

(define (block-cursor block symbol-table?)
  (let ((symbol-table
	 (and symbol-table?
	      (let ((info (compiled-code-block/dbg-info block symbol-table?)))
		(and info (dbg-info/labels info)))))
	(start (compiled-code-block/code-start block)))
    (make-cursor block start symbol-table)))

(define (write-instructions cursor)
  (fluid-let ((*unparser-radix* 16))
    (let ((end (compiled-code-block/code-end (cursor-block cursor))))
      (let loop ()
	(if (< (cursor-offset cursor) end)
	    (begin
	      (write-instruction cursor)
	      (loop)))))))

(define (write-instruction cursor)
  (write-offset cursor)
  (let* ((start (cursor-offset cursor))
	 (entry (cursor-external-entry cursor)))
    (if entry
	(begin
	  (write entry)
	  (newline))
	(let* ((start (cursor-offset cursor))
	       (instruction
		(ignore-errors
		 (lambda ()
		   (decode-rt-coding-type 'instruction
					  (lambda ()
					    (next-unsigned-byte cursor)))))))
	  (if (not (condition? instruction))
	      (begin
		(write instruction)
		(let ((comment (instruction-comment instruction cursor)))
		  (if comment
		      (begin
			(write-string " ; ")
			(write comment))))
		(newline))
	      (begin
		(set-cursor-offset! cursor start)
		(write `(BYTE U ,(next-unsigned-byte cursor)))
		(newline)))))))

(define (write-offset cursor)
  (let ((label (cursor-label cursor)))
    (if label
	(begin
	  (write-char #\Tab)
	  (write-string label)
	  (write-string ":\n"))))
  (let ((offset (cursor-offset cursor)))
    (if disassembler/write-addresses?
	(write-string
	 (number->string (+ offset disassembler/base-address) 16)))
    (if disassembler/write-offsets?
	(write-string (number->string offset 16)))
    (write-char #\Tab)))

(define (cursor-label cursor)
  (and disassembler/symbolize-output?
       (cursor-symbol-table cursor)
       (disassembler/lookup-symbol
	(cursor-symbol-table cursor)
	(cursor-offset cursor))))

(define (cursor-external-entry cursor)
  ;; External entries come in two parts: an entry type, and a gc offset.
  (let ((start (cursor-offset cursor)))
    (or (let ((entry-type (cursor-entry-type cursor)))
	  (and entry-type
	       (cursor-gc-offset? cursor)
	       `(ENTRY ,entry-type)))
	(begin
	  (set-cursor-offset! cursor start)
	  #f))))

(define (cursor-entry-type cursor)
  (decipher-entry-type-code (next-unsigned-16-bit-word cursor)))

(define (cursor-increment! cursor bytes)
  (set-cursor-offset! cursor (+ (cursor-offset cursor) bytes)))

(define (cursor-gc-offset? cursor)
  (let ((symbol-table (cursor-symbol-table cursor))
	(block (cursor-block cursor))
	(offset (cursor-offset cursor)))

    (define-integrable (offset-word->offset word)
      (fix:quotient (bit-string->unsigned-integer word) 2))

    (if symbol-table
	(let ((label (dbg-labels/find-offset symbol-table (+ offset 2))))
	  (and label
	       (dbg-label/external? label)
	       (begin
		 (set-cursor-offset! cursor (+ offset 2))
		 #t)))
	(and block
	     (let loop ((offset (+ offset 2)))
	       (let ((contents (read-bits block (- offset 2) 16)))
		 (if (bit-string-clear! contents 0)
		     (let* ((delta (offset-word->offset contents))
			    (offset (- offset delta)))
		       (and (positive? delta)
			    (positive? offset)
			    (loop offset)))
		     (= offset (offset-word->offset contents)))))
	     (begin
	       (set-cursor-offset! cursor (+ offset 2))
	       #t)))))

(define (write-constants cursor)
  (fluid-let ((*unparser-radix* 16))
    (let* ((block (cursor-block cursor))
	   (end (* address-units-per-object (system-vector-length block))))

      (set-cursor-offset! cursor (* address-units-per-object
				    (compiled-code-block/marked-start block)))
      (let loop ()
	(let ((offset (cursor-offset cursor)))
	  (if (< offset end)
	      (let ((object (system-vector-ref block (offset->index offset))))
		(if (object-type? (ucode-type linkage-section) object)
		    (write-linkage-section object cursor)
		    (begin
		      (write-offset cursor)
		      (write-constant object cursor)
		      (set-cursor-offset! cursor
					  (+ offset address-units-per-object))))
		(loop))))))))

(define-integrable (offset->index offset)
  (fix:quotient offset address-units-per-object))

(define (write-constant constant cursor)
  (write-string (cdr (write-to-string constant 60)))
  (cond ((lambda? constant)
	 (let ((expression (lambda-body constant)))
	   (if (and (compiled-code-address? expression)
		    (eq? (compiled-code-address->block expression)
			 (cursor-block cursor)))
	       (begin
		 (write-string "  (")
		 (let ((offset (compiled-code-address->offset expression)))
		   (let ((label
			  (disassembler/lookup-symbol
			   (cursor-symbol-table cursor) offset)))
		     (if label
			 (write-string label)
			 (write offset))))
		 (write-string ")")))))
	((compiled-code-address? constant)
	 (write-string "  (offset ")
	 (write (compiled-code-address->offset constant))
	 (write-string " in ")
	 (write (compiled-code-address->block constant))
	 (write-string ")")))
  (newline))

(define (write-linkage-section object cursor)
  (let* ((field (object-datum object))
	 (descriptor (integer-divide field #x10000)))
    (let ((kind (integer-divide-quotient descriptor))
	  (length (integer-divide-remainder descriptor)))

      (define (write-caches size writer)
	(let loop ((count (quotient length size)))
	  (if (< 0 count)
	      (begin
		(write-offset cursor)
		(writer (cursor-block cursor)
			(offset->index (cursor-offset cursor)))
		(newline)
		(cursor-increment! cursor (* size address-units-per-object))
		(loop (-1+ count))))))

      (write-offset cursor)
      (write-string "#[LINKAGE-SECTION ")
      (write (case kind
	       ((0) 'OPERATOR)
	       ((1) 'REFERENCE)
	       ((2) 'ASSIGNMENT)
	       ((3) 'GLOBAL-OPERATOR)
	       (else (error "Unknown kind of linkage section:" kind))))
      (write-string " ") (write length)
      (write-string "]\n")
      (cursor-increment! cursor address-units-per-object)
      (case kind
	((0 3)
	 (write-caches compiled-code-block/objects-per-procedure-cache
		       write-procedure-cache))
	((1 2)
	 (write-caches compiled-code-block/objects-per-variable-cache
		       (lambda (block index)
			 (write-variable-cache kind block index))))
	(else
	 (error "Unknown kind of linkage section:" kind))))))

(define (write-variable-cache kind block index)
  (let* ((cache ((ucode-primitive primitive-object-set-type 2)
		 (ucode-type hunk3)
		 (system-vector-ref block index)))
	 (refs (system-hunk3-cxr2 cache))
	 (entry
	  (find-matching-item
	   (case kind
	     ((1) (system-hunk3-cxr0 refs))
	     ((2) (system-hunk3-cxr1 refs))
	     (else (error "Not a kind of variable cache:" kind)))
	   (lambda (e)
	     (weak-assq block (cdr e))))))
    (write-string "variable cache for ")
    (if (pair? entry)
	(write (car entry))
	(write-string "... not found!"))))

(define (weak-assq obj alist)
  (let loop ((alist alist))
    (if (null? alist) #f
	(let* ((entry (car alist))
	       (key (weak-car entry)))
	  (if (eq? obj key) entry
	      (loop (cdr alist)))))))

(define (write-procedure-cache block index)
  (let ((result (read-procedure-cache block index)))
    (write (-1+ (vector-ref result 2)))
    (write-string " argument procedure cache to ")
    (case (vector-ref result 0)
      ((COMPILED INTERPRETED)
       (write (vector-ref result 1)))
      ((VARIABLE)
       (write-string "variable ")
       (write (vector-ref result 1)))
      (else
       (error "write-procedure-cache: Unknown cache kind"
	      (vector-ref result 0))))))

(define (read-procedure-cache block index)
  (let ((word (system-vector-ref block index)))
    (if (object-type? (ucode-type fixnum) word)
	;; Unlinked.
	(vector 'INTERPRETED (system-vector-ref block (1+ index)) word)
	;; Linked.
	(let ((offset (compiled-code-block/index->offset index)))
	  (let ((arity (read-unsigned-integer block offset 16))
		(opcode (read-unsigned-integer block (+ offset 2) 8))
		(operand (read-unsigned-integer block (+ offset 3) 8)))
	    (if (and (= opcode svm1-inst:ijump-u8) (= operand 0))
		(vector 'COMPILED (read-procedure block (+ offset 4)) arity)
		(error (string-append "disassembler/read-procedure-cache:"
				      " Unexpected instruction")
		       opcode operand)))))))

(define (read-procedure block offset)
  (with-absolutely-no-interrupts
   (lambda ()
     ((ucode-primitive primitive-object-set-type 2)
      (ucode-type compiled-entry)
      ((ucode-primitive make-non-pointer-object 1)
       (read-unsigned-integer block offset (* address-units-per-object 8)))))))

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

(define (instruction-comment instruction cursor)
  (let loop ((insn instruction))
    (and (pair? insn)
	 (if (and (memq (car insn) '(PCR-S8 PCR-S16 PCR-S32))
		  (pair? (cdr insn))
		  (exact-integer? (cadr insn))
		  (not (zero? (cadr insn))))
	     (+ (cadr insn) (cursor-offset cursor))
	     (or (loop (car insn))
		 (loop (cdr insn)))))))

(define (disassembler/lookup-symbol symbol-table offset)
  (and symbol-table
       (let ((label (dbg-labels/find-offset symbol-table offset)))
	 (and label 
	      (dbg-label/name label)))))

(define (read-unsigned-integer block offset size)
  (bit-string->unsigned-integer (read-bits block offset size)))

(define (read-bits block offset size-in-bits)
  (let ((word (bit-string-allocate size-in-bits))
	(bit-offset (* offset addressing-granularity)))
    (with-absolutely-no-interrupts
     (lambda ()
       (read-bits! block bit-offset word)))
    word))

(define-integrable (make-unsigned-reader nbits)
  (let ((nbytes (fix:quotient nbits addressing-granularity)))
    (lambda (cursor)
      (let ((offset (cursor-offset cursor)))
	(let ((word (read-bits (cursor-block cursor) offset nbits)))
	  (set-cursor-offset! cursor (+ offset nbytes))
	  (bit-string->unsigned-integer word))))))

(define next-unsigned-byte (make-unsigned-reader 8))
(define next-unsigned-16-bit-word (make-unsigned-reader 16))

;; These are used by dassm1.scm

(define compiled-code-block/procedure-cache-offset 0)
(define compiled-code-block/objects-per-procedure-cache 2)
(define compiled-code-block/objects-per-variable-cache 1)

;; global variable used by runtime/udata.scm -- Moby yuck!

(set! compiled-code-block/bytes-per-object address-units-per-object)