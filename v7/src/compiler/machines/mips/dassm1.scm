#| -*-Scheme-*-

$Id: dassm1.scm,v 1.12 2008/01/30 20:01:51 cph Exp $

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
  (disassembler/instructions false start-address end-address false))

(define (disassembler/write-instruction-stream symbol-table instruction-stream)
  (fluid-let ((*unparser-radix* 16))
    (disassembler/for-each-instruction instruction-stream
      (lambda (offset instruction)
	(disassembler/write-instruction symbol-table
					offset
					(lambda () (display instruction)))))))

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
			      (sc-macro-transformer
			       (lambda (form environment)
				 environment
				 (apply microcode-type (cdr form))))))
		  (ucode-type linkage-section))
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