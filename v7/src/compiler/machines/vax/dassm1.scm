#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/dassm1.scm,v 4.1 1988/01/07 21:15:30 bal Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; VAX Disassembler
;;;
;;; Matches version 4.2 of bobcat/dassm1.scm
;;;

(declare (usual-integrations))

;;; Flags that control disassembler behavior
(define disassembler/symbolize-output? true)
(define disassembler/compiled-code-heuristics? true)
(define disassembler/write-offsets? true)

;;; Operations exported from the disassembler package
(define disassembler/instructions)
(define disassembler/instructions/null?)
(define disassembler/instructions/read)
(define disassembler/lookup-symbol)

(define (compiler:write-lap-file filename #!optional symbol-table?)
  (let ((pathname (->pathname filename)))
    (with-output-to-file (pathname-new-type pathname "lap")
      (lambda ()
	(disassembler/write-compiled-code-block
	 (compiled-code-block/read-file (pathname-new-type pathname "com"))
	 (let ((pathname (pathname-new-type pathname "binf")))
	   (and (if (unassigned? symbol-table?)
		    (file-exists? pathname)
		    symbol-table?)
		(compiler-info/symbol-table
		 (compiler-info/read-file pathname)))))))))

(define (disassembler/write-compiled-code-block block symbol-table)
  (write-string "Code:\n\n")
  (disassembler/write-instruction-stream
   symbol-table
   (disassembler/instructions/compiled-code-block block symbol-table))
  (write-string "\nConstants:\n\n")
  (disassembler/write-constants-block block symbol-table))

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
	 (lambda ()
	   (let ((string
		  (with-output-to-string
		    (lambda ()
		      (display instruction)))))
	     (string-downcase! string)
	     (write-string string))))))))

(define (disassembler/for-each-instruction instruction-stream procedure)
  (let loop ((instruction-stream instruction-stream))
    (if (not (disassembler/instructions/null? instruction-stream))
	(disassembler/instructions/read instruction-stream
	  (lambda (offset instruction instruction-stream)
	    (procedure offset instruction)
	    (loop (instruction-stream)))))))

(define disassembler/write-constants-block)
(let ()

(set! disassembler/write-constants-block
  (named-lambda (disassembler/write-constants-block block symbol-table)
    (fluid-let ((*unparser-radix* 16))
      (let ((end (system-vector-size block)))
	(let loop ((index (compiled-code-block/constants-start block)))
	  (if (< index end)
	      (begin
		(disassembler/write-instruction
		 symbol-table
		 (compiled-code-block/index->offset index)
		 (lambda ()
		   (write-constant block
				   symbol-table
				   (system-vector-ref block index))))
		(loop (1+ index)))))))))

(define (write-constant block symbol-table constant)
  (write-string (cdr (write-to-string constant 60)))
  (if (lambda? constant)
      (let ((expression (lambda-body constant)))
	(if (and (compiled-code-address? expression)
		 (eq? (compiled-code-address->block expression) block))
	    (begin
	      (write-string "  (")
	      (let ((offset (compiled-code-address->offset expression)))
		(let ((label (disassembler/lookup-symbol symbol-table offset)))
		  (if label
		      (write-string (string-downcase label))
		      (write offset))))
	      (write-string ")"))))))

)

(define (disassembler/write-instruction symbol-table offset write-instruction)
  (if symbol-table
      (sorted-vector/for-each symbol-table offset
	(lambda (label)
	  (write-char #\Tab)
	  (write-string (string-downcase (label-info-name label)))
	  (write-char #\:)
	  (newline))))
  (if disassembler/write-offsets?
      (begin (write-string
	      ((access unparse-number-heuristically number-unparser-package)
	       offset 16 false false))
	     (write-char #\Tab)))
  (if symbol-table
      (write-string "    "))
  (write-instruction)
  (newline))
