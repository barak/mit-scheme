;;; -*-Scheme-*-
;;;
;;; $Id: rgxcmp.scm,v 1.119 2002/02/03 03:38:56 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-2002 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Regular Expression Pattern Compiler
;;;  Translated from GNU (thank you RMS!)

(declare (usual-integrations))

;;;; Compiled Opcodes

(define-syntax define-enumeration
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (close-syntax (cadr form) environment))
	   (prefix (caddr form))
	   (suffixes (cdddr form)))
       `(BEGIN
	  ,@(let loop ((n 0) (suffixes suffixes))
	      (if (pair? suffixes)
		  (cons `(DEFINE-INTEGRABLE
			   ,(close-syntax (symbol-append prefix (car suffixes))
					  environment)
			   ,n)
			(loop (+ n 1) (cdr suffixes)))
		  '()))
	  (DEFINE ,name
	    (VECTOR ,@(map (lambda (suffix) `',suffix) suffixes))))))))

(define-enumeration re-codes re-code:

  ;; Zero bytes may appear in the compiled regular expression.
  unused

  ;; Followed by a single literal byte.
  exact-1

  ;; Followed by one byte giving n, and then by n literal bytes.
  exact-n

  line-start		;Fails unless at start of line.
  line-end		;Fails unless at end of line.

  ;; Followed by two bytes giving relative address to jump to.
  jump

  ;; Followed by two bytes giving relative address of place to result
  ;; at in case of failure.
  on-failure-jump

  ;; Throw away latest failure point and then jump to address.
  finalize-jump

  ;; Like jump but finalize if safe to do so.  This is used to jump
  ;; back to the beginning of a repeat.  If the command that follows
  ;; this jump is clearly incompatible with the one at the beginning
  ;; of the repeat, such that we can be sure that there is no use
  ;; backtracing out of repetitions already completed, then we
  ;; finalize.
  maybe-finalize-jump

  ;; Jump, and push a dummy failure point.  This failure point will be
  ;; thrown away if an attempt is made to use it for a failure.  A +
  ;; construct makes this before the first repeat.
  dummy-failure-jump

  ;; Matches any one character except for newline.
  any-char

  ;; Matches any one char belonging to specified set. First following
  ;; byte is # bitmap bytes.  Then come bytes for a bit-map saying
  ;; which chars are in.  Bits in each byte are ordered low-bit-first.
  ;; A character is in the set if its bit is 1.  A character too large
  ;; to have a bit in the map is automatically not in the set.
  char-set

  ;; Similar but match any character that is NOT one of those
  ;; specified.
  not-char-set

  ;; Starts remembering the text that is matches and stores it in a
  ;; memory register.  Followed by one byte containing the register
  ;; number.  Register numbers must be in the range 0 through
  ;; re-number-of-registers.
  start-memory

  ;; Stops remembering the text that is matched and stores it in a
  ;; memory register.  Followed by one byte containing the register
  ;; number.  Register numbers must be in the range 0 through
  ;; re-number-of-registers.
  stop-memory

  ;; Match a duplicate of something remembered.  Followed by one byte
  ;; containing the index of the memory register.
  duplicate

  buffer-start		;Succeeds if at beginning of buffer.
  buffer-end		;Succeeds if at end of buffer.
  word-char		;Matches any word-constituent character.
  not-word-char		;Matches any char that is not a word-constituent.
  word-start		;Succeeds if at word beginning.
  word-end		;Succeeds if at word end.
  word-bound		;Succeeds if at a word boundary.
  not-word-bound	;Succeeds if not at a word boundary.

  ;; Matches any character whose syntax is specified.  Followed by a
  ;; byte which contains a syntax code.
  syntax-spec

  ;; Matches any character whose syntax differs from the specified.
  not-syntax-spec
  )

;;;; Cache

(define (cached-procedure size procedure)
  (let ((cache (make-cache size)))
    (lambda (key1 key2)
      (cache-result cache procedure key1 key2))))

(define (make-cache size)
  (let ((items (make-list size)))
    (do ((items items (cdr items)))
	((null? items))
      (set-car! items (cons (cons #f #f) #f)))
    (set-cdr! (last-pair items) items)
    (cons 'CACHE items)))

(define (cache-result cache procedure key1 key2)
  (let* ((tail (cdr cache))
	 (head (cdr tail)))
    (let loop ((items head) (prev tail))
      (let ((item (car items)))
	(cond ((and (eq? key1 (caar item))
		    (eq? key2 (cdar item)))
	       (cond ((eq? tail items)
		      (set-cdr! cache prev))
		     ((not (eq? head items))
		      (without-interrupts
		       (lambda ()
			 (set-cdr! prev (cdr items))
			 (set-cdr! items head)
			 (set-cdr! tail items)))))
	       (cdr item))
	      ((eq? tail items)
	       (let ((result (procedure key1 key2)))
		 (without-interrupts
		  (lambda ()
		    (set-car! (car item) key1)
		    (set-cdr! (car item) key2)
		    (set-cdr! item result)
		    (set-cdr! cache prev)))
		 result))
	      (else
	       (loop (cdr items) items)))))))

;;;; String Compiler

(define (re-compile-char char case-fold?)
  (let ((result (string-allocate 2)))
    (vector-8b-set! result 0 re-code:exact-1)
    (string-set! result 1 (if case-fold? (char-upcase char) char))
    (make-compiled-regexp result case-fold?)))

(define re-compile-string
  (cached-procedure 16
    (lambda (string case-fold?)
      (let ((string (if case-fold? (string-upcase string) string)))
	(let ((n (string-length string)))
	  (if (fix:zero? n)
	      (make-compiled-regexp string case-fold?)
	      (let ((result
		     (string-allocate 
		      (let ((qr (integer-divide n 255)))
			(fix:+ (fix:* 257 (integer-divide-quotient qr))
			       (let ((r (integer-divide-remainder qr)))
				 (cond ((fix:zero? r) 0)
				       ((fix:= 1 r) 2)
				       (else (fix:+ r 2)))))))))
		(let loop ((n n) (i 0) (p 0))
		  (cond ((fix:= n 1)
			 (vector-8b-set! result p re-code:exact-1)
			 (vector-8b-set! result
					 (fix:1+ p)
					 (vector-8b-ref string i))
			 (make-compiled-regexp result case-fold?))
			((fix:< n 256)
			 (vector-8b-set! result p re-code:exact-n)
			 (vector-8b-set! result (fix:1+ p) n)
			 (substring-move! string i (fix:+ i n)
					  result (fix:+ p 2))
			 (make-compiled-regexp result case-fold?))
			(else
			 (vector-8b-set! result p re-code:exact-n)
			 (vector-8b-set! result (fix:1+ p) 255)
			 (let ((j (fix:+ i 255)))
			   (substring-move! string i j result (fix:+ p 2))
			   (loop (fix:- n 255) j (fix:+ p 257)))))))))))))

(define re-quote-string
  (let ((special (char-set #\[ #\] #\* #\. #\\ #\? #\+ #\^ #\$)))
    (lambda (string)
      (let ((end (string-length string)))
	(let ((n
	       (let loop ((start 0) (n 0))
		 (let ((index
			(substring-find-next-char-in-set string start end
							 special)))
		   (if index
		       (loop (1+ index) (1+ n))
		       n)))))
	  (if (zero? n)
	      string
	      (let ((result (string-allocate (+ end n))))
		(let loop ((start 0) (i 0))
		  (let ((index
			 (substring-find-next-char-in-set string start end
							  special)))
		    (if index
			(begin
			  (substring-move! string start index result i)
			  (let ((i (+ i (- index start))))
			    (string-set! result i #\\)
			    (string-set! result
					 (1+ i)
					 (string-ref string index))
			    (loop (1+ index) (+ i 2))))
			(substring-move! string start end result i))))
		result)))))))

;;;; Char-Set Compiler

;;; Special characters:
;;; #\] must appear as first character.
;;; #\- must appear as first or last character, or it may appear
;;;     immediately after a range.
;;; #\^ must appear anywhere except as the first character in the set.

(define (re-compile-char-set pattern negate?)
  (let ((length (string-length pattern))
	(table (string-allocate 256)))
    (let ((kernel
	   (lambda (start background foreground)
	     (let ((adjoin!
		    (lambda (ascii)
		      (vector-8b-set! table ascii foreground))))
	       (vector-8b-fill! table 0 256 background)
	       (let loop
		   ((pattern (substring->list pattern start length)))
		 (if (pair? pattern)
		     (if (and (pair? (cdr pattern))
			      (char=? (cadr pattern) #\-)
			      (pair? (cddr pattern)))
			 (begin
			   (let ((end (char->ascii (caddr pattern))))
			     (let loop
				 ((index (char->ascii (car pattern))))
			       (if (fix:<= index end)
				   (begin
				     (vector-8b-set! table
						     index
						     foreground)
				     (loop (fix:+ index 1))))))
			   (loop (cdddr pattern)))
			 (begin
			   (adjoin! (char->ascii (car pattern)))
			   (loop (cdr pattern))))))))))
      (if (and (not (fix:zero? length))
	       (char=? (string-ref pattern 0) #\^))
	  (if negate?
	      (kernel 1 0 1)
	      (kernel 1 1 0))
	  (if negate?
	      (kernel 0 1 0)
	      (kernel 0 0 1))))
    (make-char-set table)))

;;;; Translation Tables

(define re-translation-table
  (let ((normal-table (make-string 256)))
    (let loop ((n 0))
      (if (< n 256)
	  (begin
	    (vector-8b-set! normal-table n n)
	    (loop (1+ n)))))
    (let ((upcase-table (string-copy normal-table)))
      (let loop ((n #x61))
	(if (< n #x7B)
	    (begin
	      (vector-8b-set! upcase-table n (- n #x20))
	      (loop (1+ n)))))
      (lambda (case-fold?)
	(if case-fold? upcase-table normal-table)))))

;;;; Pattern Compiler

(define re-number-of-registers
  10)

(define-integrable stack-maximum-length
  re-number-of-registers)

(define condition-type:re-compile-pattern
  (make-condition-type 'RE-COMPILE-PATTERN condition-type:error
      '(MESSAGE)
    (lambda (condition port)
      (write-string "Error compiling regular expression: " port)
      (write-string (access-condition condition 'MESSAGE) port))))

(define compilation-error
  (condition-signaller condition-type:re-compile-pattern
		       '(MESSAGE)
		       standard-error-handler))

(define-structure (compiled-regexp
		   (constructor %make-compiled-regexp)
		   (conc-name compiled-regexp/))
  (byte-stream #f read-only #t)
  (translation-table #f read-only #t))

(define (make-compiled-regexp byte-stream case-fold?)
  (%make-compiled-regexp byte-stream (re-translation-table case-fold?)))

(define input-list)
(define current-byte)
(define translation-table)
(define output-head)
(define output-tail)
(define output-length)
(define stack)

(define fixup-jump)
(define register-number)
(define begin-alternative)
(define pending-exact)
(define last-start)

(define re-compile-pattern
  (cached-procedure 16
    (lambda (pattern case-fold?)
      (let ((output (list 'OUTPUT)))
	(fluid-let ((input-list (map char->ascii (string->list pattern)))
		    (current-byte)
		    (translation-table (re-translation-table case-fold?))
		    (output-head output)
		    (output-tail output)
		    (output-length 0)
		    (stack '())
		    (fixup-jump #f)
		    (register-number 1)
		    (begin-alternative)
		    (pending-exact #f)
		    (last-start #f))
	  (set! begin-alternative (output-pointer))
	  (let loop ()
	    (if (input-end?)
		(begin
		  (if fixup-jump
		      (store-jump! fixup-jump re-code:jump (output-position)))
		  (if (not (stack-empty?))
		      (compilation-error "Unmatched \\("))
		  (make-compiled-regexp
		   (list->string (map ascii->char (cdr output-head)))
		   case-fold?))
		(begin
		  (compile-pattern-char)
		  (loop)))))))))

;;;; Input

(define-integrable (input-end?)
  (null? input-list))

(define-integrable (input-end+1?)
  (null? (cdr input-list)))

(define-integrable (input-peek)
  (vector-8b-ref translation-table (car input-list)))

(define-integrable (input-peek+1)
  (vector-8b-ref translation-table (cadr input-list)))

(define-integrable (input-discard!)
  (set! input-list (cdr input-list))
  unspecific)

(define-integrable (input!)
  (set! current-byte (input-peek))
  (input-discard!))

(define-integrable (input-raw!)
  (set! current-byte (car input-list))
  (input-discard!))

(define-integrable (input-peek-1)
  current-byte)

(define-integrable (input-read!)
  (if (input-end?)
      (premature-end)
      (let ((char (input-peek)))
	(input-discard!)
	char)))

(define (input-match? byte . chars)
  (memv (ascii->char byte) chars))

;;;; Output

(define-integrable (output! byte)
  (let ((tail (list byte)))
    (set-cdr! output-tail tail)
    (set! output-tail tail))
  (set! output-length (fix:1+ output-length))
  unspecific)

(define-integrable (output-re-code! code)
  (set! pending-exact #f)
  (output! code))

(define-integrable (output-start! code)
  (set! last-start (output-pointer))
  (output-re-code! code))

(define-integrable (output-position)
  output-length)

(define-integrable (output-pointer)
  (cons output-length output-tail))

(define-integrable (pointer-position pointer)
  (car pointer))

(define-integrable (pointer-ref pointer)
  (caddr pointer))

(define-integrable (pointer-operate! pointer operator)
  (set-car! (cddr pointer) (operator (caddr pointer)))
  unspecific)

(define (store-jump! from opcode to)
  (let ((p (cddr from)))
    (set-car! p opcode)
    (compute-jump (pointer-position from) to
      (lambda (low high)
	(set-car! (cdr p) low)
	(set-car! (cddr p) high)
	unspecific))))

(define (insert-jump! from opcode to)
  (compute-jump (pointer-position from) to
    (lambda (low high)
      (set-cdr! (cdr from)
		(cons* opcode low high (cddr from)))
      (set! output-length (fix:+ output-length 3))
      unspecific)))

(define (compute-jump from to receiver)
  (let ((n (fix:- to (fix:+ from 3))))
    (let ((qr
	   (integer-divide (if (fix:negative? n) (fix:+ n #x10000) n)
			   #x100)))
      (receiver (integer-divide-remainder qr)
		(integer-divide-quotient qr)))))

;;;; Stack

(define-integrable (stack-empty?)
  (null? stack))

(define-integrable (stack-full?)
  (not (fix:< (stack-length) stack-maximum-length)))

(define-integrable (stack-length)
  (length stack))

(define (stack-push! . args)
  (set! stack (cons args stack))
  unspecific)

(define (stack-pop! receiver)
  (let ((frame (car stack)))
    (set! stack (cdr stack))
    (apply receiver frame)))

(define-integrable (stack-ref-register-number i)
  (caddr (list-ref stack i)))

(define (ascii->syntax-entry ascii)
  ((ucode-primitive string->syntax-entry) (char->string (ascii->char ascii))))

;;;; Pattern Dispatch

(define-integrable (compile-pattern-char)
  (input!)
  ((vector-ref pattern-chars (input-peek-1))))

(define (premature-end)
  (compilation-error "Premature end of regular expression"))

(define (normal-char)
  (if (if (input-end?)
	  (not pending-exact)
	  (input-match? (input-peek) #\* #\+ #\? #\^))
      (begin
	(output-start! re-code:exact-1)
	(output! (input-peek-1)))
      (begin
	(if (or (not pending-exact)
		(fix:= (pointer-ref pending-exact) #x7F))
	    (begin
	      (set! last-start (output-pointer))
	      (output! re-code:exact-n)
	      (set! pending-exact (output-pointer))
	      (output! 0)))
	(output! (input-peek-1))
	(pointer-operate! pending-exact 1+))))

(define (define-pattern-char char procedure)
  (vector-set! pattern-chars (char->ascii char) procedure)
  unspecific)

(define pattern-chars
  (make-vector 256 normal-char))

(define-pattern-char #\\
  (lambda ()
    (if (input-end?)
	(premature-end)
	(begin
	  (input-raw!)
	  ((vector-ref backslash-chars (input-peek-1)))))))

(define (define-backslash-char char procedure)
  (vector-set! backslash-chars (char->ascii char) procedure)
  unspecific)

(define backslash-chars
  (make-vector 256 normal-char))

(define-pattern-char #\$
  ;; $ means succeed if at end of line, but only in special contexts.
  ;; If randomly in the middle of a pattern, it is a normal character.
  (lambda ()
    (if (or (input-end?)
	    (input-end+1?)
	    (and (input-match? (input-peek) #\\)
		 (input-match? (input-peek+1) #\) #\|)))
	(output-re-code! re-code:line-end)
	(normal-char))))

(define-pattern-char #\^
  ;; ^ means succeed if at beginning of line, but only if no preceding
  ;; pattern.
  (lambda ()
    (if (not last-start)
	(output-re-code! re-code:line-start)
	(normal-char))))

(define-pattern-char #\.
  (lambda ()
    (output-start! re-code:any-char)))

(define (define-trivial-backslash-char char code)
  (define-backslash-char char
    (lambda ()
      (output-re-code! code))))

(define-trivial-backslash-char #\< re-code:word-start)
(define-trivial-backslash-char #\> re-code:word-end)
(define-trivial-backslash-char #\b re-code:word-bound)
(define-trivial-backslash-char #\B re-code:not-word-bound)
(define-trivial-backslash-char #\` re-code:buffer-start)
(define-trivial-backslash-char #\' re-code:buffer-end)

(define (define-starter-backslash-char char code)
  (define-backslash-char char
    (lambda ()
      (output-start! code))))

(define-starter-backslash-char #\w re-code:word-char)
(define-starter-backslash-char #\W re-code:not-word-char)

(define-backslash-char #\s
  (lambda ()
    (output-start! re-code:syntax-spec)
    (output! (ascii->syntax-entry (input-read!)))))

(define-backslash-char #\S
  (lambda ()
    (output-start! re-code:not-syntax-spec)
    (output! (ascii->syntax-entry (input-read!)))))

;;;; Repeaters

(define (define-repeater-char char zero? many?)
  (define-pattern-char char
    ;; If there is no previous pattern, char not special.
    (lambda ()
      (if (not last-start)
	  (normal-char)
	  (repeater-loop zero? many?)))))

(define (repeater-loop zero? many?)
  ;; If there is a sequence of repetition chars, collapse it down to
  ;; equivalent to just one.
  (cond ((input-end?)
	 (repeater-finish zero? many?))
	((input-match? (input-peek) #\*)
	 (input-discard!)
	 (repeater-loop zero? many?))
	((input-match? (input-peek) #\+)
	 (input-discard!)
	 (repeater-loop #f many?))
	((input-match? (input-peek) #\?)
	 (input-discard!)
	 (repeater-loop zero? #f))
	(else
	 (repeater-finish zero? many?))))

(define (repeater-finish zero? many?)
  (if many?
      ;; More than one repetition allowed: put in a backward jump at
      ;; the end.
      (compute-jump (output-position)
		    (fix:- (pointer-position last-start) 3)
	(lambda (low high)
	  (output-re-code! re-code:maybe-finalize-jump)
	  (output! low)
	  (output! high))))
  (insert-jump! last-start
		re-code:on-failure-jump
		(fix:+ (output-position) 3))
  (if (not zero?)
      ;; At least one repetition required: insert before the loop a
      ;; skip over the initial on-failure-jump instruction.
      (insert-jump! last-start
		    re-code:dummy-failure-jump
		    (fix:+ (pointer-position last-start) 6))))

(define-repeater-char #\* #t #t)
(define-repeater-char #\+ #f #t)
(define-repeater-char #\? #t #f)

;;;; Character Sets

(define-pattern-char #\[
  (lambda ()
    (if (input-end?)
	(premature-end))
    (let ((invert?
	   (and (input-match? (input-peek) #\^)
		(begin (input-discard!) #t)))
	  (charset (make-string 32 (ascii->char 0))))
      (let loop
	  ((chars
	    (if (input-match? (input-peek) #\])
		(begin
		  (input-discard!)
		  (list (char->integer #\])))
		'())))
	(if (input-end?)
	    (premature-end))
	(let ((char (input-read!)))
	  (if (input-match? char #\])
	      (begin
		(for-each
		 (lambda (char)
		   ((ucode-primitive re-char-set-adjoin!) charset
							  (char->ascii char)))
		 (char-set-members
		  (re-compile-char-set
		   (list->string (map ascii->char (reverse! chars)))
		   #f))))
	      (loop (cons char chars)))))
      (output-start! (if invert? re-code:not-char-set re-code:char-set))
      ;; Discard any bitmap bytes that are all 0 at the end of
      ;; the map.  Decrement the map-length byte too.
      (let loop ((n 31))
	(cond ((not (fix:= 0 (vector-8b-ref charset n)))
	       (output! (fix:+ n 1))
	       (let loop ((i 0))
		 (output! (vector-8b-ref charset i))
		 (if (fix:< i n)
		     (loop (fix:+ i 1)))))
	      ((fix:= 0 n)
	       (output! 0))
	      (else
	       (loop (fix:- n 1))))))))

;;;; Alternative Groups

(define-backslash-char #\(
  (lambda ()
    (if (stack-full?)
	(compilation-error "Nesting too deep"))
    (if (fix:< register-number re-number-of-registers)
	(begin
	  (output-re-code! re-code:start-memory)
	  (output! register-number)))
    (stack-push! (output-pointer)
		 fixup-jump
		 register-number
		 begin-alternative)
    (set! last-start #f)
    (set! fixup-jump #f)
    (set! register-number (fix:1+ register-number))
    (set! begin-alternative (output-pointer))
    unspecific))

(define-backslash-char #\)
  (lambda ()
    (if (stack-empty?)
	(compilation-error "Unmatched close paren"))
    (if fixup-jump
	(store-jump! fixup-jump re-code:jump (output-position)))
    (stack-pop!
     (lambda (op fj rn bg)
       (set! last-start op)
       (set! fixup-jump fj)
       (set! begin-alternative bg)
       (if (fix:< rn re-number-of-registers)
	   (begin
	     (output-re-code! re-code:stop-memory)
	     (output! rn)))))))

(define-backslash-char #\|
  (lambda ()
    (insert-jump! begin-alternative
		  re-code:on-failure-jump
		  (fix:+ (output-position) 6))
    (if fixup-jump
	(store-jump! fixup-jump re-code:jump (output-position)))
    (set! fixup-jump (output-pointer))
    (output! re-code:unused)
    (output! re-code:unused)
    (output! re-code:unused)
    (set! pending-exact #f)
    (set! last-start #f)
    (set! begin-alternative (output-pointer))
    unspecific))

(define (define-digit-char digit)
  (let ((char (digit->char digit)))
    (define-backslash-char char
      (lambda ()
	(if (fix:< digit register-number)
	    (let ((n (stack-length)))
	      (let search-stack ((i 0))
		(cond ((not (fix:< i n))
		       (output-start! re-code:duplicate)
		       (output! digit))
		      ((fix:= (stack-ref-register-number i) digit)
		       (normal-char))
		      (else
		       (search-stack (fix:1+ i))))))
	    (normal-char))))))

(for-each define-digit-char '(1 2 3 4 5 6 7 8 9))

;;;; Compiled Pattern Disassembler

(define (hack-fastmap pattern)
  (let ((compiled-pattern (re-compile-pattern pattern #f))
	(cs (char-set)))
    ((ucode-primitive re-compile-fastmap)
     compiled-pattern
     (re-translation-table #f)
     (get-char-syntax standard-char-syntax-table)
     cs)
    (char-set-members cs)))

(define (re-disassemble-pattern compiled-pattern)
  (let ((n (string-length compiled-pattern)))
    (let loop ((i 0))
      (newline)
      (write i)
      (write-string " (")
      (if (< i n)
	  (case (let ((re-code-name
		       (vector-ref re-codes
				   (vector-8b-ref compiled-pattern i))))
		  (write re-code-name)
		  re-code-name)
	    ((UNUSED LINE-START LINE-END ANY-CHAR BUFFER-START BUFFER-END
	      WORD-CHAR NOT-WORD-CHAR WORD-START WORD-END WORD-BOUND
	      NOT-WORD-BOUND)
	     (write-string ")")
	     (loop (1+ i)))
	    ((EXACT-1)
	     (write-string " ")
	     (let ((end (+ i 2)))
	       (write (substring compiled-pattern (1+ i) end))
	       (write-string ")")
	       (loop end)))
	    ((EXACT-N)
	     (write-string " ")
	     (let ((start (+ i 2))
		   (n (vector-8b-ref compiled-pattern (1+ i))))
	       (let ((end (+ start n)))
		 (write (substring compiled-pattern start end))
		 (write-string ")")
		 (loop end))))
	    ((JUMP ON-FAILURE-JUMP MAYBE-FINALIZE-JUMP DUMMY-FAILURE-JUMP)
	     (write-string " ")
	     (let ((end (+ i 3))
		   (offset
		    (+ (* 256 (vector-8b-ref compiled-pattern (+ i 2)))
		       (vector-8b-ref compiled-pattern (1+ i)))))
	       (write (+ end (if (< offset #x8000) offset (- offset #x10000))))
	       (write-string ")")
	       (loop end)))
	    ((CHAR-SET NOT-CHAR-SET)
	     (let ((end (+ (+ i 2) (vector-8b-ref compiled-pattern (1+ i)))))
	       (let spit ((i (+ i 2)))
		 (if (< i end)
		     (begin
		       (write-string " ")
		       (let ((n (vector-8b-ref compiled-pattern i)))
			 (if (< n 16) (write-char #\0))
			 (write-string (number->string n 16)))
		       (spit (1+ i)))
		     (begin
		       (write-string ")")
		       (loop i))))))
	    ((START-MEMORY STOP-MEMORY DUPLICATE)
	     (write-string " ")
	     (write (vector-8b-ref compiled-pattern (1+ i)))
	     (write-string ")")
	     (loop (+ i 2)))
	    ((SYNTAX-SPEC NOT-SYNTAX-SPEC)
	     (write-string " ")
	     (write (string-ref " .w_()'\"$\\/<>"
				(vector-8b-ref compiled-pattern (1+ i))))
	     (write-string ")")
	     (loop (+ i 2))))
	  (begin
	    (write 'end)
	    (write-string ")"))))))