#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Regular Expression Pattern Compiler
;;;  Translated from GNU (thank you RMS!)

(declare (usual-integrations))

;;;; Compiled Opcodes

(define-syntax define-enumeration
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (let ((name (cadr form))
	   (prefix (caddr form))
	   (suffixes (cdddr form)))
       `(begin
	  ,@(let loop ((n 0) (suffixes suffixes))
	      (if (pair? suffixes)
		  (cons `(define-integrable
			   ,(symbol prefix (car suffixes))
			   ,n)
			(loop (+ n 1) (cdr suffixes)))
		  '()))
	  (define ,name
	    (vector ,@(map (lambda (suffix) `',suffix) suffixes))))))))

(define (re-code-name byte)
  (and (fix:< byte (vector-length re-codes))
       (vector-ref re-codes byte)))

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
  not-syntax-spec)

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
    (cons* 'cache (make-thread-mutex) items)))

(define-integrable (with-cache-locked cache thunk)
  (with-thread-mutex-lock (cadr cache)
    (lambda ()
      (without-interruption thunk))))

(define (cache-result cache procedure key1 key2)
  (with-cache-locked cache
    (lambda ()
      (let* ((tail (cddr cache))
	     (head (cdr tail)))
	(let loop ((items head) (prev tail))
	  (let ((item (car items)))
	    (cond ((and (eq? key1 (caar item))
			(eq? key2 (cdar item)))
		   (cond ((eq? tail items)
			  (set-cdr! (cdr cache) prev))
			 ((not (eq? head items))
			  (set-cdr! prev (cdr items))
			  (set-cdr! items head)
			  (set-cdr! tail items)))
		   (cdr item))
		  ((eq? tail items)
		   (let ((result (procedure key1 key2)))
		     (set-car! (car item) key1)
		     (set-cdr! (car item) key2)
		     (set-cdr! item result)
		     (set-cdr! (cdr cache) prev)
		     result))
		  (else
		   (loop (cdr items) items)))))))))

;;;; String Compiler

(define (re-compile-char char case-fold?)
  (guarantee 8-bit-char? char 're-compile-char)
  (let ((result (make-bytevector 2)))
    (bytevector-u8-set! result 0 re-code:exact-1)
    (bytevector-u8-set! result 1
			(char->integer
			 (if case-fold? (char-sort-of-upcase char) char)))
    (make-compiled-regexp result case-fold?)))

(define re-compile-string
  (cached-procedure 16
    (lambda (string case-fold?)
      (guarantee 8-bit-string? string 're-compile-string)
      (let ((end (string-length string))
	    (builder (bytevector-builder))
	    (get-byte
	     (if case-fold?
		 (lambda (i)
		   (char->integer (char-sort-of-upcase (string-ref string i))))
		 (lambda (i)
		   (char->integer (string-ref string i))))))
	(let ((copy!
	       (lambda (start end)
		 (do ((i start (fix:+ i 1)))
		     ((not (fix:< i end)))
		   (builder (get-byte i))))))
	  (let loop ((start 0))
	    (if (fix:< start end)
		(let ((n (fix:- end start)))
		  (if (fix:< n #x100)
		      (if (fix:= n 1)
			  (begin
			    (builder re-code:exact-1)
			    (builder (get-byte start)))
			  (begin
			    (builder re-code:exact-n)
			    (builder n)
			    (copy! start end)))
		      (begin
			(builder re-code:exact-n)
			(builder #xFF)
			(let ((start* (fix:+ start #xFF)))
			  (copy! start start*)
			  (loop start*))))))))
	(make-compiled-regexp (builder) case-fold?)))))

(define char-set:re-special
  (char-set #\[ #\] #\* #\. #\\ #\? #\+ #\^ #\$))

(define (re-quote-string string)
  (let ((builder (string-builder)))
    (string-for-each (lambda (char)
		       (if (char-in-set? char char-set:re-special)
			   (builder #\\))
		       (builder char))
		     string)
    (builder)))

;;;; Pattern Compiler

(define re-number-of-registers
  10)

(define-integrable stack-maximum-length
  re-number-of-registers)

(define condition-type:re-compile-pattern
  (make-condition-type 're-compile-pattern condition-type:error
      '(compilation-context message)
    (lambda (condition port)
      (write-string "Error compiling regular expression: " port)
      (write-string (access-condition condition 'message) port))))

(define compilation-error
  (condition-signaller condition-type:re-compile-pattern
		       '(compilation-context message)
		       standard-error-handler))

(define-structure (compiled-regexp
		   (constructor %make-compiled-regexp)
		   (conc-name compiled-regexp/))
  (byte-stream #f read-only #t)
  (translation-table #f read-only #t))

;; Needed so that we stay within ISO 8859-1.
(define (char-sort-of-upcase char)
  (let ((c (char-upcase char)))
    (if (fix:>= (char->integer c) #x100)
	char
	c)))

(define re-translation-table
  (let ((normal-table (make-bytevector #x100))
	(upcase-table (make-bytevector #x100)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i #x100)))
      (bytevector-u8-set! normal-table i i)
      (bytevector-u8-set! upcase-table i
			  (char->integer
			   (char-sort-of-upcase (integer->char i)))))
    (lambda (case-fold?)
      (if case-fold? upcase-table normal-table))))

(define (make-compiled-regexp bytes case-fold?)
  (%make-compiled-regexp bytes (re-translation-table case-fold?)))

(define-structure (rgxcmpctx (conc-name #f))
  input-list
  current-byte
  translation-table
  output-head
  output-tail
  output-length
  stack

  fixup-jump
  register-number
  begin-alternative
  pending-exact
  last-start)

(define re-compile-pattern
  (cached-procedure 16
    (lambda (pattern case-fold?)
      (guarantee 8-bit-string? pattern 're-compile-pattern)
      (let* ((output (list 'output))
	     (ctx (make-rgxcmpctx (map char->integer (string->list pattern))
				  #f	;current-byte
				  (re-translation-table case-fold?)
				  output ;output-head
				  output ;output-tail
				  0	 ;output-length
				  '()	 ;stack
				  #f	 ;fixup-jump
				  1	 ;register-number
				  #f	 ;begin-alternative
				  #f	 ;pending-exact
				  #f	 ;last-start
				  )))
	(set-begin-alternative! ctx (output-pointer ctx))
	(let loop ()
	  (if (input-end? ctx)
	      (begin
		(if (fixup-jump ctx)
		    (store-jump! (fixup-jump ctx)
				 re-code:jump (output-position ctx)))
		(if (not (stack-empty? ctx))
		    (compilation-error ctx "Unmatched \\("))
		(make-compiled-regexp
		 (list->bytevector (cdr (output-head ctx)))
		 case-fold?))
	      (begin
		(compile-pattern-char ctx)
		(loop))))))))

;;;; Input

(define-integrable (input-end? ctx)
  (null? (input-list ctx)))

(define-integrable (input-end+1? ctx)
  (null? (cdr (input-list ctx))))

(define-integrable (input-peek ctx)
  (bytevector-u8-ref (translation-table ctx) (car (input-list ctx))))

(define-integrable (input-peek+1 ctx)
  (bytevector-u8-ref (translation-table ctx) (cadr (input-list ctx))))

(define-integrable (input-discard! ctx)
  (set-input-list! ctx (cdr (input-list ctx))))

(define-integrable (input! ctx)
  (set-current-byte! ctx (input-peek ctx))
  (input-discard! ctx))

(define-integrable (input-raw! ctx)
  (set-current-byte! ctx (car (input-list ctx)))
  (input-discard! ctx))

(define-integrable (input-peek-1 ctx)
  (current-byte ctx))

(define-integrable (input-read! ctx)
  (if (input-end? ctx)
      (premature-end ctx)
      (let ((char (input-peek ctx)))
	(input-discard! ctx)
	char)))

(define (input-match? byte . chars)
  (memv (integer->char byte) chars))

;;;; Output

(define-integrable (output! ctx byte)
  (let ((tail (list byte)))
    (set-cdr! (output-tail ctx) tail)
    (set-output-tail! ctx tail))
  (set-output-length! ctx (fix:1+ (output-length ctx)))
  unspecific)

(define-integrable (output-re-code! ctx code)
  (set-pending-exact! ctx #f)
  (output! ctx code))

(define-integrable (output-start! ctx code)
  (set-last-start! ctx (output-pointer ctx))
  (output-re-code! ctx code))

(define-integrable (output-position ctx)
  (output-length ctx))

(define-integrable (output-pointer ctx)
  (cons (output-length ctx) (output-tail ctx)))

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

(define (insert-jump! ctx from opcode to)
  (compute-jump (pointer-position from) to
    (lambda (low high)
      (set-cdr! (cdr from)
		(cons* opcode low high (cddr from)))
      (set-output-length! ctx (fix:+ (output-length ctx) 3))
      unspecific)))

(define (compute-jump from to receiver)
  (let ((n (fix:- to (fix:+ from 3))))
    (let ((qr
	   (integer-divide (if (fix:negative? n) (fix:+ n #x10000) n)
			   #x100)))
      (receiver (integer-divide-remainder qr)
		(integer-divide-quotient qr)))))

;;;; Stack

(define-integrable (stack-empty? ctx)
  (null? (stack ctx)))

(define-integrable (stack-full? ctx)
  (not (fix:< (stack-length ctx) stack-maximum-length)))

(define-integrable (stack-length ctx)
  (length (stack ctx)))

(define (stack-push! ctx . args)
  (set-stack! ctx (cons args (stack ctx)))
  unspecific)

(define (stack-pop! ctx receiver)
  (let ((frame (car (stack ctx))))
    (set-stack! ctx (cdr (stack ctx)))
    (apply receiver frame)))

(define-integrable (stack-ref-register-number ctx i)
  (caddr (list-ref (stack ctx) i)))

(define (ascii->syntax-entry ascii)
  ((ucode-primitive string->syntax-entry) (char->string (integer->char ascii))))

;;;; Pattern Dispatch

(define-integrable (compile-pattern-char ctx)
  (input! ctx)
  ((vector-ref pattern-chars (input-peek-1 ctx)) ctx))

(define (premature-end ctx)
  (compilation-error ctx "Premature end of regular expression"))

(define (normal-char ctx)
  (if (if (input-end? ctx)
	  (not (pending-exact ctx))
	  (input-match? (input-peek ctx) #\* #\+ #\? #\^))
      (begin
	(output-start! ctx re-code:exact-1)
	(output! ctx (input-peek-1 ctx)))
      (begin
	(if (or (not (pending-exact ctx))
		(fix:= (pointer-ref (pending-exact ctx)) #x7F))
	    (begin
	      (set-last-start! ctx (output-pointer ctx))
	      (output! ctx re-code:exact-n)
	      (set-pending-exact! ctx (output-pointer ctx))
	      (output! ctx 0)))
	(output! ctx (input-peek-1 ctx))
	(pointer-operate! (pending-exact ctx) 1+))))

(define (define-pattern-char char procedure)
  (vector-set! pattern-chars (char->integer char) procedure)
  unspecific)

(define pattern-chars
  (make-vector #x100 normal-char))

(define-pattern-char #\\
  (lambda (ctx)
    (if (input-end? ctx)
	(premature-end ctx)
	(begin
	  (input-raw! ctx)
	  ((vector-ref backslash-chars (input-peek-1 ctx)) ctx)))))

(define (define-backslash-char char procedure)
  (vector-set! backslash-chars (char->integer char) procedure)
  unspecific)

(define backslash-chars
  (make-vector #x100 normal-char))

(define-pattern-char #\$
  ;; $ means succeed if at end of line, but only in special contexts.
  ;; If randomly in the middle of a pattern, it is a normal character.
  (lambda (ctx)
    (if (or (input-end? ctx)
	    (input-end+1? ctx)
	    (and (input-match? (input-peek ctx) #\\)
		 (input-match? (input-peek+1 ctx) #\) #\|)))
	(output-re-code! ctx re-code:line-end)
	(normal-char ctx))))

(define-pattern-char #\^
  ;; ^ means succeed if at beginning of line, but only if no preceding
  ;; pattern.
  (lambda (ctx)
    (if (not (last-start ctx))
	(output-re-code! ctx re-code:line-start)
	(normal-char ctx))))

(define-pattern-char #\.
  (lambda (ctx)
    (output-start! ctx re-code:any-char)))

(define (define-trivial-backslash-char char code)
  (define-backslash-char char
    (lambda (ctx)
      (output-re-code! ctx code))))

(define-trivial-backslash-char #\< re-code:word-start)
(define-trivial-backslash-char #\> re-code:word-end)
(define-trivial-backslash-char #\b re-code:word-bound)
(define-trivial-backslash-char #\B re-code:not-word-bound)
(define-trivial-backslash-char #\` re-code:buffer-start)
(define-trivial-backslash-char #\' re-code:buffer-end)

(define (define-starter-backslash-char char code)
  (define-backslash-char char
    (lambda (ctx)
      (output-start! ctx code))))

(define-starter-backslash-char #\w re-code:word-char)
(define-starter-backslash-char #\W re-code:not-word-char)

(define-backslash-char #\s
  (lambda (ctx)
    (output-start! ctx re-code:syntax-spec)
    (output! ctx (ascii->syntax-entry (input-read! ctx)))))

(define-backslash-char #\S
  (lambda (ctx)
    (output-start! ctx re-code:not-syntax-spec)
    (output! ctx (ascii->syntax-entry (input-read! ctx)))))

;;;; Repeaters

(define (define-repeater-char char zero? many?)
  (define-pattern-char char
    ;; If there is no previous pattern, char not special.
    (lambda (ctx)
      (if (not (last-start ctx))
	  (normal-char ctx)
	  (repeater-loop ctx zero? many?)))))

(define (repeater-loop ctx zero? many?)
  ;; If there is a sequence of repetition chars, collapse it down to
  ;; equivalent to just one.
  (cond ((input-end? ctx)
	 (repeater-finish ctx zero? many?))
	((input-match? (input-peek ctx) #\*)
	 (input-discard! ctx)
	 (repeater-loop ctx zero? many?))
	((input-match? (input-peek ctx) #\+)
	 (input-discard! ctx)
	 (repeater-loop ctx #f many?))
	((input-match? (input-peek ctx) #\?)
	 (input-discard! ctx)
	 (repeater-loop ctx zero? #f))
	(else
	 (repeater-finish ctx zero? many?))))

(define (repeater-finish ctx zero? many?)
  (if many?
      ;; More than one repetition allowed: put in a backward jump at
      ;; the end.
      (compute-jump (output-position ctx)
		    (fix:- (pointer-position (last-start ctx)) 3)
	(lambda (low high)
	  (output-re-code! ctx re-code:maybe-finalize-jump)
	  (output! ctx low)
	  (output! ctx high))))
  (insert-jump! ctx
		(last-start ctx)
		re-code:on-failure-jump
		(fix:+ (output-position ctx) 3))
  (if (not zero?)
      ;; At least one repetition required: insert before the loop a
      ;; skip over the initial on-failure-jump instruction.
      (insert-jump! ctx
		    (last-start ctx)
		    re-code:dummy-failure-jump
		    (fix:+ (pointer-position (last-start ctx)) 6))))

(define-repeater-char #\* #t #t)
(define-repeater-char #\+ #f #t)
(define-repeater-char #\? #t #f)

;;;; Character Sets

(define-pattern-char #\[
  (lambda (ctx)
    (if (input-end? ctx)
	(premature-end ctx))
    (let ((invert?
	   (and (input-match? (input-peek ctx) #\^)
		(begin (input-discard! ctx) #t)))
	  (bitmap (make-bytevector #x20 0)))
      (if (input-end? ctx)
	  (premature-end ctx))
      (let loop
	  ((chars
	    (if (input-match? (input-peek ctx) #\])
		(begin
		  (input-discard! ctx)
		  (list (char->integer #\])))
		'())))
	(if (input-end? ctx)
	    (premature-end ctx))
	(let ((char (input-read! ctx)))
	  (if (input-match? char #\])
	      (let ((charset
		     (re-compile-char-set
		      (list->string (map integer->char (reverse! chars)))
		      #f)))
		(do ((i 0 (fix:+ i 1)))
		    ((not (fix:< i #x100)))
		  (if (code-point-in-char-set? i charset)
		      ((ucode-primitive re-char-set-adjoin!) bitmap i))))
	      (loop (cons char chars)))))
      (output-start! ctx (if invert? re-code:not-char-set re-code:char-set))
      (let ((end
	     ;; Discard any bitmap bytes that are all 0 at the end of the map.
	     (let loop ((i #x20))
	       (if (and (fix:> i 0)
			(fix:= 0 (bytevector-u8-ref bitmap (fix:- i 1))))
		   (loop (fix:- i 1))
		   i))))
	(output! ctx end)
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i end)))
	  (output! ctx (bytevector-u8-ref bitmap i)))))))

;;;; Alternative Groups

(define-backslash-char #\(
  (lambda (ctx)
    (if (stack-full? ctx)
	(compilation-error ctx "Nesting too deep"))
    (if (fix:< (register-number ctx) re-number-of-registers)
	(begin
	  (output-re-code! ctx re-code:start-memory)
	  (output! ctx (register-number ctx))))
    (stack-push! ctx
		 (output-pointer ctx)
		 (fixup-jump ctx)
		 (register-number ctx)
		 (begin-alternative ctx))
    (set-last-start! ctx #f)
    (set-fixup-jump! ctx #f)
    (set-register-number! ctx (fix:1+ (register-number ctx)))
    (set-begin-alternative! ctx (output-pointer ctx))
    unspecific))

(define-backslash-char #\)
  (lambda (ctx)
    (if (stack-empty? ctx)
	(compilation-error ctx "Unmatched close paren"))
    (if (fixup-jump ctx)
	(store-jump! (fixup-jump ctx) re-code:jump (output-position ctx)))
    (stack-pop!
     ctx
     (lambda (op fj rn bg)
       (set-last-start! ctx op)
       (set-fixup-jump! ctx fj)
       (set-begin-alternative! ctx bg)
       (if (fix:< rn re-number-of-registers)
	   (begin
	     (output-re-code! ctx re-code:stop-memory)
	     (output! ctx rn)))))))

(define-backslash-char #\|
  (lambda (ctx)
    (insert-jump! ctx
		  (begin-alternative ctx)
		  re-code:on-failure-jump
		  (fix:+ (output-position ctx) 6))
    (if (fixup-jump ctx)
	(store-jump! (fixup-jump ctx) re-code:jump (output-position ctx)))
    (set-fixup-jump! ctx (output-pointer ctx))
    (output! ctx re-code:unused)
    (output! ctx re-code:unused)
    (output! ctx re-code:unused)
    (set-pending-exact! ctx #f)
    (set-last-start! ctx #f)
    (set-begin-alternative! ctx (output-pointer ctx))
    unspecific))

(define (define-digit-char digit)
  (let ((char (digit->char digit)))
    (define-backslash-char char
      (lambda (ctx)
	(if (fix:< digit (register-number ctx))
	    (let ((n (stack-length ctx)))
	      (let search-stack ((i 0))
		(cond ((not (fix:< i n))
		       (output-start! ctx re-code:duplicate)
		       (output! ctx digit))
		      ((fix:= (stack-ref-register-number ctx i) digit)
		       (normal-char ctx))
		      (else
		       (search-stack (fix:1+ i))))))
	    (normal-char ctx))))))

(for-each define-digit-char '(1 2 3 4 5 6 7 8 9))

;;;; Compiled Pattern Disassembler

(define (re-disassemble-pattern compiled-pattern #!optional output)
  (let ((output (if (default-object? output) (current-output-port) output))
	(input
	 (open-input-bytevector
	  (compiled-regexp/byte-stream compiled-pattern))))

    (define (get-byte)
      (let ((byte (read-u8 input)))
	(if (eof-object? byte)
	    (error "Premature pattern end."))
	byte))

    (define (write-hex-byte byte)
      (write-char (digit->char (fix:lsh byte -4) 16) output)
      (write-char (digit->char (fix:and byte #x0F) 16) output))

    (define (loop address)
      (write address output)
      (write-char #\space output)
      (let ((byte (read-u8 input)))
	(if (eof-object? byte)
	    (begin
	      (write-string "(end)" output)
	      (newline output))
	    (let ((address (fix:+ address 1))
		  (name (re-code-name byte)))
	      (write-char #\( output)
	      (write (or name 'unknown) output)
	      (let ((consumed
		     (if name
			 (known-code address name)
			 (begin
			   (write-string " #x" output)
			   (write-hex-byte byte)
			   0))))
		(write-char #\) output)
		(newline output)
		(loop (fix:+ address consumed)))))))

    (define (known-code address name)
      (case name
	((unused line-start line-end any-char buffer-start buffer-end
		 word-char not-word-char word-start word-end word-bound
		 not-word-bound)
	 0)
	((exact-1)
	 (write-char #\space output)
	 (write (string (integer->char (get-byte))) output)
	 1)
	((exact-n)
	 (write-char #\space output)
	 (let ((n (get-byte))
	       (sbuilder (string-builder)))
	   (do ((i 0 (fix:+ i 1)))
	       ((not (fix:< i 1)))
	     (sbuilder (integer->char (get-byte))))
	   (write (sbuilder) output)
	   (fix:+ 1 n)))
	((jump on-failure-jump maybe-finalize-jump dummy-failure-jump)
	 (write-char #\space output)
	 (write (fix:+ (fix:+ address 2)
		       (let* ((b1 (get-byte))
			      (b2 (get-byte))
			      (offset (fix:or b1 (fix:lsh b2 8))))
			 (if (fix:< offset #x8000)
			     offset
			     (fix:- offset #x10000))))
		output)
	 2)
	((char-set not-char-set)
	 (let ((n (get-byte)))
	   (do ((i 0 (fix:+ i 1)))
	       ((not (fix:< i n)))
	     (write-char #\space output)
	     (write-hex-byte (get-byte)))
	   (fix:+ 1 n)))
	((start-memory stop-memory duplicate)
	 (write-char #\space output)
	 (write (get-byte) output)
	 1)
	((syntax-spec not-syntax-spec)
	 (write-char #\space output)
	 (write (string-ref " .w_()'\"$\\/<>" (get-byte))
		output)
	 1)
	(else
	 (error "Unknown code name:" name))))

    (loop 0)))