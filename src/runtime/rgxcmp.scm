#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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
       `(BEGIN
	  ,@(let loop ((n 0) (suffixes suffixes))
	      (if (pair? suffixes)
		  (cons `(DEFINE-INTEGRABLE
			   ,(symbol prefix (car suffixes))
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
    (cons* 'CACHE (make-thread-mutex) items)))

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
  (let ((result (make-legacy-string 2)))
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
		     (make-legacy-string
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
			 (string-copy! result (fix:+ p 2) string i (fix:+ i n))
			 (make-compiled-regexp result case-fold?))
			(else
			 (vector-8b-set! result p re-code:exact-n)
			 (vector-8b-set! result (fix:1+ p) 255)
			 (let ((j (fix:+ i 255)))
			   (string-copy! result (fix:+ p 2) string i j)
			   (loop (fix:- n 255) j (fix:+ p 257)))))))))))))

(define char-set:re-special
  (char-set #\[ #\] #\* #\. #\\ #\? #\+ #\^ #\$))

(define (re-quote-string string)
  (let ((end (string-length string)))
    (let ((n
	   (let loop ((start 0) (n 0))
	     (let ((index
		    (string-find-next-char-in-set string char-set:re-special
						  start end)))
	       (if index
		   (loop (1+ index) (1+ n))
		   n)))))
      (if (zero? n)
	  string
	  (let ((result (make-legacy-string (+ end n))))
	    (let loop ((start 0) (i 0))
	      (let ((index
		     (string-find-next-char-in-set string char-set:re-special
						   start end)))
		(if index
		    (begin
		      (string-copy! result i string start index)
		      (let ((i (+ i (- index start))))
			(string-set! result i #\\)
			(string-set! result
				     (1+ i)
				     (string-ref string index))
			(loop (1+ index) (+ i 2))))
		    (string-copy! result i string start end))))
	    result)))))

;;;; Char-Set Compiler

;;; Special characters:
;;; #\] must appear as first character.
;;; #\- must appear as first or last character, or it may appear
;;;     immediately after a range.
;;; #\^ must appear anywhere except as the first character in the set.

(define (re-compile-char-set pattern negate?)
  (receive (scalar-values negate?*)
      (re-char-pattern->scalar-values pattern)
    (let ((char-set (char-set* scalar-values)))
      (if (if negate? (not negate?*) negate?*)
	  (char-set-invert char-set)
	  char-set))))

(define (re-char-pattern->scalar-values pattern)
  (define (loop pattern scalar-values)
    (if (pair? pattern)
	(if (and (pair? (cdr pattern))
		 (char=? (cadr pattern) #\-)
		 (pair? (cddr pattern)))
	    (loop (cdddr pattern)
		  (cons (cons (char->integer (car pattern))
			      (fix:+ (char->integer (caddr pattern)) 1))
			scalar-values))
	    (loop (cdr pattern)
		  (cons (char->integer (car pattern))
			scalar-values)))
	scalar-values))

  (let ((pattern (string->list pattern)))
    (if (and (pair? pattern)
	     (char=? (car pattern) #\^))
	(values (loop (cdr pattern) '()) #t)
	(values (loop pattern '()) #f))))

;;;; Translation Tables

(define re-translation-table
  (let ((normal-table (make-legacy-string 256)))
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
      '(COMPILATION-CONTEXT MESSAGE)
    (lambda (condition port)
      (write-string "Error compiling regular expression: " port)
      (write-string (access-condition condition 'MESSAGE) port))))

(define compilation-error
  (condition-signaller condition-type:re-compile-pattern
		       '(COMPILATION-CONTEXT MESSAGE)
		       standard-error-handler))

(define-structure (compiled-regexp
		   (constructor %make-compiled-regexp)
		   (conc-name compiled-regexp/))
  (byte-stream #f read-only #t)
  (translation-table #f read-only #t))

(define (make-compiled-regexp byte-stream case-fold?)
  (%make-compiled-regexp byte-stream (re-translation-table case-fold?)))

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
      (let* ((output (list 'OUTPUT))
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
		 (list->string (map integer->char (cdr (output-head ctx))))
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
  (vector-8b-ref (translation-table ctx) (car (input-list ctx))))

(define-integrable (input-peek+1 ctx)
  (vector-8b-ref (translation-table ctx) (cadr (input-list ctx))))

(define-integrable (input-discard! ctx)
  (let ((c ctx))
    (set-input-list! c (cdr (input-list c))))
  unspecific)

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
  (make-vector 256 normal-char))

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
  (make-vector 256 normal-char))

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
	  (charset (make-legacy-string 32 (integer->char 0))))
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
	      (begin
		(for-each
		 (lambda (char)
		   ((ucode-primitive re-char-set-adjoin!) charset
							  (char->integer char)))
		 (char-set-members
		  (re-compile-char-set
		   (list->string (map integer->char (reverse! chars)))
		   #f))))
	      (loop (cons char chars)))))
      (output-start! ctx (if invert? re-code:not-char-set re-code:char-set))
      ;; Discard any bitmap bytes that are all 0 at the end of
      ;; the map.  Decrement the map-length byte too.
      (let loop ((n 31))
	(cond ((not (fix:= 0 (vector-8b-ref charset n)))
	       (output! ctx (fix:+ n 1))
	       (let loop ((i 0))
		 (output! ctx (vector-8b-ref charset i))
		 (if (fix:< i n)
		     (loop (fix:+ i 1)))))
	      ((fix:= 0 n)
	       (output! ctx 0))
	      (else
	       (loop (fix:- n 1))))))))

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

(define (re-disassemble-pattern compiled-pattern)
  (let* ((bytes (compiled-regexp/byte-stream compiled-pattern))
	 (n (string-length bytes)))
    (let loop ((i 0))
      (newline)
      (write i)
      (write-string " (")
      (if (< i n)
	  (case (let ((re-code-name
		       (vector-ref re-codes (vector-8b-ref bytes i))))
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
	       (write (substring bytes (1+ i) end))
	       (write-string ")")
	       (loop end)))
	    ((EXACT-N)
	     (write-string " ")
	     (let ((start (+ i 2))
		   (n (vector-8b-ref bytes (1+ i))))
	       (let ((end (+ start n)))
		 (write (substring bytes start end))
		 (write-string ")")
		 (loop end))))
	    ((JUMP ON-FAILURE-JUMP MAYBE-FINALIZE-JUMP DUMMY-FAILURE-JUMP)
	     (write-string " ")
	     (let ((end (+ i 3))
		   (offset
		    (+ (* 256 (vector-8b-ref bytes (+ i 2)))
		       (vector-8b-ref bytes (1+ i)))))
	       (write (+ end (if (< offset #x8000) offset (- offset #x10000))))
	       (write-string ")")
	       (loop end)))
	    ((CHAR-SET NOT-CHAR-SET)
	     (let ((end (+ (+ i 2) (vector-8b-ref bytes (1+ i)))))
	       (let spit ((i (+ i 2)))
		 (if (< i end)
		     (begin
		       (write-string " ")
		       (let ((n (vector-8b-ref bytes i)))
			 (if (< n 16) (write-char #\0))
			 (write-string (number->string n 16)))
		       (spit (1+ i)))
		     (begin
		       (write-string ")")
		       (loop i))))))
	    ((START-MEMORY STOP-MEMORY DUPLICATE)
	     (write-string " ")
	     (write (vector-8b-ref bytes (1+ i)))
	     (write-string ")")
	     (loop (+ i 2)))
	    ((SYNTAX-SPEC NOT-SYNTAX-SPEC)
	     (write-string " ")
	     (write (string-ref " .w_()'\"$\\/<>"
				(vector-8b-ref bytes (1+ i))))
	     (write-string ")")
	     (loop (+ i 2))))
	  (begin
	    (write 'end)
	    (write-string ")"))))))