;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Regular Expression Pattern Compiler
;;;  Translated from GNU (thank you RMS!)

(declare (usual-integrations))

;;;; Compiled Opcodes

(let-syntax ((define-enumeration
	      (macro (name prefix . suffixes)
		(define (loop n suffixes)
		  (if (null? suffixes)
		      '()
		      (cons `(DEFINE ,(string->symbol
				       (string-append prefix
						      (symbol->string
						       (car suffixes))))
			       ,n)
			    (loop (1+ n) (cdr suffixes)))))
		`(BEGIN ,@(loop 0 suffixes)
			(DEFINE ,name
			  (VECTOR ,@(map (lambda (suffix) `',suffix)
					 suffixes)))))))
  (define-enumeration re-codes "RE-CODE:"
    unused		;Zero bytes may appear in the compiled regular
			;expression.

    exact-1		;Followed by a single literal byte.

    exact-n		;Followed by one byte giving n, and then by n
			;literal bytes.

    line-start		;Fails unless at start of line.
    line-end		;Fails unless at end of line.

    jump		;Followed by two bytes giving relative address
			;to jump to.

    on-failure-jump	;Followed by two bytes giving relative address
			;of place to result at in case of failure.

    finalize-jump	;Throw away latest failure point and then jump
			;to address.

    maybe-finalize-jump	;Like jump but finalize if safe to do so.
			;This is used to jump back to the beginning of
			;a repeat.  If the command that follows this
			;jump is clearly incompatible with the one at
			;the beginning of the repeat, such that we can
			;be sure that there is no use backtracing out
			;of repetitions already completed, then we
			;finalize.

    dummy-failure-jump	;Jump, and push a dummy failure point.  This
			;failure point will be thrown away if an
			;attempt is made to use it for a failure.  A +
			;construct makes this before the first repeat.

    any-char		;Matches any one character except for newline.

    char-set		;Matches any one char belonging to specified
			;set. First following byte is # bitmap bytes.
			;Then come bytes for a bit-map saying which
			;chars are in.  Bits in each byte are ordered
			;low-bit-first.  A character is in the set if
			;its bit is 1.  A character too large to have
			;a bit in the map is automatically not in the
			;set. 

    not-char-set	;Similar but match any character that is NOT
			;one of those specified.

    start-memory	;Starts remembering the text that is matches
			;and stores it in a memory register.  Followed
			;by one byte containing the register number.
			;Register numbers must be in the range 0
			;through re-number-of-registers. 

    stop-memory		;Stops remembering the text that is matched
			;and stores it in a memory register.  Followed
			;by one byte containing the register number.
			;Register numbers must be in the range 0
			;through re-number-of-registers. 

    duplicate		;Match a duplicate of something remembered.
			;Followed by one byte containing the index of
			;the memory register.

    buffer-start	;Succeeds if at beginning of buffer.
    buffer-end		;Succeeds if at end of buffer.
    word-char		;Matches any word-constituent character.
    not-word-char	;Matches any char that is not a word-constituent.
    word-start		;Succeeds if at word beginning.
    word-end		;Succeeds if at word end.
    word-bound		;Succeeds if at a word boundary.
    not-word-bound	;Succeeds if not at a word boundary.

    syntax-spec		;Matches any character whose syntax is
			;specified.  Followed by a byte which contains
			;a syntax code.

    not-syntax-spec	;Matches any character whose syntax differs
			;from the specified.

    ))

;;;; String Compiler

(define (re-compile-char char case-fold?)
  (let ((result (string-allocate 2)))
    (vector-8b-set! result 0 re-code:exact-1)
    (string-set! result 1 (if case-fold? (char-upcase char) char))
    result))

(define (re-compile-string string case-fold?)
  (if case-fold? (set! string (string-upcase string)))
  (let ((n (string-length string)))
    (if (zero? n)
	string
	(let ((result
	       (string-allocate 
		(let ((qr (integer-divide n 255)))
		  (+ (* 257 (integer-divide-quotient qr))
		     (cond ((zero? (integer-divide-remainder qr)) 0)
			   ((= 1 (integer-divide-remainder qr)) 2)
			   (else (+ (integer-divide-remainder qr) 2))))))))
	  (define (loop n i p)
	    (cond ((= n 1)
		   (vector-8b-set! result p re-code:exact-1)
		   (vector-8b-set! result (1+ p) (vector-8b-ref string i))
		   result)
		  ((< n 256)
		   (vector-8b-set! result p re-code:exact-n)
		   (vector-8b-set! result (1+ p) n)
		   (substring-move-right! string i n result (+ p 2))
		   result)
		  (else
		   (vector-8b-set! result p re-code:exact-n)
		   (vector-8b-set! result (1+ p) 255)
		   (substring-move-right! string i 255 result (+ p 2))
		   (loop (- n 255) (+ i 255) (+ p 257)))))
	  (loop n 0 0)))))

;;;; Char-Set Compiler

(define re-compile-char-set)
(let ()

(set! re-compile-char-set
(named-lambda (re-compile-char-set pattern negate?)
  (let ((length (string-length pattern))
	(char-set (string-allocate 256)))
    (define (kernel start background foreground)
      (define (loop pattern)
	(cond ((null? pattern) 'DONE)
	      ((null? (cdr pattern)) (adjoin! (char->ascii (car pattern))))
	      ((char=? (cadr pattern) #\-)
	       (if (not (null? (cddr pattern)))
		   (begin ((adjoin-range! (char->ascii (caddr pattern)))
			   (char->ascii (car pattern)))
			  (loop (cdddr pattern)))
		   (error "RE-COMPILE-CHAR-SET: Terminating hyphen")))
	      (else
	       (adjoin! (char->ascii (car pattern)))
	       (loop (cdr pattern)))))

      (define (adjoin! ascii)
	(vector-8b-set! char-set ascii foreground))

      (define (adjoin-range! end)
	(define (adjoin-loop index)
	  (if (< index end)
	      (begin (vector-8b-set! char-set index foreground)
		     (adjoin-loop (1+ index)))))
	adjoin-loop)

      (vector-8b-fill! char-set 0 256 background)
      (loop (quote-pattern (substring->list pattern start length))))
    (if (and (not (zero? length))
	     (char=? (string-ref pattern 0) #\^))
	(if negate?
	    (kernel 1 0 1)
	    (kernel 1 1 0))
	(if negate?
	    (kernel 0 1 0)
	    (kernel 0 0 1)))
    char-set)))

(define (quote-pattern pattern)
  (cond ((null? pattern) '())
	((not (char=? (car pattern) #\\))
	 (cons (car pattern)
	       (quote-pattern (cdr pattern))))
	((null? (cdr pattern))
	 (error "RE-COMPILE-CHAR-SET: Terminating backslash"))
	(else
	 (cons (cadr pattern)
	       (quote-pattern (cddr pattern))))))

)

;;;; Translation Tables

(define re-translation-table)
(let ()

(set! re-translation-table
(named-lambda (re-translation-table case-fold?)
  (if case-fold? upcase-table normal-table)))

(define normal-table
  (make-string 256))

(let loop ((n 0))
  (if (< n 256)
      (begin (vector-8b-set! normal-table n n)
	     (loop (1+ n)))))

(define upcase-table
  (string-copy normal-table))

(let loop ((n #x61))
  (if (< n #x7B)
      (begin (vector-8b-set! upcase-table n (- n #x20))
	     (loop (1+ n)))))

)

;;;; Pattern Compiler

(define re-number-of-registers 10)
(define re-compile-pattern)
(let ()
(let-syntax ()				;capture DEFINE-MACRO inside.

(declare (integrate stack-maximum-length))

(define stack-maximum-length re-number-of-registers)

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

(set! re-compile-pattern
(named-lambda (re-compile-pattern pattern case-fold?)
  (let ((output (list 'OUTPUT)))
    (fluid-let ((input-list (map char->ascii (string->list pattern)))
		(current-byte)
		(translation-table (re-translation-table case-fold?))
		(output-head output)
		(output-tail output)
		(output-length 0)
		(stack '())
		(fixup-jump #!FALSE)
		(register-number 1)
		(begin-alternative)
		(pending-exact #!FALSE)
		(last-start #!FALSE))
      (set! begin-alternative (output-pointer))
      (compile-pattern-loop)))))

(define (compile-pattern-loop)
  (if (input-end?)
      (begin (if fixup-jump
		 (store-jump! fixup-jump re-code:jump (output-position)))
	     (if (not (stack-empty?))
		 (error "Unmatched \\("))
	     (list->string (map ascii->char (cdr output-head))))
      (begin (compile-pattern-char)
	     (compile-pattern-loop))))

;;;; Input

(declare (integrate input-end? input-end+1? input-peek input-peek+1
		    input-discard! input! input-raw! input-peek-1
		    input-read!))

(define (input-end?)
  (null? input-list))

(define (input-end+1?)
  (null? (cdr input-list)))

(define (input-peek)
  (vector-8b-ref translation-table (car input-list)))

(define (input-peek+1)
  (vector-8b-ref translation-table (cadr input-list)))

(define (input-discard!)
  (set! input-list (cdr input-list)))

(define (input!)
  (set! current-byte (input-peek))
  (input-discard!))

(define (input-raw!)
  (set! current-byte (car input-list))
  (input-discard!))

(define (input-peek-1)
  current-byte)

(define (input-read!)
  (if (input-end?)
      (premature-end)
      (let ((char (input-peek)))
	(input-discard!)
	char)))

;; Maxi-bummed.
(define-macro (input-match? byte . chars)
  (if (null? (cdr chars))
      `(EQ? ,byte ,(char->ascii (car chars)))
      `(MEMQ ,byte ',(map char->ascii chars))))

;;;; Output

(declare (integrate output! output-re-code! output-start! output-position
		    output-pointer pointer-position pointer-ref
		    pointer-operate!))

(define (output! byte)
  (declare (integrate byte))
  (let ((tail (list byte)))
    (set-cdr! output-tail tail)
    (set! output-tail tail))
  (set! output-length (1+ output-length)))

(define (output-re-code! code)
  (declare (integrate code))
  (set! pending-exact #!FALSE)
  (output! code))

(define (output-start! code)
  (declare (integrate code))
  (set! last-start (output-pointer))
  (output-re-code! code))

(define (output-position)
  output-length)

(define (output-pointer)
  (cons output-length output-tail))

(define (pointer-position pointer)
  (declare (integrate pointer))
  (car pointer))

(define (pointer-ref pointer)
  (declare (integrate pointer))
  (caddr pointer))

(define (pointer-operate! pointer operator)
  (declare (integrate pointer operator))
  (set-car! (cddr pointer)
	    (operator (caddr pointer))))

(define (store-jump! from opcode to)
  (let ((p (cddr from)))
    (set-car! p opcode)
    (compute-jump (pointer-position from) to
      (lambda (low high)
	(set-car! (cdr p) low)
	(set-car! (cddr p) high)))))

(define (insert-jump! from opcode to)
  (compute-jump (pointer-position from) to
    (lambda (low high)
      (set-cdr! (cdr from)
		(cons* opcode low high (cddr from)))
      (set! output-length (+ output-length 3)))))

(define (compute-jump from to receiver)
  (let ((n (- to (+ from 3))))
    (let ((qr (integer-divide (if (negative? n) (+ n #x10000) n)
			      #x100)))
      (receiver (integer-divide-remainder qr)
		(integer-divide-quotient qr)))))

;;;; Stack

(declare (integrate stack-empty? stack-full? stack-length
		    stack-ref-register-number))

(define (stack-empty?)
  (null? stack))

(define (stack-full?)
  (>= (stack-length) stack-maximum-length))

(define (stack-length)
  (length stack))

(define (stack-push! . args)
  (set! stack (cons args stack)))

(define (stack-pop! receiver)
  (let ((frame (car stack)))
    (set! stack (cdr stack))
    (apply receiver frame)))

(define (stack-ref-register-number i)
  (declare (integrate i))
  (caddr (list-ref stack i)))

;;; Randomness

(define (ascii->syntax-entry ascii)
  (primitive-datum (string->syntax-entry (char->string (ascii->char ascii)))))

(define string->syntax-entry
  (make-primitive-procedure 'STRING->SYNTAX-ENTRY))

;;;; Pattern Dispatch

(declare (integrate compile-pattern-char))

(define (compile-pattern-char)
  (input!)
  ((vector-ref pattern-chars (input-peek-1))))

(define (premature-end)
  (error "Premature end of regular expression"))

(define (normal-char)
  (if (if (input-end?)
	  (not pending-exact)
	  (input-match? (input-peek) #\* #\+ #\? #\^))
      (begin (output-start! re-code:exact-1)
	     (output! (input-peek-1)))
      (begin (if (or (not pending-exact)
		     (= (pointer-ref pending-exact) #x7F))
		 (begin (set! last-start (output-pointer))
			(output! re-code:exact-n)
			(set! pending-exact (output-pointer))
			(output! 0)))
	     (output! (input-peek-1))
	     (pointer-operate! pending-exact 1+))))

(define (define-pattern-char char procedure)
  (vector-set! pattern-chars (char->ascii char) procedure))

(define pattern-chars
  (make-vector 256 normal-char))

(define-pattern-char #\\
  (lambda ()
    (if (input-end?)
	(premature-end)
	(begin (input-raw!)
	       ((vector-ref backslash-chars (input-peek-1)))))))

(define (define-backslash-char char procedure)
  (vector-set! backslash-chars (char->ascii char) procedure))

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
	 (repeater-loop #!FALSE many?))
	((input-match? (input-peek) #\?)
	 (input-discard!)
	 (repeater-loop zero? #!FALSE))
	(else
	 (repeater-finish zero? many?))))

(define (repeater-finish zero? many?)
  (if many?
      ;; More than one repetition allowed: put in a backward jump at
      ;; the end.
      (compute-jump (output-position)
		    (- (pointer-position last-start) 3)
	(lambda (low high)
	  (output-re-code! re-code:maybe-finalize-jump)
	  (output! low)
	  (output! high))))
  (insert-jump! last-start
		re-code:on-failure-jump
		(+ (output-position) 3))
  (if (not zero?)
      ;; At least one repetition required: insert before the loop a
      ;; skip over the initial on-failure-jump instruction.
      (insert-jump! last-start
		    re-code:dummy-failure-jump
		    (+ (pointer-position last-start) 6))))

(define-repeater-char #\* #!TRUE #!TRUE)
(define-repeater-char #\+ #!FALSE #!TRUE)
(define-repeater-char #\? #!TRUE #!FALSE)

;;;; Character Sets

(define-pattern-char #\[
  (lambda ()
    (output-start! (cond ((input-end?) (premature-end))
			 ((input-match? (input-peek) #\^)
			  (input-discard!)
			  re-code:not-char-set)
			 (else re-code:char-set)))
    (let ((charset (string-allocate 32)))
      (define (loop)
	(cond ((input-end?) (premature-end))
	      ((input-match? (input-peek) #\])
	       (input-discard!)
	       (trim 31))
	      (else (element))))

      (define (element)
	(let ((char (input-peek)))
	  (input-discard!)
	  (cond ((input-end?) (premature-end))
		((input-match? (input-peek) #\-)
		 (input-discard!)
		 (if (input-end?)
		     (premature-end)
		     (let ((char* (input-peek)))
		       (define (loop char)
			 (if (<= char char*)
			     (begin (re-char-set-adjoin! charset char)
				    (loop (1+ char)))))
		       (input-discard!)
		       (loop char))))
		(else (re-char-set-adjoin! charset char))))
	(loop))

      ;; Discard any bitmap bytes that are all 0 at the end of
      ;; the map.  Decrement the map-length byte too.
      (define (trim n)
	(define (loop i)
	  (output! (vector-8b-ref charset i))
	  (if (< i n)
	      (loop (1+ i))))
	(cond ((not (zero? (vector-8b-ref charset n)))
	       (output! (1+ n))
	       (loop 0))
	      ((zero? n) (output! 0))
	      (else (trim (-1+ n)))))

      (vector-8b-fill! charset 0 32 0)
      (cond ((input-end?) (premature-end))
	    ((input-match? (input-peek) #\]) (element))
	    (else (loop))))))

(define re-char-set-adjoin!
  (make-primitive-procedure 'RE-CHAR-SET-ADJOIN!))

;;;; Alternative Groups

(define-backslash-char #\(
  (lambda ()
    (if (stack-full?)
	(error "Nesting too deep"))
    (if (< register-number re-number-of-registers)
	(begin (output-re-code! re-code:start-memory)
	       (output! register-number)))
    (stack-push! (output-pointer)
		 fixup-jump
		 register-number
		 begin-alternative)
    (set! last-start #!FALSE)
    (set! fixup-jump #!FALSE)
    (set! register-number (1+ register-number))
    (set! begin-alternative (output-pointer))))

(define-backslash-char #\)
  (lambda ()
    (if (stack-empty?)
	(error "Unmatched close paren"))
    (if fixup-jump
	(store-jump! fixup-jump re-code:jump (output-position)))
    (stack-pop!
     (lambda (op fj rn bg)
       (set! last-start op)
       (set! fixup-jump fj)
       (set! begin-alternative bg)
       (if (< rn re-number-of-registers)
	   (begin (output-re-code! re-code:stop-memory)
		  (output! rn)))))))

(define-backslash-char #\|
  (lambda ()
    (insert-jump! begin-alternative
		  re-code:on-failure-jump
		  (+ (output-position) 6))
    (if fixup-jump
	(store-jump! fixup-jump re-code:jump (output-position)))
    (set! fixup-jump (output-pointer))
    (output! re-code:unused)
    (output! re-code:unused)
    (output! re-code:unused)
    (set! pending-exact #!FALSE)
    (set! last-start #!FALSE)
    (set! begin-alternative (output-pointer))))

(define (define-digit-char digit)
  (let ((char (digit->char digit)))
    (define-backslash-char char
      (lambda ()
	(if (>= digit register-number)
	    (normal-char)
	    (let ((n (stack-length)))
	      (define (search-stack i)
		(if (< i n)
		    (if (= (stack-ref-register-number i) digit)
			(normal-char)
			(search-stack (1+ i)))
		    (begin (output-start! re-code:duplicate)
			   (output! digit))))
	      (search-stack 0)))))))

(for-each define-digit-char '(1 2 3 4 5 6 7 8 9))

;;; end %RE-COMPILE-PATTERN
))

;;;; Compiled Pattern Disassembler
#|
(define (re-disassemble-pattern compiled-pattern)
  (let ((n (string-length compiled-pattern)))
    (define (loop i)
      (newline)
      (write i)
      (write-string " (")
      (if (< i n)
	  (let ((re-code (vector-8b-ref compiled-pattern i)))
	    (let ((re-code-name (vector-ref re-codes re-code)))
	      (write re-code-name)
	      (case re-code-name
		((unused line-start line-end any-char
		  buffer-start buffer-end
		  word-char not-word-char word-start word-end
		  word-bound not-word-bound)
		 (write-string ")")
		 (loop (1+ i)))

		((exact-1)
		 (write-string " ")
		 (let ((end (+ i 2)))
		   (write (substring compiled-pattern (1+ i) end))
		   (write-string ")")
		   (loop end)))

		((exact-n)
		 (write-string " ")
		 (let ((start (+ i 2))
		       (n (vector-8b-ref compiled-pattern (1+ i))))
		   (let ((end (+ start n)))
		     (write (substring compiled-pattern start end))
		     (write-string ")")
		     (loop end))))

		((jump on-failure-jump maybe-finalize-jump dummy-failure-jump)
		 (write-string " ")
		 (let ((end (+ i 3))
		       (offset
			(+ (* 256 (vector-8b-ref compiled-pattern (+ i 2)))
			   (vector-8b-ref compiled-pattern (1+ i)))))
		   (write (+ end
			     (if (< offset #x8000)
				 offset
				 (- offset #x10000))))
		   (write-string ")")
		   (loop end)))

		((char-set not-char-set)
		 (let ((end (+ (+ i 2)
			       (vector-8b-ref compiled-pattern (1+ i)))))
		   (define (spit i)
		     (if (< i end)
			 (begin (write-string " ")
				(let ((n (vector-8b-ref compiled-pattern i)))
				  (if (< n 16) (write-char #\0))
				  (fluid-let ((*unparser-radix* 16))
				    (write n)))
				(spit (1+ i)))
			 (begin (write-string ")")
				(loop i))))
		   (spit (+ i 2))))

		((start-memory stop-memory duplicate)
		 (write-string " ")
		 (write (vector-8b-ref compiled-pattern (1+ i)))
		 (write-string ")")
		 (loop (+ i 2)))

		((syntax-spec not-syntax-spec)
		 (write-string " ")
		 (write (string-ref " w_()'\"$\\/<>."
				    (vector-8b-ref compiled-pattern (1+ i))))
		 (write-string ")")
		 (loop (+ i 2)))

		)))
	  (write-string "END)")))
    (loop 0)))
|#