#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

;;; SRFI 180: JSON

;;; Parts loosely translated from SRFI sample implementation.

(define-record-type <json-error>
    (make-json-error r7rs-error-message r7rs-error-irritants)
    json-error?
  (r7rs-error-message json-error-message)
  (r7rs-error-irritants json-error-irritants))

(define (json-error-reason je)
  (let ((port (open-output-string)))
    (write-string (json-error-message je) port)
    (for-each (lambda (irritant)
                (write-char #\space port)
                (write irritant port))
              (json-error-irritants je))
    (get-output-string)))

(define (json-error message . irritants)
  (raise (make-json-error message irritants)))

(define (json-null? object)
  (eq? 'null object))

(define json-number-of-character-limit
  (make-parameter fx-greatest))

(define json-nesting-depth-limit
  (make-parameter fx-greatest))

(define-syntax std-input-proc
  (syntax-rules ()
    ((_ proc arg ... (var ...))
     (case-lambda
       ((var ...)
        (proc arg ... var ... (port->generator (current-input-port))))
       ((var ... port-or-generator)
        (proc arg ... var ... (convert-input-porg port-or-generator)))))))

(define (convert-input-porg port-or-generator)
  (if (and (input-port? port-or-generator)
           (textual-port? port-or-generator))
      (port->generator port-or-generator)
      port-or-generator))

(define (port->generator port)
  (lambda ()
    (read-char port)))

(define-syntax std-output-proc
  (syntax-rules ()
    ((_ proc arg ... (var ...))
     (case-lambda
       ((var ...)
        (proc arg ... var ... (port->accumulator (current-output-port))))
       ((var ... port-or-accuulator)
        (proc arg ... var ... (convert-output-pora port-or-accuulator)))))))

(define (convert-output-pora port-or-accumulator)
  (if (and (output-port? port-or-accumulator)
           (textual-port? port-or-accumulator))
      (port->accumulator port-or-accumulator)
      port-or-accumulator))

(define (port->accumulator port)
  (lambda (x)
    (cond ((char? x) (write-char x port))
          ((string? x) (write-string x port))
          ((eof-object? x) (close-output-port port))
          (else (json-error "Invalid accumulator value:" x)))))

(define json-generator
  (std-input-proc %json-generator 'normal ()))

(define json-generator-trace?
  (make-parameter #f))

(define (%json-generator read-mode g)
  (let ((g (%token-generator g read-mode))
        (limit
         (let ((limit (json-nesting-depth-limit)))
           (if (fixnum? limit)
               limit
               (json-error "Invalid nesting-depth limit:" limit))))
        (trace? (json-generator-trace?))
        (state 'initial)
        (state-stack '())
        (depth 0))

    (define-syntax define-state
      (syntax-rules ()
        ((_ (name token) transitions ...)
         (define (name token)
           (if trace?
               (begin
                 (write (list state token state-stack))
                 (newline)))
           (let ((tname (token-name token)))
             (case tname
               transitions ...
               (else (json-error "Invalid token in state:" tname state))))))))

    (define-state (initial token)
      ((array-start object-start) (start-struct! token 'final))
      ((string non-string eof) token))

    (define-state (array-first token)
      ((array-end) (end-struct! token))
      ((array-start object-start) (start-struct! token 'array-comma))
      ((string non-string) (cont! 'array-comma token)))

    (define-state (array-comma token)
      ((array-end) (end-struct! token))
      ((comma) (cont-no-emit! 'array-next)))

    (define-state (array-next token)
      ((array-start object-start) (start-struct! token 'array-comma))
      ((string non-string) (cont! 'array-comma token)))

    (define-state (object-first token)
      ((object-end) (end-struct! token))
      ((string) (cont! 'object-colon token)))

    (define-state (object-colon token)
      ((colon) (cont-no-emit! 'object-value)))

    (define-state (object-value token)
      ((array-start object-start) (start-struct! token 'object-comma))
      ((string non-string) (cont! 'object-comma token)))

    (define-state (object-comma token)
      ((object-end) (end-struct! token))
      ((comma) (cont-no-emit! 'object-next)))

    (define-state (object-next token)
      ((string) (cont! 'object-colon token)))

    (define-state (final token)
      ((eof) token))

    (define (start-struct! token k-state)
      (if (not (fx<? depth limit))
          (json-error "Nesting-depth limit reached."))
      (set! depth (fx+ depth 1))
      (set! state (if (eq? 'array-start token) 'array-first 'object-first))
      (set! state-stack (cons k-state state-stack))
      token)

    (define (end-struct! token)
      (if (not (fx>? depth 0))
          (json-error "State stack underflow."))
      (set! depth (fx- depth 1))
      (set! state (car state-stack))
      (set! state-stack (cdr state-stack))
      token)

    (define (cont! new-state token)
      (set! state new-state)
      token)

    (define (cont-no-emit! new-state)
      (set! state new-state)
      (dispatch))

    (define (dispatch)
      (case state
        ((initial) (initial (g)))
        ((array-first) (array-first (g)))
        ((array-comma) (array-comma (g)))
        ((array-next) (array-next (g)))
        ((object-first) (object-first (g)))
        ((object-colon) (object-colon (g)))
        ((object-value) (object-value (g)))
        ((object-comma) (object-comma (g)))
        ((object-next) (object-next (g)))
        ((final) (final (g)))
        (else (error "Unknown state:" state))))

    dispatch))

(define (%token-generator g read-mode)
  (let ((limit
         (let ((limit (json-number-of-character-limit)))
           (if (fixnum? limit)
               limit
               (json-error "Invalid character limit:" limit))))
        (ws
         (case read-mode
           ((normal sequence) (char-set #\tab #\return #\space #\linefeed))
           ((lines) (char-set #\tab #\return #\space))
           (else (error "Unknown read-mode" read-mode))))
        (count 0)
        (pushed #f))

    (define g*
      (case-lambda
        (()
         (if pushed
             (let ((c pushed))
               (set! pushed #f)
               c)
             (begin
               (if (fx>=? count limit)
                   (json-error "Character limit reached."))
               (let ((c (g)))
                 (if (char? c)
                     (set! count (fx+ count 1)))
                 c))))
        ((c)
         (if pushed
             (error "Can't push back twice:" pushed c))
         (set! pushed c))))

    (lambda ()
      (let ((c
             (let loop ()
               (let ((c (g*)))
                 (if (and (char? c)
                          (char-set-contains? ws c))
                     (loop)
                     c)))))
        (if (char? c)
            (case c
              ((#\{) 'object-start)
              ((#\}) 'object-end)
              ((#\[) 'array-start)
              ((#\]) 'array-end)
              ((#\:) 'colon)
              ((#\,) 'comma)
              ((#\") (read-string g*))
              ((#\-) (g* c) (read-number g*))
              ((#\t) (read-true g*))
              ((#\f) (read-false g*))
              ((#\n) (read-null g*))
              (else
               (cond ((digit? c)
                      (g* c)
                      (read-number g*))
                     ((or (and (eqv? #\linefeed c) (eq? read-mode 'lines))
                          (and (eqv? #\x1E c) (eq? read-mode 'sequence)))
                      (eof-object))
                     (else
                      (json-error "Invalid JSON initial character:" c)))))
            c)))))

(define (token-name token)
  (cond ((memq token '(array-start array-end object-start object-end
                                   comma colon))
         token)
        ((eq? 'null token) 'non-string)
        ((boolean? token) 'non-string)
        ((string? token) 'string)
        ((real? token) 'non-string)
        ((eof-object? token) 'eof)
        (else (error "Invalid token:" token))))

(define (read-string g)
  ;; expects #\" to have been seen
  (let ((acc (string-accumulator)))

    (define (loop)
      (let ((c (g)))
        (if (or (eof-object? c)
                (fx<? (char->integer c) #x20))
            (json-error "Expected JSON string char:" c))
        (case c
          ((#\") (acc (eof-object)))
          ((#\\) (acc (read-quoted)) (loop))
          (else (acc c) (loop)))))

    (define (read-quoted)
      (let ((c (g)))
        (case c
          ((#\" #\\ #\/) c)
          ((#\b) #\backspace)
          ((#\f) #\formfeed)
          ((#\n) #\linefeed)
          ((#\r) #\return)
          ((#\t) #\tab)
          ((#\u) (read-unicode))
          (else (json-error "Expected JSON string escape:" c)))))

    (define (read-unicode)
      (integer->char
       (let ((cp (read-unicode-cp)))
         (if (fx=? #xDC00 (fxand #xFC00 cp))
             (json-error "Expected Unicode high surrogate:" cp))
         (if (fx=? #xD800 (fxand #xFC00 cp))
             (begin
               (let* ((c1 (g))
                      (c2 (g)))
                 (if (not (and (eqv? #\\ c1) (eqv? #\u c2)))
                     (json-error "Expected \\u but got:" c1 c2)))
               (let ((cp2 (read-unicode-cp)))
                 (if (not (fx=? #xDC00 (fxand #xFC00 cp2)))
                     (json-error "Expected Unicode low surrogate:" cp2))
                 (fx+ (fxior (fxarithmetic-shift (fxand cp #x3FF) 10)
                             (fxand cp2 #x3FF))
                      #x10000)))
             cp))))

    (define (read-unicode-cp)
      (let* ((d1 (read-hex))
             (d2 (read-hex))
             (d3 (read-hex))
             (d4 (read-hex)))
        (fxior (fxior (fxarithmetic-shift d1 12)
                      (fxarithmetic-shift d2 8))
               (fxior (fxarithmetic-shift d3 4)
                      d4))))

    (define (read-hex)
      (let ((c (g)))
        (if (not (char? c))
            (json-error "Expected hex digit:" c))
        (let ((n (char->integer c)))
          (cond ((and (fx>=? n #x30)
                      (fx<=? n #x39))
                 (fx- n #x30))
                ((and (fx>=? n #x41)
                      (fx<=? n #x46))
                 (fx- n (fx- #x41 10)))
                ((and (fx>=? n #x61)
                      (fx<=? n #x66))
                 (fx- n (fx- #x61 10)))
                (else
                 (json-error "Expected hex digit:" c))))))

    (loop)))

(define (read-number g)
  (let ((acc (string-accumulator)))
    (let loop ()
      (let ((c (g)))
        (if (and (char? c)
                 (char-set-contains? number-chars c))
            (begin
              (acc c)
              (loop))
            (g c))))
    (let ((s (acc (eof-object))))
      (if (not (regexp-matches? number-regexp s))
          (json-error "Invalid number syntax:" s))
      (let ((x (string->number s)))
        (if (not x)
            (json-error "Invalid number syntax:" s))
        x))))

(define (digit? c)
  (and (char? c)
       (char-set-contains? digit-chars c)))

(define digit-chars
  (string->char-set "0123456789"))

(define number-chars
  (char-set-union digit-chars (string->char-set "+-.eE")))

(define number-regexp
  ;; based on https://stackoverflow.com/a/13340826/140837
  (regexp
   `(w/ascii bos
             (? #\-)
             (or #\0
                 (: (- numeric #\0)
                    (* numeric)))
             (? (: #\. (+ numeric)))
             (? (: (or #\e #\E)
                   (? (or #\- #\+))
                   (** 1 3 numeric)))
             eos)))

(define (read-true g)
  (expect-char #\r (g))
  (expect-char #\u (g))
  (expect-char #\e (g))
  #t)

(define (read-false g)
  (expect-char #\a (g))
  (expect-char #\l (g))
  (expect-char #\s (g))
  (expect-char #\e (g))
  #f)

(define (read-null g)
  (expect-char #\u (g))
  (expect-char #\l (g))
  (expect-char #\l (g))
  'null)

(define (expect-char expected c)
  (if (not (eqv? expected c))
      (json-error (string-append "Expected " (string expected) " but got:") c))
  c)

(define json-fold
  (std-input-proc %json-fold 'normal (proc a-start a-end o-start o-end seed)))

(define json-fold-trace?
  (make-parameter #f))

(define (%json-fold read-mode proc a-start a-end o-start o-end seed g)
  (let ((g (%json-generator read-mode g))
        (trace? (json-fold-trace?)))

    (define (loop seed seed-stack)
      (let ((token (g)))
        (if trace?
            (begin
              (write (list token seed seed-stack))
              (newline)))
        (if (eof-object? token)
            seed
            (case token
              ((array-start) (loop (a-start seed) (cons seed seed-stack)))
              ((object-start) (loop (o-start seed) (cons seed seed-stack)))
              ((array-end) (end-struct (a-end seed) seed-stack))
              ((object-end) (end-struct (o-end seed) seed-stack))
              (else (loop (proc token seed) seed-stack))))))

    (define (end-struct seed seed-stack)
      (loop (proc seed (car seed-stack))
            (cdr seed-stack)))

    (loop seed '())))

(define json-read
  (std-input-proc %json-read 'normal ()))

(define json-lines-read
  (std-input-proc %json-read 'lines ()))

(define json-sequence-read
  (std-input-proc %json-read 'sequence ()))

(define (%json-read read-mode g)
  (let* ((null-seed (list 'null-seed))
	 (final
          (%json-fold read-mode
                      (lambda (token seed)
                        (if (eq? null-seed seed)
                            token
                            (cons token seed)))
                      (lambda (seed) seed '())
                      (lambda (seed)
                        (list->vector (reverse seed)))
                      (lambda (seed) seed '())
                      (lambda (seed)
                        (let loop ((seed seed) (result '()))
                          (if (pair? seed)
                              (loop (cddr seed)
                                    (cons (cons (string->symbol (cadr seed))
                                                (car seed))
                                          result))
                              result)))
                      null-seed
                      g)))
    (if (eq? null-seed final)
        (json-error "Premature EOF."))
    final))

(define json-accumulator
  (std-output-proc %json-accumulator #f ()))

(define json-accumulator-trace?
  (make-parameter #f))

(define (%json-accumulator indent-allowed? a)
  (let ((state 'initial)
        (state-stack '())
	(indent #f))

    (define (dispatch token)
      (case state
	((initial) (initial token))
	((array-first) (array-first token))
	((array-next) (array-next token))
	((object-first) (object-first token))
	((object-value) (object-value token))
	((object-next) (object-next token))
	((final) (final token))
	(else (error "Unknown state:" state))))

    (define-syntax define-state
      (syntax-rules ()
        ((_ (name token) transitions ...)
         (define (name token)
	   (maybe-trace token)
           (let ((tname (token-name token)))
             (case tname
               transitions ...
               (else (json-error "Invalid token in state:" tname state))))))))

    (define-state (initial token)
      ((array-start object-start) (start-struct! token 'final))
      ((string non-string) (do-atom! token 'final)))

    (define-state (array-first token)
      ((array-end) (end-struct! token))
      ((array-start object-start) (start-struct! token 'array-next))
      ((string non-string) (do-atom! token 'array-next)))

    (define-state (array-next token)
      ((array-end) (end-struct! token))
      ((array-start object-start) (comma) (start-struct! token 'array-next))
      ((string non-string) (comma) (do-atom! token 'array-next)))

    (define-state (object-first token)
      ((object-end) (end-struct! token))
      ((string) (write-string token) (set! state 'object-value)))

    (define-state (object-value token)
      ((array-start object-start) (colon) (start-struct! token 'object-next))
      ((string non-string) (colon) (do-atom! token 'object-next)))

    (define-state (object-next token)
      ((object-end) (end-struct! token))
      ((string) (comma) (write-string token) (set! state 'object-value)))

    (define-state (final token)
      ((eof) (a token)))

    (define (start-struct! token k-state)
      (let ((start
             (lambda (opener new-state)
               (a opener)
	       (maybe-indent #f)
               (set! state new-state)
               (set! state-stack (cons k-state state-stack)))))
        (if (eq? 'array-start token)
            (start #\[ 'array-first)
            (start #\{ 'object-first))))

    (define (end-struct! token)
      (maybe-indent #f)
      (a (if (eq? 'array-end token) #\] #\}))
      (set! state (car state-stack))
      (set! state-stack (cdr state-stack)))

    (define (do-atom! token new-state)
      (write-atom token)
      (set! state new-state))

    (define (write-atom token)
      (cond ((eq? 'null token) (string-for-each a "null"))
            ((eq? #f token) (string-for-each a "false"))
            ((eq? #t token) (string-for-each a "true"))
            ((eof-object? token) (a token))
            ((string? token) (write-string token))
            ((json-number? token) (write-number token))
            (else (json-error "Invalid token:" token))))

    (define (write-string s)
      (a #\")
      (string-for-each write-char s)
      (a #\"))

    (define (write-char c)
      (case c
        ((#\") (a "\\\""))
        ((#\\) (a "\\\\"))
        ((#\backspace) (a "\\b"))
        ((#\xc) (a "\\f"))
        ((#\newline) (a "\\n"))
        ((#\return) (a "\\r"))
        ((#\tab) (a "\\t"))
        (else
         (let ((n (char->integer c)))
            (if (fx<? n #x20)
                (begin
                  (a "\\u")
                  (a (number->string n 16)))
                (a c))))))

    (define (write-number x)
      (let ((s (number->string x)))
	(string-for-each
	 (if (regexp-matches? number-regexp s)
	     a
	     (lambda (c)
	       (a c)
	       (if (eqv? #\. c)
		   (a #\0))))
	 s)))

    (define (comma)
      (a #\,)
      (maybe-indent #t))

    (define (colon)
      (a #\:)
      (maybe-indent #t))

    (define (maybe-indent or-space?)
      (cond (indent
	     (a #\newline)
	     (do ((i 0 (fx+ i 1)))
		 ((not (fx<? i indent)))
	       (a #\space)
	       (a #\space))
	     (set! indent #f))
	    (or-space?
	     (a #\space))))

    (define maybe-trace
      (if (json-accumulator-trace?)
	  (lambda (token)
	    (write (list state token state-stack))
	    (newline))
	  (lambda (token) token)))


    (if indent-allowed?
	(lambda (token)
	  ;; An indent token affects the next token read.  It's assumed that
	  ;; there won't be multiple sequential indent tokens, but if so only
	  ;; the last one has an effect.
	  (if (and (pair? token) (eq? 'indent (car token)))
	      (begin
		(maybe-trace token)
		(set! indent (cdr token)))
	      (dispatch token)))
	dispatch)))

(define json-write
  (std-output-proc %json-write (value)))

(define (%json-write value a)
  (let ((a (json-accumulator a)))
    (let loop ((value value))
      (cond ((json-atom? value)
             (a value))
            ((vector? value)
             (a 'array-start)
             (vector-for-each loop value)
             (a 'array-end))
            ((and (list? value)
                  (every (lambda (elt)
                           (and (pair? elt)
                                (symbol? (car elt))))
                         value))
             (a 'object-start)
             (for-each (lambda (elt)
                         (a (symbol->string (car elt)))
                         (loop (cdr elt)))
                       value)
             (a 'object-end))
            (else
             (json-error "Invalid JSON value:" value))))))

(define (json-atom? value)
  (or (eq? 'null value)
      (boolean? value)
      (string? value)
      (json-number? value)))

(define (json-number? value)
  (or (and (integer? value) (exact? value))
      (and (rational? value) (inexact? value))))

(define json-write-indented
  (std-output-proc %json-write-indented (value)))

(define (%json-write-indented value a)
  (let ((a (%json-accumulator #t a)))
    (let loop ((value value) (depth 0))
      (cond ((json-atom? value)
             (a value))
            ((vector? value)
	     (let ((depth* (fx+ depth 1))
		   (n (vector-length value)))
	       (cond ((fx=? n 0)
		      (a 'array-start)
		      (a 'array-end))
		     ((and (fx=? n 1)
			   (json-atom? (vector-ref value 0)))
		      (a 'array-start)
		      (a (vector-ref value 0))
		      (a 'array-end))
		     (else
		      (a (cons 'indent depth*))
		      (a 'array-start)
		      (loop (vector-ref value 0) depth*)
		      (do ((i 1 (fx+ i 1)))
			  ((not (fx<? i n)))
			(a (cons 'indent depth*))
			(loop (vector-ref value i) depth*))
		      (a (cons 'indent depth))
		      (a 'array-end)))))
            ((and (list? value)
                  (every (lambda (elt)
                           (and (pair? elt)
                                (symbol? (car elt))))
                         value))
	     (if (pair? value)
		 (let* ((depth* (fx+ depth 1))
			(write-elt
			 (lambda (elt)
			   (a (symbol->string (car elt)))
			   (loop (cdr elt) depth*))))
		   (a (cons 'indent depth*))
		   (a 'object-start)
		   (write-elt (car value))
		   (for-each (lambda (elt)
			       (a (cons 'indent depth*))
			       (write-elt elt))
			     (cdr value))
		   (a (cons 'indent depth))
		   (a 'object-end))
		 (begin
		   (a 'object-start)
		   (a 'object-end))))
            (else
             (json-error "Invalid JSON value:" value))))))