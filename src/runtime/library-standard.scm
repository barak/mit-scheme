#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; R7RS libraries: standard libraries
;;; package: (runtime library standard)

(declare (usual-integrations))

(define-deferred host-library-db
  (make-library-db 'host))

(define (finish-host-library-db!)
  (add-standard-libraries! host-library-db))

(define (add-standard-libraries! db)
  (register-libraries! (make-standard-libraries) db))

(define (make-standard-libraries)
  (map (lambda (p)
	 (let ((name (car p))
	       (exports (cdr p)))
	   (make-library name
			 'parsed-imports '()
			 'exports (map make-library-export exports)
			 'parsed-contents '()
			 'filename #f
			 'environment system-global-environment)))
       standard-libraries))

(define (check-standard-libraries!)
  (for-each (lambda (p)
	      (check-standard-library! (car p) (cdr p)))
	    standard-libraries))

(define (check-standard-library! name exports)
  (let ((missing
	 (remove (lambda (name)
		   (memq (environment-reference-type system-global-environment
						     name)
			 '(normal macro)))
		 exports)))
    (if (pair? missing)
	(warn "Missing definitions for library:" name missing))))

(define (standard-library-names)
  (map car standard-libraries))

(define (standard-library-exports name)
  (cdr (assoc name standard-libraries)))

(define (define-standard-library name exports)
  (let ((p (assoc name standard-libraries)))
    (if p
	(set-cdr! p exports)
	(begin
	  (set! standard-libraries
		(cons (cons name exports)
		      standard-libraries))
	  unspecific)))
  name)

(define standard-libraries '())

(define-standard-library '(scheme base)
  '(*
    +
    -
    ...
    /
    <
    <=
    =
    =>
    >
    >=
    _
    abs
    and
    append
    apply
    assoc
    assq
    assv
    begin
    binary-port?
    boolean=?
    boolean?
    bytevector
    bytevector-append
    bytevector-copy
    bytevector-copy!
    bytevector-length
    bytevector-u8-ref
    bytevector-u8-set!
    bytevector?
    caar
    cadr
    call-with-current-continuation
    call-with-port
    call-with-values
    call/cc
    car
    case
    cdar
    cddr
    cdr
    ceiling
    char->integer
    char-ready?
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    close-input-port
    close-output-port
    close-port
    complex?
    cond
    cond-expand
    cons
    current-error-port
    current-input-port
    current-output-port
    define
    define-record-type
    define-syntax
    define-values
    denominator
    do
    dynamic-wind
    else
    eof-object
    eof-object?
    eq?
    equal?
    eqv?
    error
    error-object-irritants
    error-object-message
    error-object?
    even?
    exact
    exact-integer-sqrt
    exact-integer?
    exact?
    expt
    features
    file-error?
    floor
    floor-quotient
    floor-remainder
    floor/
    flush-output-port
    for-each
    gcd
    get-output-bytevector
    get-output-string
    guard
    if
    include
    include-ci
    inexact
    inexact?
    input-port-open?
    input-port?
    integer->char
    integer?
    lambda
    lcm
    length
    let
    let*
    ;; let*-values
    let-syntax
    ;; let-values
    letrec
    letrec*
    letrec-syntax
    list
    list->string
    list->vector
    list-copy
    list-ref
    list-set!
    list-tail
    list?
    make-bytevector
    make-list
    make-parameter
    make-string
    make-vector
    map
    max
    member
    memq
    memv
    min
    modulo
    negative?
    newline
    not
    null?
    number->string
    number?
    numerator
    odd?
    open-input-bytevector
    open-input-string
    open-output-bytevector
    open-output-string
    or
    output-port-open?
    output-port?
    pair?
    parameterize
    peek-char
    peek-u8
    port?
    positive?
    procedure?
    quasiquote
    quote
    quotient
    raise
    raise-continuable
    rational?
    rationalize
    read-bytevector
    read-bytevector!
    read-char
    read-error?
    read-line
    read-string
    read-u8
    real?
    remainder
    reverse
    round
    set!
    set-car!
    set-cdr!
    square
    string
    string->list
    string->number
    string->symbol
    string->utf8
    string->vector
    string-append
    string-copy
    string-copy!
    string-fill!
    string-for-each
    string-length
    string-map
    string-ref
    string-set!
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    symbol->string
    symbol=?
    symbol?
    syntax-error
    syntax-rules
    textual-port?
    truncate
    truncate-quotient
    truncate-remainder
    truncate/
    u8-ready?
    unless
    unquote
    unquote-splicing
    utf8->string
    values
    vector
    vector->list
    vector->string
    vector-append
    vector-copy
    vector-copy!
    vector-fill!
    vector-for-each
    vector-length
    vector-map
    vector-ref
    vector-set!
    vector?
    when
    with-exception-handler
    write-bytevector
    write-char
    write-string
    write-u8
    zero?))

(define-standard-library '(scheme case-lambda)
  '(case-lambda))

(define-standard-library '(scheme char)
  '(char-alphabetic?
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char-downcase
    char-foldcase
    char-lower-case?
    char-numeric?
    char-upcase
    char-upper-case?
    char-whitespace?
    digit-value
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string-downcase
    string-foldcase
    string-upcase))

(define-standard-library '(scheme complex)
  '(angle
    imag-part
    magnitude
    make-polar
    make-rectangular
    real-part))

(define-standard-library '(scheme cxr)
  '(caaaar
    caaadr
    caaar
    caadar
    caaddr
    caadr
    cadaar
    cadadr
    cadar
    caddar
    cadddr
    caddr
    cdaaar
    cdaadr
    cdaar
    cdadar
    cdaddr
    cdadr
    cddaar
    cddadr
    cddar
    cdddar
    cddddr
    cdddr))

(define-standard-library '(scheme eval)
  '(environment
    eval))

(define-standard-library '(scheme file)
  '(call-with-input-file
       call-with-output-file
     delete-file
     file-exists?
     open-binary-input-file
     open-binary-output-file
     open-input-file
     open-output-file
     with-input-from-file
     with-output-to-file))

(define-standard-library '(scheme inexact)
  '(acos
    asin
    atan
    cos
    exp
    finite?
    infinite?
    log
    nan?
    sin
    sqrt
    tan))

(define-standard-library '(scheme lazy)
  '(delay
     delay-force
     force
     make-promise
     promise?))

(define-standard-library '(scheme load)
  '(load))

(define-standard-library '(scheme process-context)
  '(command-line
    emergency-exit
    exit
    get-environment-variable
    get-environment-variables))

(define-standard-library '(scheme read)
  '(read))

(define-standard-library '(scheme repl)
  '(interaction-environment))

(define-standard-library '(scheme time)
  '(current-jiffy
    current-second
    jiffies-per-second))

(define-standard-library '(scheme write)
  '(display
    write
    write-shared
    write-simple))

(define-standard-library '(scheme r5rs)
  '(*
    +
    -
    ...
    /
    <
    <=
    =
    =>
    >
    >=
    _
    abs
    acos
    and
    angle
    append
    apply
    asin
    assoc
    assq
    assv
    atan
    begin
    boolean?
    caaaar
    caaadr
    caaar
    caadar
    caaddr
    caadr
    caar
    cadaar
    cadadr
    cadar
    caddar
    cadddr
    caddr
    cadr
    call-with-current-continuation
    call-with-input-file
    call-with-output-file
    call-with-values
    car
    case
    cdaaar
    cdaadr
    cdaar
    cdadar
    cdaddr
    cdadr
    cdar
    cddaar
    cddadr
    cddar
    cdddar
    cddddr
    cdddr
    cddr
    cdr
    ceiling
    char->integer
    char-alphabetic?
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char-downcase
    char-lower-case?
    char-numeric?
    char-ready?
    char-upcase
    char-upper-case?
    char-whitespace?
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    close-input-port
    close-output-port
    complex?
    cond
    cons
    cos
    current-input-port
    current-output-port
    define
    define-syntax
    delay
    denominator
    display
    do
    dynamic-wind
    else
    eof-object?
    eq?
    equal?
    eqv?
    eval
    even?
    exact->inexact
    exact?
    exp
    expt
    floor
    for-each
    force
    gcd
    if
    imag-part
    inexact->exact
    inexact?
    input-port?
    integer->char
    integer?
    interaction-environment lambda
    lcm
    length
    let
    let*
    let-syntax
    letrec
    letrec-syntax
    list
    list->string
    list->vector
    list-ref
    list-tail
    list?
    load
    log
    magnitude
    make-polar
    make-rectangular
    make-string
    make-vector
    map
    max
    member
    memq
    memv
    min
    modulo
    negative?
    newline
    not
    ;;null-environment
    null?
    number->string
    number?
    numerator
    odd?
    open-input-file
    open-output-file
    or
    output-port?
    pair?
    peek-char
    positive?
    procedure?
    quasiquote
    quote
    quotient
    rational?
    rationalize
    read
    read-char
    real-part
    real?
    remainder
    reverse
    round
    ;;scheme-report-environment
    set!
    set-car!
    set-cdr!
    sin
    sqrt
    string
    string->list
    string->number
    string->symbol
    string-append
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string-copy
    string-fill!
    string-length
    string-ref
    string-set!
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    symbol->string
    symbol?
    syntax-rules
    tan
    truncate
    values
    vector
    vector->list
    vector-fill!
    vector-length
    vector-ref
    vector-set!
    vector?
    with-input-from-file
    with-output-to-file
    write
    write-char
    zero?))