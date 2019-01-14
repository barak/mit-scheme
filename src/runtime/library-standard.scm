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

;;;; R7RS libraries: standard libraries
;;; package: (runtime library standard)

(declare (usual-integrations))

(define-deferred host-library-db
  (make-library-db 'host))

(define-deferred current-library-db
  (make-settable-parameter host-library-db
    (lambda (value)
      (guarantee library-db? value))))

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

;; Filters the given imports to find those that are equivalent to global
;; variables, and for each one returns a pair of the "to" identifier and the
;; corresponding global identifier.  For now this is greatly simplified by
;; knowing that all standard libraries use global variables, but this will need
;; to be adapted when there are libraries that don't.
(define (standard-library-globals imports)
  (filter-map (lambda (import)
		(let ((p
		       (assoc (library-import-from-library import)
			      standard-libraries)))
		  (and p
		       (memq (library-import-from import)
			     (cdr p))
		       (cons (library-import-to import)
			     (library-import-from import)))))
	      imports))

(define (define-standard-library name exports)
  (let ((p (assoc name standard-libraries))
	(exports (extend-with-macro-deps exports)))
    (if p
	(set-cdr! p exports)
	(begin
	  (set! standard-libraries
		(cons (cons name exports)
		      standard-libraries))
	  unspecific)))
  name)

(define standard-libraries '())

;; Make sure that the names introduced by macro expansions are also included.
;; This is a kludge to work around our inability to address names globally.
(define (extend-with-macro-deps names)
  (define (scan-new to-scan new names)
    (cond ((pair? to-scan)
	   (let ((name (car to-scan))
		 (rest (cdr to-scan)))
	     (let ((p (assq name macro-dependencies)))
	       (if p
		   (scan-deps (cdr p) rest new names)
		   (scan-new rest new names)))))
	  ((pair? new)
	   (scan-new new '() names))
	  (else names)))

  (define (scan-deps deps rest new names)
    (if (pair? deps)
	(let ((dep (car deps)))
	  (if (memq dep names)
	      (scan-deps (cdr deps) rest new names)
	      (scan-deps (cdr deps)
			 rest
			 (cons dep new)
			 (cons dep names))))
	(scan-new rest new names)))

  (scan-new names '() names))

(define macro-dependencies
  '((and if)
    (and-let* and begin let)
    (assert error if not)
    (begin0 let)
    (bundle alist->bundle cons list)
    (case begin eq? eqv? if let or quote)
    (case-lambda apply default-object? error fix:= fix:>= if lambda length let)
    (circular-stream cons delay letrec)
    (cond begin if let)
    (cond-expand begin)
    (cons-stream cons delay)
    (cons-stream* cons delay)
    (define lambda named-lambda)
    (define-integrable begin lambda let set! shallow-fluid-bind)
    (define-record-type define new-make-record-type quote record-accessor
			record-constructor record-modifier record-predicate)
    (define-values begin call-with-values define lambda set!)
    (delay delay-force make-promise)
    (delay-force lambda make-unforced-promise)
    (do begin if let)
    (guard begin call-with-current-continuation if lambda let raise-continuable
	   with-exception-handler)
    (include begin)
    (include-ci begin)
    (let declare lambda letrec letrec* named-lambda)
    (let* let)
    (let-syntax* let-syntax)
    (letrec lambda let set!)
    (letrec* begin lambda let)
    (local-declare declare let)
    (parameterize cons lambda list parameterize*)
    (quasiquote append cons list list->vector quote vector)
    (receive call-with-values lambda)
    (unless begin if not)
    (when begin if)))

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
    let*-values
    let-syntax
    let-values
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
    null-environment
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
    scheme-report-environment
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

(define-standard-library '(srfi 1)
  '(alist-cons
    alist-copy
    alist-delete
    alist-delete!
    any
    append
    append!
    append-map
    append-map!
    append-reverse
    append-reverse!
    assoc
    assq
    assv
    break
    break!
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
    car
    car+cdr
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
    circular-list
    circular-list?
    concatenate
    concatenate!
    cons
    cons*
    count
    delete
    delete!
    delete-duplicates
    delete-duplicates!
    dotted-list?
    drop
    drop-right
    drop-right!
    drop-while
    eighth
    every
    fifth
    filter
    filter!
    filter-map
    find
    find-tail
    first
    fold
    fold-right
    for-each
    fourth
    iota
    last
    last-pair
    length
    length+
    list
    list-copy
    list-index
    list-ref
    list-tabulate
    list=
    lset-adjoin
    lset-diff+intersection
    lset-diff+intersection!
    lset-difference
    lset-difference!
    lset-intersection
    lset-intersection!
    lset-union
    lset-union!
    lset-xor
    lset-xor!
    lset<=
    lset=
    make-list
    map
    map!
    map-in-order
    member
    memq
    memv
    ninth
    not-pair?
    null-list?
    null?
    pair-fold
    pair-fold-right
    pair-for-each
    pair?
    partition
    partition!
    proper-list?
    reduce
    reduce-right
    remove
    remove!
    reverse
    reverse!
    second
    set-car!
    set-cdr!
    seventh
    sixth
    span
    span!
    split-at
    split-at!
    take
    take!
    take-right
    take-while
    take-while!
    tenth
    third
    unfold
    unfold-right
    unzip1
    unzip2
    unzip3
    unzip4
    unzip5
    xcons
    zip))

(define-standard-library '(srfi 2)
  '(and-let*))

(define-standard-library '(srfi 6)
  '(get-output-string
    open-input-string
    open-output-string))

(define-standard-library '(srfi 8)
  '(receive))

(define-standard-library '(srfi 9)
  '(define-record-type))

(define-standard-library '(srfi 23)
  '(error))

(define-standard-library '(srfi 27)
  '(default-random-source
    make-random-source
    random-integer
    random-real
    random-source-make-integers
    random-source-make-reals
    random-source-make-reals
    random-source-pseudo-randomize!
    random-source-randomize!
    random-source-state-ref
    random-source-state-set!
    random-source?))

(define-standard-library '(srfi 39)
  '(make-parameter
    parameterize))

(define-standard-library '(srfi 69)
  '(alist->hash-table
    hash
    hash-by-identity
    hash-table->alist
    hash-table-copy
    hash-table-delete!
    hash-table-equivalence-function
    hash-table-exists?
    hash-table-fold
    hash-table-hash-function
    hash-table-keys
    hash-table-merge!
    hash-table-ref
    hash-table-ref/default
    hash-table-set!
    hash-table-size
    hash-table-update!
    hash-table-update!/default
    hash-table-values
    hash-table-walk
    hash-table?
    make-hash-table
    string-ci-hash
    string-hash))

(define-standard-library '(srfi 131)
  '(define-record-type))