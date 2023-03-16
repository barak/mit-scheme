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

;;;; Database of supported standards

(declare (usual-integrations))

(define (std-prop std keyword selector #!optional alt-value)
  (let ((prop (assq keyword std)))
    (if prop
	(if selector (selector prop) prop)
	(begin
	  (if (default-object? alt-value)
	      (error "Missing required value:" keyword std))
	  alt-value))))

(define (std-names . keywords)

  (define (get std)
    (sort-names
     (append-map
      (lambda (keyword)
	(std-prop std keyword
		  (lambda (prop)
		    (fold (lambda (name names)
			    (if (pair? name)
				(append (get (find-lib-std name)) names)
				(cons name names)))
			  '()
			  (cdr prop)))
		  '()))
      keywords)))

  (define (find-lib-std lib)
    (or (find (lambda (std) (equal? lib (std-library std)))
	      standards)
	(error "Reference to unknown standard library:" lib)))

  get)

(define (sort-names names)
  (sort names (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(define std-bound (std-names 'bound))
(define std-bound+extra (std-names 'bound 'bound-extra))
(define std-unbound (std-names 'unbound))
(define std-unimplemented (std-names 'unimplemented))
(define std-all-names (std-names 'bound 'unbound 'unimplemented))

(define (std-library std)
  (or (std-prop std 'library cadr #f)
      (std-prop std 'srfi #f #f)))

(define (std-libraries std)
  (or (std-prop std 'libraries cdr #f)
      (std-prop std 'srfi list)))

(define (std-url std)
  (if (srfi? std)
      (let ((n (srfi-number-string std)))
	(string-append "https://srfi.schemers.org/srfi-" n "/srfi-" n ".html"))
      (std-prop std 'url cadr)))

(define (std-title std)
  (let ((title (std-prop std 'title cadr)))
    (if (srfi? std)
	(string-append "SRFI " (srfi-number-string std) ": " title)
	(std-prop std 'title cadr))))

(define (std-description std)
  (string-concatenate (std-prop std 'description cdr)))

(define (srfi? std) (std-prop std 'srfi cadr #f))
(define (srfi-number srfi) (std-prop srfi 'srfi cadr))
(define (srfi-number-string srfi) (number->string (srfi-number srfi)))

(define standards
  '(((node-name "R7RS")
     (title "Revised@sup{7} Report on the Algorithmic Language Scheme")
     (description "The core standard for the Scheme language.")
     (url "https://small.r7rs.org/attachment/r7rs.pdf")
     (support full
	      ("Multiple values aren't supported correctly."
	       "  Instead @code{values} returns a special record containing"
	       " the values, and @code{call-with-values} takes that record"
	       " apart and passes the values to the receiver."
	       "  For many uses of multiple values this is adequate."))
     (libraries (scheme base)
		(scheme case-lambda)
		(scheme char)
		(scheme complex)
		(scheme cxr)
		(scheme eval)
		(scheme file)
		(scheme inexact)
		(scheme lazy)
		(scheme load)
		(scheme process-context)
		(scheme r5rs)
		(scheme read)
		(scheme repl)
		(scheme time)
		(scheme write))
     (global all)
     (columns 3)
     (bound (scheme base)
	    (scheme case-lambda)
	    (scheme char)
	    (scheme complex)
	    (scheme cxr)
	    (scheme eval)
	    (scheme file)
	    (scheme inexact)
	    (scheme lazy)
	    (scheme load)
	    (scheme process-context)
	    (scheme read)
	    (scheme repl)
	    (scheme time)
	    (scheme write)))
    ((library (scheme base))
     (support full)
     (global all)
     (bound *
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
    ((library (scheme case-lambda))
     (support full)
     (global all)
     (bound case-lambda))
    ((library (scheme char))
     (support full)
     (global all)
     (bound char-alphabetic?
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
    ((library (scheme complex))
     (support full)
     (global all)
     (bound angle
	    imag-part
	    magnitude
	    make-polar
	    make-rectangular
	    real-part))
    ((library (scheme cxr))
     (support full)
     (global all)
     (bound caaaar
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
    ((library (scheme eval))
     (support full)
     (global all)
     (bound environment
	    eval))
    ((library (scheme file))
     (support full)
     (global all)
     (bound call-with-input-file
	    call-with-output-file
	    delete-file
	    file-exists?
	    open-binary-input-file
	    open-binary-output-file
	    open-input-file
	    open-output-file
	    with-input-from-file
	    with-output-to-file))
    ((library (scheme inexact))
     (support full)
     (global all)
     (bound acos
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
    ((library (scheme lazy))
     (support full)
     (global all)
     (bound delay
	    delay-force
	    force
	    make-promise
	    promise?))
    ((library (scheme load))
     (support full)
     (global all)
     (bound load))
    ((library (scheme process-context))
     (support full)
     (global all)
     (bound command-line
	    emergency-exit
	    exit
	    get-environment-variable
	    get-environment-variables))
    ((library (scheme read))
     (support full)
     (global all)
     (bound read))
    ((library (scheme repl))
     (support full)
     (global all)
     (bound interaction-environment))
    ((library (scheme time))
     (support full)
     (global all)
     (bound current-jiffy
	    current-second
	    jiffies-per-second))
    ((library (scheme write))
     (support full)
     (global all)
     (bound display
	    write
	    write-shared
	    write-simple))
    ((library (scheme r5rs))
     (support full)
     (global all)
     (bound *
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
    ((srfi 0)
     (title "Feature-based conditional expansion construct")
     (description
      "A means of customizing code based on implementation features."
      "  Superseded by @rseven{}.")
     (support full)
     (global all)
     (bound cond-expand))
    ((srfi 1)
     (title "List Library")
     (description
      "An extensive set of procedures for working with lists that"
      " is a superset of the list procedures defined by @rseven{}.")
     (support full)
     (global all)
     (columns 3)
     (bound alist-cons
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
    ((srfi 2)
     (title "@code{and-let*}")
     (description "A macro combining features of @code{let*} and @code{and}.")
     (support full)
     (global all)
     (bound and-let*))
    ((srfi 6)
     (title "Basic String Ports")
     (description "Ports for string I/O.")
     (support full)
     (global all)
     (bound get-output-string
	    open-input-string
	    open-output-string))
    ((srfi 8)
     (title "@code{receive}")
     (description
      "Early syntax for binding multiple values.  @rseven{}"
      " provides the more flexible @code{let-values}, which"
      " should be preferred.")
     (support full)
     (global all)
     (bound receive))
    ((srfi 9)
     (title "Record Types")
     (description
      "A basic implementation of record types."
      "  Superseded by @rseven{}.")
     (support full)
     (global all)
     (bound define-record-type))
    ((srfi 14)
     (title "Character-set Library")
     (description
      "An extensive set of definitions for working with sets of characters.")
     (support full)
     (global some)
     (bound ->char-set
	    char-set
	    char-set->list
	    char-set->string
	    char-set-adjoin
	    char-set-any
	    char-set-complement
	    char-set-contains?
	    char-set-copy
	    char-set-count
	    char-set-cursor
	    char-set-cursor-next
	    char-set-delete
	    char-set-diff+intersection
	    char-set-difference
	    char-set-every
	    char-set-filter
	    char-set-fold
	    char-set-for-each
	    char-set-hash
	    char-set-intersection
	    char-set-map
	    char-set-ref
	    char-set-size
	    char-set-unfold
	    char-set-union
	    char-set-xor
	    char-set:ascii
	    char-set:blank
	    char-set:digit
	    char-set:empty
	    char-set:full
	    char-set:graphic
	    char-set:hex-digit
	    char-set:iso-control
	    char-set:letter
	    char-set:letter+digit
	    char-set:lower-case
	    char-set:printing
	    char-set:punctuation
	    char-set:symbol
	    char-set:title-case
	    char-set:upper-case
	    char-set:whitespace
	    char-set<=
	    char-set=
	    char-set?
	    end-of-char-set?
	    list->char-set
	    string->char-set
	    ucs-range->char-set)
     (unbound char-set-adjoin!
	      char-set-complement!
	      char-set-delete!
	      char-set-diff+intersection!
	      char-set-difference!
	      char-set-filter!
	      char-set-intersection!
	      char-set-unfold!
	      char-set-union!
	      char-set-xor!
	      list->char-set!
	      string->char-set!
	      ucs-range->char-set!))
    ((srfi 23)
     (title "Error Reporting Mechanism")
     (description
      "A simple implementation of @code{error}.  Superseded by @rseven{}.")
     (support full)
     (global all)
     (bound error))
    ((srfi 27)
     (title "Sources of Random Bits")
     (description
      "Procedures to obtain pseudo-random numbers in various formats.")
     (support full)
     (global all)
     (bound default-random-source
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
    ((srfi 30)
     (title "Nested Multi-line Comments")
     (description
      "Multi-line comments that start with @samp{#|} and end with @samp{|#}."
      "  Superseded by @rseven{}.")
     (support full)
     (global none))
    ((srfi 39)
     (title "Parameter Objects")
     (description
      "A mechanism for dynamic binding.  Superseded by @rseven{}.")
     (support full)
     (global all)
     (bound make-parameter
	    parameterize))
    ((srfi 62)
     (title "S-expression comments")
     (description
      "Lexical syntax that allows individual S-expressions to be made into"
      " comments, by prefixing them with @samp{#;}."
      "  Superseded by @rseven{}.")
     (support full)
     (global none))
    ((srfi 69)
     (title "Basic Hash Tables")
     (description
      "A standard interface for hash tables.  Superseded by @asrfi{125}.")
     (support full)
     (global all)
     (bound alist->hash-table
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
    ((srfi 112)
     (title "Environment Inquiry")
     (description
      "Provides human-readable information at run time about"
      " the hardware and software configuration on which a"
      " Scheme program is being executed.")
     (support full)
     (global all)
     (bound cpu-architecture
	    implementation-name
	    implementation-version
	    machine-name
	    os-name
	    os-version))
    ((srfi 115)
     (title "Scheme Regular Expressions")
     (description
      "An implementation of regular expressions using Scheme syntax.")
     (support full)
     (global all)
     (features regexp-unicode
	       regexp-non-greedy)
     (bound char-set->sre
	    regexp
	    regexp-extract
	    regexp-fold
	    regexp-match->list
	    regexp-match-count
	    regexp-match-submatch
	    regexp-match-submatch-end
	    regexp-match-submatch-start
	    regexp-match?
	    regexp-matches
	    regexp-matches?
	    regexp-partition
	    regexp-replace
	    regexp-replace-all
	    regexp-search
	    regexp-split
	    regexp?
	    rx
	    valid-sre?))
    ((srfi 124)
     (title "Ephemerons")
     (description "Support for ephemerons, a weak-pointer mechanism.")
     (support full)
     (global all)
     (columns 3)
     (bound ephemeron-broken?
	    ephemeron-datum
	    ephemeron-key
	    ephemeron?
	    make-ephemeron
	    reference-barrier))
    ((srfi 125)
     (title "Intermediate Hash Tables")
     (description
      "A comprehensive set of procedures for hash tables."
      "  Supersedes @asrfi{69}.")
     (support full)
     (global all)
     (bound alist->hash-table
	    hash
	    hash-by-identity
	    hash-table
	    hash-table->alist
	    hash-table-clear!
	    hash-table-contains?
	    hash-table-copy
	    hash-table-count
	    hash-table-delete!
	    hash-table-difference!
	    hash-table-empty-copy
	    hash-table-empty?
	    hash-table-entries
	    hash-table-equivalence-function
	    hash-table-exists?
	    hash-table-find
	    hash-table-fold
	    hash-table-for-each
	    hash-table-hash-function
	    hash-table-intern!
	    hash-table-intersection!
	    hash-table-keys
	    hash-table-map
	    hash-table-map!
	    hash-table-map->list
	    hash-table-merge!
	    hash-table-mutable?
	    hash-table-pop!
	    hash-table-prune!
	    hash-table-ref
	    hash-table-ref/default
	    hash-table-set!
	    hash-table-size
	    hash-table-unfold
	    hash-table-union!
	    hash-table-update!
	    hash-table-update!/default
	    hash-table-values
	    hash-table-walk
	    hash-table-xor!
	    hash-table=?
	    hash-table?
	    make-hash-table
	    string-ci-hash
	    string-hash))
    ((srfi 128)
     (title "Comparators (reduced)")
     (description
      "An implementation of comparators, which encapsulate type, equality,"
      " ordering, and hashing.")
     (support full)
     (global all)
     (bound <=?
	    <?
	    =?
	    >=?
	    >?
	    boolean-hash
	    char-ci-hash
	    char-hash
	    comparator-check-type
	    comparator-equality-predicate
	    comparator-hash
	    comparator-hash-function
	    comparator-hashable?
	    comparator-if<=>
	    comparator-ordered?
	    comparator-ordering-predicate
	    comparator-register-default!
	    comparator-test-type
	    comparator-type-test-predicate
	    comparator?
	    default-hash
	    hash-bound
	    hash-salt
	    make-comparator
	    make-default-comparator
	    make-eq-comparator
	    make-equal-comparator
	    make-eqv-comparator
	    make-list-comparator
	    make-pair-comparator
	    make-vector-comparator
	    number-hash
	    string-ci-hash
	    string-hash
	    symbol-hash)
     (bound-extra (srfi 162)))
    ((srfi 129)
     (title "Titlecase procedures")
     (description "An implementation of procedures for title case.")
     (support full)
     (global all)
     (bound char-title-case?
	    char-titlecase
	    string-titlecase))
    ((srfi 131)
     (title "ERR5RS Record Syntax (reduced)")
     (description
      "An enhanced record syntax supporting abbreviations"
      " and single inheritance.")
     (support full)
     (global all)
     (bound define-record-type))
    ((srfi 133)
     (title "Vector Library (R7RS-compatible)")
     (description "A set of procedures for working with vectors.")
     (support full)
     (global none
	     "Some of the same names are there,"
	     " but they aren't the same implementations.")
     (unbound reverse-list->vector
	      reverse-vector->list
	      vector-any
	      vector-append-subvectors
	      vector-cumulate
	      vector-empty?
	      vector-every
	      vector-fold
	      vector-fold-right
	      vector-index
	      vector-index-right
	      vector-map!
	      vector-partition
	      vector-reverse!
	      vector-reverse-copy
	      vector-skip
	      vector-skip-right
	      vector-swap!
	      vector-unfold
	      vector-unfold!
	      vector-unfold-right
	      vector-unfold-right!
	      vector=
	      vector-binary-search
	      vector-concatenate
	      vector-count
	      vector-reverse-copy!))
    ((srfi 140)
     (title "Immutable Strings")
     (description "A comprehensive library for using immutable strings.")
     (support partial ("Only immutable strings are implemented."))
     (global some)
     (bound istring?
	    list->string
	    string
	    string->list
	    string->utf16
	    string->utf16be
	    string->utf16le
	    string->utf8
	    string->vector
	    string-append
	    string-ci<=?
	    string-ci<?
	    string-ci=?
	    string-ci>=?
	    string-ci>?
	    string-concatenate
	    string-downcase
	    string-fold
	    string-fold-right
	    string-foldcase
	    string-for-each
	    string-length
	    string-map
	    string-null?
	    string-ref
	    string-titlecase
	    string-upcase
	    string<=?
	    string<?
	    string=?
	    string>=?
	    string>?
	    string?
	    substring
	    utf16->string
	    utf16be->string
	    utf16le->string
	    utf8->string
	    vector->string)
     (unbound reverse-list->string
	      string-any
	      string-concatenate-reverse
	      string-contains
	      string-contains-right
	      string-count
	      string-drop
	      string-drop-right
	      string-every
	      string-filter
	      string-for-each-index
	      string-index
	      string-index-right
	      string-join
	      string-map-index
	      string-pad
	      string-pad-right
	      string-prefix-length
	      string-prefix?
	      string-remove
	      string-repeat
	      string-replace
	      string-skip
	      string-skip-right
	      string-split
	      string-suffix-length
	      string-suffix?
	      string-tabulate
	      string-take
	      string-take-right
	      string-trim
	      string-trim-both
	      string-trim-right
	      string-unfold
	      string-unfold-right
	      xsubstring)
     (unimplemented make-string
		    mstring?
		    string-append!
		    string-copy
		    string-copy!
		    string-fill!
		    string-replace!
		    string-set!))
    ((srfi 143)
     (title "Fixnums")
     (description "Definitions of fixnum-specific procedures.")
     (support full)
     (global all)
     (bound fixnum?
	    fx*
	    fx*/carry
	    fx+
	    fx+/carry
	    fx-
	    fx-/carry
	    fx-greatest
	    fx-least
	    fx-width
	    fx<=?
	    fx<?
	    fx=?
	    fx>=?
	    fx>?
	    fxabs
	    fxand
	    fxarithmetic-shift
	    fxarithmetic-shift-left
	    fxarithmetic-shift-right
	    fxbit-count
	    fxbit-field
	    fxbit-field-reverse
	    fxbit-field-rotate
	    fxbit-set?
	    fxcopy-bit
	    fxeven?
	    fxfirst-set-bit
	    fxif
	    fxior
	    fxlength
	    fxmax
	    fxmin
	    fxneg
	    fxnegative?
	    fxnot
	    fxodd?
	    fxpositive?
	    fxquotient
	    fxremainder
	    fxsqrt
	    fxsquare
	    fxxor
	    fxzero?))
    ((srfi 158)
     (title "Generators and Accumulators")
     (description "Efficient sources and sinks of objects.")
     (support full)
     (global all)
     (bound bytevector->generator
	    bytevector-accumulator
	    bytevector-accumulator!
	    circular-generator
	    count-accumulator
	    gappend
	    gcombine
	    gcons*
	    gdelete
	    gdelete-neighbor-dups
	    gdrop
	    gdrop-while
	    generator
	    generator->list
	    generator->reverse-list
	    generator->string
	    generator->vector
	    generator->vector!
	    generator-any
	    generator-count
	    generator-every
	    generator-find
	    generator-fold
	    generator-for-each
	    generator-map->list
	    generator-unfold
	    gfilter
	    gflatten
	    ggroup
	    gindex
	    gmap
	    gmerge
	    gremove
	    gselect
	    gstate-filter
	    gtake
	    gtake-while
	    list->generator
	    list-accumulator
	    make-accumulator
	    make-coroutine-generator
	    make-for-each-generator
	    make-iota-generator
	    make-range-generator
	    make-unfold-generator
	    product-accumulator
	    reverse-list-accumulator
	    reverse-vector->generator
	    reverse-vector-accumulator
	    string->generator
	    string-accumulator
	    sum-accumulator
	    vector->generator
	    vector-accumulator
	    vector-accumulator!))
    ((srfi 162)
     (title "Comparators sublibrary")
     (description "Additional useful comparator definitions.")
     (support full)
     (libraries (srfi 128) "[@emph{not} @nicode{(srfi 162)}]")
     (global all)
     (bound boolean-comparator
	    char-ci-comparator
	    char-comparator
	    comparator-max
	    comparator-max-in-list
	    comparator-min
	    comparator-min-in-list
	    default-comparator
	    eq-comparator
	    equal-comparator
	    eqv-comparator
	    list-comparator
	    pair-comparator
	    real-comparator
	    string-ci-comparator
	    string-comparator
	    vector-comparator))
    ((srfi 180)
     (title "JSON")
     (description "A set of procedures for reading and writing JSON.")
     (support full)
     (global none)
     (bound json-accumulator
	    json-accumulator-trace?
	    json-error?
	    json-error-reason
	    json-fold
	    json-generator
	    json-lines-read
	    json-nesting-depth-limit
	    json-null?
	    json-number-of-character-limit
	    json-read
	    json-sequence-read
	    json-write))
    ((srfi 219)
     (title "Define higher-order lambda")
     (description "Higher-order lambda syntax for @code{define}.")
     (support full)
     (libraries)
     (global all)
     (bound define))
    ((srfi 228)
     (title "Combining Comparators")
     (description "Procedures for simple comparator combinations.")
     (support full)
     (global all)
     (bound comparator-one
	    comparator-zero
	    make-product-comparator
	    make-sum-comparator
	    make-wrapper-comparator))))