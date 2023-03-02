(define standards
  '(((node-name "R7RS")
     (title "Revised@sup{7} Report on the Algorithmic Language Scheme")
     (description "The core standard for the Scheme language.")
     (url "@url{@value{R7RS_URL}}")
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
	    call-with-port
	    call-with-values
	    call/cc
	    car
	    case
	    case-lambda
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
	    char-foldcase
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
	    close-port
	    command-line
	    complex?
	    cond
	    cond-expand
	    cons
	    cos
	    current-error-port
	    current-input-port
	    current-jiffy
	    current-output-port
	    current-second
	    define
	    define-record-type
	    define-syntax
	    define-values
	    delay
	    delay-force
	    delete-file
	    denominator
	    digit-value
	    display
	    do
	    dynamic-wind
	    else
	    emergency-exit
	    environment
	    eof-object
	    eof-object?
	    eq?
	    equal?
	    eqv?
	    error
	    error-object-irritants
	    error-object-message
	    error-object?
	    eval
	    even?
	    exact
	    exact-integer-sqrt
	    exact-integer?
	    exact?
	    exit
	    exp
	    expt
	    features
	    file-error?
	    file-exists?
	    finite?
	    floor
	    floor-quotient
	    floor-remainder
	    floor/
	    flush-output-port
	    for-each
	    force
	    gcd
	    get-environment-variable
	    get-environment-variables
	    get-output-bytevector
	    get-output-string
	    guard
	    if
	    imag-part
	    include
	    include-ci
	    inexact
	    inexact?
	    infinite?
	    input-port-open?
	    input-port?
	    integer->char
	    integer?
	    interaction-environment
	    jiffies-per-second
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
	    load
	    log
	    magnitude
	    make-bytevector
	    make-list
	    make-parameter
	    make-polar
	    make-promise
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
	    nan?
	    negative?
	    newline
	    not
	    null?
	    number->string
	    number?
	    numerator
	    odd?
	    open-binary-input-file
	    open-binary-output-file
	    open-input-bytevector
	    open-input-file
	    open-input-string
	    open-output-bytevector
	    open-output-file
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
	    promise?
	    quasiquote
	    quote
	    quotient
	    raise
	    raise-continuable
	    rational?
	    rationalize
	    read
	    read-bytevector
	    read-bytevector!
	    read-char
	    read-error?
	    read-line
	    read-string
	    read-u8
	    real-part
	    real?
	    remainder
	    reverse
	    round
	    set!
	    set-car!
	    set-cdr!
	    sin
	    sqrt
	    square
	    string
	    string->list
	    string->number
	    string->symbol
	    string->utf8
	    string->vector
	    string-append
	    string-ci<=?
	    string-ci<?
	    string-ci=?
	    string-ci>=?
	    string-ci>?
	    string-copy
	    string-copy!
	    string-downcase
	    string-fill!
	    string-foldcase
	    string-for-each
	    string-length
	    string-map
	    string-ref
	    string-set!
	    string-upcase
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
	    tan
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
	    with-input-from-file
	    with-output-to-file
	    write
	    write-bytevector
	    write-char
	    write-shared
	    write-simple
	    write-string
	    write-u8
	    zero?))
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
    ((srfi 39)
     (title "Parameter Objects")
     (description
      "A mechanism for dynamic binding.  Superseded by @rseven{}.")
     (support full)
     (global all)
     (bound make-parameter
	    parameterize))
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
	    symbol-hash))
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
     (description "Higher-order lambda syntax for @code{define}")
     (support full)
     (libraries)
     (global all)
     (bound define))))