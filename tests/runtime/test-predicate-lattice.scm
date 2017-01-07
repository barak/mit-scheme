(define-test 'ordering
  (lambda ()
    (assert-true (predicate<= no-object? string?))
    (assert-true (predicate<= no-object? no-object?))
    (assert-false (predicate<= string? no-object?))

    (assert-false (predicate<= any-object? string?))
    (assert-true (predicate<= any-object? any-object?))
    (assert-true (predicate<= string? any-object?))

    (assert-eqv (disjoin string?) string?)
    (assert-eqv (conjoin string?) string?)

    (assert-true (predicate<= string? (disjoin string? symbol?)))
    (assert-false (predicate<= (disjoin string? symbol?) string?))

    (assert-false (predicate<= string? (conjoin string? symbol?)))
    (assert-true (predicate<= (conjoin string? symbol?) string?))))