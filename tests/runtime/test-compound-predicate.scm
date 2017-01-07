(define-test 'compound
  (lambda ()
    (test-compound-predicate-operations (disjoin) 'disjoin '())
    (test-compound-predicate-operations (conjoin) 'conjoin '())

    (assert-eqv string? (disjoin string?))
    (assert-eqv string? (disjoin string? string?))

    (assert-eqv string? (conjoin string?))
    (assert-eqv string? (conjoin string? string?))

    (test-compound-predicate-operations (disjoin string? symbol?)
                                        'disjoin
                                        (list string? symbol?))
    (test-compound-predicate-operations (conjoin string? symbol?)
                                        'conjoin
                                        (list string? symbol?))))

(define-test 'ordering
  (lambda ()
    (assert-true (predicate<= string? (disjoin string? symbol?)))
    (assert-false (predicate<= (disjoin string? symbol?) string?))

    (assert-false (predicate<= string? (conjoin string? symbol?)))
    (assert-true (predicate<= (conjoin string? symbol?) string?))))

(define (test-compound-predicate-operations predicate operator operands)
  (assert-true (compound-predicate? predicate))
  (assert-eqv (compound-predicate-operator predicate) operator)
  (assert-lset= eqv? (compound-predicate-operands predicate) operands))