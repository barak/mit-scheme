(define-test 'non-predicate
  (lambda ()
    (let ((np (lambda (object) #f)))
      (assert-false (predicate? np))
      (assert-wta-error (lambda () (predicate->tag np)))
      (assert-wta-error (lambda () (predicate-name np)))
      (assert-true (string? (predicate-description np))))))

(define-test 'simple-predicate
  (lambda ()
    (test-predicate-operations number? 'number)
    (test-predicate-operations boolean? 'boolean)
    (test-predicate-operations string? 'string)))

(define (test-predicate-operations predicate name)
  (assert-true (predicate? predicate))
  (let ((tag (predicate->tag predicate)))
    (assert-true (tag? tag))
    (assert-eqv (tag->predicate tag) predicate)
    (assert-equal (predicate-name predicate) name)
    (assert-equal (tag-name tag) name)
    (assert-equal (predicate-description predicate) (tag-description tag))))