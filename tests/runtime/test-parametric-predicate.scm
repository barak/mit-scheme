(define-test 'parametric-predicate-one-parameter
  (lambda ()
    (let ((pattern '((? base))))
      (let* ((template (make-predicate-template 'template pattern))
             (instantiator (predicate-template-instantiator template)))
        (test-template-operations template 'template pattern)

        (let ((params1 (list number?))
              (params2 (list boolean?)))
          (let ((tn (apply instantiator params1))
                (tb (apply instantiator params2)))
            (test-predicate-operations tn '(template number))
            (test-predicate-operations tb '(template boolean))
            (test-parametric-predicate-operations tn template params1)
            (test-parametric-predicate-operations tb template params2)))))))

(define-test 'parametric-predicate-two-parameters
  (lambda ()
    (let ((pattern '((?* domains -) (? base))))
      (let* ((template (make-predicate-template 'template pattern))
             (instantiator (predicate-template-instantiator template)))
        (test-template-operations template 'template pattern)

        (let ((params1 (list (list number? number?) number?))
              (params2 (list (list boolean? boolean?) boolean?)))
          (let ((tn (apply instantiator params1))
                (tb (apply instantiator params2)))
            (test-predicate-operations tn '(template (number number) number))
            (test-predicate-operations tb '(template (boolean boolean) boolean))
            (test-parametric-predicate-operations tn template params1)
            (test-parametric-predicate-operations tb template params2)))))))

(define-test 'covariant-ordering
  (lambda ()
    (let* ((template (make-predicate-template 'foo '((? a))))
           (instantiator (predicate-template-instantiator template)))
      (let ((p1 (instantiator (disjoin string? symbol?)))
            (p2 (instantiator string?))
            (p3 (instantiator symbol?)))

        (assert-true (predicate<= p1 p1))
        (assert-false (predicate<= p1 p2))
        (assert-false (predicate<= p1 p3))

        (assert-true (predicate<= p2 p1))
        (assert-true (predicate<= p2 p2))
        (assert-false (predicate<= p2 p3))

        (assert-true (predicate<= p3 p1))
        (assert-false (predicate<= p3 p2))
        (assert-true (predicate<= p3 p3))

        ))))

(define-test 'contravariant-ordering
  (lambda ()
    (let* ((template (make-predicate-template 'foo '((? a -))))
           (instantiator (predicate-template-instantiator template)))
      (let ((p1 (instantiator (disjoin string? symbol?)))
            (p2 (instantiator string?))
            (p3 (instantiator symbol?)))

        (assert-true (predicate<= p1 p1))
        (assert-true (predicate<= p1 p2))
        (assert-true (predicate<= p1 p3))

        (assert-false (predicate<= p2 p1))
        (assert-true (predicate<= p2 p2))
        (assert-false (predicate<= p2 p3))

        (assert-false (predicate<= p3 p1))
        (assert-false (predicate<= p3 p2))
        (assert-true (predicate<= p3 p3))

        ))))

(define-test 'mixed-ordering
  (lambda ()
    (let* ((template (make-predicate-template 'foo '((? a -) (? b))))
           (instantiator (predicate-template-instantiator template)))
      (let ((p1 (instantiator (disjoin string? symbol?)
                              (disjoin string? symbol?)))
            (p2 (instantiator string? string?))
            (p3 (instantiator string? (disjoin string? symbol?)))
            (p4 (instantiator (disjoin string? symbol?) string?)))

        (for-each (lambda (predicate)
                    (assert-true (predicate<= predicate predicate)))
                  (list p1 p2 p3 p4))

        (assert-false (predicate<= p2 p1))
        (assert-false (predicate<= p3 p1))
        (assert-true (predicate<= p4 p1))

        (assert-false (predicate<= p3 p2))
        (assert-true (predicate<= p4 p2))

        (assert-true (predicate<= p2 p3))
        (assert-false (predicate<= p2 p4))

        ))))

(define-test 'template-patterns
  (lambda ()
    (let ((operators '(? ?* ?+))
          (names '(a b c))
          (polarities '(+ = -)))

      (assert-false (template-pattern? '()))
      (assert-false (template-pattern-element? '()))
      (assert-false (template-pattern? '(())))
      (for-each (lambda (symbol)
                  (assert-false (template-pattern? symbol))
                  (assert-false (template-pattern-element? symbol))
                  (assert-false (template-pattern? (list symbol)))
                  (assert-false (template-pattern-element? (list symbol)))
                  (assert-false (template-pattern? (list (list symbol)))))
                (append operators names polarities))

      (let ((elements (elementwise-lists-of (list operators names polarities))))
        (for-each
         (lambda (element)
           (assert-true (template-pattern? (list element)))
           (assert-false (template-pattern? element))
           (for-each
            (lambda (permutation)
              (let ((assertion
                     (if (equal? permutation element)
                         assert-true
                         assert-false)))
                (assertion (template-pattern-element? permutation))
                (assertion (template-pattern? (list permutation)))
                (assertion (template-pattern-element? (take permutation 2)))
                (assertion (template-pattern? (list (take permutation 2))))))
            (all-permutations-of element)))
         elements)

        (for-each
         (lambda (elements)
           ((if (= (length elements)
                   (length (delete-duplicates (map cadr elements) eqv?)))
                assert-true
                assert-false)
            (template-pattern? elements)))
         (append
          (elementwise-lists-of (list elements elements))
          (elementwise-lists-of (list elements elements elements))))))))

(define-test 'match-template-pattern
  (lambda ()
    (assert-wta-error (lambda () (match-numbers '((? a)) 1)))
    (assert-equal (match-numbers '((? a)) '(1))
                  '((a + 1)))
    (assert-equal (match-numbers '((? a -) (? b)) '(1 2))
                  '((a - 1)
                    (b + 2)))
    (assert-equal (match-numbers '((?* a) (? b -)) '((1 2 3) 2))
                  '((a + (1 2 3))
                    (b - 2)))
    (assert-equal (match-numbers '((?+ a -) (? b)) '((1 2 3) 2))
                  '((a - (1 2 3))
                    (b + 2)))
    (assert-equal (match-numbers '((?* a) (? b -)) '(() 2))
                  '((a + ())
                    (b - 2)))
    (assert-simple-error (lambda () (match-numbers '((?+ a -) (? b)) '(() 2))))
    (assert-simple-error (lambda () (match-numbers '((?* a) (? b -)) '(1 2))))
    (assert-simple-error (lambda () (match-numbers '((?+ a -) (? b)) '(1 2))))))

(define (test-template-operations template name pattern)
  (assert-true (predicate-template? template))
  (assert-false (predicate? template))
  (assert-eqv (predicate-template-name template) name)
  (assert-equal pattern (predicate-template-pattern template))
  (assert-lset= eq?
                (predicate-template-parameter-names template)
                (map template-pattern-element-name pattern))
  (let ((predicate (predicate-template-predicate template)))
    (assert-true (predicate? predicate))
    (assert-true (predicate<= predicate parametric-predicate?))
    (assert-false (predicate<= parametric-predicate? predicate))))

(define (test-predicate-operations predicate name)
  (assert-true (predicate? predicate))
  (let ((tag (predicate->tag predicate)))
    (assert-true (tag? tag))
    (assert-eqv (tag->predicate tag) predicate)
    (assert-equal (predicate-name predicate) name)
    (assert-equal (tag-name tag) name)))

(define (test-parametric-predicate-operations predicate template parameters)
  (assert-false (simple-predicate? predicate))
  (assert-false (compound-predicate? predicate))
  (assert-true (parametric-predicate? predicate))
  (assert-eqv (parametric-predicate-template predicate) template)
  (assert-lset= eq?
                (parametric-predicate-names predicate)
                (predicate-template-parameter-names template))
  (assert-lset= equal?
                (map (lambda (name)
                       ((predicate-template-accessor name template) predicate))
                     (predicate-template-parameter-names template))
                parameters))

(define (parametric-predicate-names predicate)
  (predicate-template-parameter-names
   (parametric-predicate-template predicate)))

(define (match-numbers pattern values)
  (parameter-bindings->alist (match-template-pattern pattern values number?)))

(define (parameter-bindings->alist bindings)
  (map (lambda (binding)
         (list (parameter-binding-name binding)
               (parameter-binding-polarity binding)
               (parameter-binding-value binding)))
       bindings))

(define (all-permutations-of items)
  (let loop ((items items))
    (if (pair? items)
        (append-map (lambda (index)
                      (map (let ((head (list-ref items index)))
                             (lambda (tail)
                               (cons head tail)))
                           (loop (delete-item items index))))
                    (iota (length items)))
        '(()))))

(define (delete-item items index)
  (append (take items index)
          (cdr (drop items index))))

(define (elementwise-lists-of lists)
  (let loop ((lists lists))
    (if (pair? lists)
        (append-map (let ((tails (loop (cdr lists))))
                      (lambda (head)
                        (map (lambda (tail)
                               (cons head tail))
                             tails)))
                    (car lists))
        '(()))))