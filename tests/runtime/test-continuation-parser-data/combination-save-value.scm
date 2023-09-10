(list (lambda () (list (error "foo") (+ 1 2)))
      (lambda () (list (+ 1 2) (error "foo"))))