(list (lambda () (error "foo") (+ 1 2))
      (lambda () (newline) (error "foo") (+ 1 2)))