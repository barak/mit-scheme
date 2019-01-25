; A vaguely nontrivial program involving a recursive procedure.

(declare (usual-integrations))

(let ()
  (define (map-reduce kons f nil l)
    (let g ((l l))
      (if (pair? l)
	  (kons (f (car l)) (g (cdr l)))
	  nil)))
  ((ucode-primitive exit-with-value 1)
   (map-reduce (identity (lambda (x y) (fix:+ x y)))
	       (identity (lambda (z) (fix:- 0 z)))
	       0
	       (identity '(1 -2 3 -4 2)))))
