(declare (usual-integrations))


(define assq/1
  (named-lambda (assq key alist)
    (let loop ((alist* alist))
      (if (pair? alist*)
	  (begin
	    (if (not (pair? (car alist*)))
		(error:wrong-type-argument alist "alist" 'assq))
	    (if (eq? (car (car alist*)) key)
		(car alist*)
		(loop (cdr alist*))))
	  (begin
	    (if (not (null? alist*))
		(error:wrong-type-argument alist "alist" 'assq))
	    #F)))))
