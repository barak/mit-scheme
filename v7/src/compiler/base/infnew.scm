(declare (usual-integrations))

(define (generation-phase2 label-bindings external-labels)
  (make-compiler-info
   '()
   '()
   (list->vector
    (sort (map (lambda (association)
		 (make-label-info
		  (symbol->string (car association))
		  (cdr association)
		  (let loop ((external-labels external-labels))
		    (cond ((null? external-labels) false)
			  ((eq? (car association) (car external-labels)) true)
			  (else (loop (cdr external-labels)))))))
	       label-bindings)
	  (lambda (x y)
	    (< (label-info-offset x) (label-info-offset y)))))))