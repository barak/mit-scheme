(declare (usual-integrations))


(define (assq key alist)
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
	  #F))))


(define get #F)
(define put #F)

(let ((properties '()))
  (define (our-get x y)
    (let ((x-cut (assq x properties)))
      (if x-cut
	  (let ((value (assq y (cdr x-cut))))
	    (if value (cdr value) '()))
	  '())))
  (define (our-put x y z)
    (let ((x-cut (assq x properties)))
      (if x-cut
	  (let ((value (assq y (cdr x-cut))))
	    (if value
		(set-cdr! value z)
		(set-cdr! x-cut (cons (cons y z) (cdr x-cut)))))
	  (set! properties `((,x . ((,y . ,z))) ,@properties))))
    'OK)
  (set! get our-get)
  (set! put our-put))


(define gensym generate-uninterned-symbol)
