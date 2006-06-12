(declare (usual-integrations))

(define-structure
    (pc-sample-zone
     (conc-name pc-sample-zone/)
     (constructor %make-pc-sample-zone (name index))
     (print-procedure
      (standard-unparser-method 'PC-SAMPLE-ZONE
	(lambda (zone port)
	  (write-char #\space port)
	  (write (pc-sample-zone/name zone) port)
	  (write-char #\space port)
	  (write (pc-sample-zone/count zone) port)))))

  name					; user name
  index					; index if user zone
  (with-count 0))

;; List of either (1) weak `zone . index' pairs, (2) available +ve
;; indexes or (3) unavailable -ve indexes (i.e. during allocation).
;; Zero is not a valid index and is reserved for the zone `Other'.
(define zones)

(define zone:other)
(define counts-cache)

(define (pc-sample-zone/count zone)
  (let ((index  (pc-sample-zone/index zone)))
    (if (and (<= 0 index) (< index (flo:vector-length counts-cache)))
	(flo:vector-ref counts-cache index)
	'COUNT-UNKNOWN)))

(define (make-pc-sample-zone name)
  (define (allocate!)
    (let loop ((pair zones))
      (and (pair? pair)
	   (let ((mark  (car pair)))
	     (cond ((and (fixnum? mark) (> mark 0))
		    (set-car! pair (- mark))
		    pair)
		   ((and (weak-pair? mark) (not (system-pair-car mark)))
		    (set-car! pair (- (system-pair-cdr mark)))
		    pair)
		   (else ;assume -ve fixnum
		    (loop (cdr pair))))))))

  (let ((pair (without-interrupts allocate!)))
    (if pair
	(let* ((index (- (car pair)))
	       (zone (%make-pc-sample-zone name index)))
	  (set-car! pair (weak-cons zone index))
	  zone)
	(error "Out of free zone indexes"))))

(define (get-zones)
  (let loop ((list zones))
    (cond ((null? list) '())
	  ((and (weak-pair? (car list)) (system-pair-car (car list)))
	   => (lambda (zone) (cons zone (loop (cdr list)))))
	  (else (loop (cdr list))))))

(define (wrap-with-zone procedure zone)
  (define-integrable set-zone! (ucode-primitive %pc-sample/set-zone! 1))
  (if (fixnum? (pc-sample-zone/index zone))
      (let ()
	;; The following wrappers need to be closed over the zone to stop it
	;; begin GC-ed.
	(define (default-wrapper . arguments)
	  (cond ((set-zone! (pc-sample-zone/index zone))
		 => (lambda (previous-zone)
		      (let ((result (apply procedure arguments)))
			(set-zone! previous-zone)
			result)))
		(else
		 (apply procedure arguments))))

	(define (wrapper/1-arg arg-1)
	  (cond ((set-zone! (pc-sample-zone/index zone))
		 => (lambda (previous-zone)
		      (let ((result (procedure arg-1)))
			(set-zone! previous-zone)
			result)))
		(else
		 (procedure arg-1))))

	(define (wrapper/2-args arg-1 arg-2)
	  (cond ((set-zone! (pc-sample-zone/index zone))
		 => (lambda (previous-zone)
		      (let ((result (procedure arg-1 arg-2)))
			(set-zone! previous-zone)
			result)))
		(else
		 (procedure arg-1 arg-2))))

	(define (wrapper/3-args arg-1 arg-2 arg-3)
	  (cond ((set-zone! (pc-sample-zone/index zone))
		 => (lambda (previous-zone)
		      (let ((result (procedure arg-1 arg-2 arg-3)))
			(set-zone! previous-zone)
			result)))
		(else
		 (procedure arg-1 arg-2 arg-3))))
	  
	(let ((arity (procedure-arity procedure)))
	  (cond ((equal? arity '(1 . 1)) wrapper/1-arg)
		((equal? arity '(2 . 2)) wrapper/2-args)
		((equal? arity '(3 . 3)) wrapper/3-args)
		(else default-wrapper))))
      (error "Cant wrap with" zone)))
    

(define (read-zone-counts!)
  ((ucode-primitive %pc-sample/read-zones! 1) counts-cache))

(define (reset-zone-counts!)
  ((ucode-primitive %pc-sample/clear-zones! 0)))

(define (display-zone-report)
  (read-zone-counts!)
  (let ((zones (get-zones)))
    (let ((total (apply + (map pc-sample-zone/count zones))))
      (let ((pct (if (zero? total)
		     (lambda (zone) zone 0)
		     (lambda (zone)
		       (* 100. (/ (pc-sample-zone/count zone) total))))))
	(pp (sort (map (lambda (zone)
			 (list (pct zone) zone))
		       zones)
		  (lambda (x y) (> (car x) (car y)))))))))

(define (initialize-package!)
  (let* ((max-zone ((ucode-primitive %pc-sample/max-zone 0))))
    (set! zone:other (%make-pc-sample-zone 'other 0))
    (set! zones
	  (cons (weak-cons zone:other #f)
		(cdr (make-initialized-list max-zone identity-procedure))))
    (set! counts-cache (flo:vector-cons max-zone))
    (reset-zone-counts!)
    (read-zone-counts!)))