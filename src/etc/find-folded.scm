;;; A small program to find symbols that will change if case folded.

(define (print-directory-symbols file-entries #!optional port)
  (for-each (lambda (file-entry)
	      (print-file-symbols file-entry port))
	    file-entries))

(define (print-file-symbols file-entry #!optional port)
  (fresh-line port)
  (write-char #\( port)
  (write (car file-entry) port)
  (for-each (lambda (symbol)
	      (newline port)
	      (write-char #\space port)
	      (write symbol port))
	    (cdr file-entry))
  (write-char #\) port)
  (newline port))

(define (find-symbols-in-directory directory)
  (remove (lambda (p)
	    (null? (cdr p)))
	  (map find-symbols-in-file
	       (directory-read
		(merge-pathnames "*.scm" (pathname-as-directory directory))))))

(define (find-symbols-in-file filename)
  (with-notification
   (lambda (port)
     (write-string "Checking file " port)
     (write-string (->namestring filename) port))
   (lambda ()
     (let ((code
	    (parameterize ((param:reader-fold-case? #f))
	      (ignore-errors
	       (lambda ()
		 (read-file filename))))))
       (cons (->namestring filename)
	     (if (condition? code)
		 (begin (warn code) '())
		 (sort (find-symbols code) symbol<?)))))))

(define (find-symbols code)
  (cond ((interned-symbol? code)
	 (if (symbol-changes-when-case-folded? code)
	     (list code)
	     '()))
	((pair? code)
	 (lset-union eq?
		     (find-symbols (car code))
		     (find-symbols (cdr code))))
	((vector? code)
	 (find-symbols (vector->list code)))
	(else
	 '())))

(define (symbol-changes-when-case-folded? symbol)
  (string-find-first-index char-changes-when-case-folded?
			   (symbol->string symbol)))