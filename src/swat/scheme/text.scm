;;; -*- Scheme -*-

;;; Scrollable text widgets (only vertical scrollbar makes sense,
;;; since text can't be extended horizontally without changing the
;;; size of the top level window).

#|
(define (make-scrollable-text . options)
  (let ((text (apply make-text options))
	(vscroll (make-scrollbar '(-orient vert))))
    (let ((sb-command
	   (lambda ()
	     (ask-widget
	      vscroll
	      `(configure -command
			  ,(string-append (tk-widget->pathname text)
					  " yview")))))
	  (c-command
	   (lambda ()
	     (ask-widget
	      text
	      `(configure -yscrollcommand
			  ,(string-append (tk-widget->pathname vscroll) " set"))))))
      (defer text sb-command)
      (defer vscroll c-command)
      (make-hbox text vscroll))))
|#

(define (make-scrollable-text . options)
  (let ((text (apply make-text options))
	(vscroll (make-scrollbar '(-orient vert))))
    (let ((c-command
	   (lambda ()
	     (ask-widget
	      text
	      `(configure -yscrollcommand
			  ,(string-append (tk-widget->pathname vscroll) " set"))))))
      (defer vscroll c-command)
      (set-callback!
       vscroll
       (lambda (n)
	 (let ((n (string->number n)))
	   (ask-widget text `(yview -pickplace ,n)))))
      (make-hbox text vscroll))))

(define (scrollable-text-text scrollable-text)
  (car (box-children scrollable text)))

(define (scrollable-text-vscroll scrollable-text)
  (cadr (box-children scrollable-text)))



;;; Text has special protect-from-gc! procedures

(define (text-protect-from-gc! text stuff)
  (let ((crud (crud-that-I-dont-want-to-gc-away text)))
    (set-cdr! crud (cons stuff (cdr crud))))
  'done)

(define (text-unprotect-from-gc! text stuff)
  (let ((crud (crud-that-I-dont-want-to-gc-away text)))  
    (set-cdr! crud (delq! stuff (cdr crud))))
  'done)  

(define (text-flush-protect-list! text)
  (let ((crud (crud-that-I-dont-want-to-gc-away text)))  
    (set-cdr! crud '()))
  'done)  



;;; TextTags

(define (make-text-tag text index1 . index2)
  (let ((name (tk-gen-name "texttag")))
    (ask-widget text `(tag add ,name ,index1 ,@index2))
    (let ((texttag (make-texttag texttag-ask-widget
				 texttag-add-event-handler!
				 'invalid
				 name
				 text
				 '())))
      (text-protect-from-gc! text texttag)
      texttag)))

(define (texttag-add-event-handler! tag event handler substitutions)
  (let ((text (TextTag.text tag))
	(handler (proc-with-transformed-args handler substitutions)))
    (set-texttag.callbacks! tag
			    (cons handler (texttag.callbacks tag)))
    (ask-widget text
		`(tag bind
		  ,(TextTag.name tag)
		  ,event
		  ("SchemeCallBack" ,(object-hash handler *our-hash-table*)
				    ,@substitutions)))))

(define (texttag-ask-widget tag arg-list)
  (let* ((tag-name (TextTag.name tag))
	 (text     (TextTag.text tag))
	 (command  (car arg-list))
	 (new-arg-list (cons "tag"
			     (cons command
				   (cons tag-name (cdr arg-list))))))
    (let ((result (ask-widget text new-arg-list)))
      (if (eq? command 'delete)
	  (text-unprotect-from-gc! text tag))
      result)))

