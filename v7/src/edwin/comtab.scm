;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/comtab.scm,v 1.57 1989/08/14 09:22:19 cph Rel $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Command Tables

(declare (usual-integrations))

(define-structure (comtab (constructor make-comtab ()))
  (dispatch-alists (cons '() '()) read-only true)
  (button-alist '()))

(define (set-comtab-entry! alists char command)
  (let ((char (remap-alias-char char)))
    (let ((entry (assq char (cdr alists))))
      (if entry
	  (set-cdr! entry command)
	  (set-cdr! alists (cons (cons char command) (cdr alists))))))
  unspecific)

(define (make-prefix-char! alists char alists*)
  (let ((char (remap-alias-char char)))
    (let ((entry (assq char (car alists))))
      (if entry
	  (set-cdr! entry alists*)
	  (set-car! alists (cons (cons char alists*) (car alists))))))
  unspecific)

(define (comtab-lookup-prefix comtabs key if-undefined if-defined)
  (cond ((char? key)
	 (if-defined (comtab-dispatch-alists (car comtabs)) key))
	((pair? key)
	 (if (null? (cdr key))
	     (if-defined (comtab-dispatch-alists (car comtabs)) (car key))
	     (let loop
		 ((char->alist (car (comtab-dispatch-alists (car comtabs))))
		  (chars key))
	       (let ((entry (assq (remap-alias-char (car chars)) char->alist)))
		 (if entry
		     (if (null? (cddr chars))
			 (if-defined (cdr entry) (cadr chars))
			 (loop (cadr entry) (cdr chars)))
		     (if if-undefined
			 (if-undefined)
			 (error "Not a prefix character" (car chars))))))))
	(else
	 (error "Illegal comtab key" key))))

(define (comtab-entry comtabs key)
  (let ((continue
	 (if (button? key)
	     (lambda ()
	       (and (not (null? (cdr comtabs)))
		    (comtab? (cadr comtabs))
		    (comtab-entry (cdr comtabs) key)))
	     (lambda ()
	       (cond ((null? (cdr comtabs))
		      bad-command)
		     ((comtab? (cadr comtabs))
		      (comtab-entry (cdr comtabs) key))
		     (else
		      (cadr comtabs)))))))
    (let ((try
	   (lambda (key alist)
	     (let ((entry (assq key alist)))
	       (if entry
		   (cdr entry)
		   (continue))))))
      (cond ((or (char? key) (pair? key))
	     (comtab-lookup-prefix comtabs key continue
	       (lambda (alists char)
		 (try (remap-alias-char char) (cdr alists)))))
	    ((button? key)
	     (try key (comtab-button-alist (car comtabs))))
	    (else
	     (error "Illegal comtab key" key))))))

(define bad-command
  (name->command '^r-bad-command))

(define (prefix-char-list? comtabs chars)
  (let loop
      ((char->alist (car (comtab-dispatch-alists (car comtabs))))
       (chars (if (list? chars) chars (list chars))))
    (or (null? chars)
	(let ((entry (assq (remap-alias-char (car chars)) char->alist)))
	  (if entry
	      (loop (cadr entry) (cdr chars))
	      (and (not (null? (cdr comtabs)))
		   (comtab? (cadr comtabs))
		   (prefix-char-list? (cdr comtabs) chars)))))))

(define (define-key mode key command)
  (let ((comtabs (mode-comtabs (->mode mode)))
	(command (->command command)))
    (if (button? key)
	(let ((alist (comtab-button-alist (car comtabs))))
	  (let ((entry (assq key alist)))
	    (if entry
		(set-cdr! entry command)
		(set-comtab-button-alist! (car comtabs)
					  (cons (cons key command) alist)))))
	(let ((normal-key
	       (lambda (key)
		 (comtab-lookup-prefix comtabs key false
		   (lambda (alists char)
		     (set-comtab-entry! alists char command))))))
	  (cond ((or (char? key) (pair? key))
		 (normal-key key))
		((char-set? key)
		 (for-each normal-key (char-set-members key)))
		(else
		 (error "Illegal comtab key" key))))))
  key)

(define (define-prefix-key mode key command)
  (let ((comtabs (mode-comtabs (->mode mode)))
	(command (->command command)))
    (if (or (char? key) (pair? key))
	(comtab-lookup-prefix comtabs key false
	  (lambda (alists char)
	    (set-comtab-entry! alists char command)
	    (make-prefix-char! alists char (cons '() '()))))
	(error "Illegal comtab key" key)))
  key)

(define (define-default-key mode command)
  (let ((comtabs (mode-comtabs (->mode mode)))
	(command (->command command)))
    (if (not (or (null? (cdr comtabs)) (command? (cadr comtabs))))
	(error "Can't define default key for this mode" mode))
    (set-cdr! comtabs (list command)))
  'DEFAULT-KEY)

(define (comtab-key-bindings comtabs command)
  (define (search-comtabs comtabs)
    (let ((bindings
	   (search-comtab '() (comtab-dispatch-alists (car comtabs)))))
      (if (and (not (null? (cdr comtabs)))
	       (comtab? (cadr comtabs)))
	  (append! bindings (search-comtabs (cdr comtabs)))
	  bindings)))

  (define (search-comtab prefix dispatch-alists)
    (define (search-prefix-map alist)
      (if (null? alist)
	  (map (lambda (char) (append prefix (list char)))
	       (search-command-map (cdr dispatch-alists)))
	  (append! (search-comtab (append prefix (list (caar alist)))
				  (cdar alist))
		   (search-prefix-map (cdr alist)))))

    (define (search-command-map alist)
      (cond ((null? alist)
	     '())
	    ((eq? command (cdar alist))
	     (cons (caar alist) (search-command-map (cdr alist))))
	    (else
	     (search-command-map (cdr alist)))))

    (search-prefix-map (car dispatch-alists)))

  ;; Filter out shadowed bindings.
  (list-transform-positive (search-comtabs comtabs)
    (lambda (xchar)
      (eq? command (comtab-entry comtabs xchar)))))

(define (comtab->alist comtab)
  (let loop ((prefix '()) (da (comtab-dispatch-alists comtab)))
    (append! (map (lambda (element)
		    (cons (append prefix (list (car element)))
			  (cdr element)))
		  (cdr da))
	     (append-map (lambda (element)
			   (loop (append prefix (list (car element)))
				 (cdr element)))
			 (car da)))))