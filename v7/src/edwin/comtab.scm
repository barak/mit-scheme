;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/comtab.scm,v 1.60 1991/08/06 15:39:30 arthur Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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

(define (set-comtab-entry! alists key command)
  (let ((entry (assq key (cdr alists))))
    (if entry
	(set-cdr! entry command)
	(set-cdr! alists (cons (cons key command) (cdr alists))))))

(define (make-prefix-key! alists key alists*)
  (let ((entry (assq key (car alists))))
    (if entry
	(set-cdr! entry alists*)
	(set-car! alists
		  (cons (cons key alists*)
			(car alists))))))

(define (comtab-lookup-prefix comtabs key if-undefined if-defined)
  (let ((alists (comtab-dispatch-alists (car comtabs))))
    (cond ((key? key)
	   (if-defined alists (remap-alias-key key)))
	  ((pair? key)
	   (let ((keys (map remap-alias-key key)))
	     (let loop ((alists alists) (keys keys))
	       (let ((key (car keys))
		     (keys (cdr keys)))
		 (cond ((null? keys)
			(if-defined alists key))
		       ((assq key (car alists))
			=> (lambda (entry) (loop (cdr entry) keys)))
		       ((assq key (cdr alists))
			(error "Illegal prefix key:" key))
		       ((not if-undefined)
			(set-comtab-entry! alists
					   key
					   (ref-command-object prefix-key))
			(let ((alists* (cons '() '())))
			  (make-prefix-key! alists key alists*)
			  (loop alists* keys)))
		       (else
			(if-undefined)))))))
	  (else
	   (error "Illegal comtab key" key)))))

(define (comtab-entry comtabs key)
  (let ((continue
	 (if (button? key)
	     (lambda ()
	       (and (not (null? (cdr comtabs)))
		    (comtab? (cadr comtabs))
		    (comtab-entry (cdr comtabs) key)))
	     (lambda ()
	       (cond ((null? (cdr comtabs))
		      (ref-command-object undefined))
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
      (cond ((or (key? key) (pair? key))
	     (comtab-lookup-prefix comtabs key continue
	       (lambda (alists key)
		 (try key (cdr alists)))))
	    ((button? key)
	     (try key (comtab-button-alist (car comtabs))))
	    (else
	     (error "Illegal comtab key" key))))))

(define (prefix-key-list? comtabs keys)
  (let loop
      ((key->alist (car (comtab-dispatch-alists (car comtabs))))
       (keys (if (list? keys) keys (list keys))))
    (or (null? keys)
	(let ((entry (assq (remap-alias-key (car keys)) key->alist)))
	  (if entry
	      (loop (cadr entry) (cdr keys))
	      (and (not (null? (cdr comtabs)))
		   (comtab? (cadr comtabs))
		   (prefix-key-list? (cdr comtabs) keys)))))))

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
		   (lambda (alists key)
		     (set-comtab-entry! alists key command))))))
	  (cond ((or (key? key) (pair? key))
		 (normal-key key))
		((char-set? key)
		 (for-each normal-key (char-set-members key)))
		(else
		 (error "Illegal comtab key" key))))))
  key)

(define (define-prefix-key mode key command)
  (let ((comtabs (mode-comtabs (->mode mode)))
	(command (->command command)))
    (if (not (or (key? key) (pair? key)))
	(error "Illegal comtab key" key))
    (comtab-lookup-prefix comtabs key false
      (lambda (alists key)
	(set-comtab-entry! alists key command)
	(make-prefix-key! alists key (cons '() '())))))
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
	  (map (lambda (key) (append prefix (list key)))
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
    (lambda (xkey)
      (eq? command (comtab-entry comtabs xkey)))))

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