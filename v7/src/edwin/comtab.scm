;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Command Tables

(declare (usual-integrations))

(define make-comtab)
(define comtab?)
(define comtab-entry)
(define prefix-char-list?)
(define comtab-key-bindings)
(define define-key)
(define define-prefix-key)
(define define-default-key)

(define comtab-package
  (make-environment

(set! make-comtab
(named-lambda (make-comtab)
  (vector comtab-tag (cons '() '()))))

(set! comtab?
(named-lambda (comtab? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? comtab-tag (vector-ref object 0)))))

(define comtab-tag "Comtab")
(define (comtab-dispatch-alists comtab) (vector-ref comtab 1))

(define-unparser comtab-tag
  (lambda (comtab)
    (write-string "Comtab ")
    (write (primitive-datum comtab))))

(define (remap-char char)
  (char-upcase (remap-alias-char char)))

(define (set-comtab-entry! alists char command)
  (let ((char (remap-char char)))
    (let ((entry (assq char (cdr alists))))
      (if entry
	  (set-cdr! entry command)
	  (set-cdr! alists (cons (cons char command) (cdr alists)))))))

(define (make-prefix-char! alists char alists*)
  (let ((char (remap-char char)))
    (let ((entry (assq char (car alists))))
      (if entry
	  (set-cdr! entry alists*)
	  (set-car! alists (cons (cons char alists*) (car alists)))))))

(define (comtab-lookup-prefix comtabs char receiver #!optional if-undefined)
  (define (loop char->alist chars)
    (let ((entry (assq (remap-char (car chars)) char->alist)))
      (if entry
	  (if (null? (cddr chars))
	      (receiver (cdr entry) (cadr chars))
	      (loop (cadr entry) (cdr chars)))
	  (if (unassigned? if-undefined)
	      (error "Not a prefix character" (car chars))
	      (if-undefined)))))
  (cond ((char? char)
	 (receiver (comtab-dispatch-alists (car comtabs)) char))
	((pair? char)
	 (if (null? (cdr char))
	     (receiver (comtab-dispatch-alists (car comtabs)) (car char))
	     (loop (car (comtab-dispatch-alists (car comtabs))) char)))
	(else
	 (error "Unrecognizable character" char))))

(set! comtab-entry
(named-lambda (comtab-entry comtabs xchar)
  (define (continue)
    (cond ((null? (cdr comtabs)) bad-command)
	  ((comtab? (cadr comtabs)) (comtab-entry (cdr comtabs) xchar))
	  (else (cadr comtabs))))
  (comtab-lookup-prefix comtabs xchar
    (lambda (alists char)
      (let ((entry (assq (remap-char char) (cdr alists))))
	(if entry
	    (cdr entry)
	    (continue))))
    continue)))

(define bad-command
  (name->command "^R Bad Command"))

(set! prefix-char-list?
(named-lambda (prefix-char-list? comtabs chars)
  (define (loop char->alist chars)
    (or (null? chars)
	(let ((entry (assq (remap-char (car chars)) char->alist)))
	  (if entry
	      (loop (cadr entry) (cdr chars))
	      (and (not (null? (cdr comtabs)))
		   (comtab? (cadr comtabs))
		   (prefix-char-list? (cdr comtabs) chars))))))
  (loop (car (comtab-dispatch-alists (car comtabs))) chars)))

(set! define-key
(named-lambda (define-key mode-name char command-name)
  (let ((comtabs (mode-comtabs (name->mode mode-name)))
	(command (name->command command-name)))
    (cond ((or (char? char) (pair? char))
	   (%define-key comtabs char command))
	  ((char-set? char)
	   (for-each (lambda (char) (%define-key comtabs char command))
		     (char-set-members char)))
	  (else (error "DEFINE-KEY: Not a character" char))))
  char))

(define (%define-key comtabs xchar command)
  (comtab-lookup-prefix comtabs xchar
    (lambda (alists char)
      (set-comtab-entry! alists char command))))

(set! define-prefix-key
(named-lambda (define-prefix-key mode-name char command-name)
  (let ((comtabs (mode-comtabs (name->mode mode-name)))
	(command (name->command command-name)))
    (cond ((or (char? char) (pair? char))
	   (comtab-lookup-prefix comtabs char
	     (lambda (alists char)
	       (set-comtab-entry! alists char command)
	       (make-prefix-char! alists char (cons '() '())))))
	  (else (error "DEFINE-PREFIX-KEY: Not a character" char))))
  char))

(set! define-default-key
(named-lambda (define-default-key mode-name command-name)
  (let ((comtabs (mode-comtabs (name->mode mode-name))))
    (if (not (or (null? (cdr comtabs)) (command? (cadr comtabs))))
	(error "Can't define default key for this mode" mode-name))
    (set-cdr! comtabs (list (name->command command-name))))  'DEFAULT-KEY))

(set! comtab-key-bindings
(named-lambda (comtab-key-bindings comtabs command)
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
      (cond ((null? alist) '())
	    ((eq? command (cdar alist))
	     (cons (caar alist) (search-command-map (cdr alist))))
	    (else
	     (search-command-map (cdr alist)))))

    (search-prefix-map (car dispatch-alists)))

  ;; Filter out shadowed bindings.
  (list-transform-positive (search-comtabs comtabs)
    (lambda (xchar)
      (eq? command (comtab-entry comtabs xchar))))))

;;; end COMTAB-PACKAGE
))

;;; Edwin Variables:
;;; Scheme Environment: (access comtab-package edwin-package)
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
