;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/comtab.scm,v 1.52 1989/04/28 22:48:47 cph Exp $
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
  (dispatch-alists (cons '() '()) read-only true))

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

(define (comtab-lookup-prefix comtabs char receiver #!optional if-undefined)
  (define (loop char->alist chars)
    (let ((entry (assq (remap-alias-char (car chars)) char->alist)))
      (if entry
	  (if (null? (cddr chars))
	      (receiver (cdr entry) (cadr chars))
	      (loop (cadr entry) (cdr chars)))
	  (if (default-object? if-undefined)
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

(define (comtab-entry comtabs xchar)
  (let ((continue
	 (lambda ()
	   (cond ((null? (cdr comtabs)) bad-command)
		 ((comtab? (cadr comtabs)) (comtab-entry (cdr comtabs) xchar))
		 (else (cadr comtabs))))))
    (comtab-lookup-prefix comtabs xchar
      (lambda (alists char)
	(let ((entry (assq (remap-alias-char char) (cdr alists))))
	  (if entry
	      (cdr entry)
	      (continue))))
      continue)))

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

(define (define-key mode-name char command-name)
  (let ((comtabs (mode-comtabs (name->mode mode-name)))
	(command (name->command command-name)))
    (cond ((or (char? char) (pair? char))
	   (%define-key comtabs char command))
	  ((char-set? char)
	   (for-each (lambda (char) (%define-key comtabs char command))
		     (char-set-members char)))
	  (else
	   (error "not a character" char))))
  char)

(define (%define-key comtabs xchar command)
  (comtab-lookup-prefix comtabs xchar
    (lambda (alists char)
      (set-comtab-entry! alists char command))))

(define (define-prefix-key mode-name char command-name)
  (let ((comtabs (mode-comtabs (name->mode mode-name)))
	(command (name->command command-name)))
    (if (or (char? char) (pair? char))
	(comtab-lookup-prefix comtabs char
	  (lambda (alists char)
	    (set-comtab-entry! alists char command)
	    (make-prefix-char! alists char (cons '() '()))))
	(error "not a character" char)))
  char)

(define (define-default-key mode-name command-name)
  (let ((comtabs (mode-comtabs (name->mode mode-name))))
    (if (not (or (null? (cdr comtabs)) (command? (cadr comtabs))))
	(error "Can't define default key for this mode" mode-name))
    (set-cdr! comtabs (list (name->command command-name))))  'DEFAULT-KEY)

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