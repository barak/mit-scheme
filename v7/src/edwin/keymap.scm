;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/keymap.scm,v 1.8 1989/08/14 09:22:41 cph Rel $
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

;;;; Command Summary

(declare (usual-integrations))

(define-command describe-bindings
  "Show a list of all defined keys, and their definitions.
The list is put in a buffer, which is displayed."
  ()
  (lambda ()
    (with-output-to-help-display
     (lambda ()
       (let ((alists (comtabs->alists (current-comtabs))))
	 (if (not (null? alists))
	     (let ((n
		    (+ (apply max
			      (map (lambda (elements)
				     (apply max
					    (map (lambda (element)
						   (string-length
						    (car element)))
						 elements)))
				   alists))
		       2)))
	       (let ((write-element
		      (lambda (element)
			(write-string
			 (string-append (pad-on-right-to (car element) n)
					" "
					(cdr element)))
			(newline))))
		 (let ((write-elements
			(lambda (elements)
			  (write-element '("key" . "binding"))
			  (write-element '("---" . "-------"))
			  (for-each (lambda (elements)
				      (newline)
				      (for-each write-element elements))
				    (sort-by-prefix elements)))))
		   (write-elements (car alists))
		   (for-each (lambda (elements)
			       (newline)
			       (write-elements elements))
			     (cdr alists)))))))))))

(define-command make-command-summary
  "Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first."
  ()
  (lambda ()
    (with-output-to-help-display
     (lambda ()
       (let ((alists (comtabs->alists (current-comtabs))))
	 (if (not (null? alists))
	     (begin
	       (write-summary-keymap (car alists))
	       (for-each (lambda (alist)
			   (write-string separator)
			   (write-summary-keymap alist))
			 (cdr alists)))))))))

(define separator
  "
===============================================================================

")

(define (write-summary-keymap alist)
  (let ((element-lists (sort-by-prefix alist)))
    (if (not (null? element-lists))
	(let loop
	    ((entry (car element-lists))
	     (element-lists (cdr element-lists)))
	  (write-summary-style-elements entry)
	  (if (not (null? element-lists))
	      (begin
		(newline)
		(loop (car element-lists) (cdr element-lists))))))))

(define (write-summary-style-elements elements)
  (let loop ((elements (reorder-list elements)))
    (if (not (null? elements))
	(let ((element->string
	       (lambda (element)
		 (string-append
		  (let ((string (car element)))
		    (if (< (string-length string) 9)
			(pad-on-right-to string 9)
			(let loop ((n 16))
			  (if (< (string-length string) n)
			      (pad-on-right-to string n)
			      (loop (+ n 8))))))
		  (cdr element)))))
	  (let ((string (element->string (car elements))))
	    (if (null? (cdr elements))
		(begin
		  (write-string string)
		  (newline))
		(begin
		  (write-string (pad-on-right-to string 39))
		  (write-char #\space)
		  (write-string (element->string (cadr elements)))
		  (newline)
		  (loop (cddr elements)))))))))

(define (reorder-list items)
  (let ((tail (list-tail items (integer-ceiling (length items) 2))))
    (let loop ((items items) (items* tail))
      (cond ((eq? items tail) '())
	    ((null? items*) (list (car items)))
	    (else
	     (cons* (car items)
		    (car items*)
		    (loop (cdr items) (cdr items*))))))))

(define (comtabs->alists comtabs)
  (let loop ((comtabs comtabs))
    (cons (sort-and-simplify (comtab->alist (car comtabs)))
	  (if (and (not (null? (cdr comtabs)))
		   (comtab? (cadr comtabs)))
	      (loop (cdr comtabs))
	      '()))))

(define (sort-and-simplify elements)
  (map (lambda (element)
	 (cons (xchar->name (car element))
	       (command-name-string (cdr element))))
       (sort elements (lambda (a b) (xchar<? (car a) (car b))))))

(define (sort-by-prefix elements)
  (let ((prefix-alist '()))
    (let ((make-entry
	   (lambda (prefix element)
	     (let ((entry
		    (list-search-positive prefix-alist
		      (lambda (entry)
			(string=? (car entry) prefix)))))
	       (if entry
		   (set-cdr! entry (cons element (cdr entry)))
		   (set! prefix-alist
			 (cons (list prefix element) prefix-alist)))
	       unspecific))))
      (for-each (lambda (element)
		  (let ((string (car element)))
		    (let ((has-prefix
			   (lambda (index)
			     (make-entry (string-head string index) element)))
			  (index (string-find-previous-char string #\space)))
		      (cond (index
			     (has-prefix (1+ index)))
			    ((string-prefix? "M-C-" string)
			     (has-prefix 4))
			    ((or (string-prefix? "M-" string)
				 (string-prefix? "C-" string))
			     (has-prefix 2))
			    (else
			     (make-entry "" element))))))
		elements))
    (map (lambda (entry)
	   (group-elements (reverse! (cdr entry))))
	 (sort prefix-alist (lambda (x y) (string<? (car x) (car y)))))))

(define (group-elements elements)
  (if (or (null? elements)
	  (null? (cdr elements)))
      elements
      (let ((command-name (cdar elements)))
	(if (string=? command-name (cdadr elements))
	    (let ((last
		   (let loop ((elements (cdr elements)))
		     (if (or (null? (cdr elements))
			     (not (string=? command-name (cdadr elements))))
			 elements
			 (loop (cdr elements))))))
	      (cons (cons (string-append (caar elements)
					 " .. "
					 (caar last))
			  command-name)
		    (group-elements (cdr last))))
	    (cons (car elements) (group-elements (cdr elements)))))))