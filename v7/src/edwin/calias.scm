;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/calias.scm,v 1.8 1989/08/14 09:22:15 cph Rel $
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

;;;; Alias Characters

(declare (usual-integrations))

(define alias-characters '())

(define (define-alias-char char alias)
  (let ((entry (assq char alias-characters)))
    (if entry
	(set-cdr! entry alias)
	(set! alias-characters (cons (cons char alias) alias-characters))))
  unspecific)

(define (undefine-alias-char char)
  (set! alias-characters (del-assq! char alias-characters))
  unspecific)

(define (remap-alias-char char)
  (let ((entry (assq char alias-characters)))
    (cond (entry
	   (remap-alias-char (cdr entry)))
	  ((odd? (quotient (char-bits char) 2)) ;Control bit is set
	   (let ((code (char-code char))
		 (remap
		  (lambda (code)
		    (make-char code (- (char-bits char) 2)))))
	     (cond ((<= #x40 code #x5F) (remap (- code #x40)))
		   ((<= #x61 code #x7A) (remap (- code #x60)))
		   (else char))))
	  (else char))))

(define (unmap-alias-char char)
  (if (and (ascii-controlified? char)
	   (let ((code (char-code char)))
	     (not (or (= code #x09)	;tab
		      (= code #x0A)	;linefeed
		      (= code #x0C)	;page
		      (= code #x0D)	;return
		      (= code #x1B)	;altmode
		      )))
	   (even? (quotient (char-bits char) 2)))
      (unmap-alias-char
       (make-char (let ((code (char-code char)))
		    (+ code (if (<= #x01 code #x1A) #x60 #x40)))
		  (+ (char-bits char) 2)))
      (let ((entry
	     (list-search-positive alias-characters
	       (lambda (entry)
		 (eqv? (cdr entry) char)))))
	(if entry
	    (unmap-alias-char (car entry))
	    char))))

(define-integrable (ascii-controlified? char)
  (< (char-code char) #x20))

(define-variable enable-emacs-key-names
  "*If true, keys are shown using Emacs-style names."
  true)

(define (char-name char)
  (if (ref-variable enable-emacs-key-names)
      (emacs-char-name char true)
      (char->name (unmap-alias-char char))))

(define (emacs-char-name char handle-prefixes?)
  (let ((code (char-code char))
	(bits (char-bits char))
	(normal (lambda () (char->name (unmap-alias-char char)))))
    (let ((process-code
	   (lambda ()
	     (cond ((< #x20 code #x7F) (char->name (make-char code 0)))
		   ((= code #x09) "TAB")
		   ((= code #x0A) "LFD")
		   ((= code #x0D) "RET")
		   ((= code #x1B) "ESC")
		   ((= code #x20) "SPC")
		   ((= code #x7F) "DEL")
		   (else
		    (char->name
		     (make-char (+ code (if (<= #x01 code #x1A) #x60 #x40))
				2)))))))
      (cond ((zero? bits) (process-code))
	    ((not handle-prefixes?) (normal))
	    ((= 1 bits) (string-append "ESC " (process-code)))
	    ((= 2 bits) (string-append "C-^ " (process-code)))
	    ((= 3 bits) (string-append "C-z " (process-code)))
	    (else (normal))))))

(define (xchar->name xchar)
  (let ((chars (xchar->list xchar)))
    (string-append-separated
     (char-name (car chars))
     (let ((char-name
	    (if (ref-variable enable-emacs-key-names)
		(lambda (char)
		  (emacs-char-name char false))
		(lambda (char)
		  (char->name (unmap-alias-char char))))))
       (let loop ((chars (cdr chars)))
	 (if (null? chars)
	     ""
	     (string-append-separated
	      (char-name (car chars))
	      (loop (cdr chars)))))))))

(define (xchar<? x y)
  (let loop ((x (xchar->list x)) (y (xchar->list y)))
    (or (char<? (car x) (car y))
	(and (char=? (car x) (car y))
	     (not (null? (cdr y)))
	     (or (null? (cdr x))
		 (loop (cdr x) (cdr y)))))))

(define (xchar->list xchar)
  (cond ((char? xchar)
	 (list xchar))
	((and (not (null? xchar))
	      (list-of-type? xchar char?))
	 xchar)
	((and (string? xchar)
	      (not (string-null? xchar)))
	 (string->list xchar))
	(else
	 (error "Not a character or list of characters" xchar))))