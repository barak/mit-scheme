;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/calias.scm,v 1.13 1992/04/22 20:51:33 mhwu Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
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

;;;; Alias Keys

(declare (usual-integrations))

(define alias-keys '())

(define (define-alias-key key alias)
  (let ((entry (assq key alias-keys)))
    (if entry
	(set-cdr! entry alias)
	(set! alias-keys (cons (cons key alias) alias-keys))))
  unspecific)

(define (undefine-alias-key key)
  (set! alias-keys (del-assq! key alias-keys))
  unspecific)

(define (remap-alias-key key)
  (let ((entry (assq key alias-keys)))
    (cond (entry
	   (remap-alias-key (cdr entry)))
	  ((and (char? key)
		(odd? (quotient (char-bits key) 2))) ;Control bit is set
	   (let ((code (char-code key))
		 (remap
		  (lambda (code)
		    (make-char code (- (char-bits key) 2)))))
	     (cond ((<= #x40 code #x5F) (remap (- code #x40)))
		   ((<= #x61 code #x7A) (remap (- code #x60)))
		   (else key))))
	  (else key))))

(define (unmap-alias-key key)
  (if (and (char? key)
	   (ascii-controlified? key)
	   (let ((code (char-code key)))
	     (not (or (= code #x09)	;tab
		      (= code #x0A)	;linefeed
		      (= code #x0C)	;page
		      (= code #x0D)	;return
		      (= code #x1B)	;altmode
		      )))
	   (even? (quotient (char-bits key) 2)))
      (unmap-alias-key
       (make-char (let ((code (char-code key)))
		    (+ code (if (<= #x01 code #x1A) #x60 #x40)))
		  (+ (char-bits key) 2)))
      (let ((entry
	     (list-search-positive alias-keys
	       (lambda (entry)
		 (eqv? (cdr entry) key)))))
	(if entry
	    (unmap-alias-key (car entry))
	    key))))

(define-integrable (ascii-controlified? char)
  (< (char-code char) #x20))

(define-variable enable-emacs-key-names
  "True means keys are shown using Emacs-style names."
  true
  boolean?)

(define (key-name key)
  (cond ((ref-variable enable-emacs-key-names)
	 (emacs-key-name key true))
	((char? key)
	 (char->name (unmap-alias-key key)))
	((special-key? key)
	 (special-key/name key))
        (else
          (error "key-name: Unknown key type" key))))

(define (xkey->name xkey)
  (let ((keys (xkey->list xkey)))
    (string-append-separated
     (key-name (car keys))
     (let ((key-name
	    (if (ref-variable enable-emacs-key-names)
		(lambda (key)
		  (emacs-key-name key false))
		(lambda (key)
		  (key-name (unmap-alias-key key))))))
       (let loop ((keys (cdr keys)))
	 (if (null? keys)
	     ""
	     (string-append-separated
	      (key-name (car keys))
	      (loop (cdr keys)))))))))

(define (emacs-key-name key handle-prefixes?)
  (cond ((char? key)
         (let ((code (char-code key))
               (bits (char-bits key)))
           (let ((prefix
                   (lambda (bits suffix)
                     (if (zero? bits)
                         suffix
                         (string-append "M-" suffix)))))
             (let ((process-code
                     (lambda (bits)
                       (cond ((< #x20 code #x7F)
                              (prefix bits (string (ascii->char code))))
                             ((= code #x09) (prefix bits "TAB"))
                             ((= code #x0A) (prefix bits "LFD"))
                             ((= code #x0D) (prefix bits "RET"))
                             ((= code #x1B) (prefix bits "ESC"))
                             ((= code #x20) (prefix bits "SPC"))
                             ((= code #x7F) (prefix bits "DEL"))
                             (else
                               (string-append
                                 (if (zero? bits) "C-" "C-M-")
                                 (string
                                   (ascii->char
                                     (+ code
                                        (if (<= #x01 code #x1A)
                                            #x60
                                            #x40))))))))))
               (cond ((< bits 2)
                      (process-code bits))
                     ((and handle-prefixes? (< bits 4))
                      (string-append (if (= 2 bits) "C-^ " "C-z ")
                                     (process-code 0)))
                     (else
                       (char->name (unmap-alias-key key))))))))
        ((special-key? key)
         (special-key/name key))
        (else
          (error "emacs-key-name: Unknown key type" key))))

(define (key? object)
  (or (char? object)
      (special-key? object)))

(define (key<? key1 key2)
  (if (char? key1)
      (if (char? key2)
	  (char<? key1 key2)
	  (<= (char-bits key1) (special-key/bucky-bits key2)))
      (let ((bits1 (special-key/bucky-bits key1)))
	(if (char? key2)
	    (< bits1 (char-bits key2))
	    (let ((bits2 (special-key/bucky-bits key2)))
	      (or (< bits1 bits2)
		  (and (= bits1 bits2)
		       (string<? (special-key/name key1)
				 (special-key/name key2)))))))))

(define (key=? key1 key2)
  (if (and (char? key1)
	   (char? key2))
      (char=? key1 key2)
      (and (special-key? key1)
	   (special-key? key2)
	   (string=? (special-key/name key1)
		     (special-key/name key2))
	   (= (special-key/bucky-bits key1)
	      (special-key/bucky-bits key2)))))

(define (xkey<? x y)
  (let loop ((x (xkey->list x)) (y (xkey->list y)))
    (or (key<? (car x) (car y))
	(and (key=? (car x) (car y))
	     (not (null? (cdr y)))
	     (or (null? (cdr x))
		 (loop (cdr x) (cdr y)))))))

(define (xkey->list xkey)
  (cond ((key? xkey)
	 (list xkey))
	((and (not (null? xkey))
	      (list-of-type? xkey key?))
	 xkey)
	((and (string? xkey)
	      (not (string-null? xkey)))
	 (string->list xkey))
	(else
	 (error "Not a key or list of keys" xkey))))