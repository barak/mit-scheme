;;; -*-Scheme-*-
;;;
;;; $Id: calias.scm,v 1.22 2001/12/20 21:27:55 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

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
  #t
  boolean?)

(define (key-name key)
  (cond ((ref-variable enable-emacs-key-names) (emacs-key-name key #t))
	((char? key) (char->name (unmap-alias-key key)))
	((special-key? key) (special-key/name key))
	((button? key) (button-name key))
        (else (error "Unknown key type:" key))))

(define (button-name button)
  (string-append "button-"
		 (if (button/down? button) "down" "up")
		 "-"
		 (number->string (button/number button))))

(define (xkey->name xkey)
  (let ((keys (xkey->list xkey)))
    (string-append-separated
     (key-name (car keys))
     (let ((key-name
	    (if (ref-variable enable-emacs-key-names)
		(lambda (key)
		  (emacs-key-name key #f))
		(lambda (key)
		  (key-name (unmap-alias-key key))))))
       (let loop ((keys (cdr keys)))
	 (if (pair? keys)
	     (string-append-separated (key-name (car keys))
				      (loop (cdr keys)))
	     ""))))))

(define (emacs-key-name key handle-prefixes?)
  (cond ((char? key)
         (let ((code (char-code key))
               (bits (char-bits key)))
	   (define (prefix bits suffix)
	     (if (zero? bits)
		 suffix
		 (string-append "M-" suffix)))
	   (define (process-code bits)
	     (if (<= code #x20)
		 (cond ((= code #x09) (prefix bits "TAB"))
		       ((= code #x0A) (prefix bits "LFD"))
		       ((= code #x0D) (prefix bits "RET"))
		       ((= code #x1B) (prefix bits "ESC"))
		       ((= code #x20) (prefix bits "SPC"))
		       (else
			(string-append (if (zero? bits) "C-" "C-M-")
				       (string
					(integer->char
					 (+ (if (<= #x01 code #x1A) #x60 #x40)
					    code))))))
		 (prefix bits
			 (if (= code #x7F)
			     "DEL"
			     (vector-ref (ref-variable char-image-strings #f)
					 code)))))
	   (cond ((< bits 2)		; no bits or Meta only
		  (process-code bits))
		 ((and handle-prefixes? (< bits 4))
		  (string-append (if (= 2 bits) "C-^ " "C-z ")
				 (process-code 0)))
		 (else
		  (char->name (unmap-alias-key key))))))
	((special-key? key) (special-key/name key))
	((button? key) (button-name key))
        (else (error "Unknown key type:" key))))

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
	     (pair? (cdr y))
	     (or (not (pair? (cdr x)))
		 (loop (cdr x) (cdr y)))))))

(define (xkey->list xkey)
  (cond ((or (key? xkey) (button? xkey))
	 (list xkey))
	((and (pair? xkey)
	      (list-of-type? xkey key?))
	 xkey)
	((and (string? xkey)
	      (not (string-null? xkey)))
	 (string->list xkey))
	(else
	 (error "Not a key or list of keys" xkey))))

;;;; Special Keys (system-dependent)

(define-structure (special-key (constructor %make-special-key)
			       (conc-name special-key/)
			       (print-procedure
				(standard-unparser-method 'SPECIAL-KEY
				  (lambda (key port)
				    (write-char #\space port)
				    (write-string (special-key/name key)
						  port)))))
  (symbol #f read-only #t)
  (bucky-bits #f read-only #t))

(define (intern-special-key name bucky-bits)
  (let ((name-entry (assq name (cdr hashed-keys))))
    (if name-entry
	(let ((bits-entry (assq bucky-bits (cdr name-entry))))
	  (if bits-entry
	      (cdr bits-entry)
	      (let ((new-key (%make-special-key name bucky-bits)))
		(set-cdr! name-entry
			  (cons (cons bucky-bits new-key)
				(cdr name-entry)))
		new-key)))
	(let ((new-key (%make-special-key name bucky-bits)))
	  (set-cdr! hashed-keys
		    (cons (cons name (list (cons bucky-bits new-key)))
			  (cdr hashed-keys)))
	  new-key))))

(define hashed-keys
  (list 'HASHED-KEYS))

(define (special-key/name special-key)
  (string-append (bucky-bits->name (special-key/bucky-bits special-key))
		 (symbol-name (special-key/symbol special-key))))

(define (bucky-bits->name bits)
  (let ((bucky-bit-map '#("M-" "C-" "S-" "H-" "T-")))
    (let loop ((n (fix:- (vector-length bucky-bit-map) 1))
	       (bit (fix:lsh 1 (fix:- (vector-length bucky-bit-map) 1)))
	       (name ""))
      (cond ((fix:< n 0)
	     name)
	    ((fix:= 0 (fix:and bit bits))
	     (loop (fix:- n 1) (fix:lsh bit -1) name))
	    (else
	     (loop (fix:- n 1)
		   (fix:lsh bit -1)
		   (string-append (vector-ref bucky-bit-map n) name)))))))

(define (make-special-key name bits)
  (hook/make-special-key name bits))

(define hook/make-special-key
  intern-special-key)

;; Predefined special keys
(let-syntax ((make-key
	      (lambda (name)
		`(DEFINE ,name (INTERN-SPECIAL-KEY ',name 0)))))
  (make-key backspace)
  (make-key stop)
  (make-key f1)
  (make-key f2)
  (make-key f3)
  (make-key f4)
  (make-key menu)
  (make-key system)
  (make-key user)
  (make-key f5)
  (make-key f6)
  (make-key f7)
  (make-key f8)
  (make-key f9)
  (make-key f10)
  (make-key f11)
  (make-key f12)
  (make-key insertline)
  (make-key deleteline)
  (make-key insertchar)
  (make-key deletechar)
  (make-key home)
  (make-key prior)
  (make-key next)
  (make-key up)
  (make-key down)
  (make-key left)
  (make-key right)
  (make-key select)
  (make-key print))