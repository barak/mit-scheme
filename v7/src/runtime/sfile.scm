#| -*-Scheme-*-

$Id: sfile.scm,v 14.37 2004/10/28 22:39:56 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1999,2001,2003,2004 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Simple File Operations
;;; package: (runtime simple-file-ops)

(declare (usual-integrations))

(define (file-exists-direct? filename)
  (let ((result
	 ((ucode-primitive file-exists-direct? 1)
	  (->namestring (merge-pathnames filename)))))
    (if (eq? 0 result)
	#t
	result)))

(define (file-exists-indirect? filename)
  (let ((result
	 ((ucode-primitive file-exists? 1)
	  (->namestring (merge-pathnames filename)))))
    (if (eq? 0 result)
	#f
	result)))

(define file-exists? file-exists-indirect?)

(define file-type-direct)
(define file-type-indirect)
(let ((make-file-type
       (lambda (procedure)
	 (lambda (filename)
	   (let ((n (procedure (->namestring (merge-pathnames filename)))))
	     (and n
		  (let ((types
			 '#(REGULAR
			    DIRECTORY
			    UNIX-SYMBOLIC-LINK
			    UNIX-CHARACTER-DEVICE
			    UNIX-BLOCK-DEVICE
			    UNIX-NAMED-PIPE
			    UNIX-SOCKET
			    OS2-NAMED-PIPE
			    WIN32-NAMED-PIPE)))
		    (if (fix:< n (vector-length types))
			(vector-ref types n)
			'UNKNOWN))))))))
  (set! file-type-direct
	(make-file-type (ucode-primitive file-type-direct 1)))
  (set! file-type-indirect
	(make-file-type (ucode-primitive file-type-indirect 1))))

(define (file-regular? filename)
  (eq? 'REGULAR (file-type-indirect filename)))

(define (file-directory? filename)
  (eq? 'DIRECTORY (file-type-indirect filename)))

(define (file-symbolic-link? filename)
  ((ucode-primitive file-symlink? 1)
   (->namestring (merge-pathnames filename))))
(define file-soft-link? file-symbolic-link?)

(define (file-access filename amode)
  ((ucode-primitive file-access 2)
   (->namestring (merge-pathnames filename))
   amode))

(define (file-readable? filename)
  (file-access filename 4))

(define (file-writeable? filename)
  ((ucode-primitive file-access 2)
   (let ((pathname (merge-pathnames filename)))
     (let ((filename (->namestring pathname)))
       (if ((ucode-primitive file-exists? 1) filename)
	   filename
	   (directory-namestring pathname))))
   2))
(define file-writable? file-writeable?) ;upwards compatability

(define (file-executable? filename)
  (file-access filename 1))

(define (file-touch filename)
  ((ucode-primitive file-touch 1) (->namestring (merge-pathnames filename))))

(define (make-directory name)
  ((ucode-primitive directory-make 1)
   (->namestring (directory-pathname-as-file (merge-pathnames name)))))

(define (delete-directory name)
  ((ucode-primitive directory-delete 1)
   (->namestring (directory-pathname-as-file (merge-pathnames name)))))

(define (rename-file from to)
  ((ucode-primitive file-rename) (->namestring (merge-pathnames from))
				 (->namestring (merge-pathnames to))))

(define (delete-file filename)
  ((ucode-primitive file-remove) (->namestring (merge-pathnames filename))))

(define (hard-link-file from to)
  ((ucode-primitive file-link-hard 2) (->namestring (merge-pathnames from))
				      (->namestring (merge-pathnames to))))

(define (soft-link-file from to)
  ((ucode-primitive file-link-soft 2) (->namestring from)
				      (->namestring (merge-pathnames to))))

(define (delete-file-no-errors filename)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler (list condition-type:file-error
				   condition-type:port-error)
	 (lambda (condition)
	   condition
	   (k #f))
       (lambda ()
	 (delete-file filename)
	 #t)))))

(define (file-eq? x y)
  ((ucode-primitive file-eq?) (->namestring (merge-pathnames x))
			      (->namestring (merge-pathnames y))))

(define (current-file-time)
  (call-with-temporary-file-pathname file-modification-time))

(define (directory-file-names directory #!optional include-dots?)
  (let ((channel
	 (directory-channel-open
	  (->namestring (pathname-as-directory directory))))
	(include-dots?
	 (if (default-object? include-dots?) #f include-dots?)))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop
	     (if (and (not include-dots?)
		      (or (string=? "." name)
			  (string=? ".." name)))
		 result
		 (cons name result)))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (file-processed? filename input-type output-type)
  (file-modification-time<=? (pathname-default-type filename input-type)
			     (pathname-new-type filename output-type)))

(define (file-modification-time<? p1 p2)
  (< (or (file-modification-time p1) -1)
     (or (file-modification-time p2) -1)))

(define (file-modification-time<=? p1 p2)
  (<= (or (file-modification-time p1) -1)
      (or (file-modification-time p2) -1)))

;;;; Temporary files

(define (call-with-temporary-filename receiver)
  (call-with-temporary-file-pathname
   (lambda (pathname)
     (receiver (->namestring pathname)))))

(define (call-with-temporary-file-pathname receiver)
  (let ((pathname (temporary-file-pathname)))
    (dynamic-wind
     (lambda () unspecific)
     (lambda () (receiver pathname))
     (lambda () (deallocate-temporary-file pathname)))))

(define (allocate-temporary-file pathname)
  (and (not (file-exists? pathname))
       (let ((objects (get-fixed-objects-vector))
	     (slot (fixed-objects-vector-slot 'FILES-TO-DELETE))
	     (filename (->namestring pathname)))
	 (without-interrupts
	  (lambda ()
	    (and (file-touch pathname)
		 (begin
		   (vector-set! objects slot
				(cons filename (vector-ref objects slot)))
		   ((ucode-primitive set-fixed-objects-vector! 1) objects)
		   #t)))))))

(define (deallocate-temporary-file pathname)
  (delete-file-no-errors pathname)
  (let ((objects (get-fixed-objects-vector))
	(slot (fixed-objects-vector-slot 'FILES-TO-DELETE))
	(filename (->namestring pathname)))
    (without-interrupts
     (lambda ()
       (vector-set! objects slot
		    (delete! filename (vector-ref objects slot)))
       ((ucode-primitive set-fixed-objects-vector! 1) objects)))))

;;;; Init files

(define (guarantee-init-file-specifier object procedure)
  (if (not (init-file-specifier? object))
      (error:wrong-type-argument object "init-file specifier" procedure)))

(define (init-file-specifier? object)
  (and (list? object)
       (for-all? object
	 (lambda (object)
	   (and (string? object)
		(not (string-null? object)))))))

(define (guarantee-init-file-directory pathname)
  (let ((directory (user-homedir-pathname)))
    (if (not (file-directory? directory))
	(error "Home directory doesn't exist:" directory)))
  (let loop ((pathname pathname))
    (let ((directory (directory-pathname pathname)))
      (if (not (file-directory? directory))
	  (begin
	    (loop (directory-pathname-as-file directory))
	    (make-directory directory))))))

(define (open-input-init-file specifier)
  (open-input-file (init-file-specifier->pathname specifier)))

(define (open-output-init-file specifier #!optional append?)
  (let ((pathname (init-file-specifier->pathname specifier)))
    (guarantee-init-file-directory pathname)
    (open-output-file pathname (if (default-object? append?) #f append?))))

;;;; MIME types

(define (pathname-mime-type pathname)
  (pathname-type->mime-type (pathname-type pathname)))

(define (pathname-type->mime-type type)
  (and (string? type)
       (let ((string (os/suffix-mime-type type)))
	 (and string
	      (string->mime-type string)))))

(define-record-type <mime-type>
    (%%make-mime-type top-level subtype)
    mime-type?
  (top-level mime-type/top-level)
  (subtype mime-type/subtype))

(define (make-mime-type top-level subtype)
  (guarantee-mime-token top-level 'MAKE-MIME-TYPE)
  (guarantee-mime-token subtype 'MAKE-MIME-TYPE)
  (%make-mime-type top-level subtype))

(define (%make-mime-type top-level subtype)
  (let ((e (vector-length top-level-mime-types))
	(new (lambda () (%%make-mime-type top-level subtype))))
    (let loop ((i 0))
      (if (fix:< i e)
	  (if (eq? (vector-ref top-level-mime-types i) top-level)
	      (hash-table/intern! (vector-ref interned-mime-types i)
				  subtype
				  new)
	      (loop (fix:+ i 1)))
	  (hash-table/intern! unusual-interned-mime-types
			      (cons top-level subtype)
			      new)))))

(define top-level-mime-types
  '#(TEXT IMAGE AUDIO VIDEO APPLICATION MULTIPART MESSAGE))

(set-record-type-unparser-method! <mime-type>
  (standard-unparser-method 'MIME-TYPE
    (lambda (mime-type port)
      (write-char #\space port)
      (write-string (mime-type->string mime-type) port))))

(define interned-mime-types)
(define unusual-interned-mime-types)
(define char-set:mime-token)
(define (initialize-package!)
  (set! interned-mime-types
	(let ((e (vector-length top-level-mime-types)))
	  (let ((v (make-vector e)))
	    (do ((i 0 (fix:+ i 1)))
		((not (fix:< i e)))
	      (vector-set! v i (make-eq-hash-table)))
	    v)))
  (set! unusual-interned-mime-types
	(make-equal-hash-table))
  (set! char-set:mime-token
	(char-set-difference (ascii-range->char-set #x21 #x7F)
			     (string->char-set "()<>@,;:\\\"/[]?=")))
  unspecific)

(define (mime-type->string mime-type)
  (guarantee-mime-type mime-type 'MIME-TYPE->STRING)
  (string-append (symbol-name (mime-type/top-level mime-type))
		 "/"
		 (symbol-name (mime-type/subtype mime-type))))

(define (string->mime-type string)
  (guarantee-mime-type-string string 'STRING->MIME-TYPE)
  (let ((slash (string-find-next-char string #\/)))
    (%make-mime-type (intern (string-head string slash))
		     (intern (string-tail string (fix:+ slash 1))))))

(define (mime-type-string? object)
  (and (string? object)
       (string-is-mime-type? object)))

(define (string-is-mime-type? string)
  (let ((end (string-length string)))
    (let ((i (check-mime-token-syntax string 0 end)))
      (and (fix:> i 0)
	   (fix:< i end)
	   (char=? (string-ref string i) #\/)
	   (fix:< (fix:+ i 1) end)
	   (fix:= end (check-mime-token-syntax string (fix:+ i 1) end))
	   i))))

(define (mime-token? object)
  (and (interned-symbol? object)
       (string-is-mime-token? (symbol-name object))))

(define (mime-token-string? object)
  (and (string? object)
       (string-is-mime-token? object)))

(define (string-is-mime-token? string)
  (let ((end (string-length string)))
    (fix:= end (check-mime-token-syntax string 0 end))))

(define (check-mime-token-syntax string start end)
  (let loop ((i start))
    (if (fix:< i end)
	(if (char-set-member? char-set:mime-token (string-ref string i))
	    (loop (fix:+ i 1))
	    i)
	end)))

(define-syntax define-guarantee
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL EXPRESSION) (cdr form))
	 (let ((root (cadr form))
	       (desc (close-syntax (caddr form) environment)))
	   (let ((p-name (symbol root '?))
		 (g-name (symbol 'guarantee- root))
		 (e-name (symbol 'error:not- root)))
	     `(BEGIN
		(DEFINE (,g-name OBJECT CALLER)
		  (IF (NOT (,(close-syntax p-name environment) OBJECT))
		      (,(close-syntax e-name environment) OBJECT CALLER)))
		(DEFINE (,e-name OBJECT CALLER)
		  (ERROR:WRONG-TYPE-ARGUMENT OBJECT ,desc CALLER)))))
	 (ill-formed-syntax form)))))

(define-guarantee mime-type "MIME type")
(define-guarantee mime-type-string "MIME type string")
(define-guarantee mime-token "MIME token")
(define-guarantee mime-token-string "MIME token string")