#| -*-Scheme-*-

$Id: 23e2f3d8daabae96a8d228ac1c422715bc15f4de $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
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
       (let ((mime-type (hash-table/get local-type-map type #f)))
	 (if mime-type
	     (and (mime-type? mime-type)
		  mime-type)
	     (let ((string (os/suffix-mime-type type)))
	       (and string
		    (string->mime-type string)))))))

(define (associate-pathname-type-with-mime-type type mime-type)
  (guarantee-string type 'ASSOCIATE-PATHNAME-TYPE-WITH-MIME-TYPE)
  (guarantee-mime-type mime-type 'ASSOCIATE-PATHNAME-TYPE-WITH-MIME-TYPE)
  (hash-table/put! local-type-map type mime-type))

(define (disassociate-pathname-type-from-mime-type type)
  (guarantee-string type 'DISASSOCIATE-PATHNAME-TYPE-FROM-MIME-TYPE)
  (hash-table/put! local-type-map type 'DISASSOCIATED))

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
(define local-type-map)

(define (initialize-package!)
  (set! interned-mime-types
	(vector-map (lambda (token) token (make-eq-hash-table))
		    top-level-mime-types))
  (set! unusual-interned-mime-types (make-equal-hash-table))
  (set! char-set:mime-token
	(char-set-difference (ascii-range->char-set #x21 #x7F)
			     (string->char-set "()<>@,;:\\\"/[]?=")))
  (set! local-type-map (make-string-hash-table))
  (associate-pathname-type-with-mime-type "scm"
					  (make-mime-type 'TEXT 'X-SCHEME))
  unspecific)

(define (mime-type->string mime-type)
  (call-with-output-string
    (lambda (port)
      (write-mime-type mime-type port))))

(define (write-mime-type mime-type port)
  (guarantee-mime-type mime-type 'WRITE-MIME-TYPE)
  (write-string (symbol-name (mime-type/top-level mime-type)) port)
  (write-string "/" port)
  (write-string (symbol-name (mime-type/subtype mime-type)) port))

(define (string->mime-type string #!optional start end)
  (vector-ref (or (*parse-string parser:mime-type string start end)
		  (error:not-mime-type-string string 'STRING->MIME-TYPE))
	      0))

(define (mime-type-string? object)
  (and (string? object)
       (string-is-mime-type? object)))

(define (string-is-mime-type? string #!optional start end)
  (*match-string matcher:mime-type string start end))

(define (mime-token? object)
  (and (interned-symbol? object)
       (string-is-mime-token? (symbol-name object))))

(define (mime-token-string? object)
  (and (string? object)
       (string-is-mime-token? object)))

(define (string-is-mime-token? string #!optional start end)
  (*match-string matcher:mime-token string start end))

(define parser:mime-type
  (*parser
   (encapsulate (lambda (v)
		  (%make-mime-type (vector-ref v 0)
				   (vector-ref v 1)))
     (seq parser:mime-token "/" parser:mime-token))))

(define matcher:mime-type
  (*matcher (seq matcher:mime-token "/" matcher:mime-token)))

(define parser:mime-token
  (*parser (map intern (match matcher:mime-token))))

(define matcher:mime-token
  (*matcher (* (char-set char-set:mime-token))))

(define-guarantee mime-type "MIME type")
(define-guarantee mime-type-string "MIME type string")
(define-guarantee mime-token "MIME token")
(define-guarantee mime-token-string "MIME token string")