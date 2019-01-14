#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Syntax Expanders
;;; package: (ffi)


;;; C-include Syntax

(define-syntax C-include
  ;;     (C-include "library") ===> #f
  (sc-macro-transformer
   (lambda (form usage-env)
     (call-with-destructured-c-include-form
      form
      (lambda (library)
	(let ((ienv (senv->runtime usage-env)))
	  (if (and (environment-bound? ienv 'C-INCLUDES)
		   (environment-assigned? ienv 'C-INCLUDES))
	      (let ((value (environment-lookup ienv 'C-INCLUDES))
		    (err (lambda (msg val)
			   (error (string-append
				   "C-includes is already bound, " msg) val))))
		(if (c-includes? value)
		    (if (string=? (c-includes/library value) library)
			#f
			(err "to a different library:"
			     (c-includes/library value)))
		    (err "but not to a c-include structure:" value)))
	      (begin
		(environment-define ienv 'C-INCLUDES (load-c-includes library))
		#f))))))))

(define (call-with-destructured-c-include-form form receiver)
  ;; Calls RECEIVER with the library.
  (cond ((null? (cdr form))
	 (serror form "A library name is required"))
	((not (string? (cadr form)))
	 (serror form "The 1st arg must be a string"))
	(else
	 (if (not (null? (cddr form)))
	     (swarn form "Too many args"))
	 (receiver (cadr form)))))

(define (load-c-includes library)
  (let ((includes (fasload (system-library-pathname
			    (string-append library "-types.bin"))
			   (not c-include-noisily?)))
	(comment (fasload
		  (system-library-pathname
		   (string-append library "-const.bin"))
		  (not c-include-noisily?))))
      (let ((enums.struct-values
	     (if (scode-comment? comment) (scode-comment-expression comment)
		 (error:wrong-type-datum comment "a fasl comment"))))
	(warn-new-cdecls includes)
	(set-c-includes/enum-values! includes (car enums.struct-values))
	(set-c-includes/struct-values! includes (cadr enums.struct-values))
	includes)))

(define (warn-new-cdecls includes)
  (for-each
   (lambda (file.modtime)
     (let ((read-modtime (cdr file.modtime))
	   (this-modtime (file-modification-time (car file.modtime))))
       (if (and this-modtime (< read-modtime this-modtime))
	   (warn "New source file:" (car file.modtime)))))
   (c-includes/files includes)))


;;; C-> and C->= Syntaxes

(define-syntax C->
  ;; (C-> event "GdkEvent any type")
  ;; ===> (#[primitive c-peek-uint] event 14)
  ;; (C-> event "GdkEvent any window" window)
  ;; ===> (#[primitive c-peek-pointer] event 4 window)
  (sc-macro-transformer
   (lambda (form usage-env)
     (expand-c->-syntax #f form usage-env))))

(define-syntax C->=
  ;; (C->= event "GdkEvent any type"  (C-enum "GDK_MAP"))
  ;; ===> (#[primitive c-poke-uint] event 14)
  ;; (C->= event "GdkEvent any window" window)
  ;; ===> (#[primitive c-poke-pointer] event 4 window)
  (sc-macro-transformer
   (lambda (form usage-env)
     (expand-c->-syntax #t form usage-env))))

(define (expand-c->-syntax poke? whole-form usage-env)
  (call-with-destructured-c->-form
   whole-form
   (lambda (alien-form type-member-spec value-form)
     (let ((includes (find-c-includes usage-env))
	   (alien-form (close-syntax alien-form usage-env))
	   (value-form (and value-form (close-syntax value-form usage-env))))
       (call-with-initial-ctype
	type-member-spec whole-form
	(lambda (ctype member-spec)
	  (let ((type (ctype-definition ctype includes)))
	    (cond
	     ((or (ctype/basic? type)
		  (ctype/pointer? type)
		  (ctype/array? type)
		  (ctype/enum-defn? type))
	      (if (null? member-spec)
		  (if poke?
		      (expand-poke type alien-form 0 value-form whole-form)
		      (expand-peek type alien-form 0 value-form whole-form))
		  (let ((meta-type (cond ((ctype/basic? type) "Basic")
					 ((ctype/pointer? type) "Pointer")
					 ((ctype/array? type) "Array")
					 ((ctype/enum-defn? type) "Enum")
					 (else "?"))))
		    (swarn whole-form
			   (string-append
			    meta-type" types have no members")))))
	     ((or (ctype/struct-defn? type)
		  (ctype/union-defn? type))
	      (if (null? member-spec)
		  (swarn whole-form "Cannot peek a whole struct")
		  (let ((entry (assoc (cons* 'OFFSET ctype member-spec)
				      (c-includes/struct-values includes))))
		    (if (not entry)
			(swarn whole-form "No such member")
			(let ((offset (cadr entry))
			      (type (cddr entry)))
			  (let ((ctype (ctype-definition type includes)))
			    (if poke?
				(expand-poke ctype alien-form offset
					     value-form whole-form)
				(expand-peek ctype alien-form offset
					     value-form whole-form))))))))
	     (poke?
	      (swarn whole-form "Cannot poke C type" ctype))
	     (else
	      (swarn whole-form "Cannot peek C type" ctype))))))))))

(define (expand-poke ctype alien-form offset value-form whole-form)
  (cond ((not value-form)
	 (swarn whole-form "Missing value (3rd) arg"))
	((ctype/basic? ctype)
	 (let ((prim (ctype/primitive-modifier ctype)))
	   (if prim
	       `(,prim ,alien-form ,offset ,value-form)
	       (swarn whole-form "Cannot poke basic type" ctype))))
	((ctype/pointer? ctype)
	 (let ((prim (ucode-primitive c-poke-pointer 3)))
	   `(,prim ,alien-form ,offset ,value-form)))
	((ctype/array? ctype)
	 (swarn whole-form "Cannot poke a whole array"))
	((or (ctype/enum? ctype) (eq? ctype 'ENUM))
	 (let ((prim (ucode-primitive c-poke-uint 3)))
	   `(,prim ,alien-form ,offset ,value-form)))
	(else (swarn whole-form "Unexpected C type for poking" ctype))))

(define (expand-peek ctype alien-form offset value-form whole-form)
  (cond ((ctype/basic? ctype)
	 (if value-form (swarn whole-form "Ignoring extra (3rd) arg"))
	 (let ((prim (ctype/primitive-accessor ctype)))
	   (if prim
	       `(,prim ,alien-form ,offset)
	       (swarn whole-form "Cannot peek basic type" ctype))))
	((ctype/pointer? ctype)
	 `(,(ucode-primitive c-peek-pointer 3)
	   ,alien-form ,offset ,(or value-form '(MAKE-ALIEN))))
	((or (ctype/array? ctype) (ctype/struct? ctype))
	 (if value-form
	     `(LET ((VALUE ,value-form))
		(COPY-ALIEN-ADDRESS! VALUE ,alien-form)
		(ALIEN-BYTE-INCREMENT! VALUE ,offset)
		VALUE)
	     `(ALIEN-BYTE-INCREMENT ,alien-form ,offset)))
	((or (ctype/enum? ctype) (eq? ctype 'ENUM))
	 `(,(ucode-primitive c-peek-uint 2) ,alien-form ,offset))
	(else (swarn whole-form "Unexpected C type for peeking" ctype))))

(define (call-with-destructured-c->-form form receiver)
  ;; Calls RECEIVER with ALIEN, SPEC and VALUE (or #f) as in these forms:
  ;;
  ;;   (C-> ALIEN SPEC)		  VALUE = #f
  ;;   (C-> ALIEN SPEC* VALUE)    SPEC* specifies a pointer-type member
  ;;   (C->= ALIEN SPEC VALUE)
  ;;
  (let ((len (length form)))
    (if (< len 3)
	(swarn form "Too few args")
	(let ((alien-form (cadr form))
	      (type-member-spec (caddr form))
	      (value-form (and (= 4 len) (cadddr form))))
	  (if (< 4 len) (swarn form "Too many args"))
	  (if (not (string? type-member-spec))
	      (swarn form "2nd arg must be a string")
	      (let ((type-member-spec
		     (map string->symbol
			  (burst-string type-member-spec #\space #t))))
		(if (null? type-member-spec)
		    (swarn form "2nd arg is an empty string")
		    (receiver alien-form type-member-spec value-form))))))))


;;; C-enum Syntax

(define-syntax C-enum
  ;; (C-enum "GDK_MAP")
  ;; ===> 14
  ;; (C-enum "GdkEventType" 14)
  ;; ===> GDK_MAP
  ;; (C-enum "GdkEventType" FORM)
  ;; ===> (C-enum-name FORM '|GdkEventType|
  ;;                   '((|GDK_NOTHING| . -1) (|GDK_DELETE| . 0)...))
  (sc-macro-transformer
   (lambda (form usage-env)
     (call-with-destructured-c-enum-form
      form
      (lambda (name value-form)
	(let* ((includes (find-c-includes usage-env)))
	  (if (not value-form)
	      (lookup-enum-value name includes)
	      (if (integer? value-form)
		  (c-enum-name value-form name
				(c-enum-constant-values name form includes))
		  (let ((value (close-syntax value-form usage-env))
			(constants (c-enum-constant-values name form includes)))
		    `(C-ENUM-NAME ,value ',name ',constants))))))))))

(define (lookup-enum-value name includes)
  (let ((entry (assq name (c-includes/enum-values includes))))
    (if (not entry)
	(swarn name "No declaration of constant")
	(cdr entry))))

(define (c-enum-constant-values name form includes)
  (let ((defn (ctype-definition name includes))
	(vals (c-includes/enum-values includes)))
    (if (ctype/enum-defn? defn)
	(let loop ((consts (ctype-enum-defn/constants defn)))
	  (if (pair? consts)
	      (let* ((name (caar consts))
		     (entry (or (assq name vals)
				(begin
				  (swarn form "No value for enum constant")
				  (cons name #f)))))
		(cons entry (loop (cdr consts))))
	      '()))
	(swarn form "Not an enum type"))))

(define (call-with-destructured-c-enum-form form receiver)
  (let ((len (length form)))
    (if (< len 2)
	(swarn form "Too few args")
	(let ((type-str (cadr form))
	      (value-form (and (pair? (cddr form)) (caddr form))))
	  (if (< 3 len) (swarn form "Too many args"))
	  (if (not (string? type-str))
	      (swarn form "1st arg must be a string")
	      (let ((words (burst-string type-str #\space #t)))
		(if (null? words)
		    (swarn form "1st arg is an empty string")
		    (let ((name (cond ((and (string=? "enum" (car words))
					    (not (null? (cdr words)))
					    (null? (cddr words)))
				       `(ENUM ,(string->symbol (cadr words))))
				      ((null? (cdr words))
				       (string->symbol (car words)))
				      (else (swarn form
						   "Not an enum type name")))))
		      (if (and value-form (string? value-form))
			  (swarn form "2nd arg cannot be a string")
			  (receiver name value-form))))))))))


;;; C-sizeof and C-offset Syntaxes

(define-syntax C-sizeof
  ;; (C-sizeof "GdkColor") ===> 10
  (sc-macro-transformer
   (lambda (form usage-env)
     (expand-c-info-syntax 'SIZEOF form usage-env))))

(define-syntax C-offset
  ;; (C-offset "GdkColor green") ===> 6
  (sc-macro-transformer
   (lambda (form usage-env)
     (expand-c-info-syntax 'OFFSET form usage-env))))

(define (expand-c-info-syntax which form usage-env)
  ;; WHICH can be SIZEOF or OFFSET.
  (let ((len (length form)))
    (if (< len 2)
	(swarn form "Too few args")
	(let ((str (cadr form)))
	  (if (< 2 len) (swarn form "Too many args"))
	  (if (not (string? str))
	      (swarn form "Arg must be a string")
	      (let ((spec (map string->symbol (burst-string str #\space #t))))
		(if (null? spec)
		    (swarn form "arg is an empty string")
		    (c-info which spec form usage-env))))))))

(define (c-info which spec form usage-env)
  ;; Returns the offset or sizeof for SPEC.
  (let* ((includes (find-c-includes usage-env))
	 (btype.members
	  (call-with-initial-ctype
	   spec form
	   (lambda (ctype member-spec)
	     (let ((defn (ctype-definition ctype includes)))
	       (cond ((and (eq? which 'OFFSET) (null? member-spec))
		      (swarn form "no member specified"))
		     ((and (eq? which 'OFFSET)
			   (not (or (ctype/struct-defn? defn)
				    (ctype/union-defn? defn))))
		      (swarn form "not a struct or union type"))
		     ((and (not (eq? which 'OFFSET)) (not (null? member-spec)))
		      (if (null? (cdr member-spec))
			  (swarn form "no member name allowed")
			  (swarn form "no member names allowed")))
		     ((ctype/basic? defn)
		      (cons defn '()))
		     ((ctype/pointer? defn)
		      (cons '* '()))
		     ((or (ctype/struct-defn? defn)
			  (ctype/union-defn? defn))
		      (cons ctype member-spec))
		     (else
		      (serror form "unimplemented")))))))
	 (entry (and btype.members
		     (assoc (cons which btype.members)
			    (c-includes/struct-values includes)))))
    (cond ((not btype.members)
	   form)
	  (entry
	   (if (eq? 'OFFSET which) (cadr entry) (cdr entry)))
	  (else
	   (if (eq? 'OFFSET which)
	       (swarn form "Unknown member")
	       (swarn form "Unknown C type" btype.members))))))

(define (call-with-initial-ctype spec form receiver)
  ;; Given SPEC, a list of symbols, calls RECEIVER with a ctype and
  ;; member spec (the list of names that followed the C type spec)
  ;;
  ;; For example RECEIVER is called with
  ;;
  ;;     (* (|struct| |addrinfo|)) and (|ai_socktype|)
  ;;
  ;; when SPEC is (* |struct| |addrinfo| |ai_socktype|).
  (let ((type-name (car spec))
	(member-spec (cdr spec)))
    (cond ((memq type-name '(STRUCT UNION ENUM))
	   (if (null? member-spec)
	       (swarn form "Incomplete C type specification")
	       (receiver (list type-name (car member-spec))
			 (cdr member-spec))))
	  ((eq? type-name '*)
	   (if (null? member-spec)
	       (receiver '* '())
	       ;; Recursively strip prefix pointer op.
	       (call-with-initial-ctype
		member-spec form
		(lambda (target-ctype member-spec)
		  (receiver (list '* target-ctype)
			    member-spec)))))
	  (else
	   (receiver type-name member-spec)))))


;;; C-array-loc and -loc! Syntaxes

(define-syntax C-array-loc
  ;; (C-array-loc ALIEN "element type" INDEX)
  ;; ===>
  ;; (alien-byte-increment ALIEN (* (C-sizeof "element type") INDEX))
  (sc-macro-transformer
   (lambda (form usage-env)
     (expand-c-array-loc-syntax #f form usage-env))))

(define-syntax C-array-loc!
  ;; (C-array-loc! ALIEN "element type" INDEX)
  ;; ===>
  ;; (alien-byte-increment! ALIEN (* (C-sizeof "element type") INDEX))
  (sc-macro-transformer
   (lambda (form usage-env)
     (expand-c-array-loc-syntax #t form usage-env))))

(define (expand-c-array-loc-syntax bang? form usage-env)
  (call-with-destructured-C-array-loc-form
   form
   (lambda (alien-form str index-form)
     (let ((spec (map string->symbol (burst-string str #\space #t))))
       (if (null? spec)
	   (swarn form "2nd arg is an empty string")
	   (let ((alien-form (close-syntax alien-form usage-env))
		 (sizeof (c-info `SIZEOF spec form usage-env))
		 (index-form (close-syntax index-form usage-env))
		 (proc (if bang? 'ALIEN-BYTE-INCREMENT! 'ALIEN-BYTE-INCREMENT)))
	     `(,proc ,alien-form (* ,sizeof ,index-form))))))))

(define (call-with-destructured-C-array-loc-form form receiver)
  (let ((len (length form)))
    (if (< len 4)
	(swarn form "Too few args")
	(let ((alien-form (cadr form))
	      (type (caddr form))
	      (index-form (cadddr form)))
	  (if (> len 4) (swarn form "Too many args"))
	  (if (not (string? type))
	      (swarn form "The 2nd arg must be a string")
	      (receiver alien-form type index-form))))))


;;; C-call Syntax

(define-syntax C-call
  ;; (C-call "gtk_label_new" alien "Hello, World!")
  ;; ===>
  ;; (call-alien #[alien-function 33 gtk_label_new] alien "Hello, World!")
  (sc-macro-transformer
   (lambda (form usage-env)
     (call-with-destructured-C-call-form
      form
      (lambda (func-name arg-forms)
	(let* ((includes (find-c-includes usage-env))
	       (callouts (c-includes/callouts includes))
	       (alien (let ((entry (assq func-name callouts)))
			(if (pair? entry)
			    (cdr entry)
			    (swarn form "No declaration of callout"
				   func-name)))))
	  `(CALL-ALIEN ,alien
		       . ,(map (lambda (form) (close-syntax form usage-env))
			       arg-forms))))))))

(define (call-with-destructured-C-call-form form receiver)
  ;; Calls RECEIVER with the optional return-alien-form, func-name
  ;; (as a symbol), and the arg-forms.
  (if (not (pair? (cdr form)))
      (swarn form "No function name")
      (let ((name (cadr form))
	    (args (cddr form)))
	(if (not (string? name))
	    (swarn form "First arg must be a string")
	    (receiver (string->symbol name) args)))))


;;; C-callback Syntax

(define-syntax C-callback
  ;;     (C-callback "clicked") ===> #[alien-function "clicked"]
  ;; and
  ;;     (C-callback clicked) ===> (register-c-callback clicked)
  (sc-macro-transformer
   (lambda (form usage-env)
     (call-with-destructured-c-callback-form form
      (lambda (obj)
	(if (string? obj)
	    (let* ((c-includes (find-c-includes usage-env))
		   (callbacks (c-includes/callbacks c-includes))
		   (name (string->symbol obj)))
	      (let ((entry (assq name callbacks)))
		(if (pair? entry) (cdr entry)
		    (swarn form "No declaration of callback"))))
	    (let ((value-form (close-syntax obj usage-env)))
	      `(REGISTER-C-CALLBACK ,value-form))))))))

(define (call-with-destructured-c-callback-form form receiver)
  ;; Calls RECEIVER with the only subform.
  (let ((len (length form)))
    (if (< len 2)
	(swarn form "Too few args")
	(begin
	  (if (< 2 len)
	      (swarn form "Too many args")
	      (receiver (cadr form)))))))


;;; Utilities

(define (find-c-includes env)
  ;; Returns the c-includes structure bound to 'C-INCLUDES in ENV.
  (guarantee syntactic-environment? env 'find-c-includes)
  (let ((ienv (senv->runtime env)))
    (if (and (environment-bound? ienv 'C-INCLUDES)
	     (environment-assigned? ienv 'C-INCLUDES))
	(let ((includes (environment-lookup ienv 'C-INCLUDES)))
	  (if (c-includes? includes)
	      includes
	      (error "C-includes is not bound to a c-includes structure:"
		     includes)))
	(error "No C types have been included."))))

(define condition-type:serror
  (make-condition-type
      'ffi-syntaxer-error
      condition-type:error
      '(FORM MESSAGE)
    (lambda (condition port)
      (write-string "FFI syntax error: " port)
      (write-string (access-condition condition 'MESSAGE) port)
      (write-string " in: " port)
      (write (access-condition condition 'FORM) port)
      (write-char #\. port))))

(define serror
  (let ((signaller (condition-signaller condition-type:serror '(FORM MESSAGE)
					standard-error-handler)))
    (named-lambda (serror form message . args)
      (signaller form
		 (apply string-append
			(map (lambda (obj)
			       (if (string? obj) obj (write-to-string obj)))
			     (cons message args)))))))

(define (swarn form message . args)
  (apply warn message (append args (list 'IN form)))
  `(error "Invalid syntax" ',form))