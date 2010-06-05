#| -*-Scheme-*-

Copyright (C) 2006, 2007, 2008, 2009, 2010 Matthew Birkholz

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

;;;; C Declarations
;;; package: (ffi syntax)


(define-structure (c-includes (conc-name c-includes/)
			      (constructor make-c-includes (library))
			      ;; To be fasdump/loadable.
			      (type vector) (named 'c-includes))
  library	; String naming the DLL of trampolines (the shim).
  (files	'()) ;; Included file names and their modtimes when read.
  (type-names	'()) ;; E.g. ((gpointer (* mumble) . "prhello.cdecl")...)
  (structs	'()) ;; E.g. ((|_GdkColor| (struct ...) . "gdkcolor.cdecl")...)
  (unions	'()) ;; E.g. ((|_GdkEvent| (union ...) . "gdkevents.cdecl")...)
  (enums	'()) ;; E.g. ((|_cairo_status| (enum ...) . "cairo.cdecl")...)
  (enum-constants'()) ;;E.g. ((|CAIRO_STATUS_SUCCESS| . "prhello.cdecl")...)
  (callouts	'()) ;; E.g. ((|gdk_window_new| . #<alien-function 42...>)...)
  (callbacks	'()) ;; E.g. ((|delete_event| . #<alien-function 42...>)...)
  (enum-values	'()) ;; E.g. ((|CAIRO_STATUS_SUCCESS| . 0)...) from groveler.
  (struct-values'()) ;; List of struct info from the groveler:
  ;;                    (((sizeof |GdkColor|) . 12)
  ;;                     ((offset |GdkColor| pixel) . (0 . int))
  ;;                     ((offset |GdkColor| red) . (4 . short))
  ;;                     ((offset |GdkColor| green) . (6 . short))
  ;;                     ((offset |GdkColor| blue) . (8 . short))
  ;;                     ((sizeof (struct |_GdkColor|)) . 12)
  ;;                     ((offset (struct |_GdkColor|) pixel) . (0 . int))
  ;;                     ((offset (struct |_GdkColor|) red) . (4 . short))
  ;;                     ((offset (struct |_GdkColor|) green) . (6 . short))
  ;;                     ((offset (struct |_GdkColor|) blue) . (8 . short))...)
  )

(define (include-cdecls library)
  ;; Toplevel entry point for the generator.
  ;; Returns a new C-INCLUDES structure.
  (let ((includes (make-c-includes library))
	(cwd (if load/loading?
		 (directory-pathname (current-load-pathname))
		 (working-directory-pathname))))
    (include-cdecl-file library cwd cwd includes)
    includes))

(define c-include-noisily? #t)
(define current-filename)

(define (include-cdecl-file filename cwd twd includes)
  ;; Adds the C declarations in FILENAME to INCLUDES.  Interprets
  ;; FILENAME relative to CWD (current working directory).
  ;; Abbreviates namestrings under TWD (topmost working, build directory).

  (let* ((pathname (merge-pathnames
		    (pathname-default-type filename "cdecl") cwd))
	 (new-cwd (directory-pathname pathname))
	 (namestring (enough-namestring pathname twd))
	 (modtime (file-modification-time-indirect namestring))
	 (files (c-includes/files includes)))
    (if (not (assoc namestring files))
	(fluid-let ((current-filename namestring))
	  (set-c-includes/files! includes
				 (cons (cons namestring modtime) files))

	  (define (kernel)
	    (call-with-input-file namestring
	      (lambda (inport)
		(let loop ()
		  (let ((form (parse-object inport read-environment)))
		    (if (not (eof-object? form))
			(begin
			  (include-cdecl form new-cwd twd includes)
			  (loop))))))))

	  (if c-include-noisily?
	      (with-notification (lambda (port)
				   (write-string "Including " port)
				   (write-string namestring port))
				 kernel)
	      (kernel))))))

(define read-environment
  (make-top-level-environment '(*PARSER-CANONICALIZE-SYMBOLS?*) '(#f)))

(define (include-cdecl form cwd twd includes)
  ;; Add a top-level C declaration to INCLUDES.  If it is an
  ;; include, interprete the included filenames relative to CWD
  ;; (current working directory).
  (if (not (and (pair? form) (symbol? (car form)) (pair? (cdr form))))
      (cerror form "malformed top level C declaration"))
  (let ((keyword (car form))
	(name (cadr form))
	(rest (cddr form)))
    (case keyword
      ((|include|)
       (for-each (lambda (file) (include-cdecl-file file cwd twd includes))
		 (cdr form)))
      ((|typedef|) (include-typedef form name rest includes))
      ((|struct|) (include-struct form name rest includes))
      ((|union|) (include-union form name rest includes))
      ((|enum|) (include-enum form name rest includes))
      ((|extern|) (include-function form name rest includes))
      ((|callback|) (include-function form name rest includes))
      (else (cerror form "unknown top level keyword"))))
  unspecific)

(define (include-typedef form name rest includes)
  ;; Add a top-level (typedef NAME . REST) C declaration to INCLUDES.
  (if (not (and (symbol? name)
		(pair? rest) (null? (cdr rest))))
      (cerror form "malformed typedef declaration"))
  (let* ((ctypes (c-includes/type-names includes))
	 (entry (assq name ctypes)))
    (if entry (cerror form "already defined in " (cddr entry)))
    (let* ((ctype (valid-ctype (car rest) includes))
	   (new (cons name (cons ctype current-filename))))
      (set-c-includes/type-names! includes (cons new ctypes))
      unspecific)))

(define (include-struct form name members includes)
  ;; Add a top-level (struct NAME . MEMBERS) C declaration to INCLUDES.
  (if (not (and (symbol? name) (pair? members) (list? members)))
      (cerror form "malformed named struct declaration"))
  (let* ((structs (c-includes/structs includes))
	 (entry (assq name structs)))
    (if entry (cerror form "already defined in " (cddr entry)))
    (let* ((anon (cons 'STRUCT
		       (map (lambda (member)
			      (valid-struct-member member includes))
			    members)))
	   (info (cons anon current-filename)))
      (set-c-includes/structs!
       includes (cons (cons name info) structs))
      unspecific)))

(define (valid-struct-member form includes)
  ;; Returns (NAME . CTYPE) given a MEMBER C declaration.
  ;; Adds any internal named struct/union/enum types to INCLUDES.
  (if (not (and (pair? form) (symbol? (car form))
		(pair? (cdr form)) (null? (cddr form))))
      (cerror form "malformed struct member"))
  (let ((name (car form))
	(ctype (valid-ctype (cadr form) includes)))
    (cons name ctype)))

(define (include-union form name members includes)
  ;; Add a top-level (union NAME . MEMBERS) C declaration to INCLUDES.
  (if (not (and (symbol? name) (pair? members) (list? members)))
      (cerror form "malformed named union declaration"))
  (let* ((unions (c-includes/unions includes))
	 (entry (assq name unions)))
    (if entry (cerror form "already defined in " (cddr entry)))
    (let* ((anon (cons 'UNION
		       (map (lambda (member)
			      (valid-union-member member includes))
			    members)))
	   (info (cons anon current-filename)))
      (set-c-includes/unions!
       includes (cons (cons name info) unions))
      unspecific)))

(define (valid-union-member form includes)
  ;; Returns (NAME . CTYPE) given a MEMBER C declaration.
  ;; Adds any internal named struct/union/enum types to INCLUDES.
  (if (not (and (pair? form) (symbol? (car form))
		(pair? (cdr form)) (null? (cddr form))))
      (cerror form "malformed union member"))
  (let ((name (car form))
	(ctype (valid-ctype (cadr form) includes)))
    (cons name ctype)))

(define (include-enum form name constants includes)
  ;; Add a top-level (enum NAME . CONSTANTS) C declaration to INCLUDES.
  ;; Also accepts an unnamed (enum . CONSTANTS) C declaration.
  (if (not (list? constants))
      (cerror form "malformed named enum declaration"))
  (if (symbol? name)
      (let* ((enums (c-includes/enums includes))
	     (entry (assq name enums)))
	(if entry (cerror form "already defined in " (cddr entry)))
	(let* ((anon (cons 'ENUM
			   (valid-enum-constants constants includes)))
	       (info (cons anon current-filename)))
	  (set-c-includes/enums!
	   includes (cons (cons name info) enums))))
      (valid-enum-constants (cdr form) includes)))

(define (valid-enum-constants forms includes)
  ;; Returns a list of (NAME) pairs for each enum constant declaration
  ;; in FORMS.  Also adds enum constants to INCLUDES.
  (let loop ((forms forms))
    (if (null? forms) '()
	(let ((name (valid-enum-constant (car forms) includes)))
	  (cons name (loop (cdr forms)))))))

(define (valid-enum-constant form includes)
  ;; Returns (NAME), the name of the validated enum constant declared
  ;; by FORM.  Immediately adds the constant to the list in INCLUDES,
  ;; checking that it is not already there.
  (if (not (and (pair? form) (symbol? (car form))
		;; 1 or 2 args
		(or (null? (cdr form))
		    (and (pair? (cdr form)) (null? (cddr form))))))
      (cerror form "malformed enum constant declaration"))
  (if (pair? (cdr form))
      (cwarn (cadr form) "ignored enum value"))
  (let* ((name (car form))
	 (constants (c-includes/enum-constants includes))
	 (entry (assq name constants)))
    (if entry (cerror form "already defined in " (cdr entry)))
    (set-c-includes/enum-constants!
     includes (cons (cons name current-filename) constants))
    (list name)))

(define (include-function form rettype rest includes)
  ;; Callouts/backs have much in common here, thus this shared
  ;; procedure, which uses the keyword still at the head of FORM to
  ;; munge the correct alist in INCLUDES.
  (if (not (and (pair? rest) (symbol? (car rest))
		(list? (cdr rest))))
      (cerror form "malformed "(symbol-name (car form))" declaration"))
  (let* ((name (car rest))
	 (params (cdr rest))
	 (others (if (eq? 'EXTERN (car form))
		     (c-includes/callouts includes)
		     (c-includes/callbacks includes)))
	 (entry (assq name others)))
    (if entry (cerror form "already defined in "
		      (alien-function/filename (cdr entry))))
    (let ((new (cons name
		     (make-alien-function
		      (symbol-name name)
		      (c-includes/library includes)
		      (valid-ctype rettype includes)
		      (valid-params params includes)
		      current-filename))))
      (if (eq? 'EXTERN (car form))
	  (set-c-includes/callouts! includes (cons new others))
	  (set-c-includes/callbacks! includes (cons new others)))
      unspecific)))

(define (valid-params forms includes)
  ;; Returns a list -- (NAME CTYPE) for each parameter declaration
  ;; form in FORMS.
  (if (null? forms) '()
      (cons (valid-param (car forms) includes)
	    (valid-params (cdr forms) includes))))

(define (valid-param form includes)
  ;; Returns (NAME CTYPE) after validating FORM.
  (if (not (and (pair? form) (symbol? (car form))
		(pair? (cdr form))
		(null? (cddr form))))
      (cerror form "malformed parameter declaration"))
  (let ((name (car form))
	(ctype (valid-ctype (cadr form) includes)))
    (list name ctype)))

(define (valid-ctype form includes)
  ;; Returns a valid ctype expression, a copy of FORM.  Modifies
  ;; INCLUDES with any internal struct/union/enum declarations.
  (cond ((symbol? form) form)
	((ctype/pointer? form) form)
	((ctype/const? form)
	 (list 'CONST (valid-ctype (cadr form) includes)))

	((ctype/struct-name? form) form)
	((ctype/struct-anon? form)
	 (cons 'STRUCT (map (lambda (member)
			      (valid-struct-member member includes))
			    (cdr form))))
	((ctype/struct-named? form)
	 (include-struct form (cadr form) (cddr form) includes)
	 (list 'STRUCT (cadr form)))

	((ctype/union-name? form) form)
	((ctype/union-anon? form)
	 (cons 'UNION (map (lambda (member)
			     (valid-union-member member includes))
			   (cdr form))))
	((ctype/union-named? form)
	 (include-union form (cadr form) (cddr form))
	 (list 'UNION (cadr form)))

	((ctype/enum-name? form) form)
	((ctype/enum-anon? form)
	 (cons 'ENUM (valid-enum-constants (cdr form) includes)))
	((ctype/enum-named? form)
	 (include-enum form (cadr form) (cddr form) includes)
	 (list 'ENUM (cadr form)))

	((ctype/array? form)
	 (list 'ARRAY
	       (valid-ctype (ctype-array/element-type form) includes)
	       (ctype-array/size form)))

	(else (cerror form "bogus C type declaration"))))

(define condition-type:cerror
  (make-condition-type
   'ffi-cdecl-error
   condition-type:error
   '(FORM FILENAME MESSAGE)
   (lambda (condition port)
     (write-string "Error: " port)
     (write-string (access-condition condition 'MESSAGE) port)
     (write-string ":" port)
     (write-string (access-condition condition 'FILENAME) port)
     (write-string ": " port)
     (write (access-condition condition 'FORM) port))))

(define cerror
  (let ((signaller (condition-signaller condition-type:cerror
					'(FORM FILENAME MESSAGE)
					standard-error-handler)))
    (named-lambda (cerror form message . args)
      (signaller form current-filename
		 (apply string-append
			(map (lambda (obj)
			       (if (string? obj) obj (write-to-string obj)))
			     (cons message args)))))))

(define condition-type:cwarn
  (make-condition-type
   'ffi-cdecl-warning
   condition-type:warning
   '(FORM FILENAME MESSAGE)
   (lambda (condition port)
     (write-string (access-condition condition 'MESSAGE) port)
     (write-string ":" port)
     (write-string (access-condition condition 'FILENAME) port)
     (write-string ": " port)
     (write (access-condition condition 'FORM) port))))

(define cwarn
  (let ((signaller (condition-signaller condition-type:cwarn
					'(FORM FILENAME MESSAGE)
					standard-warning-handler)))
    (named-lambda (cwarn form message . args)
      (with-simple-restart 'MUFFLE-WARNING "Ignore warning."
        (lambda ()
	  (signaller form current-filename
		     (apply string-append
			    (map (lambda (obj)
				   (if (string? obj) obj (write-to-string obj)))
				 (cons message args)))))))))