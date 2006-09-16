#| -*-Scheme-*-

$Id: cout.scm,v 1.24 2006/09/16 11:19:09 gjr Exp $

Copyright (c) 1992-1999, 2006 Massachusetts Institute of Technology

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

;;;; C-output fake assembler and linker
;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define-syntax let*/mv
  (rsc-macro-transformer
   (lambda (form environment)
     environment			; unused
     (let ((body (cddr form)))
       (let recur ((bindings (cadr form)))
	 (cond ((null? bindings)
		`(BEGIN
		   ,@body))
	       ((not (pair? (caar bindings)))
		`(LET (,(car bindings))
		   ,(recur (cdr bindings))))
	       (else
		(let ((values-names (caar bindings))
		      (values-form (cadar bindings)))
		  `(WITH-VALUES (LAMBDA () ,values-form)
		     (LAMBDA ,values-names
		       ,(recur (cdr bindings))))))))))))

(define *use-stackify?* true)
(define *disable-timestamps?* false)
(define *C-procedure-name* 'DEFAULT)

(define *subblocks*)			;referenced by stackify

(define (stringify-data object output-pathname)
  (if (not *use-stackify?*)
      (stringify-data/traditional object output-pathname)
      (stringify-data/stackify object output-pathname)))
  
(define (stringify-data/stackify object output-pathname)
  (let* ((str (stackify 0 object))
	 (handle (or (and output-pathname
			  (let ((dir (pathname-directory output-pathname)))
			    (string-append
			     (if (or (not dir) (null? dir))
				 ""
				 (car (last-pair dir)))
			     "_"
			     (pathname-name output-pathname))))
		     "handle"))
	 (data-name
	  (canonicalize-label-name
	   (string-append handle "_data" (make-time-stamp)))))
	
    (list-of-strings->string
     (append (file-prefix)
	     (file-header 0 handle #f #f #f data-name)
	     (list "#ifndef WANT_ONLY_CODE\n")
	     (stackify-output->data-decl "prog" str)
	     (list "\n")
	     (object-function-header/stackify data-name)
	     (list "\tDECLARE_VARIABLES_FOR_OBJECT();\n\n")
	     (list
	      "\treturn (unstackify (((unsigned char *) (& prog[0])), 0));")
	     (function-trailer data-name)
	     (list "#endif /* WANT_ONLY_CODE */\n")))))

(define (stringify-data/traditional object output-pathname)
  (let*/mv (((vars prefix suffix) (handle-top-level-data/traditional object))
	    (handle (or (and output-pathname
			     (let ((dir (pathname-directory output-pathname)))
			       (string-append
				(if (or (not dir) (null? dir))
				    ""
				    (car (last-pair dir)))
				"_"
				(pathname-name output-pathname))))
			"handle"))
	    (data-name
	     (canonicalize-label-name
	      (string-append handle "_data" (make-time-stamp)))))

    (list-of-strings->string
     (append (file-prefix)
	     (file-header 0 handle #f #f #f data-name)
	     (list "#ifndef WANT_ONLY_CODE\n")
	     (object-function-header/traditional data-name)
	     (->variable-declarations vars)
	     (list "\tDECLARE_VARIABLES_FOR_OBJECT();\n")
	     (list "\n\t")
	     prefix
	     suffix
	     (list "\n\treturn (top_level_object);\n")
	     (function-trailer data-name)
	     (list "#endif /* WANT_ONLY_CODE */\n")))))

(define (stringify suffix initial-label lap-code info-output-pathname)
  ;; returns <code-name data-name ntags symbol-table code proxy>
  (define (canonicalize-name name full?)
    (if full?
	(canonicalize-label-name name)
	(C-quotify-string name)))

  (define (choose-name full? default midfix time-stamp)
    (let ((path (and info-output-pathname
		     (merge-pathnames
		      (if (pair? info-output-pathname)
			  (car info-output-pathname)
			  info-output-pathname)))))
    
      (cond ((not *C-procedure-name*)
	     (string-append default suffix time-stamp))
	    ((not (eq? *C-procedure-name* 'DEFAULT))
	     (string-append *C-procedure-name*
			    midfix
			    suffix))
	    ((not path)
	     (string-append default suffix time-stamp))
	    ((or (string-null? suffix) *disable-timestamps?*)
	     (let ((dir (pathname-directory path)))
	       (string-append
		(if (or (not dir) (null? dir))
		    default
		    (canonicalize-name (car (last-pair dir)) full?))
		"_"
		(canonicalize-name (pathname-name path) full?)
		midfix
		suffix)))
	    (else
	     (string-append
	      (canonicalize-name (pathname-name path) full?)
	      "_"
	      default
	      suffix
	      time-stamp)))))

  (define (gen-code-name time-stamp)
    (choose-name true "code" "" time-stamp))

  (define (gen-data-name time-stamp)
    (choose-name true "data" "_data" time-stamp))

  (define (gen-handle-name time-stamp)
    (choose-name false "" "" time-stamp))

  (define (subroutine-information-1)
    (cond ((eq? *invoke-interface* 'INFINITY)
	   (values (list "") (list "")))
	  ((< *invoke-interface* 5)
	   (values (list-tail (list
			       "\nDEFLABEL(invoke_interface_0);\n"
			       "\tutlarg_1 = 0;\n"
			       "\nDEFLABEL(invoke_interface_1);\n"
			       "\tutlarg_2 = 0;\n"
			       "\nDEFLABEL(invoke_interface_2);\n"
			       "\tutlarg_3 = 0;\n"
			       "\nDEFLABEL(invoke_interface_3);\n"
			       "\tutlarg_4 = 0;\n"
			       "\nDEFLABEL(invoke_interface_4);\n\t"
			       "INVOKE_INTERFACE_CODE ();\n")
			      *invoke-interface*)
		   (list "\tint utlarg_code;\n"
			 "\tlong utlarg_1, utlarg_2, utlarg_3, utlarg_4;\n")))
	  (else
	   (error "subroutine-information-1: Utilities take at most 4 args"
		  *invoke-interface*))))

  (define (subroutine-information-2)
    (if *used-invoke-primitive*
	(values (list "\nDEFLABEL(invoke_primitive);\n\t"
		      "INVOKE_PRIMITIVE_CODE ();")
		(list "\tSCHEME_OBJECT primitive;\n"
		      "\tlong primitive_nargs;\n"))
	(values (list "") (list ""))))

  (define (subroutine-information)
    (let*/mv (((code-1 vars-1) (subroutine-information-1))
	      ((code-2 vars-2) (subroutine-information-2)))
      (values (append code-1 code-2)
	      (append vars-1 vars-2))))

  (if *purification-root-object*
      (define-object "PURIFICATION_ROOT"
	(if (vector? (cdr *purification-root-object*))
	    *purification-root-object*
	    (cons (car *purification-root-object*)
		  (list->vector
		   (reverse (cdr *purification-root-object*)))))))

  (define-object (special-label/debugging)
    (let frob ((obj info-output-pathname))
      (cond ((pathname? obj)
	     (->namestring/shared obj))
	    ((pair? obj)
	     (cons (frob (car obj))
		   (frob (cdr obj))))
	    (else
	     obj))))

  (define-object (special-label/environment) unspecific)

  (let*/mv ((label-offset 1)		; First word is vector header
	    (initial-offset (label->offset initial-label))
	    ((first-free-offset ntags label-defines label-dispatch
				label-block-initialization symbol-table)
	     (handle-labels label-offset))
	    ((first-object-offset free-defines
				  free-block-initialization free-symbols)
	     (handle-free-refs-and-sets first-free-offset))
	    ((cc-block-size decl-code decl-data
			    xtra-procs object-prefix
			    object-defines temp-vars
			    object-block-initialization)
	     (handle-objects first-object-offset))
	    (time-stamp (make-time-stamp))
	    (handle (gen-handle-name time-stamp))
	    (code-name (gen-code-name time-stamp))
	    (data-name (gen-data-name time-stamp))
	    (decl-code-name (string-append "decl_" code-name))
	    (decl-data-name (string-append "decl_" data-name))
	    ((extra-code extra-variables)
	     (subroutine-information))
	    ((proxy xtra-procs* decl-code* decl-data* data-prefix data-body)
	     (data-function-body (string-null? suffix)
				 ntags
				 data-name
				 initial-offset
				 cc-block-size
				 temp-vars
				 object-prefix
				 label-block-initialization
				 free-block-initialization
				 object-block-initialization))
	    (use-stackify? *use-stackify?*))
    (values
     code-name
     data-name
     ntags
     (cons* (cons (special-label/environment)
		  (- cc-block-size 1))
	    (cons (special-label/debugging)
		  (- cc-block-size 2))
	    (append free-symbols symbol-table))
     (list-of-strings->string
      (map (lambda (x)
	     (list-of-strings->string x)) 
	   (list
	    (if (string-null? suffix)
		(file-prefix)
		'())

	    ;; Extra code

	    xtra-procs
	    xtra-procs*

	    ;; defines for the code

	    label-defines
	    object-defines
	    free-defines
	    (list "\n")

	    ;; the code itself

	    (list "#ifndef WANT_ONLY_DATA\n")
	    (let ((header (code-function-header code-name)))
	      (if (string-null? suffix)
		  header
		  (cons "static " header)))
	    (function-decls)
	    (register-declarations)
	    extra-variables
	    (list
	     "\n"
	     ;; The assignment is necessary to ensure that we restart properly
	     ;; after an interrupt when the dynamic link is live
	     ;; (see DLINK_INTERRUPT_CHECK and comp_interrupt_restart
	     "\tRdl = (OBJECT_ADDRESS (Rvl));\n"
	     "\tgoto perform_dispatch;\n\n"
	     "DEFLABEL(pop_return);\n\t"
	     "Rpc = (OBJECT_ADDRESS (*Rsp++));\n\n"
	     "DEFLABEL(perform_dispatch);\n\n\t"
	     "switch ((* ((unsigned long *) Rpc))"
	     " - dispatch_base)\n\t{")
	    label-dispatch
	    (list
	     "\n\t  default:\n\t\t"
	     "UNCACHE_VARIABLES ();\n\t\t"
	     "return (Rpc);\n\t}\n\t")
	    (map stringify-object lap-code)
	    extra-code
	    (function-trailer code-name)
	    (list
	     "#endif /* WANT_ONLY_DATA */\n")

	    (if (and (string-null? suffix) use-stackify?)
		(list "\f\n")
		'())

	    ;; the data generator

	    data-prefix

	    (if (or (string-null? suffix)
		    (not use-stackify?))
		(append
		 (list "\n")
		 (list "#ifndef WANT_ONLY_CODE\n")
		 (let ((header (data-function-header data-name)))
		   (if (string-null? suffix)
		       header
		       (cons "static " header)))
		 data-body
		 (function-trailer data-name)
		 (list "#endif /* WANT_ONLY_CODE */\n"))
		'())

	    ;; File footer

	    (if (and (string-null? suffix) use-stackify?)
		(list "\f\n")
		'())

	    (cond ((not (string-null? suffix))
		   '())
		  ((not use-stackify?)
		   (file-decls/traditional decl-code-name
					   decl-code
					   decl-data-name
					   decl-data))
		  (else
		   (file-decls/stackify decl-code-name
					decl-code*
					decl-data-name
					decl-data*)))

	    (if (string-null? suffix)
		(file-header ntags handle
			     decl-code-name code-name
			     decl-data-name data-name)
		'())
	    )))
     proxy)))

(define (data-function-body top-level?
			    ntags
			    data-name
			    initial-offset
			    cc-block-size
			    temp-vars
			    object-prefix
			    label-block-initialization
			    free-block-initialization
			    object-block-initialization)
  ;; returns <proxy xtra-procs decl-code decl-data data-prefix data-body>
  (cond ((not *use-stackify?*)
	 (values
	  #f				; proxy
	  '()				; xtra-procs
	  #f				; decl-code
	  #f				; decl-data
	  '()				; data-prefix
	  (map (lambda (x) (list-of-strings->string x))
	       (list (list "\tSCHEME_OBJECT object"
			   " = (ALLOCATE_VECTOR ("
			   (number->string (- cc-block-size 1))
			   "L));\n"
			   "\tSCHEME_OBJECT * current_block"
			   " = (OBJECT_ADDRESS (object));\n")
		     (->variable-declarations temp-vars)
		     (list "\tDECLARE_VARIABLES_FOR_DATA();\n")
		     (list "\n\t")
		     object-prefix
		     label-block-initialization
		     free-block-initialization
		     object-block-initialization
		     (list "\n\treturn (&current_block["
			   (stringify-object initial-offset)
			   "]);\n")))))
	((or (not (null? temp-vars))
	     (not (null? object-prefix)))
	 (error "data-function-body: stackify inconsistency"))
	((not top-level?)
	 (values
	  (list->vector (append label-block-initialization
				free-block-initialization
				object-block-initialization))
	  '()				; xtra-procs
	  '()				; decl-code
	  '()				; decl-data
	  '()				; data-prefix
	  '()				; data-body
	  ))
	(else
	 (fluid-let ((*subblocks* '()))
	   (let ((name (string-append "prog_" data-name))
		 (str
		  (stackify
		   ntags
		   (list->vector (append label-block-initialization
					 free-block-initialization
					 object-block-initialization)))))
		 
	     (set! *subblocks* (reverse! *subblocks*))
	     (values
	      #f			; proxy
	      (append-map fake-block->c-code *subblocks*) ; xtra-procs*
	      *subblocks*		; decl-code
	      '()			; decl-data
	      (append
	       (list "#ifndef WANT_ONLY_CODE\n")
	       (stackify-output->data-decl name str)
	       (list "#endif /* WANT_ONLY_CODE */\n"))
	      (list
	       "\tSCHEME_OBJECT ccb, * current_block;\n"
	       "\tDECLARE_VARIABLES_FOR_DATA();\n\n"
	       "\tccb = (unstackify (((unsigned char *)\n"
	       "\t                    (& " name "[0])),\n"
	       "\t                   dispatch_base));\n"
	       "\tcurrent_block = (OBJECT_ADDRESS (ccb));\n"
	       "\treturn (& current_block["
	       (stringify-object initial-offset)
	       "]);")))))))

(define (stackify-output->data-decl name str)
  (append (list "static CONST unsigned char "
		name
		"["
		(number->string (string-length str))
		"] =\n")
	  (C-quotify-data-string/breakup str)
	  (list ";\n")))

(define-integrable (list-of-strings->string strings)
  (%string-append strings))

(define-integrable (%symbol->string sym)
  (system-pair-car sym))

(define (code-function-header name)
  (list "SCHEME_OBJECT *\n"
	"DEFUN (" name ", (Rpc, dispatch_base),\n\t"
	"SCHEME_OBJECT * Rpc AND entry_count_t dispatch_base)\n"
	"{\n"))

(define (data-function-header name)
  (list "SCHEME_OBJECT *\n"
	"DEFUN (" name ", (dispatch_base), entry_count_t dispatch_base)\n"
	"{\n"))

(define (object-function-header/traditional name)
  (list "SCHEME_OBJECT\n"
	"DEFUN_VOID (" name ")\n"
	"{\n\tSCHEME_OBJECT top_level_object;\n"))

(define (object-function-header/stackify name)
  (list "SCHEME_OBJECT\n"
	"DEFUN_VOID (" name ")\n"
	"{\n"))

(define (function-decls)
  (list
   "\tREGISTER SCHEME_OBJECT * current_block;\n"
   "\tDECLARE_VARIABLES ();\n"
   ;; Rdl is initialized right before perform_dispatch.
   "\tSCHEME_OBJECT * Rdl;\n"))

(define (function-trailer name)
  (list "\n} /* End of " name ". */\n"))

(define (make-define-statement symbol val)
  (string-append "#define " (if (symbol? symbol)
				(symbol->string symbol)
				symbol)
		 " "
		 (if (number? val)
		     (number->string val)
		     val)
		 "\n"))

(define (file-prefix)
  (let ((time (get-decoded-time)))
    (list "/* Emacs: this is -*- C -*- code. */\n\n"
	  "/* C code produced\n   "
	  (decoded-time/date-string time)
	  " at "
	  (decoded-time/time-string time)
	  "\n   by Liar version "
	  (or (get-subsystem-version-string "liar") "?.?")
	  ".\n */\n\n"
	  "#include \"liarc.h\"\n\n")))

(define (file-header ntags handle
		     decl-code-name code-name
		     decl-data-name data-name)
  (if (= ntags 0)
      (list "#ifndef WANT_ONLY_CODE\n"
	    ;; This must be a single line!
	    "DECLARE_DATA_OBJECT (\"" handle
	    "\", " data-name ")\n"
	    "#endif /* WANT_ONLY_CODE */\n\n"
	    "DECLARE_DYNAMIC_OBJECT_INITIALIZATION (\""
	    handle "\")\n")
      (list "#ifndef WANT_ONLY_DATA\n"
	    ;; This must be a single line!
	    "DECLARE_COMPILED_CODE (\"" handle
	    "\", " (number->string ntags)
	    ", " decl-code-name
	    ", " code-name ")\n"
	    "#endif /* WANT_ONLY_DATA */\n\n"
	    "#ifndef WANT_ONLY_CODE\n"
	    ;; This must be a single line!
	    "DECLARE_COMPILED_DATA (\"" handle
	    "\", " (if *use-stackify?* "NO_SUBBLOCKS" decl-data-name)
	    ", " data-name ")\n"
	    "#endif /* WANT_ONLY_CODE */\n\n"
	    "DECLARE_DYNAMIC_INITIALIZATION (\""
	    handle "\")\n")))
		     
(define (make-time-stamp)
  (if *disable-timestamps?*
      "_timestamp"
      (let ((time (get-decoded-time)))
	(string-append
	 "_"
	 (number->string (decoded-time/second time)) "_"
	 (number->string (decoded-time/minute time)) "_"
	 (number->string (decoded-time/hour time)) "_"
	 (number->string (decoded-time/day time)) "_"
	 (number->string (decoded-time/month time)) "_"
	 (number->string (decoded-time/year time))))))
		     
(define (->variable-declarations vars)
  (if (null? vars)
      (list "")
      `("\tSCHEME_OBJECT\n\t  "
	,(car vars)
	,@(append-map (lambda (var)
			(list ",\n\t  " var))
		      (cdr vars))
	";\n")))

(define (file-decls/traditional decl-code-name decl-code
				decl-data-name decl-data)
  (append (list "#ifndef WANT_ONLY_DATA\n")
	  (list
	   "int\n"
	   "DEFUN_VOID (" decl-code-name ")\n{\n")
	  decl-code
	  (list "\treturn (0);\n}\n"
		"#endif /* WANT_ONLY_DATA */\n\n")
	  (list "#ifndef WANT_ONLY_CODE\n")
	  (list
	   "int\n"
	   "DEFUN_VOID (" decl-data-name ")\n{\n")
	  decl-data
	  (list "\treturn (0);\n}\n"
		"#endif /* WANT_ONLY_CODE */\n\n")))

(define (file-decls/stackify decl-code-name code-blocks
			     decl-data-name data-blocks)
  (append
   (append (list "#ifndef WANT_ONLY_DATA\n")
	   (if (or (null? code-blocks)
		   (null? (cdr code-blocks)))
	       '()
	       (code-blocks->array-decl decl-code-name code-blocks))
	   (list
	    "int\n"
	    "DEFUN_VOID (" decl-code-name ")\n{\n")
	   (if (or (null? code-blocks)
		   (null? (cdr code-blocks)))
	       (map fake-block->code-decl
		    code-blocks)
	       (list "\tDECLARE_SUBCODE_MULTIPLE (arr_"
		     decl-code-name
		     ");\n"))
	   (list "\treturn (0);\n}\n"
		 "#endif /* WANT_ONLY_DATA */\n\n"))
   (if *use-stackify?*
       '()
       (append
	(list "#ifndef WANT_ONLY_CODE\n")
	(if (or (null? data-blocks)
		(null? (cdr data-blocks)))
	    '()
	    (data-blocks->array-decl decl-data-name data-blocks))
	(list
	 "int\n"
	 "DEFUN_VOID (" decl-data-name ")\n{\n")
	(if (or (null? data-blocks)
		(null? (cdr data-blocks)))
	    (map fake-block->data-decl data-blocks)
	    (list "\tDECLARE_SUBDATA_MULTIPLE (arr_"
		  decl-data-name
		  ");\n"))
	(list "\treturn (0);\n}\n"
	      "#endif /* WANT_ONLY_CODE */\n\n")
	))))

(define (code-blocks->array-decl decl-code-name code-blocks)
  (append (list "static CONST struct liarc_code_S arr_"
		decl-code-name
		"["
		(number->string (length code-blocks))
		"] =\n{\n")
	  (map (lambda (code-block)
		 (string-append
		  "  { \""
		  (fake-block/tag code-block)
		  "\", "
		  (number->string (fake-block/ntags code-block))
		  ", "
		  (fake-block/c-proc code-block)
		  " },\n"))
	       code-blocks)
	  (list "};\n\n")))

(define (data-blocks->array-decl decl-data-name data-blocks)
  (append (list "static CONST struct liarc_data_S arr_"
		decl-data-name
		"["
		(number->string (length data-blocks))
		"] =\n{\n")
	  (map (lambda (data-block)
		 (string-append
		  "  { \""
		  (fake-block/tag data-block)
		  "\", "
		  (fake-block/d-proc data-block)
		  " },\n"))
	       data-blocks)
	  (list "};\n\n")))

(define char-set:all
  (predicate->char-set (lambda (char) char true)))

(define char-set:C-string-quoted
  (char-set-union
   ;; Not char-set:not-graphic
   (char-set-difference char-set:all
			(char-set-intersection char-set:graphic
					       (ascii-range->char-set 0 #x7f)))
   (char-set #\\ #\" #\? (integer->char #xA0))))

(define char-set:C-named-chars
  (char-set #\\ #\" #\Tab #\BS  ;; #\' Scheme does not quote it in strings
	    ;; #\VT #\BEL	;; Cannot depend on ANSI C
	    #\Linefeed #\Return #\Page))

;; This is intended for shortish character strings with the occasionall escape.

(define (C-quotify-string string)
  (let* ((len (string-length string))
	 ;; The maximum expansion is *4, hence we can allocate it all here
	 (temp (make-string (fix:* 4 len))))
    (let loop ((src 0) (dst 0))
      (if (fix:>= src len)
	  (substring temp 0 dst)
	  (let ((index (substring-find-next-char-in-set
			string src len char-set:C-string-quoted)))
	    (if (not index)
		(begin
		  (substring-move! string src len temp dst)
		  (loop len (fix:+ dst (fix:- len src))))
		(let* ((i+1 (fix:+ index 1))
		       (sub (C-quotify-string-char
			     (string-ref string index)
			     (and (fix:< i+1 len)
				  (string-ref string i+1))))
		       (len* (string-length sub))
		       (off (fix:+ dst (fix:- index src))))
		  (if (> len* 4)
		      (error "C-quotify-string: Large character expansion!"
			     sub))
		  (if (not (fix:= index src))
		      (substring-move! string src index temp dst))
		  (substring-move! sub 0 len* temp off)
		  (loop i+1 (fix:+ off len*)))))))))

;; The following routine relies on the fact that Scheme and C use the
;; same quoting convention for the named characters when they appear
;; in strings.

(define (C-quotify-string-char char next)
  (cond ((char-set-member? char-set:C-named-chars char)
	 (let ((result (write-to-string (string char))))
	   (substring result 1 (-1+ (string-length result)))))
	((char=? char #\NUL)
	 ;; Avoid ambiguities
	 (if (or (not next)
		 (not (char-set-member? char-set:numeric next)))
	     "\\0"
	     "\\000"))
	((char=? char #\?)
	 ;; Avoid tri-graphs
	 "\\?")
	(else
	 (string-append
	  "\\"
	  (let ((s (number->string (char-code char) 8)))
	    (if (< (string-length s) 3)
		(string-append (make-string (- 3 (string-length s)) #\0)
			       s)
		s))))))

;; This is intended for binary data encoded as a character string
;; where most of the characters are not really characters at all.

(define (C-quotify-data-string/breakup string)
  (let ((len (string-length string)))
    (define (flush end temp res)
      (if (= end 0)
	  res
	  (cons* "\"" (substring temp 0 end) "\t\""
		 (if (null? res)
		     res
		     (cons "\n" res)))))

    (define (done end temp res)
      (reverse! (flush end temp res)))

    (define (step3 index pos temp res)
      (let* ((i+1 (fix:+ index 1))
	     (sub (C-quotify-string-char
		   (string-ref string index)
		   (and (fix:< i+1 len)
			(string-ref string i+1))))
	     (len* (string-length sub))
	     (next (fix:+ pos len*)))
	(if (fix:> len* 4)
	    (error "C-quotify-string/breakup: Large character expansion!"
		   sub))
	(if (fix:>= next 65)
	    (error "C-quotify-string/breakup: Overrun!" next))
	(substring-move! sub 0 len* temp pos)
	(if (fix:>= next 60)
	    (step1 i+1 0 (make-string 65) (flush next temp res))
	    (step1 i+1 next temp res))))

    (define (step2 src lim dst temp res)
      (cond ((fix:< src lim)
	     (let ((room (fix:- 60 dst))
		   (have (fix:- lim src)))
	       (cond ((fix:<= have room)
		      (substring-move! string src lim temp dst)
		      (step2 lim lim (fix:+ dst have) temp res))
		     ((fix:= room 0)
		      (step2 src lim 0 (make-string 65) (flush dst temp res)))
		     (else
		      (let ((src* (fix:+ src room))
			    (end (fix:+ dst room)))
			(substring-move! string src src* temp dst)
			(step2 src* lim 0 (make-string 65)
			       (flush end temp res)))))))
	    ((fix:>= lim len)
	     (done dst temp res))
	    ((fix:>= dst 60)
	     (step3 lim 0 (make-string 65) (flush dst temp res)))
	    (else
	     (step3 lim dst temp res))))

    (define (step1 src dst temp res)
      (if (fix:>= src len)
	  (done dst temp res)
	  (let ((index (substring-find-next-char-in-set
			string src len char-set:C-string-quoted)))
	    (cond ((not index)
		   (step2 src len dst temp res))
		  ((fix:= index src)
		   (step3 index dst temp res))
		  (else
		   (step2 src index dst temp res))))))
      
    (step1 0 0 (make-string 65) '())))

(define (stringify-object x)
  (cond ((string? x)
	 x)
	((symbol? x)
	 (%symbol->string x))
	((number? x)
	 (number->string x))
	(else
	 (error "stringify: Unknown frob" x))))

(define (handle-objects start-offset)
  (if *use-stackify?*
      (handle-objects/stackify start-offset)
      (handle-objects/traditional start-offset)))

(define (handle-objects/stackify start-offset)
  ;; returns <next-offset decl-code decl-data xtra-procs object-prefix
  ;;         object-defines temp-vars object-block-initialization>
  (define (iter offset table defines objects)
    (if (null? table)
	(values offset
		#f			; xtra code decls
		#f			; xtra data decls
		'()			; xtra procs
		'()
		defines
		'()
		(reverse! objects))
	(let ((entry (car table)))
	  (iter (+ offset 1)
		(cdr table)
		(cons (make-define-statement (entry-label entry) offset)
		      defines)
		(cons (entry-value entry)
		      objects)))))

  (iter start-offset
	(reverse (table->list-of-entries objects))
	'()				; defines
	'()				; objects
	))

(define (handle-objects/traditional start-offset)
  ;; All the reverses produce the correct order in the output block.
  ;; The incoming objects are reversed
  ;; (environment, debugging label, purification root, etc.)
  ;; returns <next-offset decl-code decl-data xtra-procs object-prefix
  ;;         object-defines temp-vars object-block-initialization>

  (fluid-let ((new-variables '())
	      (*subblocks* '())
	      (num 0))

    (define (iter offset table names defines objects)
      (if (null? table)
	  (with-values
	      (lambda () (->constructors (reverse names)
					 (reverse objects)))
	    (lambda (prefix suffix)
	      (values offset
		      (map fake-block->code-decl *subblocks*)
		      (map fake-block->data-decl *subblocks*)
		      (append-map fake-block->c-code *subblocks*)
		      prefix
		      defines
		      new-variables
		      suffix)))
	  (let ((entry (car table)))
	    (iter (+ offset 1)
		  (cdr table)
		  (cons (string-append "current_block["
				       (entry-label entry) "]")
			names)
		  (cons (make-define-statement (entry-label entry) offset)
			defines)
		  (cons (entry-value entry)
			objects)))))

    (iter start-offset
	  (reverse (table->list-of-entries objects))
	  '()				; names
	  '()				; defines
	  '()				; objects
	  )))

(define (handle-top-level-data/traditional object)
  (fluid-let ((new-variables '())
	      (num 0))
    (with-values
	(lambda () (->constructors (list "top_level_object")
				   (list object)))
      (lambda (prefix suffix)
	(values new-variables prefix suffix)))))

(define-integrable *execute-cache-size-in-words* 2)
(define-integrable *variable-cache-size-in-words* 1)

(define (handle-free-refs-and-sets start-offset)
  ;; process free-uuo-links free-references free-assignments global-uuo-links
  ;; returns <next-offset define-code data-init-code symbol-table-components>

  (define (make-linkage-section-header start kind count)
    (if *use-stackify?*
	(stackify/make-linkage-header kind count)
	(let ((kind
	       (case kind
		 ((operator-linkage-kind) "OPERATOR_LINKAGE_KIND")
		 ((global-operator-linkage-kind) "GLOBAL_OPERATOR_LINKAGE_KIND")
		 ((assignment-linkage-kind) "ASSIGNMENT_LINKAGE_KIND")
		 ((reference-linkage-kind) "REFERENCE_LINKAGE_KIND")
		 (else (error "make-linkage-section-header: unknown kind"
			      kind)))))
	  (string-append "current_block[" (number->string start)
			 "L] = (MAKE_LINKER_HEADER (" kind
			 ", " (number->string count) "));\n\t"))))

  (define (insert-symbol label symbol)
    (let ((name (symbol->string symbol)))
      (string-append "current_block[" label
		     "] = (C_SYM_INTERN ("
		     (number->string (string-length name))
		     ", \"" name "\"));\n\t")))

  (define (process-links start links kind)
    (if (null? (cdr links))
	(values start 0 '() '())
	(let ((use-stackify? *use-stackify?*))
	  ;; The following code implicitly assumes that
	  ;; *execute-cache-size-in-words* is 2 -- check it
	  (if (and use-stackify? (not (= *execute-cache-size-in-words* 2)))
	      (error "process-links: Size inconsistency"))
	  (let process ((count 0)
			(links (cdr links))
			(offset (+ start 1))
			(defines '())
			(inits '()))
	    (cond ((null? links)
		   (values offset
			   1
			   (reverse defines)
			   (cons (make-linkage-section-header start kind count)
				 (reverse inits))))
		  ((null? (cdr (car links)))
		   (process count (cdr links) offset defines inits))
		  (else
		   (let ((entry (cadar links)))
		     (let ((name (caar links))
			   (arity (car entry))
			   (symbol (cdr entry)))
		       (process (1+ count)
				(cons (cons (caar links) (cddar links))
				      (cdr links))
				(+ offset *execute-cache-size-in-words*)
				(cons (make-define-statement symbol offset)
				      defines)
				(if use-stackify?
				    (cons* (stackify/make-uuo-arity arity)
					   (stackify/make-uuo-name name)
					   inits)
				    (cons (string-append
					   (insert-symbol symbol name)
					   "current_block["
					   symbol
					   " + 1] = ((SCHEME_OBJECT) ("
					   (number->string arity) "));\n\t")
					  inits)))))))))))

  (define (process-table start table kind)
    (let ((use-stackify? *use-stackify?*))
      ;; The following code implicitly assumes that
      ;; *variable-cache-size-in-words* is 1 -- check it below

      (define (iter offset table defines inits)
	(if (null? table)
	    (values offset
		    1
		    (reverse defines)
		    (cons (make-linkage-section-header start kind
						       (- offset (+ start 1)))
			  (reverse inits)))
	    (let ((symbol (entry-label (car table))))
	      (iter (+ offset *variable-cache-size-in-words*)
		    (cdr table)
		    (cons (make-define-statement symbol offset)
			  defines)
		    (if use-stackify?
			(cons (stackify/make-var-ref-entry
			       (entry-value (car table)))
			      inits)
			(cons (insert-symbol symbol (entry-value (car table)))
			      inits))))))

      (if (and use-stackify? (not (= *variable-cache-size-in-words* 1)))
	  (error "process-links: Size inconsistency"))

      (if (null? table)
	  (values start 0 '() '())
	  (iter (+ start 1) table '() '()))))

  (let*/mv (((offset uuos? uuodef uuoinit)
	     (process-links start-offset free-uuo-links
			    'operator-linkage-kind))
	    ((offset refs? refdef refinit)
	     (process-table offset
			    (table->list-of-entries free-references)
			    'reference-linkage-kind))
	    ((offset asss? assdef assinit)
	     (process-table offset
			    (table->list-of-entries free-assignments)
			    'assignment-linkage-kind))
	    ((offset glob? globdef globinit)
	     (process-links offset global-uuo-links
			    'global-operator-linkage-kind))
	    (free-references-sections (+ uuos? refs? asss? glob?)))
    
    (values
     offset
     (append uuodef refdef assdef globdef
	     (list (make-define-statement (special-label/free-references)
					  start-offset)
		   (make-define-statement (special-label/number-of-sections)
					  free-references-sections)))
     (append uuoinit refinit assinit globinit)
     (list (cons (special-label/free-references)
		 start-offset)
	   (cons (special-label/number-of-sections)
		 free-references-sections)))))

(define-integrable *label-sizes-in-words* 2)

(define (handle-labels label-block-offset)
  ;; returns <next-offset n-labels define-code dispatch-code
  ;;          data-init-code symbol-table-components>
  (let ((use-stackify? *use-stackify?*))
    (define (iter offset tagno labels label-defines
		  label-dispatch label-block-initialization
		  label-bindings)
      (if (null? labels)
	  (values (- offset 1)
		  tagno
		  (reverse label-defines)
		  (reverse label-dispatch)
		  (if (not use-stackify?)
		      (cons (string-append
			     "current_block["
			     (number->string label-block-offset)
			     "L] = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, "
			     (number->string (- (- offset 1)
						(+ label-block-offset 1)))
			     "));\n\t")
			    (reverse label-block-initialization))
		      (cons (stackify/make-nm-header
			     (- (- offset 1)
				(+ label-block-offset 1)))
			    (reverse label-block-initialization)))
		  label-bindings)
	  (let* ((label-data (car labels))
		 (a-symbol (or (symbol-1 label-data)
			       (symbol-2 label-data))))
	    (iter (+ offset *label-sizes-in-words*)
		  (+ tagno 1)
		  (cdr labels)
		  (cons (string-append
			 (make-define-statement a-symbol offset)
			 (let ((other-symbol (or (symbol-2 label-data)
						 (symbol-1 label-data))))
			   (if (eq? other-symbol a-symbol)
			       ""
			       (make-define-statement other-symbol a-symbol)))
			 (if (dispatch-1 label-data)
			     (make-define-statement (dispatch-1 label-data)
						    tagno)
			     "")
			 (if (dispatch-2 label-data)
			     (make-define-statement (dispatch-2 label-data)
						    tagno)
			     ""))
			label-defines)
		  (cons (string-append
			 "\n\t  case "
			 (number->string tagno) ":\n\t\t"
			 "current_block = (Rpc - " a-symbol ");\n\t\t"
			 "goto "
			 (symbol->string (or (label-1 label-data)
					     (label-2 label-data)))
			 ";\n")
			label-dispatch)
		  (add-label-initialization use-stackify?
					    a-symbol
					    tagno
					    offset
					    (code-word-sel label-data)
					    label-block-initialization)
		  (append
		   (if (label-1 label-data)
		       (list (cons (label-1 label-data) offset))
		       '())
		   (if (label-2 label-data)
		       (list (cons (label-2 label-data) offset))
		       '())
		   label-bindings)))))

    (iter (+ label-block-offset *label-sizes-in-words*)	; offset
	  0				; tagno
	  (reverse! labels)		; labels
	  '()				; label-defines
	  '()				; label-dispatch
	  '()				; label-block-initialization
	  '()				; label-bindings
	  )))

(define (add-label-initialization use-stackify? a-symbol tagno
				  offset code-word rest)
  (if use-stackify?
      (begin
	;; Note: This implicitly knows that a label takes up two words.
	(if (not (= *label-sizes-in-words* 2))
	    (error "add-label-initialization: Size inconsistency"))
	(cons* (stackify/make-label-relative-entry tagno)
	       (stackify/make-label-descriptor code-word offset)
	       rest))
      (cons (string-append "WRITE_LABEL_DESCRIPTOR(&current_block["
			   a-symbol "], 0x"
			   (number->string code-word 16)
			   ", " a-symbol ");\n\t"
			   "current_block [" a-symbol
			   "] = (dispatch_base + "
			   (number->string tagno)
			   ");\n\t")
	    rest)))

(define-structure (fake-compiled-procedure
		   (constructor make-fake-compiled-procedure)
		   (conc-name fake-procedure/))
  (block-name false read-only true)
  (label-tag false read-only true)
  (block false read-only true)
  (label-value false read-only true))

(define-structure (fake-compiled-block
		   (constructor make-fake-compiled-block)
		   (conc-name fake-block/))
  (name false read-only true)
  (tag false read-only true)
  (c-proc false read-only true)
  (d-proc false read-only true)
  (c-code false read-only true)
  (index false read-only true)
  (ntags false read-only true)
  (proxy false read-only true))

(define fake-compiled-block-name-prefix "ccBlock")

(define (fake-compiled-block-name number)
  (string-append fake-compiled-block-name-prefix
		 "_" (number->string (-1+ number))))

(define (fake-block->code-decl block)
  (string-append "\tDECLARE_SUBCODE (\""
		 (fake-block/tag block)
		 "\", " (number->string (fake-block/ntags block))
		 ", NO_SUBBLOCKS, "
		 (fake-block/c-proc block) ");\n"))

(define (fake-block->data-decl block)
  (string-append "\tDECLARE_SUBDATA (\""
		 (fake-block/tag block)
		 "\", NO_SUBBLOCKS, "
		 (fake-block/d-proc block) ");\n"))

(define (fake-block->c-code block)
  (list (fake-block/c-code block)
	"\f\n"))

;; Miscellaneous utilities

(define (->namestring/shared path)
  (if (and *shared-namestring*
	   (eq? (weak-car *shared-namestring*) path))
      (weak-cdr *shared-namestring*)
      (let* ((ns (->namestring path))
	     (wp (weak-cons path ns)))
	(set! *shared-namestring* wp)
	ns)))

(define (string-reverse string)
  (let* ((len (string-length string))
	 (res (make-string len)))
    (do ((i (fix:- len 1) (fix:- i 1))
	 (j 0 (fix:+ j 1)))
	((fix:= j len) res)
      (string-set! res i (string-ref string j)))))

(define-integrable (guaranteed-fixnum? value)
  (and (exact-integer? value)
       (<= signed-fixnum/lower-limit value)
       (< value signed-fixnum/upper-limit)))

(define-integrable (guaranteed-long? value)
  (and (exact-integer? value)
       (<= guaranteed-long/lower-limit value)
       (< value guaranteed-long/upper-limit)))
