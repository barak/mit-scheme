#| -*-Scheme-*-

$Id: cout.scm,v 1.22 2002/11/20 19:45:49 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; C-output fake assembler and linker
;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define *C-procedure-name* 'DEFAULT)
(define *disable-timestamps?* false)

(define (stringify suffix initial-label lap-code info-output-pathname)
  (define (stringify-object x)
    (cond ((string? x)
	   x)
	  ((symbol? x)
	   (%symbol->string x))
	  ((number? x)
	   (number->string x))
	  (else
	   (error "stringify: Unknown frob" x))))

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

  (define (choose-proc-name default midfix time-stamp)
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
	       (string-append (if (or (not dir) (null? dir))
				  default
				  (canonicalize-label-name
				   (car (last-pair dir))))
			      "_"
			      (canonicalize-label-name (pathname-name path))
			      midfix
			      suffix)))
	    (else
	     (string-append (canonicalize-label-name (pathname-name path))
			    "_"
			    default
			    suffix
			    time-stamp)))))

  (define (subroutine-information-1)
    (cond ((eq? *invoke-interface* 'INFINITY)
	   (values (list "") (list "")))
	  ((< *invoke-interface* 5)
	   (values (list-tail (list
			       "\ninvoke_interface_0:\n\tutlarg_1 = 0;\n"
			       "\ninvoke_interface_1:\n\tutlarg_2 = 0;\n"
			       "\ninvoke_interface_2:\n\tutlarg_3 = 0;\n"
			       "\ninvoke_interface_3:\n\tutlarg_4 = 0;\n"
			       "\ninvoke_interface_4:\n\t"
			       "INVOKE_INTERFACE_CODE ();\n")
			      *invoke-interface*)
		   (list "\tint utlarg_code;\n"
			 "\tlong utlarg_1, utlarg_2, utlarg_3, utlarg_4;\n")))
	  (else
	   (error "subroutine-information-1: Utilities take at most 4 args"
		  *invoke-interface*))))

  (define (subroutine-information-2)
    (if *used-invoke-primitive*
	(values (list "\ninvoke_primitive:\n\t"
		      "INVOKE_PRIMITIVE_CODE ();")
		(list "\tSCHEME_OBJECT primitive;\n"
		      "\tlong primitive_nargs;\n"))
	(values (list "") (list ""))))

  (define (subroutine-information)
    (with-values subroutine-information-1
      (lambda (code-1 vars-1)
	(with-values subroutine-information-2
	  (lambda (code-2 vars-2)
	    (values (append code-1 code-2)
		    (append vars-1 vars-2)))))))

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
	     (->namestring obj))
	    ((pair? obj)
	     (cons (frob (car obj))
		   (frob (cdr obj))))
	    (else
	     obj))))

  (define-object (special-label/environment) unspecific)

  (let ((n 1)				; First word is vector header
	(initial-offset (label->offset initial-label)))
    (with-values (lambda () (handle-labels n))
      (lambda (n ntags
	       label-defines label-dispatch
	       label-block-initialization symbol-table)
	(with-values (lambda () (handle-free-refs-and-sets n))
	  (lambda (n free-defines free-block-initialization free-symbols)
	    (with-values (lambda () (handle-objects n))
	      (lambda (n decl-code decl-data
			 xtra-procs object-prefix
			 object-defines temp-vars
			 object-block-initialization)
		(let* ((time-stamp (make-time-stamp))
		       (code-name
			(choose-proc-name "code" "" time-stamp))
		       (data-name
			(choose-proc-name "data" "_data" time-stamp))
		       (decl-code-name (string-append "decl_" code-name))
		       (decl-data-name (string-append "decl_" data-name)))
		  (with-values subroutine-information
		    (lambda (extra-code extra-variables)
		      (values
		       code-name
		       data-name
		       ntags
		       (cons* (cons (special-label/environment)
				    (-1+ n))
			      (cons (special-label/debugging)
				    (- n 2))
			      (append free-symbols symbol-table))
		       (list-of-strings->string
			(map (lambda (x)
			       (list-of-strings->string x)) 
			     (list
			      (if (string-null? suffix)
				  (append
				   (file-prefix)
				   (list
				    "#ifndef WANT_ONLY_DATA\n"
				    ;; This must be a single line!
				    "DECLARE_COMPILED_CODE (\"" code-name
				    "\", " (number->string ntags)
				    ", " decl-code-name
				    ", " code-name ")\n"
				    "#endif /* WANT_ONLY_DATA */\n\n"
				    "#ifndef WANT_ONLY_CODE\n"
				    ;; This must be a single line!
				    "DECLARE_COMPILED_DATA (\"" code-name
				    "\", " decl-data-name
				    ", " data-name ")\n"
				    "#endif /* WANT_ONLY_CODE */\n\n"
				    "DECLARE_DYNAMIC_INITIALIZATION (\""
				    code-name "\")\n\n"))
				  '())
			      xtra-procs

			      (if (string-null? suffix)
				  (append
				   (list "#ifndef WANT_ONLY_DATA\n")
				   (list
				    "int\n"
				    "DEFUN_VOID (" decl-code-name ")\n{\n\t")
				   decl-code
				   (list "return (0);\n}\n"
					 "#endif /* WANT_ONLY_DATA */\n\n")
				   (list "#ifndef WANT_ONLY_CODE\n")
				   (list
				    "int\n"
				    "DEFUN_VOID (" decl-data-name ")\n{\n\t")
				   decl-data
				   (list "return (0);\n}\n"
					 "#endif /* WANT_ONLY_CODE */\n\n"))
				  '())

			      label-defines
			      object-defines
			      free-defines
			      (list "\n")
			  
			      (list "#ifndef WANT_ONLY_CODE\n")
			      (let ((header (data-function-header data-name)))
				(if (string-null? suffix)
				    header
				    (cons "static " header)))
			      (list "\tSCHEME_OBJECT object"
				    " = (ALLOCATE_VECTOR ("
				    (number->string (- n 1))
				    "L));\n"
				    "\tSCHEME_OBJECT * current_block"
				    " = (OBJECT_ADDRESS (object));\n")
			      (->variable-declarations temp-vars)
			      (list "\n\t")
			      object-prefix
			      label-block-initialization
			      free-block-initialization
			      object-block-initialization
			      (list "\n\treturn (&current_block["
				    (stringify-object initial-offset)
				    "]);\n")
			      (function-trailer data-name)
			      (list "#endif /* WANT_ONLY_CODE */\n")
			      (list "\n")

			      (list "#ifndef WANT_ONLY_DATA\n")
			      (let ((header (code-function-header code-name)))
				(if (string-null? suffix)
				    header
				    (cons "static " header)))
			      (function-decls)
			      (register-declarations)
			      extra-variables
			      (list
			       "\n\tgoto perform_dispatch;\n\n"
			       "pop_return:\n\t"
			       "Rpc = (OBJECT_ADDRESS (*Rsp++));\n\n"
			       "perform_dispatch:\n\n\t"
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
			       "#endif /* WANT_ONLY_DATA */\n"))))))))))))))))

(define-integrable (list-of-strings->string strings)
  (apply string-append strings))

(define-integrable (%symbol->string sym)
  (system-pair-car sym))

(define (file-prefix)
  (let ((time (get-decoded-time)))
    (list "/* Emacs: this is properly parenthesized -*- C -*- code.\n"
	  "   Thank God it was generated by a machine.\n"
	  " */\n\n"
	  "/* C code produced\n   "
	  (decoded-time/date-string time)
	  " at "
	  (decoded-time/time-string time)
	  "\n   by Liar version "
	  (or (get-subsystem-version-string "liar") "?.?")
	  ".\n */\n\n"
	  "#include \"liarc.h\"\n\n")))

(define (code-function-header name)
  (list "SCHEME_OBJECT *\n"
	"DEFUN (" name ", (Rpc, dispatch_base),\n\t"
	"SCHEME_OBJECT * Rpc AND unsigned long dispatch_base)\n"
	"{\n"))

(define (data-function-header name)
  (list "SCHEME_OBJECT *\n"
	"DEFUN (" name ", (dispatch_base), unsigned long dispatch_base)\n"
	"{\n"))

(define (function-decls)
  (list
   "\tREGISTER SCHEME_OBJECT * current_block;\n"
   "\tSCHEME_OBJECT * Rdl;\n"
   "\tDECLARE_VARIABLES ();\n"))

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

;;;; Object constructors

(define new-variables)
(define *subblocks*)
(define num)

(define (generate-variable-name)
  (set! new-variables
	(cons (string-append "tmpObj" (number->string num))
	      new-variables))
  (set! num (1+ num))
  (car new-variables))

(define-integrable (table/find table value)
  ;; assv ?
  (assq value table))

(define-integrable (guaranteed-fixnum? value)
  (and (exact-integer? value)
       (<= signed-fixnum/lower-limit value)
       (< value signed-fixnum/upper-limit)))

(define-integrable (guaranteed-long? value)
  (and (exact-integer? value)
       (<= guaranteed-long/lower-limit value)
       (< value guaranteed-long/upper-limit)))

(define trivial-objects
  (list #f #t '() unspecific))

(define (trivial? object)
  (or (memq object trivial-objects)
      (guaranteed-fixnum? object)))

(define *depth-limit* 2)

(define (name-if-complicated node depth)
  (cond ((fake-compiled-block? node)
	 (let ((name (fake-block/name node)))
	   (set! new-variables (cons name new-variables))
	   name))
	((or (%record? node)
	     (vector? node)
	     (> depth *depth-limit*))
	 (generate-variable-name))
	(else
	 false)))  

(define (build-table nodes)
  (map cdr
       (sort (sort/enumerate
	      (list-transform-positive
		  (let loop ((nodes nodes)
			     (table '()))
		    (if (null? nodes)
			table
			(loop (cdr nodes)
			      (insert-in-table (car nodes)
					       0
					       table))))
		(lambda (pair)
		  (cdr pair))))
	     (lambda (entry1 entry2)
	       (let ((obj1 (cadr entry1))
		     (obj2 (cadr entry2)))
		 (if (not (fake-compiled-block? obj2))
		     (or (fake-compiled-block? obj1)
			 (< (car entry1) (car entry2)))
		     (and (fake-compiled-block? obj1)
			  (< (fake-block/index obj1)
			     (fake-block/index obj2)))))))))

;; Hack to make sort a stable sort

(define (sort/enumerate l)
  (let loop ((l l) (n 0) (l* '()))
    (if (null? l)
	l*
	(loop (cdr l)
	      (1+ n)
	      (cons (cons n (car l))
		    l*)))))

(define (insert-in-table node depth table)
  (cond ((trivial? node)
	 table)
	((table/find table node)
	 => (lambda (pair)
	      (if (not (cdr pair))
		  (set-cdr! pair (generate-variable-name)))
	      table))
	(else
	 (let* ((name (name-if-complicated node depth))
		(depth* (if name 1 (1+ depth)))
		(table (cons (cons node name) table)))

	   (define-integrable (do-vector-like node vlength vref)
	     (let loop ((table table)
			(i (vlength node)))
	       (if (zero? i)
		   table
		   (let ((i-1 (-1+ i)))
		     (loop (insert-in-table (vref node i-1)
					    depth*
					    table)
			   i-1)))))
	     
	   (cond ((pair? node)
		  ;; Special treatment on the CDR because of RCONSM.
		  (insert-in-table
		   (car node)
		   depth*
		   (insert-in-table (cdr node)
				    (if name 1 depth)
				    table)))
		 ((vector? node)
		  (do-vector-like node vector-length vector-ref))
		 ((or (fake-compiled-procedure? node)
		      (fake-compiled-block? node))
		  table)
		 ((%record? node)
		  (do-vector-like node %record-length %record-ref))
		 (else
		  ;; Atom
		  table))))))

(define (top-level-constructor object&name)
  ;; (values prefix suffix)
  (let ((name (cdr object&name))
	(object (car object&name)))
    (cond ((pair? object)
	   (values '()
		   (list name " = (cons (SHARP_F, SHARP_F));\n\t")))
	  ((fake-compiled-block? object)
	   (set! *subblocks* (cons object *subblocks*))
	   (values (list name " = (initialize_subblock (\""
			 (fake-block/c-proc object)
			 "\"));\n\t")
		   '()))
	  ((fake-compiled-procedure? object)
	   (values '()
		   (list name " = "
			 (compiled-procedure-constructor
			  object)
			 ";\n\t")))
	  ((vector? object)
	   (values '()
		   (list name " = (ALLOCATE_VECTOR ("
			 (number->string (vector-length object))
			 "));\n\t")))
	  ((%record? object)
	   (values '()
		   (list name " = (ALLOCATE_RECORD ("
			 (number->string (%record-length object))
			 "));\n\t")))
	  (else
	   (values '()
		   (list name "\n\t  = "
			 (->simple-C-object object)
			 ";\n\t"))))))

(define (top-level-updator object&name table)
  (let ((name (cdr object&name))
	(object (car object&name)))

    (define-integrable (do-vector-like object vlength vref vset-name)
      (let loop ((i (vlength object))
		 (code '()))
	(if (zero? i)
	    code
	    (let ((i-1 (- i 1)))
	      (loop i-1
		    `(,vset-name " (" ,name ", "
				 ,(number->string i-1) ", "
				 ,(constructor (vref object i-1)
					       table)
				 ");\n\t"
				 ,@code))))))

    (cond ((pair? object)
	   (list "SET_PAIR_CAR (" name ", "
		 (constructor (car object) table) ");\n\t"
		 "SET_PAIR_CDR (" name ", "
		 (constructor (cdr object) table) ");\n\t"))
	  ((or (fake-compiled-block? object)
	       (fake-compiled-procedure? object))
	   '(""))
	  ((%record? object)
	   (do-vector-like object %record-length %record-ref "RECORD_SET"))
	  ((vector? object)
	   (do-vector-like object vector-length vector-ref "VECTOR_SET"))
	  (else
	   '("")))))

(define (constructor object table)
  (let process ((object object))
    (cond ((table/find table object) => cdr)
	  ((pair? object)
	   (cond ((or (not (pair? (cdr object)))
		      (table/find table (cdr object)))
		  (string-append "(CONS (" (process (car object)) ", "
				 (process (cdr object)) "))"))
		 (else
		  (let loop ((npairs 0)
			     (object object)
			     (frobs '()))
		    (if (and (pair? object) (not (table/find table object)))
			(loop (1+ npairs)
			      (cdr object)
			      (cons (car object) frobs))
			;; List is reversed to call rconsm
			(string-append
			 "(RCONSM (" (number->string (1+ npairs))
			 (apply string-append
				(map (lambda (frob)
				       (string-append ",\n\t\t"
						      (process frob)))
				     (cons object frobs)))
			 "))"))))))
	  ((fake-compiled-procedure? object)
	   (compiled-procedure-constructor object))
	  ((or (fake-compiled-block? object)
	       (vector? object)
	       (%record? object))
	   (error "constructor: Can't build directly"
		  object))
	  (else
	   (->simple-C-object object)))))

(define (compiled-procedure-constructor object)
  (string-append "(CC_BLOCK_TO_ENTRY ("
		 (fake-procedure/block-name object)
		 ", "
		 (number->string
		  (fake-procedure/label-index object))
		 "))"))

(define (top-level-constructors table)
  ;; (values prefix suffix)
  ;; (append-map top-level-constructor table)
  (let loop ((table (reverse table)) (prefix '()) (suffix '()))
    (if (null? table)
	(values prefix suffix)
	(with-values (lambda () (top-level-constructor (car table)))
	  (lambda (prefix* suffix*)
	    (loop (cdr table)
		  (append prefix* prefix)
		  (append suffix* suffix)))))))

(define (->constructors names objects)
  ;; (values prefix-code suffix-code)
  (let* ((table (build-table objects)))
    (with-values (lambda () (top-level-constructors table))
      (lambda (prefix suffix)
	(values prefix
		(append suffix
			(append-map (lambda (object&name)
				      (top-level-updator object&name table))
				    table)
			(append-map
			 (lambda (name object)
			   (list (string-append name "\n\t  = "
						(constructor object table)
						";\n\t")))
			 names
			 objects)))))))

(define (string-reverse string)
  (let* ((len (string-length string))
	 (res (make-string len)))
    (do ((i (fix:- len 1) (fix:- i 1))
	 (j 0 (fix:+ j 1)))
	((fix:= j len) res)
      (string-set! res i (string-ref string j)))))

(define (->simple-C-object object)
  (cond ((symbol? object)
	 (let ((name (symbol->string object)))
	   (string-append "(C_SYM_INTERN ("
			  (number->string (string-length name))
			  "L, \"" (C-quotify-string name) "\"))")))
	((string? object)
	 (string-append "(C_STRING_TO_SCHEME_STRING ("
			(number->string (string-length object))
			"L, \"" (C-quotify-string object) "\"))"))
	((number? object)
	 (let process ((number object))
	   (cond ((flo:flonum? number)
		  (string-append "(DOUBLE_TO_FLONUM ("
				 (number->string number) "))"))
		 ((guaranteed-long? number)
		  (string-append "(LONG_TO_INTEGER ("
				 (number->string number) "L))"))
		 ((exact-integer? number)
		  (let ((bignum-string
			 (number->string (if (negative? number)
					     (- number)
					     number)
					 16)))
		    (string-append "(DIGIT_STRING_TO_INTEGER ("
				   (if (negative? number)
				       "true, "
				       "false, ")
				   (number->string
				    (string-length bignum-string))
				   "L, \"" bignum-string "\"))")))
		 ((and (exact? number) (rational? number))
		  (string-append "(MAKE_RATIO ("
				 (process (numerator number))
				 ", " (process (denominator number))
				 "))"))
		 ((and (complex? number) (not (real? number)))
		  (string-append "(MAKE_COMPLEX ("
				 (process (real-part number))
				 ", " (process (imag-part number))
				 "))"))
		 (else
		  (error "scheme->C-object: Unknown number" number)))))
	((eq? #f object)
	 "SHARP_F")
	((eq? #t object)
	 "SHARP_T")
	((primitive-procedure? object)
	 (let ((arity (primitive-procedure-arity object)))
	   (if (< arity -1)
	       (error "scheme->C-object: Unknown arity primitive" object)
	       (string-append "(MAKE_PRIMITIVE_PROCEDURE (\""
			      (symbol->string
			       (primitive-procedure-name object))
			      "\", "
			      (number->string arity)
			      "))"))))
	((char? object)
	 (string-append "(MAKE_CHAR ("
			(let ((bits (char-bits object)))
			  (if (zero? bits)
			      "0"
			      (string-append "0x" (number->string bits 16))))
			", ((unsigned) "
			(C-quotify-char (make-char (char-code object) 0))
			")))"))
	((bit-string? object)
	 (let ((string (number->string (bit-string->unsigned-integer object)
				       16)))
	   (string-append "(DIGIT_STRING_TO_BIT_STRING ("
			  (number->string (bit-string-length object)) "L, "
			  (number->string (string-length string)) "L, \""
			  (string-reverse string)
			  "\"))")))
	((null? object)
	 "NIL")
	((eq? object unspecific)
	 "UNSPECIFIC")
	((or (object-type? (ucode-type true) object)
	     (object-type? (ucode-type false) object))
	 ;; Random assorted objects, e.g.: #!rest, #!optional
	 (string-append "(MAKE_OBJECT ("
			(if (object-type? (ucode-type true) object)
			    "TC_TRUE"
			    "TC_FALSE")
			", "
			(number->string (object-datum object))
			"L))"))
	;; Note: The following are here because of the Scode interpreter
	;; and the runtime system.
	;; They are not necessary for ordinary code.
	((interpreter-return-address? object)
	 (string-append "(MAKE_OBJECT (TC_RETURN_CODE, 0x"
			(number->string (object-datum object) 16)
			"))"))
	(else
	 (error "->simple-C-object: unrecognized-type"
		object))))

(define char-set:C-char-quoted
  (char-set-union char-set:not-graphic (char-set #\\ #\')))

(define char-set:C-string-quoted
  (char-set-union char-set:not-graphic (char-set #\\ #\")))

(define char-set:C-named-chars
  (char-set #\\ #\" #\Tab #\BS  ;; #\' Scheme does not quote it in strings
	    ;; #\VT #\BEL	;; Cannot depend on ANSI C
	    #\Linefeed #\Return #\Page))

(define (C-quotify-string string)
  (let ((index (string-find-next-char-in-set string char-set:C-string-quoted)))
    (if (not index)
	string
	(string-append
	 (substring string 0 index)
	 (C-quotify-string-char (string-ref string index))
	 (C-quotify-string
	  (substring string (1+ index) (string-length string)))))))

;; The following two routines rely on the fact that Scheme and C
;; use the same quoting convention for the named characters when they
;; appear in strings.

(define (C-quotify-string-char char)
  (cond ((char-set-member? char-set:C-named-chars char)
	 (let ((result (write-to-string (string char))))
	   (substring result 1 (-1+ (string-length result)))))
	((char=? char #\NUL)
	 "\\0")
	(else
	 (string-append
	  "\\"
	  (let ((s (number->string (char-code char) 8)))
	    (if (< (string-length s) 3)
		(string-append (make-string (- 3 (string-length s)) #\0)
			       s)
		s))))))

(define (C-quotify-char char)
  (cond ((not (char-set-member? char-set:C-char-quoted char))
	 (string #\' char #\'))
	((char-set-member? char-set:C-named-chars char)
	 (string-append
	  "'"
	  (let ((s (write-to-string (make-string 1 char))))
	    (substring s 1 (-1+ (string-length s))))
	  "'"))
	((char=? char #\')
	 "'\\''")
	((char=? char #\NUL)
	 "'\\0'")
	(else
	 (string-append
	  "'\\"
	  (let ((s (number->string (char-code char) 8)))
	    (if (< (string-length s) 3)
		(string-append (make-string (- 3 (string-length s)) #\0)
			       s)
		s))
	  "'"))))

(define (handle-objects n)
  ;; All the reverses produce the correct order in the output block.
  ;; The incoming objects are reversed
  ;; (environment, debugging label, purification root, etc.)
  ;; (values new-n decl-code decl-data xtra-procs object-prefix
  ;;         object-defines temp-vars object-block-initialization)

  (fluid-let ((new-variables '())
	      (*subblocks* '())
	      (num 0))

    (define (iter n table names defines objects)
      (if (null? table)
	  (with-values
	      (lambda () (->constructors (reverse names)
					 (reverse objects)))
	    (lambda (prefix suffix)
	      (values n
		      (map fake-block->code-decl *subblocks*)
		      (map fake-block->data-decl *subblocks*)
		      (append-map fake-block->c-code *subblocks*)
		      prefix
		      defines
		      new-variables
		      suffix)))
	  (let ((entry (car table)))
	    (iter (1+ n)
		  (cdr table)
		  (cons (string-append "current_block["
				       (entry-label entry) "]")
			names)
		  (cons (make-define-statement (entry-label entry) n)
			defines)
		  (cons (entry-value entry)
			objects)))))

    (iter n (reverse (table->list-of-entries objects)) '() '() '())))

(define (handle-free-refs-and-sets start-offset)
  ;; process free-uuo-links free-references free-assignments global-uuo-links
  ;; return n defines initialization

  (define (make-linkage-section-header start kind count)
    (string-append "current_block[" (number->string start)
		   "L] = (MAKE_LINKER_HEADER (" kind
		   ", " (number->string count) "));\n\t"))

  (define (insert-symbol label symbol)
    (let ((name (symbol->string symbol)))
      (string-append "current_block[" label
		     "] = (C_SYM_INTERN ("
		     (number->string (string-length name))
		     ", \"" name "\"));\n\t")))

  (define (process-links start links kind)
    (if (null? (cdr links))
	(values start 0 '() '())
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
			      (+ offset 2)
			      (cons (make-define-statement symbol offset)
				    defines)
			      (cons (string-append
				     (insert-symbol symbol name)
				     "current_block["
				     symbol
				     " + 1] = ((SCHEME_OBJECT) ("
				     (number->string arity) "));\n\t")
				    inits)))))))))

  (define (process-table start table kind)
    (define (iter n table defines inits)
      (if (null? table)
	  (values n
		  1
		  (reverse defines)
		  (cons (make-linkage-section-header start kind
						     (- n (+ start 1)))
			(reverse inits)))
	  (let ((symbol (entry-label (car table))))
	    (iter (1+ n)
		  (cdr table)
		  (cons (make-define-statement symbol n)
			defines)
		  (cons (insert-symbol symbol (entry-value (car table)))
			inits)))))

    (if (null? table)
	(values start 0 '() '())
	(iter (1+ start) table '() '())))

  (with-values
      (lambda () (process-links start-offset free-uuo-links
				"OPERATOR_LINKAGE_KIND"))
    (lambda (offset uuos? uuodef uuoinit)
      (with-values
	  (lambda ()
	    (process-table offset
			   (table->list-of-entries free-references)
			   "REFERENCE_LINKAGE_KIND"))
	(lambda (offset refs? refdef refinit)
	  (with-values
	      (lambda ()
		(process-table offset
			       (table->list-of-entries free-assignments)
			       "ASSIGNMENT_LINKAGE_KIND"))
	    (lambda (offset asss? assdef assinit)
	      (with-values
		  (lambda () (process-links offset global-uuo-links
					    "GLOBAL_OPERATOR_LINKAGE_KIND"))
		(lambda (offset glob? globdef globinit)
		  (let ((free-references-sections (+ uuos? refs? asss? glob?)))
		    (values
		     offset
		     (append
		      uuodef refdef assdef globdef
		      (list
		       (make-define-statement
			(special-label/free-references)
			start-offset)
		       (make-define-statement
			(special-label/number-of-sections)
			free-references-sections)))
		     (append uuoinit refinit assinit globinit)
		     (list (cons (special-label/free-references)
				 start-offset)
			   (cons (special-label/number-of-sections)
				 free-references-sections)))))))))))))

(define (handle-labels n)
  (define (iter offset tagno labels label-defines
		label-dispatch label-block-initialization
		label-bindings)
    (if (null? labels)
	(values (- offset 1)
		tagno
		(reverse label-defines)
		(reverse label-dispatch)
		(cons (string-append
		       "current_block["
		       (number->string n)
		       "L] = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, "
		       (number->string (- (- offset 1) (+ n 1)))
		       "));\n\t")
		      (reverse label-block-initialization))
		label-bindings)
	(let* ((label-data (car labels))
	       (a-symbol (or (symbol-1 label-data)
			     (symbol-2 label-data))))
	  (iter (+ offset 2)
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
		(cons (string-append
		       "WRITE_LABEL_DESCRIPTOR(&current_block["
		       a-symbol "], 0x"
		       (number->string (code-word-sel label-data) 16)
		       ", " a-symbol ");\n\t"
		       "current_block [" a-symbol
		       "] = (dispatch_base + " (number->string tagno) ");\n\t")
		      label-block-initialization)
		(append
		 (if (label-1 label-data)
		     (list (cons (label-1 label-data) offset))
		     '())
		 (if (label-2 label-data)
		     (list (cons (label-2 label-data) offset))
		     '())
		 label-bindings)))))

    (iter (+ 2 n) 0 (reverse! labels) '() '() '() '()))

(define-structure (fake-compiled-procedure
		   (constructor make-fake-compiled-procedure)
		   (conc-name fake-procedure/))
  (block-name false read-only true)
  (label-index false read-only true))

(define-structure (fake-compiled-block
		   (constructor make-fake-compiled-block)
		   (conc-name fake-block/))
  (name false read-only true)
  (c-proc false read-only true)
  (d-proc false read-only true)
  (c-code false read-only true)
  (index false read-only true)
  (ntags false read-only true))

(define fake-compiled-block-name-prefix "ccBlock")

(define (fake-compiled-block-name number)
  (string-append fake-compiled-block-name-prefix
		 "_" (number->string (-1+ number))))

(define (fake-block->code-decl block)
  (string-append "DECLARE_SUBCODE (\""
		 (fake-block/c-proc block)
		 "\", " (number->string (fake-block/ntags block))
		 ", NO_SUBBLOCKS, "
		 (fake-block/c-proc block) ");\n\t"))

(define (fake-block->data-decl block)
  (string-append "DECLARE_SUBDATA (\""
		 (fake-block/c-proc block)
		 "\", NO_SUBBLOCKS, "
		 (fake-block/d-proc block) ");\n\t"))

(define (fake-block->c-code block)
  (list (fake-block/c-code block)
	"\f\n"))