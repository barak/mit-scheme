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
		  `(RECEIVE ,values-names ,values-form
		     ,(recur (cdr bindings)))))))))))

(define *use-stackify?* #t)
(define *disable-nonces?* #f)

(define *subblocks*)			;referenced by stackify

(define (stringify-data object)
  (if (not *use-stackify?*)
      (stringify-data/traditional object)
      (stringify-data/stackify object)))

(define (stringify-data/stackify object)
  (let* ((str (stackify 0 object))
	 (handle (default-file-handle))
	 (data-name
	  (canonicalize-label-name
	   (string-append handle "_data_" (make-nonce)))))
    (c:group (file-prefix)
	     (c:line)
	     (c:data-section
	      (stackify-output->data-decl 'prog str)
	      (c:line)
	      (c:fn #f 'sobj data-name '()
		(c:scall "DECLARE_VARIABLES_FOR_OBJECT")
		(c:line)
		(c:return (c:ecall 'unstackify
				   (c:cast 'uchar* (c:aptr 'prog 0))
				   (c:ecall 'sizeof 'prog)
				   0))))
	     (c:line)
	     (declare-data-object handle data-name))))

(define (stringify-data/traditional object)
  (let*/mv (((vars prefix suffix) (handle-top-level-data/traditional object))
	    (handle (default-file-handle))
	    (data-name
	     (canonicalize-label-name
	      (string-append handle "_data_" (make-nonce)))))
    (c:group (file-prefix)
	     (c:line)
	     (c:data-section
	      (c:fn #f 'sobj data-name '()
		(c:decl 'sobj 'top_level_object)
		(c:group* (map (lambda (var) (c:decl 'sobj var)) vars))
		(c:scall "DECLARE_VARIABLES_FOR_OBJECT")
		(c:line)
		(c:group* prefix)
		(c:group* suffix)
		(c:return 'top_level_object)))
	     (c:line)
	     (declare-data-object handle data-name))))

(define (declare-data-object handle proc)
  (c:group (c:data-section (declare-object handle proc))
	   (c:line)
	   (declare-dynamic-object-initialization handle)))

(define (stringify suffix initial-label lap-code info-output-pathname)
  ;; returns <code-name data-name ntags symbol-table code proxy>
  (let ((top-level? (string-null? suffix)))

    (define (gen-code-name nonce)
      (choose-name #f "code" "" nonce))

    (define (gen-data-name nonce)
      (choose-name #f "data" "_data" nonce))

    (define (gen-handle-name nonce)
      (choose-name #t "" "" nonce))

    (define (choose-name handle? default midfix nonce)
      (let ((nsuffix
	     (if (or *disable-nonces?* (and handle? top-level?))
		 ""
		 (string-append "_" nonce))))
	(string-append
	 (let ((name (default-file-handle)))
	   (if handle?
	       (C-quotify-string name)
	       (canonicalize-label-name name)))
	 (cond ((not info-output-pathname)
		(string-append default suffix nsuffix))
	       (top-level?
		(string-append midfix nsuffix))
	       (else
		(string-append "_" default suffix))))))

    (define (subroutine-information)
      (let*/mv (((decls-1 code-1) (subroutine-information-1))
		((decls-2 code-2) (subroutine-information-2)))
	(values (c:group decls-1 decls-2)
		(c:group code-1 code-2))))

    (define (subroutine-information-1)
      (if (eq? *invoke-interface* 'INFINITY)
	  (values (c:group)
		  (c:group))
	  (begin
	    (if (not (< *invoke-interface* 5))
		(error "Utilities take at most 4 args:" *invoke-interface*))
	    (values (c:line "INVOKE_INTERFACE_DECLS")
		    (c:exdent
		     (c:line (vector-ref '#("INVOKE_INTERFACE_TARGET_0"
					    "INVOKE_INTERFACE_TARGET_1"
					    "INVOKE_INTERFACE_TARGET_2"
					    "INVOKE_INTERFACE_TARGET_3"
					    "INVOKE_INTERFACE_TARGET_4")
					 *invoke-interface*)))))))

    (define (subroutine-information-2)
      (if *used-invoke-primitive*
	  (values (c:line "INVOKE_PRIMITIVE_DECLS")
		  (c:exdent (c:line "INVOKE_PRIMITIVE_TARGET")))
	  (values (c:group)
		  (c:group))))

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
	      (nonce (make-nonce))
	      (handle (gen-handle-name nonce))
	      (code-name (gen-code-name nonce))
	      (data-name (gen-data-name nonce))
	      (decl-code-name (string-append "decl_" code-name))
	      (decl-data-name (string-append "decl_" data-name))
	      ((extra-decls extra-code)
	       (subroutine-information))
	      ((proxy xtra-procs* decl-code* data-generator)
	       (make-data-generator top-level?
				    ntags
				    data-name
				    initial-offset
				    cc-block-size
				    temp-vars
				    object-prefix
				    label-block-initialization
				    free-block-initialization
				    object-block-initialization)))
      (values
       code-name
       data-name
       ntags
       (cons* (cons (special-label/environment)
		    (- cc-block-size 1))
	      (cons (special-label/debugging)
		    (- cc-block-size 2))
	      (append free-symbols symbol-table))
       (c:group
	(if top-level?
	    (c:group (file-prefix)
		     (c:line))
	    (c:group))

	;; Extra code

	xtra-procs
	xtra-procs*

	;; defines for the code

	(c:group* label-defines)
	object-defines
	free-defines
	(c:line)

	(let ((fn
	       (c:fn (not top-level?) 'sobj* code-name
		   (list (cons 'sobj* (c:pc-reg))
			 (cons 'entry_count_t 'dispatch_base))
		 (c:decl 'sobj* 'current_block)
		 (c:scall "DECLARE_VARIABLES")
		 ;; dlink is initialized right before perform_dispatch.
		 (c:decl-unused 'sobj* (c:dlink-reg))
		 (register-declarations)
		 extra-decls
		 (c:line)
		 ;; The assignment is necessary to ensure that we
		 ;; restart properly after an interrupt when the
		 ;; dynamic link is live (see DLINK_INTERRUPT_CHECK
		 ;; and comp_interrupt_restart)
		 (c:= (c:dlink-reg) (c:object-address (c:val-reg)))
		 (c:goto 'perform_dispatch)
		 (c:label 'pop_return)
		 (c:= (c:pc-reg) (c:object-address (c:pop)))
		 (c:label 'perform_dispatch)
		 (c:switch (c:- (c:* (c:cast 'ulong* (c:pc-reg)))
				'dispatch_base)
		   (c:group* (map (lambda (item)
				    (c:group item
					     (c:line)))
				  label-dispatch))
		   (c:case #f
			   (c:scall "UNCACHE_VARIABLES")
			   (c:return (c:pc-reg))))
		 (c:group* (map maybe-lap-comment lap-code))
		 extra-code)))
	  (if top-level?
	      (c:group
	       (if *use-stackify?*
		   (top-level/stackify handle ntags fn
				       decl-code-name code-name decl-code*
				       data-generator data-name)
		   (top-level/traditional handle ntags fn
					  decl-code-name code-name decl-code
					  data-generator
					  decl-data-name data-name decl-data))
	       (c:line)
	       (if (> ntags 0)
		   (declare-dynamic-initialization handle)
		   (declare-dynamic-object-initialization handle)))
	      (c:group
	       (c:code-section fn)
	       (if *use-stackify?*
		   (c:group)
		   (c:group (c:line)
			    (c:data-section data-generator)))))))
       proxy))))

(define (maybe-lap-comment instruction)
  (if (lap-comment? instruction)
      (c:line (c:comment (write-to-string (lap-comment-text instruction))))
      instruction))

(define (lap-comment? object)
  (and (pair? object)
       (eq? 'COMMENT (car object))
       (pair? (cdr object))
       (null? (cddr object))))

(define (lap-comment-text comment)
  (cadr comment))

(define (make-data-generator top-level?
			     ntags
			     data-name
			     initial-offset
			     cc-block-size
			     temp-vars
			     object-prefix
			     label-block-initialization
			     free-block-initialization
			     object-block-initialization)
  ;; returns <proxy xtra-procs decl-code data-generator>
  (if *use-stackify?*
      (make-data-generator/stackify top-level?
				    ntags
				    data-name
				    initial-offset
				    label-block-initialization
				    free-block-initialization
				    object-block-initialization)
      (make-data-generator/traditional top-level?
				       data-name
				       initial-offset
				       cc-block-size
				       temp-vars
				       object-prefix
				       label-block-initialization
				       free-block-initialization
				       object-block-initialization)))

(define (make-data-generator/traditional top-level?
					 data-name
					 initial-offset
					 cc-block-size
					 temp-vars
					 object-prefix
					 label-block-initialization
					 free-block-initialization
					 object-block-initialization)
  (values #f
	  (c:group)
	  '()
	  (c:fn (not top-level?) 'sobj* data-name
	      (list (cons 'entry_count_t 'dispatch_base))
	    (c:decl 'sobj
		    'object
		    (c:ecall "ALLOCATE_VECTOR"
			     (c:cast 'ulong (- cc-block-size 1))))
	    (c:decl 'sobj* 'current_block (c:object-address 'object))
	    (c:group* (map (lambda (var) (c:decl 'sobj var)) temp-vars))
	    (c:scall "DECLARE_VARIABLES_FOR_DATA")
	    (c:line)
	    (c:group* object-prefix)
	    (c:group* label-block-initialization)
	    (c:group* free-block-initialization)
	    (c:group* object-block-initialization)
	    (c:return (c:cptr initial-offset)))))

(define (make-data-generator/stackify top-level?
				      ntags
				      data-name
				      initial-offset
				      label-block-initialization
				      free-block-initialization
				      object-block-initialization)
  (let ((initv
	 (list->vector (append label-block-initialization
			       free-block-initialization
			       object-block-initialization))))
    (if top-level?
	(fluid-let ((*subblocks* '()))
	  (let ((name (string-append "prog_" data-name))
		(str (stackify ntags initv)))
	    (let ((subblocks (reverse! *subblocks*)))
	      (values #f
		      (c:group* (map fake-block->c-code subblocks))
		      subblocks
		      (c:group
		       (stackify-output->data-decl name str)
		       (c:line)
		       (c:fn #f 'sobj* data-name
			   (list (cons 'entry_count_t 'dispatch_base))
			 (c:decl 'sobj 'ccb)
			 (c:decl 'sobj* 'current_block)
			 (c:scall "DECLARE_VARIABLES_FOR_DATA")
			 (c:line)
			 (c:= 'ccb
			      (c:ecall 'unstackify
				       (c:cast 'uchar* (c:aptr name 0))
				       (c:ecall 'sizeof name)
				       'dispatch_base))
			 (c:= 'current_block (c:object-address 'ccb))
			 (c:return (c:cptr initial-offset))))))))
	(values initv (c:group) '() (c:group)))))

(define (stackify-output->data-decl name str)
  (c:group (c:line "static const unsigned char " (c:var name)
		   " [" (c:expr (string-length str)) "] =")
	   (c:indent*
	    (let ((strings (C-quotify-data-string/breakup str)))
	      (let ((p (last-pair strings)))
		(set-car! p (string-append (car p) ";")))
	      (map c:line strings)))))

(define (file-prefix)
  (c:group (c:line (c:comment "Emacs: this is -*- C -*- code,"))
	   (c:line (c:comment "generated "
			      (get-decoded-time)
			      " by Liar version "
			      (or (get-subsystem-version-string "liar/c")
				  "UNKNOWN")
			      "."))
	   (c:line)
	   (c:include "liarc.h")))

(define (make-nonce)
  (if *disable-nonces?*
      "nonce"
      (bytevector->hexadecimal (random-bytevector 8))))

(define (top-level/stackify handle ntags code-fn
			    decl-code-name code-name code-blocks
			    data-generator data-name)
  (if (> ntags 0)
      (c:group (c:code-section code-fn
			       (c:line)
			       (declare-subcodes decl-code-name code-blocks)
			       (c:line)
			       (declare-code handle ntags
					     decl-code-name code-name))
	       (c:page)
	       (c:data-section data-generator
			       (c:line)
			       (declare-data-no-subblocks handle data-name)))
      (c:group (c:code-section code-fn
			       (c:line)
			       (declare-subcodes decl-code-name code-blocks))
	       (c:page)
	       (c:data-section data-generator
			       (c:line)
			       (declare-object handle data-name)))))

(define (top-level/traditional handle ntags code-fn
			       decl-code-name code-name decl-code
			       data-generator
			       decl-data-name data-name decl-data)
  (let ((decl-code-fn
	 (c:fn #f 'int decl-code-name '()
	   decl-code
	   (c:return 0)))
	(decl-data-fn
	 (c:fn #f 'int decl-data-name '()
	   decl-data
	   (c:return 0))))
    (if (> ntags 0)
	(c:group (c:code-section code-fn
				 (c:line)
				 decl-code-fn
				 (c:line)
				 (declare-code handle ntags
					       decl-code-name code-name))
		 (c:line)
		 (c:data-section data-generator
				 (c:line)
				 decl-data-fn
				 (c:line)
				 (declare-data handle
					       decl-data-name data-name)))
	(c:group (c:code-section code-fn
				 (c:line)
				 decl-code-fn)
		 (c:line)
		 (c:data-section data-generator
				 (c:line)
				 decl-data-fn
				 (c:line)
				 (declare-object handle data-name))))))

(define (declare-code handle ntags decl proc)
  ;; This must be a single line!
  (c:line (c:call "DECLARE_COMPILED_CODE" (c:string handle) ntags decl proc)))

(define (declare-data-no-subblocks handle proc)
  ;; This must be a single line!
  (c:line (c:call "DECLARE_COMPILED_DATA_NS" (c:string handle) proc)))

(define (declare-data handle decl proc)
  ;; This must be a single line!
  (c:line (c:call "DECLARE_COMPILED_DATA" (c:string handle) decl proc)))

(define (declare-object handle proc)
  ;; This must be a single line!
  (c:line (c:call "DECLARE_DATA_OBJECT" (c:string handle) proc)))

(define (declare-dynamic-initialization handle)
  (c:line (c:call "DECLARE_DYNAMIC_INITIALIZATION"
		  (c:string handle)
		  (c:string (bytevector->hexadecimal (random-bytevector 8))))))

(define (declare-dynamic-object-initialization handle)
  (c:line (c:call "DECLARE_DYNAMIC_OBJECT_INITIALIZATION"
		  (c:string handle)
		  (c:string (bytevector->hexadecimal (random-bytevector 8))))))

(define (declare-subcodes decl-name blocks)
  (if (and (pair? blocks)
	   (pair? (cdr blocks)))
      (let ((arrname (string-append "arr_" decl-name)))
	(c:group (c:array-decl "static const struct liarc_code_S"
		     arrname
		     (length blocks)
		   (map (lambda (code-block)
			  (c:struct-init
			   (c:string (fake-block/tag code-block))
			   (fake-block/ntags code-block)
			   (fake-block/c-proc code-block)))
			blocks))
		 (c:line)
		 (c:fn #f 'int decl-name '()
		       (c:scall "DECLARE_SUBCODE_MULTIPLE" arrname)
		       (c:return 0))))
      (c:fn #f 'int decl-name '()
	    (c:group* (map fake-block->code-decl blocks))
	    (c:return 0))))

;; This is intended for short strings with an occasional escape.

(define (C-quotify-string string)
  (let* ((len (string-length string))
	 ;; The maximum expansion is *4, hence we can allocate it all here
	 (temp (make-string (fix:* 4 len))))
    (let loop ((src 0) (dst 0))
      (if (fix:>= src len)
	  (substring temp 0 dst)
	  (let ((index (string-find-next-char-in-set
			string char-set:C-string-quoted src len)))
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

;; This relies on the fact that Scheme and C use the same quoting
;; convention for the named characters when they appear in strings.

(define (C-quotify-string-char char next)
  (cond ((char-in-set? char char-set:C-named-chars)
	 (let ((result (write-to-string (string char))))
	   (substring result 1 (-1+ (string-length result)))))
	((char=? char #\NUL)
	 ;; Avoid ambiguities
	 (if (or (not next)
		 (not (char-in-set? next char-set:numeric)))
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

(define char-set:C-string-quoted
  (char-set-union
   ;; Not char-set:not-graphic
   (char-set-invert
    (char-set-intersection char-set:graphic (ascii-range->char-set 0 #x7f)))
   (char-set #\\ #\" #\? (integer->char #xA0))))

(define char-set:C-named-chars
  (char-set #\\ #\" #\Tab #\BS  ;; #\' Scheme does not quote it in strings
	    #\VT #\BEL
	    #\Linefeed #\Return #\Page))

;; This is intended for binary data encoded as a string where most of
;; the characters are not really characters at all.

(define (C-quotify-data-string/breakup string)
  (let ((builder (string-builder)))
    (map (lambda (part)
	   (builder 'reset!)
	   (builder #\")
	   (string-for-each
	    (lambda (char)
	      (let ((b (char->integer char)))
		(builder #\\)
		(builder #\x)
		(builder (digit->char (fix:quotient b #x10) 16))
		(builder (digit->char (fix:remainder b #x10) 16))))
	    part)
	   (builder #\")
	   (builder 'immutable))
	 (split-string string 64))))

(define (split-string string n)
  (let ((len (string-length string)))
    (let loop ((start 0) (parts '()))
      (let ((end (fix:+ start n)))
	(if (fix:<= end len)
	    (reverse!
	     (cons (string-slice string start len)
		   parts))
	    (loop end
		  (cons (string-slice string start end)
			parts)))))))

(define (handle-objects start-offset)
  (if *use-stackify?*
      (handle-objects/stackify start-offset)
      (handle-objects/traditional start-offset)))

(define (handle-objects/stackify start-offset)
  ;; returns <next-offset decl-code decl-data xtra-procs object-prefix
  ;;         object-defines temp-vars object-block-initialization>
  (let iter
      ((offset start-offset)
       (table (reverse (table->list-of-entries objects)))
       (defines '())
       (objects '()))
    (if (pair? table)
	(let ((entry (car table)))
	  (iter (+ offset 1)
		(cdr table)
		(cons (c:define (entry-label entry) offset) defines)
		(cons (entry-value entry) objects)))
	(values offset
		(c:group)		; code decls
		(c:group)		; data decls
		(c:group)		; procs
		'()			; object-prefix
		(c:group* defines)
		'()
		(reverse! objects)	; object-block-initialization
		))))

(define (handle-objects/traditional start-offset)
  ;; All the reverses produce the correct order in the output block.
  ;; The incoming objects are reversed
  ;; (environment, debugging label, purification root, etc.)
  ;; returns <next-offset decl-code decl-data xtra-procs object-prefix
  ;;         object-defines temp-vars object-block-initialization>

  (fluid-let ((new-variables '())
	      (*subblocks* '())
	      (num 0))
    (let iter
	((offset start-offset)
	 (table (reverse (table->list-of-entries objects)))
	 (names '())
	 (defines '())
	 (objects '()))
      (if (pair? table)
	  (let ((entry (car table)))
	    (iter (+ offset 1)
		  (cdr table)
		  (cons (c:cref (entry-label entry)) names)
		  (cons (c:define (entry-label entry) offset) defines)
		  (cons (entry-value entry) objects)))
	  (receive (prefix suffix)
	      (->constructors (reverse names)
			      (reverse objects))
	    (values offset
		    (c:group* (map fake-block->code-decl *subblocks*))
		    (c:group* (map fake-block->data-decl *subblocks*))
		    (c:group* (map fake-block->c-code *subblocks*))
		    (map c:line prefix)
		    (c:group* defines)
		    new-variables
		    suffix))))))

(define (handle-top-level-data/traditional object)
  (fluid-let ((new-variables '())
	      (num 0))
    (receive (prefix suffix)
	(->constructors (list "top_level_object")
			(list object))
      (values new-variables prefix suffix))))

(define-integrable *execute-cache-size-in-words* 2)
(define-integrable *variable-cache-size-in-words* 1)

(define (handle-free-refs-and-sets start-offset)
  ;; process free-uuo-links free-references free-assignments global-uuo-links
  ;; returns <next-offset define-code data-init-code symbol-table-components>

  (define (process-links start links kind)
    (if (pair? (cdr links))
	(begin
	  ;; The following code implicitly assumes that
	  ;; *execute-cache-size-in-words* is 2 -- check it
	  (if (and *use-stackify?* (not (= *execute-cache-size-in-words* 2)))
	      (error "process-links: Size inconsistency"))
	  (let process ((count 0)
			(links (cdr links))
			(offset (+ start 1))
			(defines '())
			(inits '()))
	    (cond ((null? links)
		   (values offset
			   1
			   (reverse! defines)
			   (cons (make-linkage-section-header start kind count)
				 (reverse! inits))))
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
				(cons (c:define symbol offset)
				      defines)
				(if *use-stackify?*
				    (cons* (stackify/make-uuo-arity arity)
					   (stackify/make-uuo-name name)
					   inits)
				    (cons (c:group
					   (insert-symbol symbol name)
					   (c:= (c:cref (c:+ symbol 1))
						(c:cast 'sobj arity)))
					  inits)))))))))
	(values start 0 '() '())))

  (define (process-table start table kind)
    ;; The following code implicitly assumes that
    ;; *variable-cache-size-in-words* is 1 -- check it below

    (define (iter offset table defines inits)
      (if (pair? table)
	  (let ((symbol (entry-label (car table))))
	    (iter (+ offset *variable-cache-size-in-words*)
		  (cdr table)
		  (cons (c:define symbol offset)
			defines)
		  (if *use-stackify?*
		      (cons (stackify/make-var-ref-entry
			     (entry-value (car table)))
			    inits)
		      (cons (insert-symbol symbol (entry-value (car table)))
			    inits))))
	  (values offset
		  1
		  (reverse! defines)
		  (cons (make-linkage-section-header start kind
						     (- offset (+ start 1)))
			(reverse! inits)))))

    (if (and *use-stackify?* (not (= *variable-cache-size-in-words* 1)))
	(error "process-links: Size inconsistency"))

    (if (pair? table)
	(iter (+ start 1) table '() '())
	(values start 0 '() '())))

  (define (make-linkage-section-header start kind count)
    (if *use-stackify?*
	(stackify/make-linkage-header kind count)
	(c:= (c:cref start)
	     (c:ecall "MAKE_LINKER_HEADER"
		      (case kind
			((operator-linkage-kind)
			 "OPERATOR_LINKAGE_KIND")
			((global-operator-linkage-kind)
			 "GLOBAL_OPERATOR_LINKAGE_KIND")
			((assignment-linkage-kind)
			 "ASSIGNMENT_LINKAGE_KIND")
			((reference-linkage-kind)
			 "REFERENCE_LINKAGE_KIND")
			(else
			 (error "Unknown linkage kind:" kind)))
		      count))))

  (define (insert-symbol label symbol)
    (let ((name (symbol->string symbol)))
      (c:= (c:cref label)
	   (c:ecall "C_SYM_INTERN"
		    (string-length name)
		    (c:string name)))))

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
     (c:group* (append! uuodef refdef assdef globdef
			(list (c:define (special-label/free-references)
					start-offset)
			      (c:define (special-label/number-of-sections)
					free-references-sections))))
     (append uuoinit refinit assinit globinit)
     (list (cons (special-label/free-references)
		 start-offset)
	   (cons (special-label/number-of-sections)
		 free-references-sections)))))

(define-integrable *label-sizes-in-words* 2)

(define (handle-labels label-block-offset)
  ;; returns <next-offset n-labels define-code dispatch-code
  ;;          data-init-code symbol-table-components>
  (let iter
      ((offset (+ label-block-offset *label-sizes-in-words*))
       (tagno 0)
       (labels (reverse! labels))
       (label-defines '())
       (label-dispatch '())
       (label-block-initialization '())
       (label-bindings '()))
    (if (pair? labels)
	(let* ((label-data (car labels))
	       (symbol (or (symbol-1 label-data)
			   (symbol-2 label-data))))
	  (iter (+ offset *label-sizes-in-words*)
		(+ tagno 1)
		(cdr labels)
		(cons (c:group (c:define symbol offset)
			       (let ((symbol*
				      (or (symbol-2 label-data)
					  (symbol-1 label-data))))
				 (if (eq? symbol* symbol)
				     (c:group)
				     (c:define symbol* symbol)))
			       (if (dispatch-1 label-data)
				   (c:define (dispatch-1 label-data) tagno)
				   (c:group))
			       (if (dispatch-2 label-data)
				   (c:define (dispatch-2 label-data) tagno)
				   (c:group)))
		      label-defines)
		(cons (c:case tagno
			      (c:= 'current_block (c:- (c:pc-reg) symbol))
			      (c:goto (or (label-1 label-data)
					  (label-2 label-data))))
		      label-dispatch)
		(add-label-initialization symbol
					  tagno
					  offset
					  (code-word-sel label-data)
					  label-block-initialization)
		(append! (if (label-1 label-data)
			     (list (cons (label-1 label-data) offset))
			     '())
			 (if (label-2 label-data)
			     (list (cons (label-2 label-data) offset))
			     '())
			 label-bindings)))
	(values (- offset 1)
		tagno
		(reverse! label-defines)
		(reverse! label-dispatch)
		(cons (if *use-stackify?*
			  (stackify/make-nm-header
			   (- (- offset 1)
			      (+ label-block-offset 1)))
			  (c:= (c:cref label-block-offset)
			       (c:make-object "TC_MANIFEST_NM_VECTOR"
					      (- (- offset 1)
						 (+ label-block-offset 1)))))
		      (reverse! label-block-initialization))
		label-bindings))))

(define (add-label-initialization symbol tagno offset code-word rest)
  (if *use-stackify?*
      (begin
	;; Note: This implicitly knows that a label takes up two words.
	(if (not (= *label-sizes-in-words* 2))
	    (error "add-label-initialization: Size inconsistency"))
	(cons* (stackify/make-label-relative-entry tagno)
	       (stackify/make-label-descriptor code-word offset)
	       rest))
      (cons (c:group (c:scall "WRITE_LABEL_DESCRIPTOR"
			      (c:cptr symbol)
			      (c:hex code-word)
			      symbol)
		     (c:= (c:cref symbol) (c:+ 'dispatch_base tagno)))
	    rest)))

(define-structure (fake-compiled-procedure
		   (constructor make-fake-compiled-procedure)
		   (conc-name fake-procedure/))
  (block-name #f read-only #t)
  (label-tag #f read-only #t)
  (block #f read-only #t)
  (label-value #f read-only #t))

(define-structure (fake-compiled-block
		   (constructor make-fake-compiled-block)
		   (conc-name fake-block/))
  (name #f read-only #t)
  (tag #f read-only #t)
  (c-proc #f read-only #t)
  (d-proc #f read-only #t)
  (c-code #f read-only #t)
  (index #f read-only #t)
  (ntags #f read-only #t)
  (proxy #f read-only #t))

(define fake-compiled-block-name-prefix "ccBlock")

(define (fake-compiled-block-name number)
  (string-append fake-compiled-block-name-prefix
		 "_" (number->string (-1+ number))))

(define (fake-block->code-decl block)
  (c:scall "DECLARE_SUBCODE"
	   (c:string (fake-block/tag block))
	   (fake-block/ntags block)
	   (fake-block/c-proc block)))

(define (fake-block->data-decl block)
  (c:scall "DECLARE_SUBDATA"
	   (c:string (fake-block/tag block))
	   (fake-block/d-proc block)))

(define (fake-block->c-code block)
  (c:group (fake-block/c-code block)
	   (c:page)))

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

;;; Edwin Variables:
;;; lisp-indent/c:fn: 4
;;; lisp-indent/c:switch: 1
;;; lisp-indent/let*/mv: 1
;;; lisp-indent/c:array-decl: 3
;;; End:
