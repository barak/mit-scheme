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

;;;; Trampoline Generator
;;; package: (ffi generator)


(define c-generate-noisily? #t)

(define (c-generate library #!optional prefix)
  (let ((prefix (if (default-object? prefix) "" prefix))
	(includes (include-cdecls library)))
    (guarantee-string prefix 'c-generate)
    (let ((shim.c (string-append library "-shim.c")))
      (if c-generate-noisily?
	  (with-notification
	   (lambda (port)
	     (write-string "Generating " port)
	     (write shim.c port))
	   (lambda ()
	     (gen-trampolines shim.c prefix includes)))
	  (gen-trampolines shim.c prefix includes)))
    (let ((const.c (string-append library "-const.c")))
      (if c-generate-noisily?
	  (with-notification
	   (lambda (port)
	     (write-string "Generating " port)
	     (write (enough-namestring const.c) port))
	   (lambda ()
	     (gen-groveler const.c prefix includes)))
	  (gen-groveler const.c prefix includes)))
    (let ((types.bin (string-append library "-types.bin")))
      (fasdump includes types.bin (not c-generate-noisily?)))))

(define (gen-trampolines pathname prefix includes)
  (with-output-to-file pathname
    (lambda ()
      (write-string
       (string-append
	"/* -*-C-*- */

#include <mit-scheme.h>

/* Prefix */
" prefix "
/* End Prefix */
"))
      (gen-callout-trampolines includes)
      (if (null? (c-includes/callbacks includes))
	  unspecific
	  (gen-callback-trampolines includes)))))


;;; Callout Trampolines

(define (gen-callout-trampolines includes)
  (for-each
   (lambda (name.alienf)
     (with-simple-restart 'CONTINUE "Continue generating callout trampolines."
       (lambda ()
	 (bind-condition-handler
	  (list condition-type:simple-error)
	  (lambda (condition)
	    (let ((restart (find-restart 'CONTINUE condition))
		  (msg (access-condition condition 'MESSAGE))
		  (irr (access-condition condition 'IRRITANTS)))
	      (apply warn msg irr)
	      (if restart
		  (invoke-restart restart))))
	  (lambda ()
	    (gen-callout-trampoline (car name.alienf) (cdr name.alienf)
				    includes))))))
   (reverse (c-includes/callouts includes))))

(define (gen-callout-trampoline name alien includes)
  (let* ((ret-ctype (alien-function/return-type alien))
	 (params (alien-function/parameters alien))
	 (ret-var (callout-return-variable params)))

    ;; The second part first.
    (let ((tos-var (new-variable "tos" params)))
      (let ((declares
	     (callout-part2-decls tos-var ret-var ret-ctype includes))
	    (restores
	     (callout-restores name tos-var ret-var ret-ctype includes))
	    (return
	     (callout-return tos-var ret-var ret-ctype includes))
	    (name (symbol-name name)))
	(write-string
	 (string-append "
SCM
Scm_continue_"name" (void)
\{
  /* Declare. */" declares "

  /* Restore. */" restores "

  /* Return. */" return "
}"))))

    ;; The first part second.
    (let ((declares (callout-part1-decls ret-var ret-ctype params includes))
	  (inits (callout-inits ret-ctype params includes))
	  (call (callout-call name ret-var ret-ctype params includes))
	  (saves (callout-saves ret-var ret-ctype includes)))
      (let ((name (symbol-name name)))
	(write-string
	 (string-append "
void
Scm_"name" (void)
\{
  /* Declare. */" declares "

  /* Init. */" inits "

  /* Call. */
  callout_seal (&Scm_continue_"name");" call "

  /* Save. */
  callout_unseal (&Scm_continue_"name");" saves "

  callout_continue (&Scm_continue_"name");
  /* NOTREACHED */
}
"))))))

(define (matching-param? string params)
  (find-matching-item params
    (lambda (param) (string=? string (symbol-name (car param))))))

(define (new-variable root-name params)
  ;; Returns a name (string) for a variable that must be distinct from
  ;; those in the PARAMS alist.
  (let loop ((n 0))
    (let ((name (string-append root-name (number->string n))))
      (if (not (matching-param? name params))
	  name
	  (if (> n 9)
	      (error "Could not generate a unique variable:" root-name)
	      (loop (1+ n)))))))

(define (callout-part2-decls tos-var ret-var ret-ctype includes)
  ;; Returns a multi-line string declaring the variables to be used in
  ;; the second part of a callout trampoline.  See the Owner's Manual.
  (let ((ctype (definite-ctype ret-ctype includes))
	(decl (decl-string ret-ctype)))
    (string-append "
  char * "tos-var";"
     (if (not (ctype/void? ctype)) (string-append "
  "decl" "ret-var";") "") "
  SCM "ret-var"s;")))

(define (callout-restores name tos-var ret-var ret-ctype includes)
  (let* ((ctype (definite-ctype ret-ctype includes))
	 (tramp2 (string-append "&Scm_continue_" (symbol-name name)))
	 (ret-decl (decl-string ret-ctype)))
    (string-append "
  "tos-var" = callout_lunseal ("tramp2");"
     (if (not (ctype/void? ctype)) (string-append "
  CSTACK_LPOP ("ret-decl", "ret-var", "tos-var");") ""))))

(define (callout-return tos-var ret-var ret-ctype includes)
  (let ((ctype (definite-ctype ret-ctype includes)))
    (string-append
     (if (ctype/void? ctype)
	 (string-append "
  "ret-var"s = unspecific();")
	 (string-append "
  "ret-var"s = "(callout-return-converter ctype)" ("ret-var");")) "
  callout_pop ("tos-var");
  return ("ret-var"s);")))

(define (callout-part1-decls ret-var ret-ctype params includes)
  (let ((ctype (definite-ctype ret-ctype includes))
	(ret-decl (decl-string ret-ctype)))
    (string-append
     (if (not (ctype/void? ctype))
	 (string-append "
  "ret-decl" "ret-var";")
	 "")
     (apply string-append (let loop ((params params))
			    (if (null? params) '()
				(cons
				 (let* ((param (car params))
					(name (symbol-name (car param)))
					(type (cadr param))
					(decl (decl-string type)))
				   (string-append "
  "decl" "name";"))
				 (loop (cdr params)))))))))

(define (callout-inits ret-ctype params includes)
  ;; Returns a multi-line string in C syntax for the Init section.
  (let* ((alien-ret-arg? (ctype/pointer? (definite-ctype ret-ctype includes)))
	 (nargs
	  ;; (c-call 1:alien-function 2:ret-alien 3:arg1)
	  ;; (c-call 1:alien-function 2:arg1)
	  (number->string (+ (length params) (if alien-ret-arg? 2 1)))))
    (string-append "
  check_number_of_args ("nargs");"
     (apply string-append
	    (let loop ((params params)
		       (n (if alien-ret-arg? 3 2)))
	      (if (null? params) '()
		  (cons
		   (let* ((param (car params))
			  (name (car param))
			  (ctype (cadr param))
			  (funcast (callout-arg-converter name ctype includes))
			  (name (symbol-name name))
			  (num (number->string n)))
		     (string-append "
  "name" = "funcast" ("num");"))
		   (loop (cdr params) (1+ n)))))))))

(define (callout-saves ret-var ret-ctype includes)
  (if (not (ctype/void? (definite-ctype ret-ctype includes)))
      (string-append "
  CSTACK_PUSH ("(decl-string ret-ctype)", "ret-var");")
      ""))

(define (callout-call name ret-var ret-ctype params includes)
  ;; Returns a multi-line string in C syntax for the Call section.
  (let ((name (symbol-name name))
	(args (decorated-string-append
	       "" ", " "" (map (lambda (param) (symbol-name (car param)))
			       params))))
    (if (not (ctype/void? (definite-ctype ret-ctype includes)))
	(string-append "
  "ret-var" = "name" ("args");")
	(string-append "
  "name" ("args");"))))

(define (callout-arg-converter name arg-ctype includes)
  ;; Returns the name of the C function that takes an argument index
  ;; and returns it as the C type ARG-CTYPE.  May have a cast
  ;; expression at the beginning.  Handles args named CALLBACK and ID
  ;; specially.
  (let ((ctype (definite-ctype arg-ctype includes))
	(decl (decl-string arg-ctype)))
    (cond ((eq? name '|CALLBACK|)
	   (string-append "("decl") arg_alien_entry"))
	  ((eq? name '|ID|)
	   (string-append "("decl") arg_long"))
	  ((ctype/pointer? ctype)
	   (string-append "("decl") arg_pointer"))
	  ((ctype/enum? ctype) "arg_long")
	  ((ctype/basic? ctype)
	   (case ctype
	     ((CHAR SHORT INT LONG) "arg_long")
	     ((UCHAR USHORT UINT ULONG) "arg_ulong")
	     ((FLOAT DOUBLE) "arg_double")
	     (else (error "Unexpected parameter type:" arg-ctype))))
	  (else (error "Unexpected parameter type:" arg-ctype)))))

(define (callout-return-converter ctype)
  ;; Returns the name of a C function that converts from the definite
  ;; C type CTYPE to the analogous Scheme object.  Note that the
  ;; pointer converter, pointer_to_scm, returns pointers via c-call's
  ;; second argument.
  (cond ((ctype/pointer? ctype) "pointer_to_scm")
	((ctype/enum? ctype) "ulong_to_scm")
	((ctype/basic? ctype)
	 (case ctype
	   ((CHAR SHORT INT LONG) "long_to_scm")
	   ((UCHAR USHORT UINT ULONG) "ulong_to_scm")
	   ((FLOAT DOUBLE) "double_to_scm")
	   ((VOID) #f)
	   (else (error "Unexpected return type:" ctype))))
	(else (error "Unexpected return type:" ctype))))

(define (callout-return-variable params)
  ;; Returns a name (string) for a variable that will hold the return
  ;; value.  Checks for two name collisions with the PARAMS, e.g. ret0
  ;; and ret0s, the latter being the SCM version of the return value.
  (let loop ((n 0))
    (let* ((ns (number->string n))
	   (name1 (string-append "ret" ns))
	   (name2 (string-append "ret" ns "s")))
      (if (and (not (matching-param? name1 params))
	       (not (matching-param? name2 params)))
	  name1
	  (if (> n 9)
	      (error "Could not generate a unique ret variable.")
	      (loop (1+ n)))))))

(define (decl-string ctype)
  ;; Returns a string in C syntax declaring the C type CTYPE.
  ;; E.g. given (* |GtkWidget|), returns "GtkWidget *".
  (cond	((eq? ctype '*) "void*")
	((eq? ctype 'uchar) "unsigned char")
	((eq? ctype 'ushort) "unsigned short")
	((eq? ctype 'uint) "unsigned int")
	((eq? ctype 'ulong) "unsigned long")
	((symbol? ctype) (symbol-name ctype))
	((ctype/pointer? ctype)
	 (string-append (decl-string (ctype-pointer/target-type ctype))
			" *"))
	((ctype/const? ctype)
	 (string-append "const "
			(decl-string (ctype-const/qualified-type ctype))))
	((ctype/struct-name? ctype)
	 (string-append "struct " (symbol-name (ctype-struct/name ctype))))
	((ctype/union-name? ctype)
	 (string-append "union " (symbol-name (ctype-union/name ctype))))
	((ctype/enum-name? ctype)
	 (string-append "enum " (symbol-name (ctype-enum/name ctype))))
	(else
	 (error "Could not generate a C type declaration:" ctype))))


;;; Callback Trampolines

(define (gen-callback-trampolines includes)
  (for-each
   (lambda (name.alienf)
     (with-simple-restart 'CONTINUE "Continue generating callback trampolines."
       (lambda ()
	 (bind-condition-handler
	  (list condition-type:simple-error)
	  (lambda (condition)
	    (let ((restart (find-restart 'CONTINUE condition))
		  (msg (access-condition condition 'MESSAGE))
		  (irr (access-condition condition 'IRRITANTS)))
	      (apply warn msg irr)
	      (if restart
		  (invoke-restart restart))))
	  (lambda ()
	    (gen-callback-trampoline (car name.alienf) (cdr name.alienf)
				     includes))))))
   (reverse (c-includes/callbacks includes))))

(define (gen-callback-trampoline name alien includes)
  (let ((ret-ctype (alien-function/return-type alien))
	(params (alien-function/parameters alien)))

    ;; The second part first.
    (let ((args-var (new-variable "arglist" params))
	  (tos-var (new-variable "tos" params)))
      (let ((declares (callback-decls params))
	    (restores (callback-restores params tos-var))
	    (constructs (callback-conses params args-var includes))
	    (name (symbol-name name)))
	(write-string
	 (string-append "
static void
Scm_kernel_"name" (void)
\{
  /* Declare. */"declares"
  SCM "args-var";
  char * "tos-var";

  /* Init. */
  "tos-var" = callback_lunseal (&Scm_kernel_"name");"restores"

  /* Construct. */
  "args-var" = empty_list();"constructs"
  callback_run_handler ((long)ID, "args-var");

  callback_return ("tos-var");
}"))))

    ;; The first part second.
    (let ((arglist (arglist params))
	  (saves (callback-saves params))
	  (return (callback-return ret-ctype includes))
	  (ret-decl (decl-string ret-ctype))
	  (name (symbol-name name)))
      (write-string
       (string-append
	"
"ret-decl"
Scm_"name" ("arglist")
\{"saves"
  callback_run_kernel ((long)ID, (CallbackKernel)&Scm_kernel_"name");"return"
}
")))))

(define (callback-decls params)
  ;; Returns a multi-line string declaring the variables to be used in
  ;; the second (inner, kernel) part of a callback trampoline.
  (apply string-append (map (lambda (param)
			      (let ((decl (decl-string (cadr param)))
				    (name (symbol-name (car param))))
				(string-append "
  "decl" "name";")))
			    params)))

(define (callback-restores params tos-var)
  ;; Returns a multi-line string setting the params from the C data stack.
  (apply string-append (map (lambda (param)
			      (let ((name (symbol-name (car param)))
				    (decl (decl-string (cadr param))))
				(string-append "
  CSTACK_LPOP ("decl", "name", "tos-var");")))
			    params)))

(define (callback-conses params args-var includes)
  ;; Returns a multi-line string constructing the arglist.
  (apply string-append
	 (map (lambda (param)
		(let ((name (car param))
		      (ctype (cadr param)))
		  (if (eq? name '|ID|)
		      ""
		      (let ((name (symbol-name name)))
			(let ((construction
			       (callback-arg-cons name ctype includes)))
			  (string-append "
  "args-var" = cons ("construction", "args-var");"))))))
	      (reverse params))))

(define (arglist params)
  (decorated-string-append
   "" ", " ""				;prefix, infix, suffix
   (map (lambda (param)
	  (string-append (decl-string (cadr param))
			 " " (symbol-name (car param))))
	params)))

(define (callback-saves params)
  (apply string-append
   (map (lambda (param)
	  (let ((name (symbol-name (car param)))
		(ctype (cadr param)))
	    (string-append "
  CSTACK_PUSH ("(decl-string ctype)", "name");")))
	(reverse params))))

(define (callback-return ret-type includes)
  ;; Returns a multi-line string that returns from a callback
  ;; trampoline with a value of type RET-TYPE, converted from
  ;; val_register.
  (let ((funcast (callback-return-converter ret-type includes)))
    (if (not funcast) "
  return;"
	(string-append "
  return ("funcast" ());"))))

(define (callback-arg-cons arg-name arg-ctype includes)
  ;; Returns a function call that applies the appropriate Scheme
  ;; constructor to the ARG-CTYPE variable ARG-NAME.
  (let ((ctype (definite-ctype arg-ctype includes)))
    (if (ctype/pointer? ctype)
	(string-append "cons_alien((void*)"arg-name")")
	(let ((func (callout-return-converter ctype)))
	  (string-append func"("arg-name")")))))

(define (callback-return-converter ret-type includes)
  ;; Returns the name of the C function that takes no arguments and
  ;; returns the interpreter's VAL register as the C type RET-CTYPE.
  (let ((ctype (definite-ctype ret-type includes)))
    (cond ((ctype/pointer? ctype)
	   (string-append "("(decl-string ret-type)")pointer_value"))
	  ((ctype/enum? ctype) "long_value")
	  ((ctype/void? ctype) #f)
	  ((ctype/basic? ctype)
	   (case ctype
	     ((CHAR SHORT INT LONG) "long_value")
	     ((UCHAR USHORT UINT ULONG) "ulong_value")
	     ((FLOAT DOUBLE) "double_value")
	     (else (error "Unexpected return type:" ctype))))
	  (else (error "Unexpected return type:" ctype)))))


;;; Groveler

(define (gen-groveler pathname prefix includes)
  (with-output-to-file pathname
    (lambda ()
      (write-string
       (string-append
	"/* -*-C-*- */

/* Prefix */
"prefix"
/* End Prefix */
" (basics-grovel-func) (enums-grovel-func includes)))
      (flush-output)
      (let* ((structs (gen-struct-grovel-funcs includes))
	     (unions (gen-union-grovel-funcs includes)))
	(let ((library (c-includes/library includes)))
	  (write-string
	   (string-append "
int
main (void)
\{
  FILE * out = fopen (\""library"-const.scm\", \"w\");
  if (out == NULL) {
    perror (\"could not open "library"-const.scm\");
    return 1;
  }
  fprintf (out, \"'( ;; "library" constants\\n\");
  fprintf (out, \"  ( ;; enum member values\\n\");
  grovel_enums(out);
  fprintf (out, \"   )\\n\");
  fprintf (out, \"  ( ;; struct values\\n\");
  grovel_basics(out);"))
	  (for-each (lambda (name) (write-string (string-append "
  "name" (out);"))) structs)
	  (for-each (lambda (name) (write-string (string-append "
  "name" (out);"))) unions)
	  (write-string
	   (string-append "
  fprintf (out, \"   ))\\n\");
  if (fclose (out)) {
    perror (\"could not close "library"-const.scm\");
    return 1;
  }
  return 0;
}
")))))))

(define (basics-grovel-func)
  (string-append "
void
grovel_basics (FILE * out)
\{"
   (apply string-append
	  (map (lambda (entry)
		 (let* ((name (car entry))
			(decl (decl-string name))
			(name (symbol-name name)))
		   (string-append "
  fprintf (out, \"   ((sizeof "name") . %ld)\\n\", (long) sizeof ("decl"));")))
		    peek-poke-primitives))
   "
\}
"))

(define (enums-grovel-func includes)
  (string-append
   "
void
grovel_enums (FILE * out)
\{"
   (apply string-append
	  (map (lambda (constant)
		 (let ((name (symbol-name (car constant))))
		   (string-append "
  fprintf (out, \"   (|"name"| . %ld)\\n\", ((long)"name"));")))
	       (c-includes/enum-constants includes)))
   "
\}
"))

(define (gen-struct-grovel-funcs includes)
  ;; Returns the names of the generated functions.
  (append-map*!
   (map (lambda (name.info)
	  ;; The named structs, top-level OR internal.
	  (let ((name (list 'STRUCT (car name.info))))
	    (gen-struct-union-grovel-func name includes)))
	(c-includes/structs includes))
   (lambda (name.info)
     ;; Typedefs giving names to struct types.
     (let* ((name (car name.info))
	    (ctype (definite-ctype name includes)))
       (if (ctype/struct? ctype)
	   (list (gen-struct-union-grovel-func name includes))
	   '())))
   (c-includes/type-names includes)))

(define (gen-union-grovel-funcs includes)
  ;; Returns the names of the generated functions.
  (append-map*!
   (map (lambda (name.info)
	  ;; The named unions, top-level OR internal.
	  (let ((name (list 'UNION (car name.info))))
	    (gen-struct-union-grovel-func name includes)))
	(c-includes/unions includes))
   (lambda (name.info)
     ;; Typedefs giving names to union types.
     (let* ((name (car name.info))
	    (ctype (definite-ctype name includes)))
       (if (ctype/union? ctype)
	   (list (gen-struct-union-grovel-func name includes))
	   '())))
   (c-includes/type-names includes)))

(define (gen-struct-union-grovel-func name includes)
  ;; Generate C code for a grovel_NAME function.
  (let ((fname (cond ((ctype/struct-name? name)
		      (string-append "grovel_struct_"
				     (symbol-name (ctype-struct/name name))))
		     ((ctype/union-name? name)
		      (string-append "grovel_union_"
				     (symbol-name (ctype-union/name name))))
		     ((symbol? name)
		      (string-append "grovel_type_" (symbol-name name)))
		     (else (error "Unexpected name:" name))))
	(ctype (definite-ctype name includes))
	(decl (decl-string name))
	(_ (lambda args (for-each write-string args))))
    (let ((key (list 'SIZEOF name)))
      (_ "
void
"fname" (FILE * out)
\{
  "decl" S;
  fprintf (out, \"   (")(write key)(_" . %ld)\\n\", (long) sizeof ("decl"));"))
    (for-each-member-path
     ctype includes
     (lambda (path brief-type)
       (let ((path (decorated-string-append
		    "" "." "" (map symbol-name path)))
	     (key (cons* 'OFFSET name path)))
	 (_ "
  fprintf (out, \"   (")(write key)(_" %ld . ")(write brief-type)(_")\\n\", (long)((char*)&(S."path") - (char*)&S));"))))
    (_ "
\}
")
    fname))

(define (for-each-member-path ctype includes receiver)
  ;; Calls RECEIVER with a path and an abbreviated type for each
  ;; member (and nested member) of the struct or union CTYPE (a C
  ;; struct or union type).  Each path is a list of member names
  ;; (symbols) -- one name for immediate members, multiple names for
  ;; nested members.  An abbreviated type is a Ctype, but is 'ENUM if
  ;; the actual type is (ENUM ...).

  (let ((type (ctype-definition ctype includes)))
    (cond ((ctype/struct-defn? type)
	   (let ((stack (list ctype)))
	     (for-each (lambda (name.type)
			 (for-each-member-path*
			  name.type stack includes receiver))
		       (ctype-struct-defn/members type))))
	  ((ctype/union-defn? type)
	   (let ((stack (list ctype)))
	     (for-each (lambda (name.type)
			 (for-each-member-path*
			  name.type stack includes receiver))
		       (ctype-union-defn/members type))))
	  (else
	   (error "Unexpected Ctype to for-each-member-path:" ctype)))))

(define (for-each-member-path* name.type stack includes receiver)
  (let ((name (car name.type))
	(type (cdr name.type)))
    (let ((ctype (ctype-definition type includes)))
      (if (member ctype stack)
	  (error "Circular definition of C type:" (car (last-pair stack))))
      (cond ((or (ctype/basic? ctype)
		 (ctype/pointer? ctype)
		 (ctype/array? ctype))
	     (receiver (list name) type))
	    ((ctype/enum? ctype)
	     (receiver (list name) 'ENUM))
	    ((ctype/struct-defn? ctype)
	     (receiver (list name) type)
	     (let ((new-stack (cons type stack)))
	       (for-each (lambda (name.type)
			   (for-each-member-path*
			    name.type new-stack includes
			    (lambda (path type)
			      (receiver (cons name path) type))))
			 (ctype-struct-defn/members ctype))))
	    ((ctype/union-defn? ctype)
	     (receiver (list name) type)
	     (let ((new-stack (cons type stack)))
	       (for-each (lambda (name.type)
			   (for-each-member-path*
			    name.type new-stack includes
			    (lambda (path type)
			      (receiver (cons name path) type))))
			 (ctype-union-defn/members ctype))))
	    (else (error "Unexpected C type from ctype-definition:" ctype))))))