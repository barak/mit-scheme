;;;; -*-Scheme-*-
;;; $Id: uitk-macros.scm,v 1.2 2001/12/23 17:21:00 cph Exp $
;;; derived from macros.sc,v 1.1 1993/02/16 14:04:09 jmiller Exp $
;;; Primitive X toolkit for Scheme->C.
;;; RHH, September, 1989.
;;; Macro definitions.

;;; Stolen on January 17, 1993 by Jim Miller for use with UITK
;;; Updated on June 27, 1993 by a bunch of us to use records instead
;;; of vectors.

;;;; SCC-DEFINE-STRUCTURE

;;;; Components can be:
;;;;   a symbol (name of structure component)
;;;;   a pair (name and default value)
;;;; Produces (in-lined)
;;;;   predicate procedure: (<name>? object)
;;;;   accessor procedures: (<name>.<component> object)
;;;;   mutator procedures: (SET-<name>.<component>! object new-value)
;;;;   internal constants: *-<name>.<component>-*
;;;;   internal contant: *-<name>.STRUCTURE-SIZE-*
;;;;   (re-)initialization procedure: (INIT-<name> object comp1 ...)
;;;;   creator procedure: (MAKE-<name> comp1 ...)

;;;; Note: The MAKE- and INIT- procedures have required arguments
;;;;       for all components that do not have default values.

;;;; Example:
;;;; (scc-define-structure dot x y (color 'black))
;;;; (define a-dot (make-dot 3 4))
;;;; (set-dot.color! a-dot 'green)
;;;; (list (dot.x a-dot) (dot.color a-dot)) -> (3 green)

(define-syntax scc-define-structure
  (non-hygienic-macro-transformer
   (lambda (name . components)
     (define (symbol-format . args)
       (string->symbol
	(apply string-append
	       (map (lambda (object)
		      (cond ((string? object) object)
			    ((symbol? object) (symbol->string object))
			    (else (error
				   'SYMBOL-FORMAT
				   "Neither symbol nor string ~A"
				   object))))
		    args))))
     (let ((size-name (symbol-format "*-" name '-STRUCTURE-SIZE "-*"))
	   (self-varname (lambda (fn-name)
			   (symbol-format 'SELF "/" name "/" fn-name)))
	   (predicate-name (symbol-format name "?")))

       (define (component-name component)
	 (if (pair? component) (car component) component))

       (define (accessor-name component)
	 (symbol-format name "." (component-name component)))

       (define (set-symbol component)
	 (symbol-format 'SET "-" name "." (component-name component) "!"))

       (define (gen-accessors components counter)
	 (if (null? components)
	     `((DEFINE-CONSTANT ,size-name ,counter))
	     (let ((cname (component-name (car components))))
	       (let ((offset-name (symbol-format "*-" name "." cname "-*"))
		     (self (self-varname cname)))
		 `((DEFINE-CONSTANT ,offset-name ,counter)
		   (DEFINE-IN-LINE (,(accessor-name cname) ,self)
		     (IF (,predicate-name ,self)
			 (VECTOR-REF ,self ,offset-name)
			 (ERROR ',(accessor-name cname)
				"Object not correct type ~A" ,self)))
		   (DEFINE-IN-LINE (,(set-symbol cname) ,self NEW-VALUE)
		     (IF (,predicate-name ,self)
			 (BEGIN
			   (VECTOR-SET! ,self ,offset-name NEW-VALUE)
			   'MODIFIED!)
			 (ERROR ',(set-symbol cname)
				"Object not correct type ~A" ,self)))
		   ,@(if *running-in-mit-scheme*
			 '()
			 `((DEFINE (,(accessor-name cname) ,self)
			     (IF (,predicate-name ,self)
				 (VECTOR-REF ,self ,offset-name)
				 (ERROR ',(accessor-name cname)
					"Object not correct type ~A" ,self)))
			   (DEFINE (,(set-symbol cname) ,self NEW-VALUE)
			     (IF (,predicate-name ,self)
				 (BEGIN
				   (VECTOR-SET! ,self ,offset-name NEW-VALUE)
				   'MODIFIED!)
				 (ERROR ',(set-symbol cname)
					"Object not correct type ~A" ,self)))))
		   ,@(gen-accessors (cdr components) (+ counter 1)))))))

       (define (make-bvl components)
	 (cond ((null? components) '())
	       ((pair? (car components)) (make-bvl (cdr components)))
	       (else (cons (car components) (make-bvl (cdr components))))))

       (define (gen-structure-initialization self-name components)
	 (if (null? components)
	     '()
	     `((,(set-symbol (car components))
		,self-name
		,@(if (pair? (car components))
		      (cdar components)
		      (list (car components))))
	       ,@(gen-structure-initialization self-name (cdr components)))))

       (let ((init-name (symbol-format 'INIT "-" name))
	     (init-self-name (self-varname 'INIT))
	     (init-bvl (make-bvl components))
	     (accessors (gen-accessors components 1))
	     (tag (symbol-format "#[" name "]")))
	 `(begin
	    (if ,*running-in-mit-scheme*
		(ADD-UNPARSER-SPECIAL-OBJECT!
		 ',tag
		 (lambda (obj)
		   (display "#[scc-object ")
		   (display ',name)
		   (display " ")
		   (display (hash obj))
		   (display "]"))))
	    ,@accessors
	    (DEFINE (,(symbol-format name '/pp) OBJ)
	      (IF (NUMBER? OBJ) (SET! OBJ (UNHASH OBJ)))
	      (FOR-EACH (LAMBDA (FIELD-NAME ACCESSOR)
			  (PP (LIST FIELD-NAME (ACCESSOR OBJ))))
			',(map component-name components)
			(LIST ,@(map accessor-name components))))
	    (DEFINE (,predicate-name OBJ)
	      (AND (VECTOR? OBJ)
		   (= (VECTOR-LENGTH OBJ) ,size-name)
		   (EQ? (VECTOR-REF OBJ 0) ',tag)))
	    (DEFINE (,init-name ,init-self-name ,@init-bvl)
	      (VECTOR-SET! ,init-self-name 0 ',tag)
	      ,@(gen-structure-initialization init-self-name components)
	      ,init-self-name)
	    (DEFINE (,(symbol-format 'MAKE "-" name) ,@init-bvl)
	      (,init-name (make-vector ,size-name) ,@init-bvl))))))))
