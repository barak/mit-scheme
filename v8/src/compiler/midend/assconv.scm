#| -*-Scheme-*-

$Id: assconv.scm,v 1.14 1996/02/08 17:10:59 adams Exp $

Copyright (c) 1994-1995 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Assignment Converter
;;; package: (compiler midend)

(declare (usual-integrations))

;;  Assignment conversion.
;;
;;  Each variable reference occurs in some dynamic extent.  Each
;;  environment belongs to an extent.  Some environments, like nested
;;  LET frames, belong to the same extent.  Extents are represented by
;;  unique values (small integers).  Later we partition bindings
;;  according to equivalence classes of the sets of extents in which
;;  references or assignments to them occur.  The idea behind this is
;;  that mutable variables that travel around together will be placed
;;  in the same multi-cell but variables that are live in different
;;  extents are allocated to different cells, allowing better GC
;;  behaviour.

;; Legal values: BY-EXTENT (as decribed above), INDIVIDUAL-CELLS (each
;; mutable binding is allocated a separate cell), ONE-MULTICELL (all
;; mutable bindings from a given frame are put in one multicell)

(define *assconv/partitioning* 'BY-EXTENT)

(define (assconv/top-level program)
  (fluid-let ((*assconv/effect-only-forms* (make-eq-hash-table))
	      (*assconv/extent-counter*    0))
    (assconv/expr (assconv/env/make '() #F (assconv/new-extent))
		  program)))


(define-macro (define-assignment-converter keyword bindings . body)
  (let ((proc-name (symbol-append 'ASSCONV/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (NAMED-LAMBDA (,proc-name ENV FORM)
	       (ASSCONV/REMEMBER ,code form))))))))

;;;; Variable manipulation forms

(define-assignment-converter LAMBDA (env lambda-list body)
  (assconv/lambda* env lambda-list body (assconv/new-extent)))

(define (assconv/lambda* env lambda-list body next-extent)
  (call-with-values
   (lambda ()
     (assconv/binding-body env
			   next-extent
			   (lambda-list->names lambda-list)
			   body))
   (lambda (shadowed body*)
     `(LAMBDA ,(if (null? shadowed)
		   lambda-list
		   (map (lambda (name)
			  (if (memq name shadowed)
			      (let ((outer-name (assconv/new-name 'IGNORED)))
				(dbg-info/remember name outer-name)
				outer-name)
			      name))
			lambda-list))
	,body*))))

(define-assignment-converter LET (env bindings body)
  (call-with-values
   (lambda ()
     (assconv/binding-body env
			   (assconv/env/extent env)
			   (map car bindings)
			   body))
   (lambda (shadowed body*)
     `(LET ,(map (lambda (binding)
		   (list (car binding)
			 (assconv/expr env (cadr binding))))
		 (if (null? shadowed)
		     bindings
		     (list-transform-negative bindings
		       (lambda (binding)
			 (memq (car binding) shadowed)))))
	,body*))))

(define-assignment-converter LOOKUP (env name)
  (let ((binding (assconv/env/lookup env name)))
    (if (not binding)
	(free-var-error name)
	(let ((result `(LOOKUP ,name)))
	  (set-assconv/binding/references!
	   binding
	   (cons result (assconv/binding/references binding)))
	  (assconv/binding/new-extent binding env)
	  result))))

(define-assignment-converter SET! (env name value)
  (let ((binding (assconv/env/lookup env name)))
    (if (not binding)
	(free-var-error name)
	(let ((result `(SET! ,name ,(assconv/expr env value))))
	  (set-assconv/binding/assignments!
	   binding
	   (cons result (assconv/binding/assignments binding)))
	  (assconv/binding/new-extent binding env)
	  result))))

;;;; Trivial forms

(define-assignment-converter QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-assignment-converter DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-assignment-converter CALL (env rator cont #!rest rands)
  (define (finish rator*)
    `(CALL ,rator*
	   ,(assconv/expr env cont)
	   ,@(assconv/expr* env rands)))
  (if (LAMBDA/? rator)			; i.e. user level LET
      (finish (assconv/lambda* env
			       (lambda/formals rator)
			       (lambda/body rator)
			       (assconv/env/extent env)))
      (finish (assconv/expr env rator))))

(define-assignment-converter BEGIN (env #!rest actions)
  (let  ((actions*   (assconv/expr* env actions)))
    (let loop ((actions actions*))
      (cond ((or (null? actions) (null? (cdr actions))))
	    (else
	     (assconv/form/set-effect-only! (car actions))
	     (loop (cdr actions)))))
    `(BEGIN ,@actions*)))

(define-assignment-converter IF (env pred conseq alt)
  `(IF ,(assconv/expr env pred)
       ,(assconv/expr env conseq)
       ,(assconv/expr env alt)))

;;; Dispatcher

(define (assconv/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (assconv/quote env expr))
    ((LOOKUP)   (assconv/lookup env expr))
    ((LAMBDA)   (assconv/lambda env expr))
    ((LET)      (assconv/let env expr))
    ((DECLARE)  (assconv/declare env expr))
    ((CALL)     (assconv/call env expr))
    ((BEGIN)    (assconv/begin env expr))
    ((IF)       (assconv/if env expr))
    ((SET!)     (assconv/set! env expr))
    ((LETREC) (not-yet-legal expr))
    (else     (illegal expr))))

(define (assconv/expr* env exprs)
  (map (lambda (expr)
	 (assconv/expr env expr))
       exprs))

(define (assconv/remember new old)
  (code-rewrite/remember new old)
  new)

(define (assconv/new-name prefix)
  (new-variable prefix))

(define (assconv/rename variable)
  (variable/rename variable))

(define (assconv/new-cell-name prefix)
  (new-variable (string-append (symbol-name prefix) "-cell")))


(define *assconv/effect-only-forms*)

(define (assconv/form/set-effect-only! form)
  (hash-table/put! *assconv/effect-only-forms* form #T))

(define (assconv/form/effect-only? form)
  (hash-table/get *assconv/effect-only-forms* form #F))

;;;; Environments and extents.

(define *assconv/extent-counter*)

(define (assconv/new-extent)
  (set! *assconv/extent-counter* (+ *assconv/extent-counter* 1))
  *assconv/extent-counter*)

(define-structure (assconv/env
		   (conc-name assconv/env/)
		   (constructor assconv/env/make (bindings parent extent)))
  parent
  extent
  bindings)

(define-structure (assconv/binding
		   (conc-name assconv/binding/)
		   (constructor assconv/binding/make (name initial-extent)))
  (name false read-only true)
  (cell-name false read-only false)
  (multicell-layout false read-only false)
  (references '() read-only false)
  (assignments '() read-only false)
  ;;  The extents associated with this binding.  The INITIAL-EXTENT is the
  ;;  extent in which the binding is introduced.  The other extents
  ;;  are for the references and assignments.
  (initial-extent #F read-only true)
  (extents '() read-only false))


(define (assconv/binding/new-extent binding env)
  (let ((extent (assconv/env/extent env)))
    (set-assconv/binding/extents!
     binding
     (cons extent (assconv/binding/extents binding)))))

(define (assconv/env/lookup env name)
  (let spine-loop ((env env))
    (and env
	 (let rib-loop ((rib (assconv/env/bindings env)))
	   (cond ((null? rib)
		  (spine-loop (assconv/env/parent env)))
		 ((eq? name (assconv/binding/name (car rib)))
		  (car rib))
		 (else
		  (rib-loop (cdr rib))))))))


(define (assconv/binding-body env next-extent names body)
  ;; (values shadowed-names body*)
  ;; SHADOWED-NAMES are those names for which we introduced internal bindings,
  ;; e.g. a LET binding.
  (let* ((frame (map (lambda (name) (assconv/binding/make name next-extent))
		     names))
	 (env*  (assconv/env/make frame env next-extent))
	 (body* (assconv/expr env* body))
	 (assigned
	  (list-transform-positive frame
	    (lambda (binding)
	      (not (null? (assconv/binding/assignments binding))))))
	 (ssa-candidates
	  (list-transform-positive assigned
	    (lambda (binding)
	      (let ((assignments (assconv/binding/assignments binding)))
		(and (null? (cdr assignments))
		     (assconv/single-assignment/trivial?
		      (car assignments))))))))
    (if (null? ssa-candidates)
	(assconv/bind-cells '() assigned body*)
	(call-with-values
	 (lambda ()
	   (assconv/single-analyze ssa-candidates body*))
	 (lambda (let-like letrec-like)
	   (assconv/bind-cells
	    (map assconv/binding/name (append let-like letrec-like))
	    (list-transform-negative assigned
	      (lambda (binding)
		(or (memq binding let-like)
		    (memq binding letrec-like))))
	    (assconv/letify 'LET
			    let-like
			    (assconv/letify 'LETREC
					    letrec-like
					    body*))))))))

(define (assconv/first-assignment body)
  (let loop ((actions (list body)))
    (and (not (null? actions))
	 (pair? (car actions))
	 (case (car (car actions))
	   ((BEGIN)
	    (loop (append (cdr (car actions)) (cdr actions))))
	   ((DECLARE)
	    (loop (cdr actions)))
	   ((SET!)
	    (and (not (null? (cdr actions)))
		 (car actions)))
	   (else
	    false)))))

(define (assconv/bind-cells shadowed-names bindings body)
  ;; (values shadowed-names body*)
  ;; Last chance to undo an assignment
  (define (finish shadowed-names bindings body)
    (if (null? bindings)
	(values shadowed-names body)
	(let ((cell-bindings (assconv/partition-cells! bindings)))
	  (for-each assconv/cellify! bindings)
	  (values
	   shadowed-names
	   `(LET ,cell-bindings
	      ,body)))))

  (define (default)
    (finish shadowed-names bindings body))

  (cond ((null? bindings)
	 (default))
	((assconv/first-assignment body)
	 => (lambda (ass)
	      (let* ((name (set!/name ass))
		     (binding
		      (list-search-positive bindings
			(lambda (binding)
			  (eq? (assconv/binding/name binding)
			       name))))
		     (value (set!/expr ass)))
		(if (or (not binding)
			(not (null? (cdr (assconv/binding/assignments
					  binding))))
			(memq name (form/free-vars value))) ; JSM
		    (default)
		    (begin
		      (form/rewrite! ass `(QUOTE ,%unspecific))
		      (finish (cons name shadowed-names)
			      (delq binding bindings)
			      (bind name value body)))))))
	(else (default))))

(define (assconv/letify keyword bindings body)
  `(,keyword
    ,(map (lambda (binding)
	     (let* ((ass    (car (assconv/binding/assignments binding)))
		    (value  (set!/expr ass)))
	       (form/rewrite! ass `(QUOTE ,%unassigned))
	       `(,(assconv/binding/name binding) ,value)))
	   bindings)
    ,body))

(define (assconv/cell-reference binding)
  `(CALL (QUOTE ,%multicell-ref)
	 (QUOTE #F)
	 (LOOKUP ,(assconv/binding/cell-name binding))
	 (QUOTE ,(assconv/binding/multicell-layout binding))
	 (QUOTE ,(assconv/binding/name binding))))

(define (assconv/cell-assignment binding value assignment-form)
  (let* ((cell-name  (assconv/binding/cell-name binding))
	 (value-name (assconv/binding/name binding))
	 (local-name (assconv/rename value-name)))
    #|					;
    ;; This returns the new value
    (bind local-name value
	  `(BEGIN
	     (CALL (QUOTE ,%cell-set!)
		   (QUOTE #F)
		   (LOOKUP ,cell-name)
		   (LOOKUP ,local-name)
		   (QUOTE ,value-name))
	     (LOOKUP ,local-name)))
    |#
    ;; This returns the old value, if needed
    (let ((write-cell!
	   `(CALL (QUOTE ,%multicell-set!)
		  (QUOTE #F)
		  (LOOKUP ,cell-name)
		  ,value
		  (QUOTE ,(assconv/binding/multicell-layout binding))
		  (QUOTE ,value-name))))
      (if (assconv/form/effect-only? assignment-form)
	  write-cell!
	  (bind local-name
		(assconv/cell-reference binding)
		`(BEGIN
		   ,write-cell!
		   (LOOKUP ,local-name)))))))

(define (assconv/partition-cells! bindings)
  ;; 1. Decide on cell structure for bindings.  This is done by using
  ;;    multicells for variables with the same dynamic extent or by a
  ;;    simpler method.
  ;; 2. Returns a list of LET-bindings to create the cells

  ;; A PARTITION is a list of headed lists.
  ;; The first two elements of the list are the cell name and format.
  ;; The remaining elements are the bindings assigned to that cell.
  
  (define (each-binding-in-its-own-cell) ;returns a partition
    (map (lambda (binding)
	   (list (assconv/new-cell-name (assconv/binding/name binding))
		 (vector (assconv/binding/name binding))
		 binding))
	 bindings))

  (define (all-in-one-cell)		;returns a partition
    (list
     (cons* (assconv/new-cell-name 'MULTI)
	    (list->vector (map assconv/binding/name bindings))
	    bindings)))

  (define (partition-by-extents)	; returns a partition
    (let ((table (make-equal-hash-table)))
      ;; collect bindings by equivalence class:
      (for-each
	  (lambda (binding)
	    (let ((extent-class
		   (sort-removing-duplicates
		    (cons (assconv/binding/initial-extent binding)
			  (assconv/binding/extents binding)))))
	      (hash-table/put! table
			       extent-class
			       (cons binding
				     (hash-table/get table extent-class '())))))
	bindings)

      (if compiler:guru?
	  (let ((k (map length (hash-table/datum-list table))))
	    (if (not (for-all? k (lambda (x) (= x 1))))
		(internal-warning "Cells partitioned:" k))))
      (map (lambda (bindings)
	     (cons* (assconv/new-cell-name
		     (if (null? (cdr bindings))
			 (assconv/binding/name (car bindings))
			 'MULTI))
		    (list->vector (map assconv/binding/name bindings))
		    bindings))
	   (hash-table/datum-list table))))

  (define (sort-removing-duplicates items)
    (define (loop l)
      (if (and (pair? l) (pair? (cdr l)))
	  (split l '() '())
	  l))

    (define (split l one two)
      (if (pair? l)
	  (split (cdr l) two (cons (car l) one))
	  (merge (loop one) (loop two))))

    (define (remove elt elts)
      (cond ((null? elts) '())
	    ((= elt (car elts)) (remove elt (cdr elts)))
	    (else elts)))

    (define (merge one two)
      (cond ((null? one) two)
	    ((null? two) one)
	    ((< (car two) (car one))
	     (cons (car two)
		   (merge (remove (car two) (cdr two)) one)))
	    ((= (car two) (car one))
	     (cons (car two)
		   (merge (remove (car two) (cdr two))
			  (remove (car two) one))))
	    (else
	     (cons (car one)
		   (merge (remove (car one) (cdr one)) two)))))

    (loop items))

  (define (impose-partition partition)
    (map (lambda (subset)
	   (let ((cell-name (first subset))
		 (layout    (second subset))
		 (bindings  (cddr subset)))
	     (for-each
		 (lambda (binding)
		   (set-assconv/binding/cell-name! binding cell-name)
		   (set-assconv/binding/multicell-layout! binding layout))
	       bindings)
	     `(,cell-name
	       (CALL (QUOTE ,%make-multicell)
		     (QUOTE #F)
		     (QUOTE ,layout)
		     ,@(map (lambda (binding)
			      `(LOOKUP ,(assconv/binding/name binding)))
			    bindings)))))
	 partition))
  (impose-partition
   (case *assconv/partitioning*
     ((BY-EXTENT)        (partition-by-extents))
     ((INDIVIDUAL-CELLS) (each-binding-in-its-own-cell))
     ((ONE-MULTICELL)    (all-in-one-cell))
     (else (internal-error 
	    "Illegal configuration of *assconv/partitioning*"
	    *assconv/partitioning*)))))

(define (assconv/cellify! binding)
  (for-each (lambda (ref)
	      (form/rewrite!
		  ref
		(assconv/cell-reference binding)))
    (assconv/binding/references binding))
  (for-each (lambda (ass)
	      (form/rewrite!
		  ass
		(assconv/cell-assignment binding (set!/expr ass) ass)))
    (assconv/binding/assignments binding))
  (dbg-info/remember (assconv/binding/name binding)
		     (assconv/cell-reference binding)))

(define (assconv/single-assignment/trivial? assignment-form)
  (let ((name  (set!/name assignment-form))
	(value (set!/expr assignment-form)))
    (or (QUOTE/? value)
	(and (LAMBDA/? value)
	     #| (not (memq name (form/free-vars value))) |#
	     ))))

(define (assconv/single-analyze ssa-candidates body)
  ;; (values let-like letrec-like)
  ;; This only recognizes very simple patterns.
  ;; It can be improved in the future.
  (if (not (pair? body))
      (values '() '())
      (let ((single-assignments
	     (map (lambda (binding)
		    (cons (car (assconv/binding/assignments binding))
			  binding))
		  ssa-candidates))
	    (finish
	     (lambda (bindings)
	       (values
		(reverse
		 (list-transform-positive bindings
		   (lambda (binding)
		     (QUOTE/? (set!/expr (first (assconv/binding/assignments
						 binding)))))))
		(reverse
		 (list-transform-positive bindings
		   (lambda (binding)
		     (LAMBDA/? (set!/expr (first (assconv/binding/assignments
						  binding)))))))))))

	(let loop ((bindings '())
		   (actions  (if (BEGIN/? body)
				 (begin/exprs body)
				 (list body))))
	  (cond ((null? actions)
		 (finish bindings))
		((assq (car actions) single-assignments)
		 => (lambda (single-assignment)
		      (loop (cons (cdr single-assignment) bindings)
			    (cdr actions))))
		((DECLARE/? (car actions))
		 (loop bindings (cdr actions)))
		((SET!/? (car actions))
		 (if (assconv/single-assignment/trivial? (car actions))
		     (loop bindings (cdr actions))
		     (finish bindings)))
		(else
		 (finish bindings)))))))