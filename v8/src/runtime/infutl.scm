#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/runtime/infutl.scm,v 1.15 1989/11/21 00:00:31 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Compiled Code Information: Utilities
;;; package: (runtime compiler-info)

(declare (usual-integrations))
(declare (integrate-external "infstr"))

(define (initialize-package!)
  (set! special-form-procedure-names
	`((,lambda-tag:unnamed . LAMBDA)
	  (,lambda-tag:internal-lambda . LAMBDA)
	  (,lambda-tag:internal-lexpr . LAMBDA)
	  (,lambda-tag:let . LET)
	  (,lambda-tag:fluid-let . FLUID-LET)
	  (,lambda-tag:make-environment . MAKE-ENVIRONMENT)))
  (set! blocks-with-memoized-debugging-info (make-population))
  (add-secondary-gc-daemon! discard-debugging-info!))

(define (compiled-code-block/dbg-info block demand-load?)
  (let ((old-info (compiled-code-block/debugging-info block)))
    (if (and (pair? old-info) (dbg-info? (car old-info)))
	(car old-info)
	(and demand-load?
	     (let ((dbg-info (read-debugging-info old-info)))
	       (if dbg-info (memoize-debugging-info! block dbg-info))
	       dbg-info)))))

(define (discard-debugging-info!)
  (without-interrupts
   (lambda ()
     (map-over-population! blocks-with-memoized-debugging-info
			   discard-block-debugging-info!)
     (set! blocks-with-memoized-debugging-info (make-population))
     unspecific)))

(define (read-debugging-info descriptor)
  (cond ((string? descriptor)
	 (let ((binf (read-binf-file descriptor)))
	   (and binf
		(if (dbg-info? binf)
		    binf
		    (and (vector? binf)
			 (not (zero? (vector-length binf)))
			 (vector-ref binf 0))))))
	((and (pair? descriptor)
	      (string? (car descriptor))
	      (exact-nonnegative-integer? (cdr descriptor)))
	 (let ((binf (read-binf-file (car descriptor))))
	   (and binf
		(vector? binf)
		(< (cdr descriptor) (vector-length binf))
		(vector-ref binf (cdr descriptor)))))
	(else
	 false)))

(define (read-binf-file filename)
  (and (file-exists? filename)
       (call-with-current-continuation
	(lambda (k)
	  (bind-condition-handler (list error-type:fasload)
	      (lambda (condition) condition (k false))
	    (lambda () (fasload filename true)))))))

(define (memoize-debugging-info! block dbg-info)
  (without-interrupts
   (lambda ()
     (let ((old-info (compiled-code-block/debugging-info block)))
       (if (not (and (pair? old-info) (dbg-info? (car old-info))))
	   (begin
	     (set-compiled-code-block/debugging-info! block
						      (cons dbg-info old-info))
	     (add-to-population! blocks-with-memoized-debugging-info
				 block)))))))

(define (un-memoize-debugging-info! block)
  (without-interrupts
   (lambda ()
     (discard-block-debugging-info! block)
     (remove-from-population! blocks-with-memoized-debugging-info block))))

(define (discard-block-debugging-info! block)
  (let ((old-info (compiled-code-block/debugging-info block)))
    (if (and (pair? old-info) (dbg-info? (car old-info)))
	(set-compiled-code-block/debugging-info! block (cdr old-info)))))

(define blocks-with-memoized-debugging-info)

(define (compiled-entry/dbg-object entry #!optional demand-load?)
  (let ((block (compiled-entry/block entry))
	(offset (compiled-entry/offset entry)))
    (let ((dbg-info
	   (compiled-code-block/dbg-info block
					 (if (default-object? demand-load?)
					     true
					     demand-load?))))
      (and dbg-info
	   (let ((find-procedure
		  (lambda ()
		    (vector-binary-search (dbg-info/procedures dbg-info)
					  <
					  dbg-procedure/label-offset
					  offset))))
	     (discriminate-compiled-entry entry
	       find-procedure
	       (lambda ()
		 (vector-binary-search (dbg-info/continuations dbg-info)
				       <
				       dbg-continuation/label-offset
				       offset))	       (lambda ()
		 (let ((expression (dbg-info/expression dbg-info)))
		   (if (= offset (dbg-expression/label-offset expression))
		       expression
		       (find-procedure))))
	       (lambda ()
		 false)))))))

(define (compiled-entry/block entry)
  (if (compiled-closure? entry)
      (compiled-entry/block (compiled-closure->entry entry))
      (compiled-code-address->block entry)))

(define (compiled-entry/offset entry)
  (if (compiled-closure? entry)
      (compiled-entry/offset (compiled-closure->entry entry))
      (compiled-code-address->offset entry)))

(define (compiled-entry/filename entry)
  (let loop
      ((info
	(compiled-code-block/debugging-info (compiled-entry/block entry))))
    (cond ((string? info) (values info false))
	  ((not (pair? info)) (values false false))
	  ((dbg-info? (car info)) (loop (cdr info)))
	  ((string? (car info))
	   (values (car info)
		   (and (exact-nonnegative-integer? (cdr info))
			(cdr info))))
	  (else (values false false)))))

(define (dbg-labels/find-offset labels offset)
  (vector-binary-search labels < dbg-label/offset offset))

(define (dbg-info-vector/blocks-vector info)
  (let ((items (dbg-info-vector/items info)))
    (cond ((vector? items) items)
	  ((and (pair? items)
		(pair? (cdr items))
		(vector? (cadr items)))
	   (cadr items))
	  (else (error "Illegal dbg-info-vector" info)))))

(define (dbg-info-vector/purification-root info)
  (let ((items (dbg-info-vector/items info)))
    (cond ((vector? items) false)
	  ((and (pair? items)
		(eq? (car items) 'COMPILED-BY-PROCEDURES)
		(pair? (cdr items))
		(pair? (cddr items)))
	   (caddr items))
	  (else (error "Illegal dbg-info-vector" info)))))

(define (fasload/update-debugging-info! value com-pathname)
  (let ((process-block
	 (lambda (block)
	   (let ((binf-filename
		  (process-binf-filename
		   (compiled-code-block/debugging-info block)
		   com-pathname)))
	     (set-compiled-code-block/debugging-info! block binf-filename)
	     binf-filename))))
    (cond ((compiled-code-address? value)
	   (process-block (compiled-code-address->block value)))
	  ((and (comment? value)
		(dbg-info-vector? (comment-text value)))
	   (let ((blocks (dbg-info-vector/blocks-vector (comment-text value))))
	     (let ((binf-filename (process-block (vector-ref blocks 0)))
		   (end (vector-length blocks)))
	       (let loop ((index 1))
		 (if (< index end)
		     (begin
		       (set-car! (compiled-code-block/debugging-info
				  (vector-ref blocks index))
				 binf-filename)
		       (loop (1+ index)))))))))))

(define (process-binf-filename binf-filename com-pathname)
  (pathname->string
   (rewrite-directory
    (let ((binf-pathname
	   (pathname->absolute-pathname
	    (->pathname binf-filename))))
      (if (and (equal? (pathname-name binf-pathname)
		       (pathname-name com-pathname))
	       (not (equal? (pathname-type binf-pathname)
			    (pathname-type com-pathname)))
	       (equal? (pathname-version binf-pathname)
		       (pathname-version com-pathname)))
	  (pathname-new-type com-pathname (pathname-type binf-pathname))
	  binf-pathname)))))

(define directory-rewriting-rules
  '())

(define (add-directory-rewriting-rule! match replace)
  (let ((match (pathname->absolute-pathname (->pathname match)))
	(replace (pathname->absolute-pathname (->pathname replace))))
    (let ((rule
	   (list-search-positive directory-rewriting-rules
	     (lambda (rule)
	       (equal? (pathname-directory (car rule))
		       (pathname-directory match))))))
      (if rule
	  (set-cdr! rule replace)
	  (set! directory-rewriting-rules
		(cons (cons match replace)
		      directory-rewriting-rules)))))
  unspecific)

(define (rewrite-directory pathname)
  (let ((rule
	 (list-search-positive directory-rewriting-rules
	   (lambda (rule)
	     (directory-prefix? (pathname-directory pathname)
				(pathname-directory (car rule)))))))
    (if rule
	(pathname-new-directory
	 pathname
	 (append (pathname-directory (cdr rule))
		 (list-tail (pathname-directory pathname)
			    (length (pathname-directory (car rule))))))
	pathname)))

(define (directory-prefix? x y)
  (or (null? y)
      (and (not (null? x))
	   (equal? (car x) (car y))
	   (directory-prefix? (cdr x) (cdr y)))))

(define (dbg-block/dynamic-link-index block)
  (vector-find-next-element (dbg-block/layout block)
			    dbg-block-name/dynamic-link))

(define (dbg-block/ic-parent-index block)
  (vector-find-next-element (dbg-block/layout block)
			    dbg-block-name/ic-parent))

(define (dbg-block/normal-closure-index block)
  (vector-find-next-element (dbg-block/layout block)
			    dbg-block-name/normal-closure))

(define (dbg-block/return-address-index block)
  (vector-find-next-element (dbg-block/layout block)
			    dbg-block-name/return-address))

(define (dbg-block/static-link-index block)
  (vector-find-next-element (dbg-block/layout block)
			    dbg-block-name/static-link))

(define (dbg-block/find-name block name)
  (let ((layout (dbg-block/layout block)))
    (let ((end (vector-length layout)))
      (let loop ((index 0))
	(and (< index end)
	     (if (let ((item (vector-ref layout index)))
		   (and (dbg-variable? item)
			(eq? name (dbg-variable/name item))))
		 index
		 (loop (1+ index))))))))

(define (compiled-procedure/name entry)
  (let ((procedure
	 (compiled-entry/dbg-object entry load-debugging-info-on-demand?)))
    (and procedure
	 (let ((name (dbg-procedure/name procedure)))
	   (or (special-form-procedure-name? name)
	       (symbol->string name))))))

(define load-debugging-info-on-demand?
  false)

(define (special-form-procedure-name? name)
  (let ((association (assq name special-form-procedure-names)))
    (and association
	 (symbol->string (cdr association)))))

(define special-form-procedure-names)

(define (compiled-procedure/lambda entry)
  (let ((procedure (compiled-entry/dbg-object entry)))
    (and procedure
	 (dbg-procedure/source-code procedure))))

(define (compiled-expression/scode entry)
  (let ((object (compiled-entry/dbg-object entry)))
    (or (and (dbg-procedure? object)
	     (let ((scode (dbg-procedure/source-code object)))
	       (and scode
		    (lambda-body scode))))
	entry)))