#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/infutl.scm,v 1.5 1988/12/30 23:30:00 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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
  (set! blocks-with-memoized-debugging-info (make-population))
  (set! special-form-procedure-names
	`((,lambda-tag:unnamed . LAMBDA)
	  (,lambda-tag:internal-lambda . LAMBDA)
	  (,lambda-tag:internal-lexpr . LAMBDA)
	  (,lambda-tag:let . LET)
	  (,lambda-tag:fluid-let . FLUID-LET)
	  (,lambda-tag:make-environment . MAKE-ENVIRONMENT)))
  unspecific)

(define (compiled-code-block/dbg-info block demand-load?)
  (let ((old-info (compiled-code-block/debugging-info block)))
    (if (and (pair? old-info) (dbg-info? (car old-info)))
	(car old-info)
	(and demand-load?
	     (let ((dbg-info (read-debugging-info old-info)))
	       (if dbg-info
		   (memoize-debugging-info! block dbg-info))
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
	   (and binf (dbg-info? binf) binf)))	((and (pair? descriptor)
	      (string? (car descriptor))
	      (integer? (cdr descriptor)))
	 (let ((binf (read-binf-file (car descriptor))))
	   (and binf
		(dbg-info-vector? binf)
		(vector-ref (dbg-info-vector/items binf) (cdr descriptor)))))
	(else
	 false)))

(define (read-binf-file filename)
  (and (file-exists? filename)
       (fasload filename true)))
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
					     false
					     demand-load?))))
      (and dbg-info
	   (discriminate-compiled-entry entry
	     (lambda ()
	       (vector-binary-search (dbg-info/procedures dbg-info)
				     <
				     dbg-procedure/label-offset
				     offset))
	     (lambda ()
	       (vector-binary-search (dbg-info/continuations dbg-info)
				     <
				     dbg-continuation/label-offset
				     offset))
	     (lambda ()
	       (let ((expression (dbg-info/expression dbg-info)))
		 (and (= offset (dbg-expression/label-offset expression))
		      expression)))
	     (lambda ()
	       false))))))

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
    (cond ((string? info)
	   info)
	  ((pair? info)
	   (cond ((string? (car info)) (car info))
		 ((dbg-info? (car info)) (loop (cdr info)))
		 (else false)))
	  (else
	   false))))

(define (dbg-labels/find-offset labels offset)
  (vector-binary-search labels < dbg-label/offset offset))

(define (vector-binary-search vector < unwrap-key key)
  (let loop ((start 0) (end (vector-length vector)))
    (and (< start end)
	 (let ((midpoint (quotient (+ start end) 2)))
	   (let ((item (vector-ref vector midpoint)))
	     (let ((key* (unwrap-key item)))
	       (cond ((< key key*) (loop start midpoint))
		     ((< key* key) (loop (1+ midpoint) end))
		     (else item))))))))
(define (fasload/update-debugging-info! value com-pathname)
  (let ((process-filename
	 (lambda (binf-filename)
	   (let ((binf-pathname (string->pathname binf-filename)))
	     (if (and (equal? (pathname-name binf-pathname)
			      (pathname-name com-pathname))
		      (not (equal? (pathname-type binf-pathname)
				   (pathname-type com-pathname)))
		      (equal? (pathname-version binf-pathname)
			      (pathname-version com-pathname)))
		 (pathname->string
		  (pathname-new-type com-pathname
				     (pathname-type binf-pathname)))
		 binf-filename)))))
    (let ((process-entry
	   (lambda (entry)
	     (let ((block (compiled-code-address->block entry)))
	       (let ((info (compiled-code-block/debugging-info block)))
		 (cond ((string? info)
			(set-compiled-code-block/debugging-info!
			 block
			 (process-filename info)))
		       ((and (pair? info) (string? (car info)))
			(set-car! info (process-filename (car info))))))))))
      (cond ((compiled-code-address? value)
	     (process-entry value))
	    ((comment? value)
	     (let ((text (comment-text value)))
	       (if (dbg-info-vector? text)
		   (for-each
		    process-entry
		    (vector->list (dbg-info-vector/items text))))))))))

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
	     (if (dbg-name=? name (vector-ref layout index))
		 index
		 (loop (1+ index))))))))

(define-integrable (symbol->dbg-name symbol)
  (cond ((object-type? (ucode-type interned-symbol) symbol)
	 (system-pair-car symbol))
	((object-type? (ucode-type uninterned-symbol) symbol)
	 symbol)
	(else
	 (error "SYMBOL->DBG-NAME: not a symbol" symbol))))

(define (dbg-name? object)
  (or (string? object)
      (object-type? (ucode-type interned-symbol) object)
      (object-type? (ucode-type uninterned-symbol) object)))

(define (dbg-name/normal? object)
  (or (string? object)
      (object-type? (ucode-type uninterned-symbol) object)))

(define (dbg-name=? x y)
  (or (eq? x y)
      (let ((name->string
	     (lambda (name)
	       (cond ((string? name)
		      name)
		     ((object-type? (ucode-type interned-symbol) name)
		      (system-pair-car name))
		     (else
		      false)))))
	(let ((x (name->string x)) (y (name->string y)))
	  (and x y (string-ci=? x y))))))

(define (dbg-name<? x y)
  (let ((name->string
	 (lambda (name)
	   (cond ((string? name)
		  name)
		 ((or (object-type? (ucode-type interned-symbol) name)
		      (object-type? (ucode-type uninterned-symbol) name))
		  (system-pair-car name))
		 (else
		  (error "Illegal dbg-name" name))))))
    (string-ci<? (name->string x) (name->string y))))

(define (dbg-name/string name)
  (cond ((string? name)
	 name)
	((object-type? (ucode-type interned-symbol) name)
	 (system-pair-car name))
	((object-type? (ucode-type uninterned-symbol) name)
	 (write-to-string name))
	(else
	 (error "Illegal dbg-name" name))))

  (let ((procedure
	 (compiled-entry/dbg-object entry *compiler-info/load-on-demand?*)))
  (let ((procedure (compiled-entry/dbg-object entry)))
    (and procedure
	 (let ((name (dbg-procedure/name procedure)))
	   (or (special-form-procedure-name? name)
	       name)))))
(define *compiler-info/load-on-demand?*
  false)


(define (special-form-procedure-name? name)
  (let ((association
	 (list-search-positive special-form-procedure-names
	   (lambda (association)
	     (dbg-name=? (car association) name)))))
    (and association
	 (symbol->string (cdr association)))))
(define special-form-procedure-names)	entry)))