#| -*-Scheme-*-

$Id: infnew.scm,v 1.5 1995/07/14 01:00:34 adams Exp $

Copyright (c) 1988-1994 Massachusetts Institute of Technology

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

;;;; Debugging Information
;;; package: (compiler debugging-information)

(declare (usual-integrations))

(define (info-generation-phase-2 expression procedures continuations)
  ;; (values expression prcoedures continuations)
  (define (debug-info selector object)
    (or (selector object)
	(begin
	  (if compiler:guru?
	      (warn "Missing debugging info" object))
	  false)))
  (values
   (and expression (debug-info rtl-expr/debugging-info expression))
   (list-transform-negative
       (map (lambda (procedure)
	      (let ((info (debug-info rtl-procedure/debugging-info procedure)))
		(and info
		     ;;(set-dbg-procedure/external-label!
		     ;; info
		     ;; (rtl-procedure/%external-label procedure))
		     info)))
	    procedures)
     false?)
   (list-transform-negative
       (map (lambda (continuation)
	      (rtl-continuation/debugging-info continuation))
	    continuations)
     false?)))

(define (info-generation-phase-3 expression procedures continuations
				 label-bindings external-labels
				 constant-offset-map)
  (let ((label-bindings (labels->dbg-labels label-bindings))
	(no-datum '(NO-DATUM))
	(labels (make-string-hash-table))
	(blocks-seen (make-eq-hash-table)))
    (define (initialize-label label-binding)
      (for-each (lambda (key)
		  (let ((datum (hash-table/get labels key no-datum)))
		    (if (not (eq? datum no-datum))
			(error "Redefining label:" key datum)))
		  (hash-table/put! labels key (cdr label-binding)))
	(car label-binding)))
    (define (map-label/fail label)
      (let ((key (system-pair-car label)))
	(let ((datum (hash-table/get labels key no-datum)))
	  (if (eq? datum no-datum)
	      (error "Missing label:" key))
	  datum)))
    (define (map-label/false label)
      (hash-table/get labels (system-pair-car label) #f))

    (define (map-block block)
      ;; Rewrite path elements that are defined in terms of the compiled code
      ;; block - i.e. things which are labels or constants in the
      ;; constants block.
      (if (new-dbg-block? block)
	  (begin
	    (map-block (new-dbg-block/parent block))
	    (for-each-vector-element  (new-dbg-block/variables block)
	      (lambda (var)
		(define (map-path! path)
		  (cond  ((not (pair? path)))
			 ((and (eq? (caar path) 'CC-ENTRY)
			       (symbol? (cdar path)))
			  (let ((label  (map-label/fail (cdr (car path)))))
			    (if (dbg-label/external? label)
				(set-cdr! (car path) (dbg-label/offset label))
				(set-new-dbg-variable/path! var #F)))
			  (map-path! (cdr path)))
			 ((and (eq? (caar path) 'INTEGRATED)
			       (not (interned-symbol? (cdar path)))
			       (constant-offset-map (cdar path)))
			  => (lambda (offset)
			       (set-car! path (cons 'CONSTANT-BLOCK offset))
			       (map-path! (cdr path))))
			 (else (map-path! (cdr path)))))
		(if (new-dbg-variable? var)
		    (map-path! (new-dbg-variable/path var))))))))
    (for-each initialize-label label-bindings)
    (for-each (lambda (label)
		(set-dbg-label/external?! (map-label/fail label) true))
      external-labels)
    (if expression
	(set-dbg-expression/label!
	 expression
	 (map-label/fail (dbg-expression/label expression))))
    (for-each
	(lambda (procedure)
	  (let* ((internal-label (new-dbg-procedure/label procedure))
		 (mapped-label   (map-label/false internal-label)))
	    (set-new-dbg-procedure/label! procedure mapped-label)
	    (cond ;;((dbg-procedure/external-label procedure)
	          ;; => (lambda (label)
	          ;;	 (set-dbg-procedure/external-label!
	          ;;	  procedure
	          ;;	  (map-label/fail label))))
	          ((not mapped-label)
	           (error "Missing label" internal-label)))
	    (map-block (new-dbg-procedure/block procedure))))
      procedures)
    (for-each
	(lambda (continuation)
	  (set-dbg-continuation/label!
	   continuation
	   (map-label/fail (dbg-continuation/label continuation)))
	  (map-block (dbg-continuation/block continuation)))
      continuations)
    (make-dbg-info
     expression
     (list->vector (sort procedures new-dbg-procedure<?))
     (list->vector (sort continuations dbg-continuation<?))
     (list->vector (map cdr label-bindings)))))

(define (labels->dbg-labels label-bindings)
  (map (lambda (offset-binding)
	 (let ((names (cdr offset-binding)))
	   (cons names
		 (make-dbg-label-2 (choose-distinguished-label names)
				   (car offset-binding)))))
       (let ((offsets (make-rb-tree = <)))
	 (for-each (lambda (binding)
		     (let ((offset (cdr binding))
			   (name (system-pair-car (car binding))))
		       (let ((datum (rb-tree/lookup offsets offset #f)))
			 (if datum
			     (set-cdr! datum (cons name (cdr datum)))
			     (rb-tree/insert! offsets offset (list name))))))
		   label-bindings)
	 (rb-tree->alist offsets))))

(define (choose-distinguished-label names)
  (if (null? (cdr names))
      (car names)
      (let ((distinguished
	     (list-transform-negative names
	       (lambda (name)
		 (or (standard-name? name "label")
		     (standard-name? name "end-label"))))))
	(cond ((null? distinguished)
	       (min-suffix names))
	      ((null? (cdr distinguished))
	       (car distinguished))
	      (else
	       (min-suffix distinguished))))))

(define char-set:label-separators
  (char-set #\- #\_))

(define (min-suffix names)
  (let ((suffix-number
	 (lambda (name)
	   (let ((index (string-find-previous-char-in-set
			 name
			 char-set:label-separators)))
	     (if (not index)
		 (error "Illegal label name" name))
	     (let ((suffix (string-tail name (1+ index))))
	       (let ((result (string->number suffix)))
		 (if (not result)
		     (error "Illegal label suffix" suffix))
		 result))))))
    (car (sort names (lambda (x y)
		       (< (suffix-number x)
			  (suffix-number y)))))))

(define (standard-name? string prefix)
  (let ((index (string-match-forward-ci string prefix))
	(end (string-length string)))
    (and (= index (string-length prefix))
	 (>= (- end index) 2)
	 (let ((next (string-ref string index)))
	   (or (char=? #\- next)
	       (char=? #\_ next)))
	 (let loop ((index (1+ index)))
	   (or (= index end)
	       (and (char-numeric? (string-ref string index))
		    (loop (1+ index))))))))