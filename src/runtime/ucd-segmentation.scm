#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Unicode: segmentation
;;; package: (runtime ucd-segmentation)

(declare (usual-integrations))

(add-boot-deps! '(runtime dynamic))

(define (make-evolver codes abbrevs extra-states char->code rules)
  (let-values (((transitions new-states)
		(convert-to-transitions
		 (parse-rules (expand-abbrevs abbrevs rules)
			      codes
			      extra-states))))
    (let ((any-states (append codes extra-states)))
      (let ((sot-index (length any-states))
	    (eot-code (length codes)))
	(let ((new-start (fix:+ sot-index 1)))
	  (let ((diagram
		 (optimize-state-diagram
		  (generate-state-diagram
		   (expand-transitions transitions any-states codes)
		   (append any-states '(sot) new-states)
		   (append codes '(eot))))))
	    (let ((all-states (list->vector (map car diagram)))
		  (all-codes (list->vector (map car (cdar diagram)))))
	      (%make-evolver
	       all-codes all-states char->code diagram
	       (make-interpreter diagram char->code
				 sot-index eot-code new-start
				 all-codes all-states)))))))))

(define-record-type <evolver>
    (%make-evolver codes states char->code diagram interpreter)
    evolver?
  (codes evolver-codes)
  (states evolver-states)
  (char->code evolver-char->code)
  (diagram evolver-diagram)
  (interpreter evolver-interpreter))

(define (expand-abbrevs abbrevs tree)
  (let loop ((tree tree))
    (if (list? tree)
	(if (or-op? tree)
	    (apply make-or-op (map loop (or-op-elts tree)))
	    (map loop tree))
	(let ((p (assq tree abbrevs)))
	  (if p
	      (loop (cadr p))
	      tree)))))

(define (or-op? item)
  (and (pair? item)
       (eq? (car item) 'or)
       (list? (cdr item))))

(define (make-or-op . items)
  (cons 'or (append-map item-names items)))

(define (item-names item)
  (if (or-op? item)
      (or-op-elts item)
      (list item)))

(define-integrable or-op-elts cdr)

;;; <rule> = <state> <step>* <break> <code> <state-name>?
;;; <state> = <state-name> | sot | any | (or <state-name>+)
;;; <state-name> = <code-name> | <extra-state-name>
;;; <code> = <code-name> | eot | any | (or <code-name>+)
;;; <step> = <break> <step-names> | (* <break> <step-names>)
;;; <break> = _ | /
;;; <step-names> = <code-name> | (or <code-name>+)

(define (parse-rules rules codes extra-states)

  (define (parse-rule rule)
    (if (not (and (pair? rule) (state? (car rule))))
	(error "Unable to parse start:" rule))
    (let loop ((items (cdr rule)) (n (length (cdr rule))) (steps '()))
      (cond ((and (= n 2)
		  (break? (car items))
		  (code? (cadr items)))
	     (list (car rule)
		   (reverse steps)
		   (list (car items) (cadr items) #f)))
	    ((and (= n 3)
		  (break? (car items))
		  (code? (cadr items))
		  (state-name? (caddr items)))
	     (list (car rule)
		   (reverse steps)
		   items))
	    ((and (> n 2)
		  (break? (car items))
		  (step-names? (cadr items)))
	     (loop (cddr items)
		   (- n 2)
		   (cons (list 'once (car items) (cadr items))
			 steps)))
	    ((and (> n 1)
		  (let ((item (car items)))
		    (and (list? item)
			 (= (length item) 3)
			 (eq? (car item) '*)
			 (break? (cadr item))
			 (step-names? (caddr item)))))
	     (loop (cdr items)
		   (- n 1)
		   (cons (car items) steps)))
	    (else
	     (error "Unable to parse tail:" items)
	     unspecific))))

  (define (state? item)
    (or (eq? item 'sot)
	(eq? item 'any)
	(state-name? item)
	(and (or-op? item)
	     (every state-name? (or-op-elts item)))))

  (define (state-name? item)
    (or (code-name? item)
	(memq item extra-states)))

  (define (code? item)
    (or (eq? item 'eot)
	(eq? item 'any)
	(step-names? item)))

  (define (code-name? item)
    (memq item codes))

  (define (step-names? item)
    (or (code-name? item)
	(and (or-op? item)
	     (every code-name? (or-op-elts item)))))

  (define (break? item)
    (or (eq? '_ item) (eq? '/ item)))

  (map parse-rule rules))

(define (convert-to-transitions parsed-rules)
  (let ((prev-name 0)
	(new-states '()))

    (define (convert-rule parsed-rule tail)
      (do-steps (car parsed-rule)
		(cadr parsed-rule)
		(caddr parsed-rule)
		tail))

    (define (do-steps from steps end tail)
      (if (pair? steps)
	  (fold-right (lambda (from tail)
			(do-step from
				 (car steps)
				 tail
				 (lambda (to tail)
				   (do-steps to (cdr steps) end tail))))
		      tail
		      (item-names from))
	  (let ((break? (eq? (car end) '/))
		(code (cadr end))
		(to (caddr end)))
	    (cons (make-transition from code break? to)
		  tail))))

    (define (do-step from step tail k)
      (let ((break? (eq? (cadr step) '/))
	    (codes (caddr step)))
	(if (eq? '* (car step))
	    (let ((to (new-state from codes break?)))
	      (cons* (make-transition from codes break? to)
		     (make-transition to codes break? to)
		     (k (make-or-op from to) tail)))
	    (fold-right (lambda (code tail)
			  (let ((to (new-state from code break?)))
			    (cons (make-transition from code break? to)
				  (k to tail))))
			tail
			(item-names (caddr step))))))

    (define (new-state state code break?)
      (let ((key (list state code break?)))
	(let ((p (assoc key new-states)))
	  (if p
	      (cdr p)
	      (let ((name prev-name))
		(set! prev-name (fix:+ prev-name 1))
		(set! new-states (cons (cons key name) new-states))
		name)))))

    (let ((transitions (fold-right convert-rule '() parsed-rules)))
      (values transitions (reverse (map cdr new-states))))))

(define-record-type <transition>
    (make-transition from code break? to)
    transition?
  (from transition-from)
  (code transition-code)
  (break? transition-break?)
  (to transition-to))

(define (transition->list transition)
  (list (transition-from transition)
	(transition-code transition)
	(transition-break? transition)
	(transition-to transition)))

(define (expand-transitions transitions states codes)

  (define (expand-one transition)
    (let ((break? (transition-break? transition))
	  (to (transition-to transition)))
      (append-map (lambda (from)
		    (map (lambda (code)
			   (make-transition from code break? (or to code)))
			 (rewrite-names (transition-code transition) codes)))
		  (rewrite-names (transition-from transition) states))))

  (define (rewrite-names names all-names)
    (cond ((eq? names 'any) all-names)
	  ((or (symbol? names) (index-fixnum? names)) (list names))
	  ((or-op? names) (or-op-elts names))
	  (else (error "Unrecognized name:" names))))

  (append-map expand-one transitions))

(define (generate-state-diagram transitions states codes)

  ;; Organize the transitions so that potentials are first, and at most one
  ;; definite is last.
  (define (match-transitions from code)
    (let-values
	(((potential definite)
	  (partition (lambda (transition)
		       (index-fixnum? (transition-to transition)))
		     (filter (lambda (transition)
			       (and (eq? from (transition-from transition))
				    (eq? code (transition-code transition))))
			     transitions))))
      (append potential
	      (if (pair? definite)
		  (take definite 1)
		  '()))))

  (define (convert transitions)
    (delete-duplicates
     (map (lambda (transition)
	    (cons (transition-break? transition)
		  (transition-to transition)))
	  transitions)))

  (map (lambda (state)
	 (cons state
	       (map (lambda (code)
		      (cons code (convert (match-transitions state code))))
		    codes)))
       states))

(define (optimize-state-diagram diagram)

  (define (find-equivalents candidates fixed)
    (let loop ((candidates candidates) (collapsible '()) (unique '()))
      (cond ((pair? candidates)
	     (let ((s1 (car candidates)))
	       (let-values (((equivalents non-equivalents)
			     (partition (lambda (s2) (equivalent-states? s1 s2))
					(cdr candidates))))
		 (if (pair? equivalents)
		     (loop non-equivalents
			   (cons (cons s1 equivalents) collapsible)
			   unique)
		     (loop non-equivalents
			   collapsible
			   (cons s1 unique))))))
	    ((pair? collapsible)
	     (collapse! collapsible unique fixed))
	    (else
	     (append fixed (sort-states unique))))))

  (define (collapse! collapsible unique fixed)
    (let ((collapsible (map sort-states collapsible)))
      (let ((dict (generate-dictionary collapsible)))
	(let ((candidates
	       (sort-states (append (map car collapsible) unique))))
	  (find-equivalents (rewrite-states candidates dict)
			    (rewrite-states fixed dict))))))

  (call-with-values (lambda ()
		      (partition (lambda (state) (index-fixnum? (car state)))
				 diagram))
    find-equivalents))

(define (equivalent-states? s1 s2)
  (let ((n1 (car s1))
	(n2 (car s2)))
    (let loop ((cs1 (cdr s1)) (cs2 (cdr s2)))
      (if (pair? cs1)
	  (and (pair? cs2)
	       (loop (car cs1) (car cs2))
	       (loop (cdr cs1) (cdr cs2)))
	  (and (not (pair? cs2))
	       (or (eq? cs1 cs2)
		   (and (eq? cs1 n1) (eq? cs2 n2))))))))

(define (sort-states states)
  (sort states (lambda (a b) (fix:< (car a) (car b)))))

(define (generate-dictionary collapsible)
  (let ((dict
	 (append-map (lambda (equivalents)
		       (let ((names (map car equivalents)))
			 (let ((to (car names)))
			   (map (lambda (from) (cons from to))
				(cdr names)))))
		     collapsible)))
    (lambda (name)
      (let ((p (assq name dict)))
	(if p
	    (cdr p)
	    name)))))

(define (rewrite-states states dict)
  (map (lambda (state)
	 (cons (dict (car state))
	       (map (lambda (c)
		      (cons (car c)
			    (delete-duplicates
			     (map (lambda (link)
				    (cons (car link)
					  (dict (cdr link))))
				  (cdr c)))))
		    (cdr state))))
       states))

(define (make-interpreter diagram char->code sot-index eot-code new-start
			  all-codes all-states)
  (let ((sv (create-state-vector diagram)))

    (define (evolve-states states code i)
      (let ((states* (evolve-states-1 states code i)))
	(if (trace-interpreter?)
	    (with-notification
		(lambda (port)
		  (write (map convert-state states) port)
		  (write-char #\space port)
		  (write (vector-ref all-codes code) port)
		  (write-char #\space port)
		  (write i port)
		  (write-string " -> " port)
		  (write (map convert-state states*) port))))
	states*))

    (define (evolve-states-1 states code i)

      (define (continue states)
	(if (pair? states)
	    (let ((state (car states)))
	      (evolve-state (vector-ref (vector-ref sv (state-index state))
					code)
			    (state-breaks state)
			    (cdr states)))
	    '()))

      (define (evolve-state links breaks rest)
	(let loop ((links links))
	  (if (pair? links)
	      (let ((link (car links)))
		(let ((break? (car link))
		      (to (cdr link)))
		  (cons (make-state to (if break? (cons i breaks) breaks))
			(if (fix:<= new-start to)
			    (loop (cdr links))
			    '()))))
	      (continue rest))))

      (continue states))

    (define (convert-state state)
      (make-state (let ((index (state-index state)))
		    (if (fix:= index -1)
			'eot
			(vector-ref all-states index)))
		  (state-breaks state)))

    (define-integrable make-state cons)
    (define-integrable state-index car)
    (define-integrable state-breaks cdr)

    (lambda (string #!optional start end)
      (let* ((end (fix:end-index end (string-length string)))
	     (start (fix:start-index start end)))
	(let loop ((i start) (states (list (make-state sot-index '()))))
	  (if (fix:< i end)
	      (loop (fix:+ i 1)
		    (evolve-states states (char->code (string-ref string i)) i))
	      (let ((states (evolve-states states eot-code i)))
		(if (not (and (pair? states) (null? (cdr states))))
		    (error "Interpretation didn't converge:" states))
		(reverse (state-breaks (car states))))))))))

(define (create-state-vector state-diagram)

  (define (state-name->index name)
    (if (eq? name 'eot)
	-1
	(list-index (lambda (entry)
		      (eq? (car entry) name))
		    state-diagram)))

  (list->vector
   (map (lambda (state-entry)
	  (list->vector
	   (map (lambda (code-entry)
		  (map (lambda (link)
			 (cons (car link) (state-name->index (cdr link))))
		       (cdr code-entry)))
		(cdr state-entry))))
	state-diagram)))

;;; Debugging support:

(define (evolver-string->code-names evolver)
  (let ((char->code-name (evolver-char->code-name evolver)))
    (lambda (string)
      (map char->code-name (string->list string)))))

(define (evolver-char->code-name evolver)
  (let ((codes (evolver-codes evolver))
	(char->code (evolver-char->code evolver)))
    (lambda (char)
      (vector-ref codes (char->code char)))))

(define (evolver-show-transitions evolver)
  (let ((interpreter (evolver-interpreter evolver)))
    (lambda (string #!optional start end)
      (parameterize ((trace-interpreter? #t))
	(interpreter string start end)))))

(define-deferred trace-interpreter?
  (make-settable-parameter #f))