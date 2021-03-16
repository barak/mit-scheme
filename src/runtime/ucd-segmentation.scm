#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

(define (folder evolver caller)
  (let ((interpreter (evolver-interpreter evolver)))
    (lambda (kons knil string #!optional start end)
      (let ((end (fix:end-index end (string-length string) caller)))
	(fold kons
	      knil
	      (interpreter string
			   (fix:start-index start end caller)
			   end))))))

(define (right-folder evolver caller)
  (let ((interpreter (evolver-interpreter evolver)))
    (lambda (kons knil string #!optional start end)
      (let ((end (fix:end-index end (string-length string) caller)))
	(fold-right kons
		    knil
		    (interpreter string
				 (fix:start-index start end caller)
				 end))))))

(define (streamer evolver caller)
  (let ((interpreter (evolver-interpreter evolver)))
    (lambda (string #!optional start end)
      (let ((end (fix:end-index end (string-length string) caller)))
	(list->stream (interpreter string
				   (fix:start-index start end caller)
				   end))))))

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
    (lambda (string)
      (parameterize ((trace-interpreter? #t))
	(interpreter string 0 (string-length string))))))

(define (make-evolver codes abbrevs extra-states char->code rules)
  (let-values (((transitions new-states)
		(convert-to-transitions
		 (parse-rules (expand-abbrevs abbrevs rules)
			      codes
			      extra-states))))
    (let ((any-states (append codes extra-states)))
      (let ((all-states (append any-states '(sot) new-states))
	    (all-codes (append codes '(eot))))
	(let ((diagram
	       (generate-state-diagram
		(expand-transitions transitions any-states codes)
		all-states
		all-codes)))
	  (let ((all-states (list->vector all-states))
		(all-codes (list->vector all-codes)))
	    (%make-evolver
	     all-codes all-states char->code diagram
	     (let ((sot-index (length any-states))
		   (eot-code (length codes)))
	       (let ((new-start (fix:+ sot-index 1)))
		 (make-interpreter diagram char->code
				   sot-index eot-code new-start
				   all-codes all-states))))))))))

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
  (cons 'or
	(append-map (lambda (item)
		      (if (or-op? item)
			  (or-op-elts item)
			  (list item)))
		    items)))

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
  (let ((prev-name 0))

    (define (convert-rule parsed-rule transitions)
      (let loop
	  ((state (car parsed-rule))
	   (steps (cadr parsed-rule)))
	(if (pair? steps)
	    (let ((step (car steps)))
	      (let ((break? (eq? (cadr step) '/))
		    (state-names (caddr step))
		    (state1 (new-name)))
		(if (eq? '* (car step))
		    (cons* (make-transition state state-names break? state1)
			   (make-transition state1 state-names break? state1)
			   (loop (make-or-op state state1) (cdr steps)))
		    (cons (make-transition state state-names break? state1)
			  (loop state1 (cdr steps))))))
	    (let ((end (caddr parsed-rule)))
	      (let ((break? (eq? (car end) '/))
		    (code (cadr end))
		    (state1 (caddr end)))
		(cons (make-transition state code break? state1)
		      transitions))))))

    (define (new-name)
      (set! prev-name (fix:+ prev-name 1))
      prev-name)

    (define (get-names)
      (iota prev-name 1))

    (let ((transitions (fold-right convert-rule '() parsed-rules)))
      (values transitions (get-names)))))

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
    (map (lambda (transition)
	   (cons (transition-break? transition)
		 (transition-to transition)))
	 transitions))

  (map (lambda (state)
	 (cons state
	       (map (lambda (code)
		      (cons code (convert (match-transitions state code))))
		    codes)))
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

    (lambda (string start end)
      (let loop ((i start) (states (list (make-state sot-index '()))))
	(if (fix:< i end)
	    (loop (fix:+ i 1)
		  (evolve-states states (char->code (string-ref string i)) i))
	    (let ((states (evolve-states states eot-code i)))
	      (if (not (and (pair? states) (null? (cdr states))))
		  (error "Interpretation didn't converge:" states))
	      (reverse (state-breaks (car states)))))))))

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

(define-deferred trace-interpreter?
  (make-settable-parameter #f))