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

;;;; Unicode: segmentation
;;; package: (runtime ucd-segmentation)

(declare (usual-integrations))

(define (folder evolver caller)
  (let ((char->code (evolver-char->code evolver))
	(initialize (evolver-initialize evolver))
	(evolve (evolver-evolve evolver))
	(finalize (evolver-finalize evolver)))
    (lambda (kons knil string #!optional start end)
      (let* ((end (fix:end-index end (string-length string) caller))
	     (start (fix:start-index start end caller)))

	(define (loop state i ctx prev-break acc)
	  (if (fix:< i end)
	      (evolve state (get-code i) i ctx prev-break
		(lambda (state i+ ctx break break1 break2)
		  (loop state i+ ctx break
			(fold-breaks break break1 break2 acc))))
	      (finalize state i ctx prev-break
		(lambda (state i+ ctx break break1 break2)
		  (declare (ignore state i+ ctx))
		  (fold-breaks break break1 break2 acc)))))

	(define-integrable (get-code i)
	  (char->code (string-ref string i)))

	(define-integrable (fold-breaks break break1 break2 acc)
	  (if break1
	      (if break2
		  (kons break break1 (kons break1 break2 acc))
		  (kons break break1 acc))
	      acc))

	(if (fix:< start end)
	    (initialize (get-code start) start
	      (lambda (state i+ ctx break break1 break2)
		(declare (ignore break1 break2))
		(loop state i+ ctx break (if break (kons break #f knil) knil))))
	    knil)))))

(define (right-folder evolver caller)
  (let ((char->code (evolver-char->code evolver))
	(initialize (evolver-initialize evolver))
	(evolve (evolver-evolve evolver))
	(finalize (evolver-finalize evolver)))
    (lambda (kons knil string #!optional start end)
      (let* ((end (fix:end-index end (string-length string) caller))
	     (start (fix:start-index start end caller)))

	(define (loop state i ctx prev-break)
	  (if (fix:< i end)
	      (evolve state (get-code i) i ctx prev-break
		(lambda (state i+ ctx break break1 break2)
		  (fold-breaks break break1 break2 (loop state i+ ctx break))))
	      (finalize state i ctx prev-break
		(lambda (state i+ ctx break break1 break2)
		  (declare (ignore state i+ ctx))
		  (fold-breaks break break1 break2 knil)))))

	(define-integrable (get-code i)
	  (char->code (string-ref string i)))

	(define-integrable (fold-breaks break break1 break2 acc)
	  (if break1
	      (if break2
		  (kons break1 break2 (kons break break1 acc))
		  (kons break break1 acc))
	      acc))

	(if (fix:< start end)
	    (initialize (get-code start) start
	      (lambda (state i+ ctx break break1 break2)
		(declare (ignore break1 break2))
		(if break
		    (kons break #f (loop state i+ ctx break))
		    (loop state i+ break ctx))))
	    knil)))))

(define (make-evolver codes extra-states char->code transitions)
  (let* ((codes (append codes '(eot)))
	 (states (append codes '(sot) extra-states))
	 (save-states
	  (filter-map (lambda (state)
			(and (save-state? state)
			     (save-state-name state)))
		      states))
	 (states
	  (map (lambda (state)
		 (if (save-state? state)
		     (save-state-name state)
		     state))
	       states))
	 (transitions (expand-transitions states codes transitions))
	 (diagram (generate-state-diagram states codes transitions save-states))
	 (v (generate-state-vector states diagram)))
    (%make-evolver codes states char->code transitions save-states diagram
      (let ((state (name->index 'sot states)))
	(lambda (code i k)
	  ((vector-ref (vector-ref v state) code) i #f #f k)))
      (lambda (state code i prev-break ctx k)
	((vector-ref (vector-ref v state) code) i prev-break ctx k))
      (let ((code (name->index 'eot codes)))
	(lambda (state i prev-break ctx k)
	  ((vector-ref (vector-ref v state) code) i prev-break ctx k))))))

(define-record-type <evolver>
    (%make-evolver codes states char->code transitions save-states diagram
		   initialize evolve finalize)
    evolver?
  (codes evolver-codes)
  (states evolver-states)
  (char->code evolver-char->code)
  (transitions evolver-transitions)
  (save-states evolver-save-states)
  (diagram evolver-diagram)
  (initialize evolver-initialize)
  (evolve evolver-evolve)
  (finalize evolver-finalize))

;;; Debugging support:

(define (evolver-string->code-names evolver)
  (let ((char->code-name (evolver-char->code-name evolver)))
    (lambda (string)
      (map char->code-name (string->list string)))))

(define (evolver-char->code-name evolver)
  (let ((codes (evolver-codes evolver))
	(char->code (evolver-char->code evolver)))
    (lambda (char)
      (list-ref codes (char->code char)))))

(define (evolver-show-transitions evolver)
  (let ((string->code-names (evolver-string->code-names evolver))
	(diagram (evolver-diagram evolver)))

    (define (lookup state code)
      (assq code (cdr (assq state diagram))))

    (lambda (string)
      (let loop ((state 'sot) (codes (string->code-names string)))
	(if (pair? codes)
	    (let ((t (lookup state (car codes))))
	      (write-line (cons state t))
	      (loop (cadr t) (cdr codes)))
	    (write-line (cons state (lookup state 'eot))))))))

(define (generate-state-vector states diagram)
  (list->vector
   (map (lambda (entry)
	  (list->vector
	   (map (lambda (entry)
		  (generate-state (name->index (cadr entry) states)
				  (caddr entry)
				  (cadddr entry)))
		(cdr entry))))
	diagram)))

(define (generate-state to action break?)
  (case action
    ((no-ctx preserve)
     (if break?
	 (lambda (i ctx prev-break k)
	   (k to (fix:+ i 1) ctx i prev-break #f))
	 (lambda (i ctx prev-break k)
	   (k to (fix:+ i 1) ctx prev-break #f #f))))
    ((save)
     (lambda (i ctx prev-break k)
       (declare (ignore ctx))
       (k to (fix:+ i 1) (cons i break?) prev-break #f #f)))
    ((restore)
     (if break?
	 (lambda (i ctx prev-break k)
	   (if (cdr ctx)
	       (k to (fix:+ i 1) #f i (car ctx) prev-break)
	       (k to (fix:+ i 1) #f i prev-break #f)))
	 (lambda (i ctx prev-break k)
	   (if (cdr ctx)
	       (k to (fix:+ i 1) #f (car ctx) prev-break #f)
	       (k to (fix:+ i 1) #f prev-break #f #f)))))
    ((restore-break)
     (if break?
	 (lambda (i ctx prev-break k)
	   (k to (fix:+ i 1) #f i (car ctx) prev-break))
	 (lambda (i ctx prev-break k)
	   (k to (fix:+ i 1) #f (car ctx) prev-break #f))))
    ((restore-no-break)
     (if break?
	 (lambda (i ctx prev-break k)
	   (declare (ignore ctx))
	   (k to (fix:+ i 1) #f i prev-break #f))
	 (lambda (i ctx prev-break k)
	   (declare (ignore ctx))
	   (k to (fix:+ i 1) #f prev-break #f #f))))
    (else
     (error "Unrecognized action:" action))))

(define (generate-state-diagram states codes transitions save-states)

  (define (code-one from code)
    (let ((matches (match-transitions from code)))
      (let ((best (car matches))
	    (fallbacks (cdr matches)))
	(let ((to
	       (let ((to (transition-to best)))
		 (cond ((to-is-from? to) from)
		       ((to-is-code? to) code)
		       (else to)))))
	  (cons to
		(encode-break (memq from save-states)
			      (memq to save-states)
			      (transition-break best)
			      (find constant-break?
				    (map transition-break fallbacks))))))))

  (define (match-transitions from code)
    (cond ((eq? 'sot from)
	   (append (filter (lambda (transition)
			     (match-transition from code transition))
			   transitions)
		   (filter (lambda (transition)
			     (match-transition from 'any transition))
			   transitions)))
	  ((eq? 'eot code)
	   (append (filter (lambda (transition)
			     (match-transition from code transition))
			   transitions)
		   (filter (lambda (transition)
			     (match-transition 'any code transition))
			   transitions)))
	  (else
	   (append (filter (lambda (transition)
			     (match-transition from code transition))
			   transitions)
		   (filter (lambda (transition)
			     (or (match-transition from 'any transition)
				 (match-transition 'any code transition)))
			   transitions)
		   (filter (lambda (transition)
			     (match-transition 'any 'any transition))
			   transitions)))))

  (define (match-transition from code transition)
    (and (eq? from (transition-from transition))
	 (eq? code (transition-code transition))))

  (map (lambda (state)
	 (cons state
	       (map (lambda (code)
		      (cons code (code-one state code)))
		    codes)))
       states))

(define (expand-transitions states codes transitions)
  (append-map (lambda (transition)
		(apply expand-transition states codes transition))
	      transitions))

(define (expand-transition states codes from break code #!optional to)
  (let ((to (if (default-object? to) '(code) to)))

    (define (make-transition from break code to)
      (if (not (or (to-is-from? to) (to-is-code? to)))
	  (check-name to states))
      (list from break code to))

    (append-map (lambda (from)
		  (check-name from states)
		  (map (lambda (code)
			 (check-name code codes)
			 (make-transition from break code to))
		       (if (list? code) code (list code))))
		(if (list? from) from (list from)))))

(define-integrable (to-is-from? to)
  (equal? '(from) to))

(define-integrable (to-is-code? to)
  (equal? '(code) to))

(define (transition-from t) (list-ref t 0))
(define (transition-break t) (list-ref t 1))
(define (transition-code t) (list-ref t 2))
(define (transition-to t) (list-ref t 3))

(define (encode-break from-save-state? to-save-state? break inherited)

  (define (encode-simple break)
    (if (inherited-break? break)
	(encode-inherited)
	(encode-constant break)))

  (define-integrable (encode-constant break)
    (eq? '/ break))

  (define (encode-inherited)
    (if (not inherited) (error "Missing inherited break"))
    (encode-constant inherited))

  (cond ((simple-break? break)
	 (list (if from-save-state?
		   (if to-save-state? 'preserve 'restore)
		   (if to-save-state? 'save 'no-ctx))
	       (encode-simple break)))
	((and from-save-state? (compound-break? break))
	 (list (let ((prefix (car break)))
		 (cond (to-save-state? 'preserve)
		       ((inherited-break? prefix) 'restore)
		       ((encode-constant prefix) 'restore-break)
		       (else 'restore-no-break)))
	       (encode-simple (cadr break))))
	(else
	 (error "Unrecognized break:" break))))

(define-integrable (constant-break? break)
  (or (eq? '_ break)
      (eq? '/ break)))

(define-integrable (inherited-break? break)
  (eq? '? break))

(define-integrable (simple-break? break)
  (or (constant-break? break)
      (inherited-break? break)))

(define (compound-break? break)
  (and (pair? break)
       (simple-break? (car break))
       (pair? (cdr break))
       (simple-break? (cadr break))
       (null? (cddr break))))

(define (save-state? object)
  (and (pair? object)
       (eq? 'save (car object))
       (pair? (cdr object))
       (interned-symbol? (cadr object))
       (null? (cddr object))))

(define (save-state-name state)
  (cadr state))

(define (name->index name names)
  (let ((index
	 (list-index (lambda (name*)
		       (eq? name name*))
		     names)))
    (if (not index)
	(error:bad-range-argument name))
    index))

(define (check-name name names)
  (if (not (or (eq? 'any name) (memq name names)))
      (error:bad-range-argument name)))