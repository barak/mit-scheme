;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/comtab.scm,v 1.62 1992/01/14 18:34:34 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Command Tables

(declare (usual-integrations))

(define-structure (comtab (constructor make-comtab ()))
  (vector 0)
  (alist '()))

(define (comtab-get comtab key)
  (let ((vector (comtab-vector comtab)))
    (if (and (vector? vector)
	     (char? key)
	     (< (char->integer key) (vector-length vector)))
	(vector-ref vector (char->integer key))
	(let ((entry (assq key (comtab-alist comtab))))
	  (and entry
	       (cdr entry))))))

(define (comtab-put! comtab key datum)
  (cond ((not datum)
	 (comtab-remove! comtab key))
	((and (char? key) (< (char->integer key) 256))
	 (let ((vector (comtab-vector comtab)))
	   (if (vector? vector)
	       (vector-set! vector (char->integer key) datum)
	       (let ((alist (comtab-alist comtab)))
		 (let ((entry (assq key alist)))
		   (if entry
		       (set-cdr! entry datum)
		       (let ((vector (+ vector 1))
			     (alist (cons (cons key datum) alist)))
			 (if (< vector 64)
			     (without-interrupts
			      (lambda ()
				(set-comtab-vector! comtab vector)
				(set-comtab-alist! comtab alist)))
			     (let ((vector (make-vector 256 false)))
			       (let ((alist
				      (list-transform-negative alist
					(lambda (entry)
					  (let ((key (car entry)))
					    (and (char? key)
						 (< (char->integer key) 256)
						 (begin
						   (vector-set!
						    vector
						    (char->integer key)
						    (cdr entry))
						   true)))))))
				 (without-interrupts
				  (lambda ()
				    (set-comtab-vector! comtab vector)
				    (set-comtab-alist! comtab alist))))))))))))
	 ;; Defining a lower-case character defines the corresponding
	 ;; upper-case character to be an alias if not already defined.
	 (if (char-lower-case? key)
	     (let ((key* (char-upcase key)))
	       (if (not (comtab-get comtab key*))
		   (comtab-put! comtab key* (cons comtab key))))))
	(else
	 (let ((alist (comtab-alist comtab)))
	   (let ((entry (assq key alist)))
	     (if entry
		 (set-cdr! entry datum)
		 (set-comtab-alist! comtab
				    (cons (cons key datum) alist))))))))

(define (comtab-remove! comtab key)
  (if (and (char? key) (< (char->integer key) 256))
      (let ((vector (comtab-vector comtab)))
	(if (vector? vector)
	    (vector-set! vector (char->integer key) false)
	    (let ((alist (comtab-alist comtab)))
	      (let ((entry (assq key alist)))
		(if entry
		    (let ((vector (- vector 1))
			  (alist (delq entry alist)))
		      (without-interrupts
		       (lambda ()
			 (set-comtab-vector! comtab vector)
			 (set-comtab-alist! comtab alist)))))))))
      (set-comtab-alist! comtab (del-assq key (comtab-alist comtab)))))

(define (valid-comtabs? object)
  (or (mode? object)
      (symbol? object)
      (comtab? object)
      (list-of-comtabs? object)))

(define (guarantee-comtabs object procedure)
  (cond ((mode? object)
	 (mode-comtabs object))
	((symbol? object)
	 (mode-comtabs (->mode object)))
	((comtab? object)
	 (list object))
	((list-of-comtabs? object)
	 object)
	(else
	 (error:wrong-type-argument object "list of comtabs" procedure))))

(define (mode-name? object)
  (and (symbol? object)
       (string-table-get editor-modes (symbol->string object))))

(define (list-of-comtabs? object)
  (and (not (null? object))
       (list? object)
       (for-all? object comtab?)))

(define (valid-key? object)
  (or (key? object)
      (prefixed-key? object)
      (button? object)))

(define (prefixed-key? object)
  (let loop ((object object))
    (and (pair? object)
	 (key? (car object))
	 (or (null? (cdr object))
	     (loop (cdr object))))))

(define (valid-datum? object)
  (or (not object)
      (command? object)
      (comtab? object)
      (command&comtab? object)
      (comtab-alias? object)))

(define (command&comtab? object)
  (and (pair? object)
       (command? (car object))
       (comtab? (cdr object))))

(define (comtab-alias? object)
  (and (pair? object)
       (valid-comtabs? (car object))
       (valid-key? (cdr object))))

(define (comtab-alias/dereference datum)
  (lookup-key (car datum) (cdr datum)))

(define (lookup-key comtabs key)
  (let ((comtabs (guarantee-comtabs comtabs 'LOOKUP-KEY)))
    (let ((simple-lookup
	   (lambda (key)
	     (let loop ((comtabs* comtabs))
	       (cond ((comtab-get (car comtabs*) key)
		      => handle-datum)
		     ((not (null? (cdr comtabs*)))
		      (loop (cdr comtabs*)))
		     (else
		      false))))))
      (cond ((key? key)
	     (simple-lookup (remap-alias-key key)))
	    ((button? key)
	     (simple-lookup key))
	    ((prefixed-key? key)
	     (let ((prefix (except-last-pair key))
		   (key (remap-alias-key (car (last-pair key)))))
	       (if (null? prefix)
		   (simple-lookup key)
		   (let loop ((comtabs* comtabs))
		     (let ((comtab
			    (lookup-prefix (car comtabs*) prefix false)))
		       (cond ((and comtab (comtab-get comtab key))
			      => handle-datum)
			     ((not (null? (cdr comtabs*)))
			      (loop (cdr comtabs*)))
			     (else
			      false)))))))
	    (else
	     (error:wrong-type-argument key "comtab key" 'LOOKUP-KEY))))))

(define (handle-datum datum)
  (cond ((or (command? datum)
	     (comtab? datum)
	     (command&comtab? datum))
	 datum)
	((comtab-alias? datum)
	 (comtab-alias/dereference datum))
	(else
	 (error "Illegal comtab datum:" datum))))

(define (lookup-prefix comtab prefix intern?)
  (let loop ((comtab comtab) (prefix* prefix))
    (if (null? prefix*)
	comtab
	(let ((key (remap-alias-key (car prefix*)))
	      (prefix* (cdr prefix*)))
	  (let datum-loop ((datum (comtab-get comtab key)))
	    (cond ((not datum)
		   (and intern?
			(let ((datum (make-comtab)))
			  ;; Note that this will clobber a comtab-alias
			  ;; that points to an undefined entry.
			  (comtab-put! comtab key datum)
			  (loop datum prefix*))))
		  ((comtab? datum)
		   (loop datum prefix*))
		  ((command&comtab? datum)
		   (loop (cdr datum) prefix*))
		  ((comtab-alias? datum)
		   (datum-loop (comtab-alias/dereference datum)))
		  ((command? datum)
		   (error "Key sequence too long:"
			  prefix
			  (- (length prefix) (length prefix*))))
		  (else
		   (error "Illegal comtab datum:" datum))))))))

(define (comtab-entry comtabs key)
  (let ((object (lookup-key comtabs key)))
    (cond ((not object)
	   (and (not (button? key))
		(ref-command-object undefined)))
	  ((command? object)
	   object)
	  ((command&comtab? object)
	   (car object))
	  ((comtab? object)
	   (ref-command-object prefix-key))
	  (else
	   (error "Illegal result from lookup-key:" object)))))

(define (prefix-key-list? comtabs key)
  (let ((object (lookup-key comtabs key)))
    (or (comtab? object)
	(command&comtab? object))))

(define (define-key mode key datum)
  (%define-key (car (guarantee-comtabs mode 'DEFINE-KEY))
	       key
	       (if (valid-datum? datum) datum (->command datum))
	       'DEFINE-KEY))

(define (define-prefix-key mode key #!optional command)
  (%define-key (car (guarantee-comtabs mode 'DEFINE-PREFIX-KEY))
	       (begin
		 (if (button? key)
		     (error:wrong-type-argument key
						"comtab prefix key"
						'DEFINE-PREFIX-KEY))
		 key)
	       (let ((comtab (make-comtab)))
		 (if (default-object? command)
		     comtab
		     (let ((command (->command command)))
		       (if (eq? command (ref-command-object prefix-key))
			   comtab
			   (cons command comtab)))))
	       'DEFINE-PREFIX-KEY))

(define (%define-key comtab key datum procedure)
  (cond ((or (key? key) (button? key))
	 (comtab-put! comtab (remap-alias-key key) datum))
	((char-set? key)
	 (for-each (lambda (key)
		     (comtab-put! comtab (remap-alias-key key) datum))
		   (char-set-members key)))
	((prefixed-key? key)
	 (let ((prefix (except-last-pair key)))
	   (comtab-put! (if (null? prefix)
			    comtab
			    (lookup-prefix comtab prefix true))
			(remap-alias-key (car (last-pair key)))
			datum)))
	(else
	 (error:wrong-type-argument key "comtab key" procedure)))
  key)

(define (comtab-alist* comtab)
  (list-transform-negative
      (let ((vector (comtab-vector comtab))
	    (alist (comtab-alist comtab)))
	(if (vector? vector)
	    (let ((end (vector-length vector)))
	      (let loop ((index 0))
		(if (< index end)
		    (let ((datum (vector-ref vector index)))
		      (if datum
			  (cons (cons (integer->char index) datum)
				(loop (+ index 1)))
			  (loop (+ index 1))))
		    alist)))
	    alist))
    (lambda (entry)
      (let ((key (car entry)))
	(and (char? key)
	     (char-upper-case? key)
	     (let ((datum (cdr entry)))
	       (and (pair? datum)
		    (eq? comtab (car datum))
		    (eqv? (char-downcase key) (cdr datum)))))))))

(define (comtab->alist comtab)
  (let loop ((prefix '()) (comtab comtab))
    (append-map!
     (lambda (entry)
       (if (and (button? (car entry))
		(not (null? prefix)))
	   '()
	   (let ((prefix (append prefix (list (car entry)))))
	     (let ((key (if (null? (cdr prefix)) (car prefix) prefix)))
	       (let datum-loop ((datum (cdr entry)))
		 (cond ((not datum)
			'())
		       ((command? datum)
			(list (cons key datum)))
		       ((comtab? datum)
			(loop prefix datum))
		       ((command&comtab? datum)
			(cons (cons key (car datum))
			      (loop prefix (cdr datum))))
		       ((comtab-alias? datum)
			(datum-loop (comtab-alias/dereference datum)))
		       (else
			(error "Illegal comtab datum:" datum))))))))
     (comtab-alist* comtab))))

(define (comtab-key-bindings comtabs command)
  (let ((comtabs (guarantee-comtabs comtabs 'COMTAB-KEY-BINDINGS))
	(command (->command command)))
    ;; In addition to having a binding of COMMAND, every key in the
    ;; result satisfies VALID-KEY?.  This eliminates bindings that are
    ;; shadowed by other bindings.
    (let ((valid-key?
	   (lambda (key)
	     (let ((datum (lookup-key comtabs key)))
	       (cond ((command? datum)
		      (eq? command datum))
		     ((comtab? datum)
		      (eq? command (ref-command-object prefix-key)))
		     ((command&comtab? datum)
		      (eq? command (car datum)))
		     (else
		      false))))))
      (let loop ((comtabs comtabs))
	(if (null? comtabs)
	    '()
	    (%comtab-bindings (car comtabs)
			      (loop (cdr comtabs))
			      command
			      valid-key?))))))

(define (%comtab-bindings comtab keys command valid-key?)
  (let comtab-loop ((comtab comtab) (keys keys) (prefix '()))
    (let alist-loop ((entries (comtab-alist* comtab)))
      (if (null? entries)
	  keys
	  (let ((key (append prefix (list (caar entries)))))
	    (let datum-loop
		((datum (cdar entries))
		 (keys (alist-loop (cdr entries))))
	      (cond ((not datum)
		     keys)
		    ((command? datum)
		     (if (and (eq? datum command)
			      (valid-key? key))
			 (cons key keys)
			 keys))
		    ((comtab? datum)
		     (let ((keys (comtab-loop datum keys key)))
		       (if (and (eq? command (ref-command-object prefix-key))
				(valid-key? key))
			   (cons key keys)
			   keys)))
		    ((command&comtab? datum)
		     (datum-loop (car datum)
				 (datum-loop (cdr datum) keys)))
		    ((comtab-alias? datum)
		     (datum-loop (comtab-alias/dereference datum) keys))
		    (else
		     (error "Illegal comtab datum:" datum)))))))))