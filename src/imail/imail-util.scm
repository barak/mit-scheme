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

;;;; IMAIL mail reader: utilities

(declare (usual-integrations))

(define (guarantee-index index procedure)
  (if (not (index-fixnum? index))
      (error:wrong-type-argument index "index" procedure)))

(define (source->list source)
  (let ((item (source)))
    (if (eof-object? item)
	'()
	(let ((head (list item)))
	  (let loop ((prev head))
	    (let ((item (source)))
	      (if (eof-object? item)
		  head
		  (let ((this (list item)))
		    (set-cdr! prev this)
		    (loop this)))))))))

(define (list->source items)
  (lambda ()
    (if (pair? items)
	(let ((item (car items)))
	  (set! items (cdr items))
	  item)
	(eof-object))))

(define (cut-list! items predicate)
  (if (or (not (pair? items)) (predicate (car items)))
      (values '() items)
      (let loop ((prev items) (this (cdr items)))
	(if (or (not (pair? this)) (predicate (car this)))
	    (begin
	      (set-cdr! prev '())
	      (values items this))
	    (loop this (cdr this))))))

(define (burst-list items predicate)
  (let loop ((items items) (groups '()))
    (if (pair? items)
	(let find-next ((items (cdr items)) (group (list (car items))))
	  (if (and (pair? items) (not (predicate (car items))))
	      (find-next (cdr items) (cons (car items) group))
	      (loop items (cons (reverse! group) groups))))
	(reverse! groups))))

(define (count-matching-items items predicate)
  (let loop ((items items) (count 0))
    (if (pair? items)
	(loop (cdr items)
	      (if (predicate (car items))
		  (fix:+ count 1)
		  count))
	count)))

(define (remove-duplicates items predicate)
  (let loop ((items items) (items* '()))
    (if (pair? items)
	(loop (cdr items)
	      (if (let loop ((items* (cdr items)))
		    (and (pair? items*)
			 (or (predicate (car items) (car items*))
			     (loop (cdr items*)))))
		  items*
		  (cons (car items) items*)))
	(reverse! items*))))

(define (remove-duplicates! items predicate)
  (define (trim-initial-segment items)
    (cond ((pair? items)
	   (if (test-item items)
	       (trim-initial-segment (cdr items))
	       (begin
		 (locate-initial-segment items (cdr items))
		 items)))
	  ((null? items) items)
	  (else (lose))))

  (define (locate-initial-segment prev this)
    (cond ((pair? this)
	   (if (test-item this)
	       (set-cdr! prev (trim-initial-segment (cdr this)))
	       (locate-initial-segment this (cdr this))))
	  ((not (null? this)) (lose))))

  (define (test-item items)
    (let loop ((items* (cdr items)))
      (and (pair? items*)
	   (or (predicate (car items) (car items*))
	       (loop (cdr items*))))))

  (define (lose)
    (error:wrong-type-argument items "list" 'REMOVE-DUPLICATES!))

  (trim-initial-segment items))

;; The cryptic LWSP means Linear White SPace.  We use it because it
;; is the terminology from RFC 822.

(define (char-lwsp? char)
  (or (char=? #\space char)
      (char=? #\tab char)))

(define char-set:lwsp
  (char-set #\space #\tab))

(define (skip-lwsp-backwards string start end)
  (let loop ((end end))
    (if (and (fix:< start end)
	     (char-lwsp? (string-ref string (fix:- end 1))))
	(loop (fix:- end 1))
	end)))

(define (skip-lwsp-until-newline string start end)
  (let loop ((index start))
    (cond ((= index end)
           #f)
          ((char-lwsp? (string-ref string index))
           (loop (+ index 1)))
          ((char=? (string-ref string index) #\newline)
           (+ index 1))
          (else
           #f))))

(define (quote-lines lines)
  (map (lambda (line)
	 (string-append "\t" line))
       lines))

(define (unquote-lines lines)
  (map (lambda (line)
	 (if (and (fix:> (string-length line) 0)
		  (char=? #\tab (string-ref line 0)))
	     (string-tail line 1)
	     (error "Unquoted line:" line)))
       lines))

(define (string->lines string #!optional line-ending)
  (substring->lines string 0 (string-length string)
		    (if (default-object? line-ending) "\n" line-ending)))

(define (substring->lines string start end #!optional line-ending)
  (let ((line-ending (if (default-object? line-ending) "\n" line-ending)))
    (let ((indexes (substring-search-all line-ending string start end))
          (n (string-length line-ending)))
      (if (pair? indexes)
	  (begin
	    (let loop ((start start) (indexes indexes))
	      (let ((start* (fix:+ (car indexes) n)))
		(set-car! indexes (substring string start (car indexes)))
		(cond ((pair? (cdr indexes))
		       (loop start* (cdr indexes)))
		      ((fix:< start* end)
		       (set-cdr! indexes
				 (list (substring string start* end)))))))
	    indexes)
	  (list (if (and (fix:= start 0)
			 (fix:= end (string-length string)))
		    string
		    (substring string start end)))))))

(define (lines->string lines #!optional line-ending)
  (decorated-string-append "" ""
			   (if (default-object? line-ending) "\n" line-ending)
			   lines))

(define (check-file-prefix pathname magic)
  (let* ((n-to-read (string-length magic))
	 (buffer (make-string n-to-read))
	 (n-read
	  (catch-file-errors (lambda (condition) condition #f)
	    (lambda ()
	      (call-with-input-file pathname
		(lambda (port)
		  (read-string! buffer port)))))))
    (and n-read
	 (fix:= n-to-read n-read)
	 (string=? buffer magic))))

(define (read-required-char port)
  (let ((char (read-char port)))
    (if (eof-object? char)
	(error "Premature end of file:" port))
    char))

(define (peek-required-char port)
  (let ((char (peek-char port)))
    (if (eof-object? char)
	(error "Premature end of file:" port))
    char))

(define (read-required-line port)
  (let ((line (read-line port)))
    (if (eof-object? line)
	(error "Premature end of file:" port))
    line))

(define (skip-to-line-start port)
  (input-port/discard-chars port char-set:newline)
  (input-port/discard-char port))

(define (skip-past-blank-line port)
  (let loop ()
    (if (not (char=? (read-required-char port) #\newline))
	(begin
	  (skip-to-line-start port)
	  (loop)))))

(define (parse-header-field-date field-value)
  (let ((t
	 (ignore-errors
	  (lambda ()
	    (string->universal-time
	     (rfc822:tokens->string
	      (rfc822:strip-comments
	       (rfc822:string->tokens field-value))))))))
    (and (not (condition? t))
	 t)))

(define (abbreviate-exact-nonnegative-integer n k)
  (if (< n (expt 10 (- k 1)))
      (string-append (string-pad-left (number->string n) (- k 1)) " ")
      (let ((s
	     (parameterize ((param:flonum-printer-cutoff
			     `(RELATIVE ,k ENGINEERING)))
	       (number->string (exact->inexact n)))))
	(let ((regs (re-string-match "\\([0-9.]+\\)e\\([0-9]+\\)" s)))
	  (let ((mantissa (re-match-extract s regs 1))
		(exponent (string->number (re-match-extract s regs 2))))
	    (if (> exponent 12)
		(make-string k #\+)
		(string-append
		 (let ((l (string-length mantissa))
		       (k (- k 1)))
		   (cond ((< l k)
			  (string-pad-left mantissa k))
			 ((= l k)
			  mantissa)
			 ((char=? #\. (string-ref mantissa (- k 1)))
			  (string-append " " (string-head mantissa (- k 1))))
			 (else
			  (string-head mantissa k))))
		 (case exponent
		   ((0) " ")
		   ((3) "k")
		   ((6) "M")
		   ((9) "G")
		   ((12) "T")))))))))

(define (exact-nonnegative-integer-digits n)
  (let loop ((j 1) (k 10))
    (if (< n k)
	j
	(loop (+ j 1) (* k 10)))))

(define (burst-comma-list-string string)
  (remove string-null?
	  (map string-trim (burst-string string #\, #f))))

(define (string-greatest-common-prefix strings)
  (let loop
      ((strings (cdr strings))
       (string (car strings))
       (index (string-length (car strings))))
    (if (null? strings)
	(substring string 0 index)
	(let ((string* (car strings)))
	  (let ((index* (string-match-forward string string*)))
	    (if (< index* index)
		(loop (cdr strings) string* index*)
		(loop (cdr strings) string index)))))))

(define (string-greatest-common-prefix-ci strings)
  (let loop
      ((strings (cdr strings))
       (string (car strings))
       (index (string-length (car strings))))
    (if (null? strings)
	(substring string 0 index)
	(let ((string* (car strings)))
	  (let ((index* (string-match-forward-ci string string*)))
	    (if (< index* index)
		(loop (cdr strings) string* index*)
		(loop (cdr strings) string index)))))))

(define (string-n-newlines string)
  (substring-n-newlines string 0 (string-length string)))

(define (substring-n-newlines string start end)
  (let loop ((start start) (n 0))
    (let ((index (substring-find-next-char string start end #\newline)))
      (if index
	  (loop (fix:+ index 1) (fix:+ n 1))
	  n))))

;;;; Broken-pipe handler

(define (handle-broken-pipe handler thunk)
  (bind-condition-handler (list condition-type:system-call-error
				condition-type:derived-port-error)
      (lambda (condition)
	(if (broken-pipe? condition)
	    (handler condition)))
    thunk))

(define (broken-pipe? condition)
  (cond ((eq? (condition/type condition) condition-type:system-call-error)
	 (and (eq? (system-call-name condition) 'WRITE)
	      (eq? (system-call-error condition) 'BROKEN-PIPE)))
	((eq? (condition/type condition) condition-type:derived-port-error)
	 (broken-pipe? (derived-port-condition condition)))
	(else #f)))

(define system-call-name
  (condition-accessor condition-type:system-call-error 'SYSTEM-CALL))

(define system-call-error
  (condition-accessor condition-type:system-call-error 'ERROR-TYPE))

(define derived-port-condition
  (condition-accessor condition-type:derived-port-error 'CONDITION))

;;;; Filename Completion

(define (pathname-complete-string pathname filter
				  if-unique if-not-unique if-not-found)
  (let ((pathname (merge-pathnames pathname))
	(if-directory
	 (lambda (pathname)
	   (if-not-unique pathname
	     (lambda ()
	       (simple-directory-read pathname (result-filter filter)))))))
    (cond ((not (safe-file-directory? (directory-pathname pathname)))
	   (if-not-found))
	  ((directory-pathname? pathname)
	   (if-directory pathname))
	  (else
	   (let ((pathnames (filtered-completions pathname filter)))
	     (cond ((not (pair? pathnames))
		    (if-not-found))
		   ((pair? (cdr pathnames))
		    (if-not-unique (->pathname
				    (string-greatest-common-prefix
				     (map ->namestring pathnames)))
				   (lambda () pathnames)))
		   ((directory-pathname? (car pathnames))
		    (if-directory (car pathnames)))
		   (else
		    (if-unique (car pathnames)))))))))

(define (pathname-completions-list pathname filter)
  (filtered-completions (merge-pathnames pathname) filter))

(define (filtered-completions pathname filter)
  (simple-directory-read-matching pathname (result-filter filter)))

(define (simple-directory-read-matching pathname accumulator)
  (let* ((directory (directory-namestring pathname))
	 (prefix (file-namestring pathname))
	 (channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read-matching channel prefix)))
	(if name
	    (loop (accumulator name directory result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (simple-directory-read pathname accumulator)
  (let* ((directory (directory-namestring pathname))
	 (channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (accumulator name directory result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define ((result-filter filter) name directory result)
  (if (or (string=? name ".") (string=? name ".."))
      result
      (let ((pathname (parse-namestring (string-append directory name) #f #f)))
	(cond ((safe-file-directory? pathname)
	       (cons (pathname-as-directory pathname) result))
	      ((filter pathname) (cons pathname result))
	      (else result)))))

(define (safe-file-directory? pathname)
  (catch-file-errors (lambda (condition) condition #f)
    (lambda ()
      (file-directory? pathname))))

(define type-code:legacy-string (microcode-type 'character-string))

(define (read-file-into-string pathname)
  (call-with-binary-input-file pathname
    (lambda (port)
      ;; XXX Fail gracefully if this is not a regular file.
      (let* ((length (binary-port-length port))
	     ;; XXX Store in an external buffer like we used to.
	     (buffer (make-bytevector length))
	     (nread (read-bytevector! buffer port)))
	;; Check to make sure that the length didn't change under us
	;; while we were reading.  If it did, this is no good.
	(and (eqv? nread length)
	     (object-new-type type-code:legacy-string buffer))))))

(define (string-port/position port)
  (cond ((textual-port-operation port 'position)
	 => (lambda (position)
	      (position port)))
	(else
	 (error "Non-positionable port:" port))))

;;;; Properties

(define-class <property-mixin> ()
  (alist define (accessor modifier)
	 accessor object-properties
	 modifier set-object-properties!
	 initial-value '()))

(define (get-property object key default)
  (let ((entry (assq key (object-properties object))))
    (if entry
	(cdr entry)
	default)))

(define (store-property! object key datum)
  (let ((alist (object-properties object)))
    (let ((entry (assq key alist)))
      (if entry
	  (set-cdr! entry datum)
	  (set-object-properties! object (cons (cons key datum) alist))))))

(define (remove-property! object key)
  (set-object-properties! object (del-assq! key (object-properties object))))

(define (intern-property! object key generator)
  (let* ((default (cons 0 0))
	 (datum (get-property object key default)))
    (if (eq? datum default)
	(let ((datum (generator object)))
	  (store-property! object key datum)
	  datum)
	datum)))

;;;; Modification events

(define-class <modification-event-mixin> ()
  (modification-count define (accessor modifier)
		      accessor object-modification-count
		      modifier set-object-modification-count!
		      initial-value 0)
  (modification-event define accessor
		      accessor object-modification-event
		      initializer make-event-distributor))

(define (receive-modification-events object procedure)
  (add-event-receiver! (object-modification-event object) procedure))

(define (ignore-modification-events object procedure)
  (remove-event-receiver! (object-modification-event object) procedure))

(define (object-modified! object type . arguments)
  (without-interrupts
   (lambda ()
     (set-object-modification-count!
      object
      (+ (object-modification-count object) 1))))
  (apply signal-modification-event object type arguments))

(define (signal-modification-event object type . arguments)
  (if *deferred-modification-events*
      (set-cdr! *deferred-modification-events*
		(cons (cons* object type arguments)
		      (cdr *deferred-modification-events*)))
      (begin
	(if imap-trace-port
	    (begin
	      (write-line (cons* 'OBJECT-EVENT object type arguments)
			  imap-trace-port)
	      (flush-output-port imap-trace-port)))
	(event-distributor/invoke! (object-modification-event object)
				   object
				   type
				   arguments))))

(define (with-modification-events-deferred thunk)
  (let ((events (list 'EVENTS)))
    (let ((v
	   (fluid-let ((*deferred-modification-events* events))
	     (thunk))))
      (for-each (lambda (event) (apply signal-modification-event event))
		(reverse! (cdr events)))
      v)))

(define *deferred-modification-events* #f)
