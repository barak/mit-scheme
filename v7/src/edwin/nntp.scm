;;; -*-Scheme-*-
;;;
;;;	$Id: nntp.scm,v 1.2 1995/05/06 02:21:44 cph Exp $
;;;
;;;	Copyright (c) 1995 Massachusetts Institute of Technology
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

;;;; NNTP Interface

;;; This program provides a high-level interface to an NNTP server.
;;; It implements a database abstraction which gives the impression
;;; that the news database is in memory and can be manipulated
;;; directly.  This abstraction largely hides the underlying server
;;; communication on which it is built.

;;; The abstraction provides models for the server, each of the groups
;;; it contains, and the headers in each group.  It also provides a
;;; method for combining headers into conversation threads.

(declare (usual-integrations))

;;;; NNTP Connection

(define-structure (nntp-connection
		   (conc-name nntp-connection:)
		   (constructor make-nntp-connection (server)))
  (server #f read-only #t)
  (process #f)
  banner
  (group-table (make-string-hash-table) read-only #t)
  (%active-groups #f)
  (reader-hook #f))

(define (nntp-connection:port connection)
  (subprocess-i/o-port (nntp-connection:process connection)))

(define (open-nntp-connection server)
  (let ((connection (make-nntp-connection server)))
    (nntp-connection:reopen connection)
    connection))

(define (nntp-connection:reopen connection)
  (let ((process
	 (let ((program (os/find-program "tcp" #f)))
	   (start-pipe-subprocess program
				  (vector (file-namestring program)
					  (nntp-connection:server connection)
					  "nntp")
				  #f))))
    (set-nntp-connection:process! connection process)
    (set-nntp-connection:banner!
     connection
     ;; Set up the line translation for the process, because the
     ;; network line translation is CR-LF regardless of the operating
     ;; system this program runs on.
     (input-port/read-line (subprocess-i/o-port process "\r\n")))))

(define (nntp-connection:closed? connection)
  (let ((port (nntp-connection:port connection)))
    (or (not port)
	(input-port/eof? port))))

(define (nntp-connection:close connection)
  (if (not (nntp-connection:closed? connection))
      (begin
	(nntp-write-command connection "quit")
	(nntp-drain-output connection)))
  (nntp-flush-input connection)
  (subprocess-delete (nntp-connection:process connection)))

(define (nntp-connection:active-groups connection)
  (or (nntp-connection:%active-groups connection)
      (let ((lines
	     (let ((msg "Reading list of news groups... "))
	       (message msg)
	       (let ((lines (list->vector (nntp-list-command connection))))
		 (message msg "done")
		 lines))))
	(let ((msg "Parsing list of news groups... "))
	  (message msg)
	  (let ((end (vector-length lines)))
	    (do ((index 0 (fix:+ index 1)))
		((fix:= index end))
	      (let ((tokens (string-tokenize (vector-ref lines index))))
		(let ((group (make-news-group connection (car tokens))))
		  (set-news-group:server-probe!
		   group
		   (let ((last (token->number (cadr tokens)))
			 (first (token->number (caddr tokens))))
		     (vector (- (+ last 1) first)
			     first
			     last)))
		  (vector-set! lines index group)))))
	  (message msg "done")
	  (sort! lines news-group:<)
	  (set-nntp-connection:%active-groups! connection lines)
	  lines))))

(define (nntp-connection:discard-active-groups-cache! connection)
  (set-nntp-connection:%active-groups! connection #f))

;;;; NNTP Commands

(define (nntp-group-command connection group-name)
  (prepare-nntp-connection connection)
  (nntp-write-command connection "group" group-name)
  (nntp-drain-output connection)
  (let ((response (nntp-read-line connection)))
    (case (nntp-response-number response)
      ((211)
       (let ((tokens (string-tokenize response)))
	 (vector (token->number (cadr tokens))
		 (token->number (caddr tokens))
		 (token->number (cadddr tokens)))))
      ((411)
       'NO-SUCH-GROUP)
      (else
       (nntp-error response)))))

;; This says how many pending HEAD requests may be sent before it's
;; necessary to starting reading the replies, to avoid deadlock.
(define nntp-maximum-request 400)

(define (nntp-head-request connection key)
  (nntp-write-command connection "head" key))

(define (nntp-head-reply connection)
  (let ((response (nntp-read-line connection)))
    (case (nntp-response-number response)
      ((221)
       (let ((tokens (string-tokenize response)))
	 (let ((article-number (cadr tokens))
	       (message-id (caddr tokens)))
	   (if (and (valid-article-number? article-number)
		    (valid-message-id? message-id))
	       (vector article-number
		       message-id
		       (nntp-read-text-lines connection))
	       'NO-SUCH-ARTICLE))))
      ((423 430)
       'NO-SUCH-ARTICLE)
      (else
       (nntp-error response)))))

(define (nntp-body-command connection key port)
  (prepare-nntp-connection connection)
  (nntp-write-command connection "body" key)
  (nntp-drain-output connection)
  (let ((response (nntp-read-line connection)))
    (case (nntp-response-number response)
      ((222)
       (nntp-read-text-1 connection port)
       #t)
      ((423 430)
       #f)
      (else
       (nntp-error response)))))

(define (nntp-list-command connection)
  (prepare-nntp-connection connection)
  (nntp-write-command connection "list")
  (nntp-drain-output connection)
  (let ((response (nntp-read-line connection)))
    (if (fix:= 215 (nntp-response-number response))
	(nntp-read-text-lines connection)
	(nntp-error response))))

(define (nntp-connection:post-article connection port)
  (prepare-nntp-connection connection)
  (nntp-write-command connection "post")
  (nntp-drain-output connection)
  (let ((response (nntp-read-line connection)))
    (if (fix:= 340 (nntp-response-number response))
	(let loop ()
	  (let ((line (input-port/read-line port)))
	    (if (eof-object? line)
		(begin
		  (nntp-write-command connection ".")
		  (nntp-drain-output connection)
		  (let ((response (nntp-read-line connection)))
		    (and (not (fix:= 240 (nntp-response-number response)))
			 response)))
		(begin
		  (nntp-write-line connection line)
		  (loop)))))
	response)))

(define (nntp-error response)
  (error "NNTP error:" response))

(define (prepare-nntp-connection connection)
  (nntp-flush-input connection)
  (if (nntp-connection:closed? connection)
      (nntp-connection:reopen connection)))

(define (nntp-flush-input connection)
  (let ((port (nntp-connection:port connection)))
    (if port
	(do ()
	    ((not (input-port/char-ready? port 100)))
	  (input-port/discard-line port)))))

(define (nntp-write-command connection string . strings)
  (let ((port (nntp-connection:port connection)))
    (output-port/write-string port string)
    (do ((strings strings (cdr strings)))
	((null? strings))
      (output-port/write-char port #\space)
      (output-port/write-string port (car strings)))
    (output-port/write-char port #\newline)))

(define (nntp-write-line connection string)
  (let ((port (nntp-connection:port connection)))
    (if (and (not (string-null? string))
	     (char=? (string-ref string 0) #\.))
	(output-port/write-char port #\.))
    (output-port/write-string port string)
    (output-port/write-char port #\newline)))

(define (nntp-drain-output connection)
  (output-port/flush-output (nntp-connection:port connection)))

(define (nntp-read-line connection)
  (input-port/read-line (nntp-connection:port connection)))

(define (nntp-response-number line)
  (if (fix:< (string-length line) 3)
      (error "Malformed NNTP response:" line))
  (substring->nonnegative-integer line 0 3))

(define (nntp-read-text connection)
  (with-string-output-port
    (lambda (port)
      (nntp-read-text-1 connection port))))

(define (nntp-read-text-1 connection port)
  (let loop ()
    (let ((line (nntp-read-line connection)))
      (let ((length (string-length line)))
	(cond ((fix:= 0 length)
	       (output-port/write-char port #\newline)
	       (loop))
	      ((char=? #\. (string-ref line 0))
	       (if (not (fix:= 1 length))
		   (begin
		     (output-port/write-substring port line 1 length)
		     (output-port/write-char port #\newline)
		     (loop))))
	      (else
	       (output-port/write-substring port line 0 length)
	       (output-port/write-char port #\newline)
	       (loop)))))))

(define (nntp-read-text-lines connection)
  (let loop ((lines '()))
    (let ((line (nntp-read-line connection)))
      (let ((length (string-length line)))
	(cond ((or (fix:= 0 length)
		   (not (char=? #\. (string-ref line 0))))
	       (loop (cons line lines)))
	      ((fix:= 1 length)
	       (reverse! lines))
	      (else
	       (loop (cons (string-tail line 1) lines))))))))

;;;; News-Group Data Structure

(define-structure (news-group
		   (conc-name news-group:)
		   (constructor %make-news-group (connection name)))
  (connection #f read-only #t)
  (name #f read-only #t)
  (header-table (make-eqv-hash-table) read-only #t)
  (server-probe #f)
  (reader-hook #f))

(define (make-news-group connection name)
  (let ((table (nntp-connection:group-table connection)))
    (or (hash-table/get table name #f)
	(let ((group (%make-news-group connection name)))
	  (hash-table/put! table name group)
	  group))))

(define-integrable (news-group:server group)
  (nntp-connection:server (news-group:connection group)))

(define (news-group:< x y)
  (string-ci<? (news-group:name x) (news-group:name y)))

(define (find-news-group connection name)
  (hash-table/get (nntp-connection:group-table connection) name #f))

(define (find-active-news-group connection name)
  (let ((table (nntp-connection:group-table connection)))
    (let ((group (hash-table/get table name #f)))
      (if group
	  (and (news-group:active? group) group)
	  (and (not (nntp-connection:%active-groups connection))
	       (let ((probe (nntp-group-command connection name)))
		 (and (not (eq? 'NO-SUCH-GROUP probe))
		      (let ((group (%make-news-group connection name)))
			(set-news-group:server-probe! group probe)
			(hash-table/put! table name group)
			group))))))))

(define (news-group:active? group)
  (news-group:maybe-update-probe! group)
  (not (eq? 'NO-SUCH-GROUP (news-group:server-probe group))))

(define-integrable (news-group:estimated-n-articles group)
  (vector-ref (news-group:guarantee-server-probe group) 0))

(define-integrable (news-group:first-article group)
  (vector-ref (news-group:guarantee-server-probe group) 1))

(define-integrable (news-group:last-article group)
  (vector-ref (news-group:guarantee-server-probe group) 2))

(define (news-group:guarantee-server-probe group)
  (news-group:maybe-update-probe! group)
  (let ((probe (news-group:server-probe group)))
    (if (eq? 'NO-SUCH-GROUP probe)
	(error "Unknown news group:" (news-group:name group)))
    probe))

(define (news-group:maybe-update-probe! group)
  (if (not (news-group:server-probe group))
      (news-group:update-probe! group)))

(define (news-group:update-probe! group)
  (set-news-group:server-probe!
   group
   (nntp-group-command (news-group:connection group)
		       (news-group:name group))))

;;;; Header Cache

(define (news-group:header group number)
  (let ((table (news-group:header-table group)))
    (let ((header (hash-table/get table number #f)))
      (and (not (eq? 'NONE header))
	   (or header
	       (let ((header (parse-header group (read-header group number))))
		 (hash-table/put! table number (or header 'NONE))
		 header))))))

(define (news-group:headers group numbers)
  (call-with-values (lambda () (cached-headers group numbers))
    (lambda (headers numbers)
      (if (null? numbers)
	  headers
	  (let ((table (news-group:header-table group)))
	    (let loop
		((headers headers)
		 (numbers numbers)
		 (responses (read-headers group numbers)))
	      (if (null? responses)
		  headers
		  (loop (let ((header (parse-header group (car responses))))
			  (hash-table/put! table
					   (car numbers)
					   (or header 'NONE))
			  (if header (cons header headers) headers))
			(cdr numbers)
			(cdr responses)))))))))

(define (cached-headers group numbers)
  (let ((table (news-group:header-table group)))
    (let loop ((numbers numbers) (headers '()) (numbers* '()))
      (if (null? numbers)
	  (values headers (reverse! numbers*))
	  (let ((header (hash-table/get table (car numbers) #f)))
	    (loop (cdr numbers)
		  (if (or (not header) (eq? 'NONE header))
		      headers
		      (cons header headers))
		  (if (not header)
		      (cons (car numbers) numbers*)
		      numbers*)))))))

(define (news-group:cached-header group number)
  (hash-table/get (news-group:header-table group) number #f))

(define (news-group:discard-cached-header! group number)
  (hash-table/remove! (news-group:header-table group) number))

(define (news-group:cached-header-numbers group)
  (hash-table/key-list (news-group:header-table group)))

(define (news-group:cached-headers group)
  (hash-table/datum-list (news-group:header-table group)))

;;;; Read Headers

(define (read-header group number)
  (news-group:update-probe! group)
  (let ((connection (news-group:connection group))
	(msg "Reading news header... "))
    (message msg)
    (prepare-nntp-connection connection)
    (nntp-head-request connection (number->string number))
    (let ((response (nntp-head-reply connection)))
      (message msg "done")
      response)))

(define (read-headers group numbers)
  (news-group:update-probe! group)
  (let ((n-to-read (length numbers))
	(connection (news-group:connection group))
	(msg "Reading news headers... ")
	(n-received 0))
    (let ((msg? (fix:>= n-to-read 100)))

      (define (send-requests numbers n)
	(do ((numbers numbers (cdr numbers))
	     (n n (fix:- n 1)))
	    ((fix:= n 0) numbers)
	  (nntp-head-request connection (number->string (car numbers)))))

      (define (receive-replies numbers n responses)
	(nntp-drain-output connection)
	(do ((numbers numbers (cdr numbers))
	     (n n (fix:- n 1))
	     (responses responses
			(cons (nntp-head-reply connection) responses)))
	    ((fix:= n 0) responses)
	  (if (and msg?
		   (begin
		     (set! n-received (fix:+ n-received 1))
		     (fix:= 0 (fix:remainder n-received 20))))
	      (message msg (integer-round (* n-received 100) n-to-read) "%"))))

      (message msg)
      (prepare-nntp-connection connection)
      (let loop ((numbers numbers) (n-left n-to-read) (responses '()))
	(if (null? numbers)
	    (begin
	      (message msg "done")
	      (reverse! responses))
	    (let ((n (min n-left nntp-maximum-request)))
	      (let ((numbers* (send-requests numbers n)))
		(loop numbers*
		      (fix:- n-left n)
		      (receive-replies numbers n responses)))))))))

;;;; Parse Headers

(define (parse-header group response)
  (and (vector? response)
       (let ((lines (vector-ref response 2)))
	 (make-news-header group
			   (token->number (vector-ref response 0))
			   (vector-ref response 1)
			   (lines->header-text lines)
			   (parse-header-lines lines)))))

(define (lines->header-text lines)
  (let ((length
	 (do ((lines lines (cdr lines))
	      (nchars 0 (fix:+ (fix:+ nchars 1) (string-length (car lines)))))
	     ((null? lines) nchars))))
    (let ((text (make-string length)))
      (let loop ((lines lines) (index 0))
	(if (not (null? lines))
	    (loop (cdr lines)
		  (let* ((line (car lines))
			 (end (string-length line)))
		    (do ((i1 0 (fix:+ i1 1))
			 (i2 index (fix:+ i2 1)))
			((fix:= i1 end)
			 (string-set! text i2 #\newline)
			 (fix:+ i2 1))
		      (string-set! text i2 (string-ref line i1)))))))
      text)))

(define (parse-header-lines lines)
  (cond ((null? lines)
	 '())
	((header-line? (car lines))
	 (let ((unfold
		(lambda (rest)
		  (let ((colon (string-find-next-char (car lines) #\:))
			(end (string-length (car lines))))
		    (cons (substring-trim (car lines) 0 colon)
			  (let ((value
				 (substring-trim (car lines)
						 (fix:+ colon 1)
						 end)))
			    (if (null? rest)
				value
				(apply string-append
				       (cons value
					     (append-map
					      (lambda (string)
						(list " "
						      (string-trim string)))
					      (reverse! rest)))))))))))
	   (let loop ((lines (cdr lines)) (rest '()))
	     (cond ((null? lines)
		    (list (unfold rest)))
		   ((header-continuation-line? (car lines))
		    (loop (cdr lines) (cons (car lines) rest)))
		   (else
		    (cons (unfold rest) (parse-header-lines lines)))))))
	(else
	 (parse-header-lines (cdr lines)))))

(define (header-line? line)
  (and (not (string-null? line))
       (not (or (char=? #\space (string-ref line 0))
		(char=? #\tab (string-ref line 0))))
       (string-find-next-char line #\:)))

(define (header-continuation-line? line)
  (and (not (string-null? line))
       (or (char=? #\space (string-ref line 0))
	   (char=? #\tab (string-ref line 0)))
       (string-find-next-char-in-set line char-set:not-whitespace)))

;;;; News-Header Data Structure

(define-structure (news-header
		   (conc-name news-header:)
		   (constructor make-news-header
				(group number message-id text alist)))
  (group #f read-only #t)
  number
  (message-id #f read-only #t)
  (text #f read-only #t)
  (alist #f read-only #t)
  (followup-to #f)
  (followups '())
  (thread #f)
  (reader-hook #f))

(define (dummy-news-header message-id)
  (make-news-header #f #f message-id #f '()))

(define (news-header:dummy? header)
  (not (news-header:text header)))

(define (news-header:field-value header name)
  (let ((field
	 (list-search-positive (news-header:alist header)
	   (lambda (field)
	     (string-ci=? (car field) name)))))
    (if field
	(cdr field)
	"")))

(define (news-header:< x y)
  (< (news-header:number x) (news-header:number y)))

(define (news-header:read-body header port)
  (nntp-body-command (news-group:connection (news-header:group header))
		     (news-header:message-id header)
		     port))

(define (news-header:xref header)
  (parse-xref-tokens
   (string-tokenize (news-header:field-value header "xref"))))

(define (parse-xref-tokens tokens)
  (if (null? tokens)
      tokens
      (let ((colon (string-find-next-char (car tokens) #\:))
	    (rest (parse-xref-tokens (cdr tokens))))
	(if colon
	    (cons (cons (string-head (car tokens) colon)
			(string-tail (car tokens) (fix:+ colon 1)))
		  rest)
	    rest))))

(define (valid-article-number? string)
  (let ((end (string-length string)))
    (and (let loop ((index 0))
	   (and (not (fix:= index end))
		(or (not (char=? #\0 (string-ref string index)))
		    (loop (fix:+ index 1)))))
	 (let loop ((index 0))
	   (or (fix:= index end)
	       (and (fix:<= (char->integer #\0)
			    (char->integer (string-ref string index)))
		    (fix:<= (char->integer (string-ref string index))
			    (char->integer #\9)))
	       (loop (fix:+ index 1)))))))

(define (valid-message-id? string)
  (let ((end (string-length string)))
    (and (fix:> end 2)
	 (char=? #\< (string-ref string 0))
	 (let loop ((index 1))
	   (and (not (fix:= index end))
		(if (char=? #\> (string-ref string index))
		    (fix:= (fix:+ index 1) end)
		    (and (not (char=? #\space (string-ref string index)))
			 (not (char=? #\< (string-ref string index)))
			 (loop (fix:+ index 1)))))))))

;;;; Conversation Threads

;;; This is by far the hairiest part of this implementation.  Headers
;;; are first organized into trees based on their "references" fields.
;;; The tree structure is reflected in their FOLLOWUP-TO and FOLLOWUPS
;;; fields.  These trees are then gathered into threads by means of
;;; subject matching.  Each resulting thread consists of a list of
;;; these trees, represented by the tree roots.  The list is sorted by
;;; the header order of the roots.

(define-structure (news-thread
		   (conc-name news-thread:)
		   (constructor make-news-thread (root-headers)))
  (root-headers #f read-only #t)
  (reader-hook #f))

(define (news-thread:< x y)
  (news-header:< (car (news-thread:root-headers x))
		 (car (news-thread:root-headers y))))

(define (news-thread:for-each-header thread procedure)
  (for-each (letrec ((loop
		      (lambda (header)
			(procedure header)
			(for-each loop (news-header:followups header)))))
	      loop)
	    (news-thread:root-headers thread)))

(define (organize-headers-into-threads headers)
  (build-followup-trees! headers)
  (sort (map make-threads-equivalent!
	     (let ((threads (associate-threads-with-trees headers)))
	       (for-each (lambda (thread)
			   (for-each guarantee-header-number
				     (news-thread:root-headers thread)))
			 threads)
	       (build-equivalence-classes
		threads
		(find-subject-associations threads))))
	news-thread:<))

(define (build-followup-trees! headers)
  (let ((references (make-eq-hash-table))
	(dummy-headers '()))
    (let ((get-refs (lambda (h) (hash-table/get references h '())))
	  (set-refs (lambda (h r) (hash-table/put! references h r))))
      (let ((id-table (make-string-hash-table)))
	(for-each (lambda (header)
		    (set-news-header:followup-to! header #f)
		    (set-news-header:followups! header '())
		    (set-news-header:thread! header #f)
		    (set-refs header (news-header:references header))
		    (hash-table/put! id-table
				     (news-header:message-id header)
				     header))
		  headers)
	(for-each (lambda (header)
		    (do ((refs (get-refs header) (cdr refs)))
			((null? refs))
		      (set-car! refs
				(let ((id (car refs)))
				  (or (hash-table/get id-table id #f)
				      (let ((header (dummy-news-header id)))
					(hash-table/put! id-table id header)
					(set! dummy-headers
					      (cons header dummy-headers))
					header))))))
		  headers))
      (for-each (lambda (header)
		  (do ((refs (get-refs header) (cdr refs)))
		      ((null? refs))
		    (if (news-header:dummy? (car refs))
			(let ((drefs (get-refs (car refs))))
			  (if (not (eq? 'BROKEN drefs))
			      (let loop ((x (cdr refs)) (y drefs))
				(cond ((null? x)
				       unspecific)
				      ((null? y)
				       (set-refs (car refs) (cdr refs)))
				      ((eq? (car x) (car y))
				       (loop (cdr x) (cdr y)))
				      (else
				       (set-refs (car refs) 'BROKEN)))))))))
		headers)
      (for-each (lambda (dummy-header)
		  (if (eq? 'BROKEN (get-refs dummy-header))
		      (set-refs dummy-header '())))
		dummy-headers)
      (let ((set-followups
	     (lambda (header)
	       (let ((refs (get-refs header)))
		 (if (not (null? refs))
		     (let ((header* (car refs)))
		       (set-news-header:followup-to! header header*)
		       (set-news-header:followups!
			header*
			(cons header (news-header:followups header*)))))))))
	(for-each set-followups headers)
	(for-each set-followups dummy-headers)))))

(define (news-header:references header)
  ;; Check the references header field to guarantee that it's
  ;; well-formed, and discard it entirely if it isn't.  This paranoia
  ;; is reasonable since I've already seen bad references during the
  ;; first few days of testing.
  (let ((tokens
	 (reverse-string-tokenize
	  (news-header:field-value header "references"))))
    (if (let loop ((tokens tokens))
	  (or (null? tokens)
	      (and (valid-message-id? (car tokens))
		   (not (member (car tokens) (cdr tokens)))
		   (loop (cdr tokens)))))
	tokens
	'())))

(define (associate-threads-with-trees headers)
  (let ((threads '()))
    (for-each (lambda (header)
		(if (not (news-header:thread header))
		    (let ((root
			   (let loop ((header header))
			     (if (news-header:followup-to header)
				 (loop (news-header:followup-to header))
				 header))))
		      (let ((thread (make-news-thread (list root))))
			(set! threads (cons thread threads))
			(news-thread:for-each-header thread
			  (lambda (header)
			    (set-news-header:thread! header thread)))))))
	      headers)
    threads))

(define (guarantee-header-number header)
  (let ((followups (news-header:followups header)))
    (for-each guarantee-header-number followups)
    (set-news-header:followups! header (sort followups news-header:<)))
  (if (not (news-header:number header))
      (let ((followups (news-header:followups header)))
	(if (null? followups)
	    (error "Dummy header has no followups:" header))
	(set-news-header:number! header
				 (news-header:number (car followups))))))

(define (find-subject-associations threads)
  (let ((subject-alist '()))
    (for-each (lambda (thread)
		(news-thread:for-each-header thread
		  (lambda (header)
		    (let ((subject
			   (canonicalize-subject
			    (news-header:field-value header "subject"))))
		      (if (not (string-null? subject))
			  (let ((entry (assoc-subject subject subject-alist)))
			    (cond ((not entry)
				   (set! subject-alist
					 (cons (list subject thread)
					       subject-alist))
				   unspecific)
				  ((not (memq thread (cdr entry)))
				   (set-cdr! entry
					     (cons thread (cdr entry)))))))))))
	      threads)
    subject-alist))

(define (canonicalize-subject subject)
  ;; This is optimized by assuming that the subject lines have no
  ;; leading or trailing white space.  The news-header parser makes
  ;; that guarantee.
  (let ((end (string-length subject)))
    (substring subject
	       (let loop ((start 0))
		 (if (substring-prefix-ci? "re:" 0 3 subject start end)
		     (loop (substring-skip-leading-space subject
							 (fix:+ start 3)
							 end))
		     start))
	       end)))

(define (assoc-subject subject alist)
  (let loop ((alist alist))
    (and (not (null? alist))
	 (let ((comparison (compare-subjects subject (caar alist))))
	   (if comparison
	       (begin
		 (if (eq? 'LEFT-PREFIX comparison)
		     (set-car! (car alist) subject))
		 (car alist))
	       (loop (cdr alist)))))))

(define (compare-subjects x y)
  (let ((xe (string-length x))
	(ye (string-length y)))
    (let ((i (substring-match-forward-ci x 0 xe y 0 ye)))
      (if (fix:= i xe)
	  (if (fix:= i ye) 'EQUAL 'LEFT-PREFIX)
	  (if (fix:= i ye) 'RIGHT-PREFIX #f)))))

(define (build-equivalence-classes threads subject-alist)
  (let ((equivalences (make-eq-hash-table)))
    (for-each (lambda (thread)
		(hash-table/put! equivalences
				 thread
				 (let ((t (list thread)))
				   (set-cdr! t (list t))
				   t)))
	      threads)
    (let ((equivalence!
	   (lambda (x y)
	     (let ((x (hash-table/get equivalences x #f))
		   (y (hash-table/get equivalences y #f)))
	       (if (not (eq? (cdr x) (cdr y)))
		   (let ((k
			  (lambda (x y)
			    (for-each (lambda (y) (set-cdr! y x)) y)
			    (set-cdr! (last-pair x) y))))
		     (if (news-thread:< (car x) (car y))
			 (k (cdr x) (cdr y))
			 (k (cdr y) (cdr x)))))))))
      (for-each (lambda (entry)
		  (let ((thread (cadr entry)))
		    (for-each (lambda (thread*) (equivalence! thread thread*))
			      (cddr entry))))
		subject-alist))
    (map (lambda (class) (map car class))
	 (eliminate-duplicates
	  (map cdr (hash-table/datum-list equivalences))))))

(define (eliminate-duplicates items)
  (let loop ((items items) (result '()))
    (if (null? items)
	result
	(loop (cdr items)
	      (if (memq (car items) result)
		  result
		  (cons (car items) result))))))

(define (make-threads-equivalent! threads)
  (let ((threads (sort threads news-thread:<)))
    (let ((thread (car threads))
	  (threads (cdr threads)))
      (for-each (lambda (thread*)
		  (news-thread:for-each-header thread*
		    (lambda (header)
		      (set-news-header:thread! header thread))))
		threads)
      (set-cdr! (news-thread:root-headers thread)
		(map (lambda (thread)
		       (car (news-thread:root-headers thread)))
		     threads))
      thread)))

;;;; Miscellaneous

(define (input-port/read-line port)
  (let ((line (input-port/read-string port char-set:newline)))
    ;; Discard delimiter, if any -- this is a no-op at EOF.
    (input-port/discard-char port)
    line))

(define (input-port/discard-line port)
  (input-port/discard-chars port char-set:newline)
  (input-port/discard-char port))

(define char-set:newline (char-set #\newline))

(define (input-port/eof? port)
  ((or (port/operation port 'EOF?) (error "Port missing EOF? operation:" port))
   port))

(define (string-tokenize string)
  (substring-tokenize string 0 (string-length string)))

(define (substring-tokenize string start end)
  (reverse! (reverse-substring-tokenize string start end)))

(define (reverse-string-tokenize string)
  (reverse-substring-tokenize string 0 (string-length string)))

(define (reverse-substring-tokenize string start end)
  (let loop ((start start) (tokens '()))
    (if (fix:= start end)
	tokens
	(let ((delimiter
	       (or (substring-find-next-char-in-set
		    string start end char-set:whitespace)
		   end)))
	  (loop (or (substring-find-next-char-in-set
		     string delimiter end char-set:not-whitespace)
		    end)
		(cons (substring string start delimiter) tokens))))))

(define (token->number token)
  (substring->nonnegative-integer token 0 (string-length token)))

(define (substring->nonnegative-integer line start end)
  (let ((get-digit
	 (lambda (index)
	   (let ((n
		  (fix:- (char->integer (string-ref line index))
			 (char->integer #\0))))
	     (if (or (fix:< n 0) (fix:> n 9))
		 (error:bad-range-argument line #f))
	     n))))
    (let loop ((index start) (n 0))
      (if (fix:= index end)
	  n
	  (loop (fix:+ index 1)
		(+ (* n 10) (get-digit index)))))))

(define (substring-skip-leading-space string start end)
  (let loop ((index start))
    (if (and (fix:< index end)
	     (or (char=? #\space (string-ref string index))
		 (char=? #\tab (string-ref string index))))
	(loop (fix:+ index 1))
	index)))

(define (substring-skip-trailing-space string start end)
  (let loop ((index end))
    (if (fix:< start index)
	(let ((index* (fix:- index 1)))
	  (if (or (char=? #\space (string-ref string index*))
		  (char=? #\tab (string-ref string index*)))
	      (loop index*)
	      index))
	index)))

(define (substring-trim string start end)
  (let ((start (substring-skip-leading-space string start end)))
    (substring string start (substring-skip-trailing-space string start end))))