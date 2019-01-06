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

;;;; 6.002ex database support

(declare (usual-integrations))

(define database-connections '())
(define *database-connection* #f)
(define *user-name*)
(define *ps-number*)
(define *page-key*)

(define (with-database-connection ps-number thunk)
  (fluid-let ((*database-connection* (open-database-connection))
	      (*user-name* (http-request-user-name))
	      (*ps-number* ps-number)
	      (*page-key* (http-request-url)))
    (let ((commit? #f))
      (dynamic-wind (lambda ()
		      (db-run-cmd "BEGIN"))
		    (lambda ()
		      (let ((v (thunk)))
			(set! commit? #t)
			v))
		    (lambda ()
		      (db-run-cmd (if commit? "COMMIT" "ROLLBACK")))))))

(define (database-connection)
  (if (not *database-connection*)
      (error "No database connection available."))
  (if (not (pgsql-conn-open? *database-connection*))
      (set! *database-connection* (open-database-connection)))
  *database-connection*)

(define (open-database-connection)
  (let ((name (url-binding-value (http-request-url) 'xdoc-db-name #t)))
    (let ((p (assoc name database-connections)))
      (if (and p (pgsql-conn-open? (cdr p)))
	  (cdr p)
	  (let ((connection (open-pgsql-conn (string-append "dbname=" name))))
	    (set! database-connections
		  (cons (cons name connection)
			database-connections))
	    connection)))))

(define (close-database)
  (do () ((not (pair? database-connections)))
    (let ((connection (cdar database-connections)))
      (set! database-connections (cdr database-connections))
      (if (pgsql-conn-open? connection)
	  (close-pgsql-conn connection)))))

(define (db-run-query . strings)
  (let ((query (string-append (apply string-append strings) ";")))
    (if debug-queries?
	(write-line `(DB-RUN-QUERY ,query)))
    (exec-pgsql-query (database-connection) query)))

(define debug-queries? #f)

(define (db-run-cmd . strings)
  (let ((result (apply db-run-query strings)))
    (let ((status (pgsql-cmd-status result)))
      (pgsql-clear result)
      status)))

(define (db-quote object)
  (if object
      (if (exact-integer? object)
	  (number->string object)
	  (string-append "'"
			 (escape-pgsql-string
			  (if (symbol? object)
			      (symbol->string object)
			      object))
			 "'"))
      "NULL"))

;;;; Problem-set registration

(define (db-register-problem-set ps-number directory)
  (db-run-cmd "DELETE FROM saved_inputs"
	      " WHERE ps_number = " (db-quote ps-number))
  (db-run-cmd "DELETE FROM saved_outputs"
	      " WHERE ps_number = " (db-quote ps-number))
  (db-run-cmd "DELETE FROM registered_outputs"
	      " WHERE ps_number = " (db-quote ps-number))
  (let ((n-parts 0)
	(n-outputs 0))
    (for-each (lambda (pathname)
		(if (not (string=? (pathname-name pathname) "index"))
		    (begin
		      (set! n-parts (+ n-parts 1))
		      (set! n-outputs
			    (+ n-outputs
			       (register-part-outputs ps-number
						      pathname)))))
		unspecific)
	      (directory-read (merge-pathnames "*.xdoc" directory)))
    (values n-parts n-outputs)))

(define (register-part-outputs ps-number pathname)
  (with-xdoc-expansion-context ps-number pathname
    (lambda (document)
      (db-run-cmd "DELETE FROM persistent_values"
		  " WHERE file_name = " (db-quote *page-key*))
      (let ((root (xml-document-root document)))
	(let ((ps-number* (int0-attribute 'problem-set root #t)))
	  (if (not (= ps-number* ps-number))
	      (error "Document has wrong problem-set number:"
		     (file-namestring pathname))))
	(let ((part (xdoc-db-id root))
	      (n-outputs 0))
	  (let loop ((elt root))
	    (for-each
	     (lambda (item)
	       (if (xml-element? item)
		   (begin
		     (if (xdoc-output? item)
			 (begin
			   (set! n-outputs (+ n-outputs 1))
			   (register-output
			    ps-number
			    (xdoc-db-id item)
			    part
			    (eq? (or (boolean-attribute 'graded item #f) 'true)
				 'true))))
		     (loop item))))
	     (xml-element-contents elt)))
	  n-outputs)))))

(define (register-output ps-number name part graded?)
  (db-run-cmd "INSERT INTO registered_outputs VALUES"
	      " (" (db-quote ps-number)
	      ", " (db-quote name)
	      ", " (db-quote part)
	      ", " (if graded? "TRUE" "FALSE")
	      ")"))

(define (db-registered-problem-sets)
  (let ((result
	 (db-run-query "SELECT DISTINCT ps_number"
		       " FROM registered_outputs"
		       " ORDER BY ps_number")))
    (let ((n (pgsql-n-tuples result)))
      (do ((i 0 (+ i 1))
	   (numbers '()
		    (cons (string->number (pgsql-get-value result i 0))
			  numbers)))
	  ((= i n)
	   (pgsql-clear result)
	   (reverse! numbers))))))

(define (db-ps-problem-names ps-number)
  (let ((result
	 (db-run-query "SELECT name"
		       " FROM registered_outputs"
		       " WHERE ps_number = " (db-quote ps-number))))
    (let ((n (pgsql-n-tuples result)))
      (do ((i 0 (+ i 1))
	   (names '() (cons (pgsql-get-value result i 0) names)))
	  ((= i n)
	   (pgsql-clear result)
	   names)))))

(define (db-problem-submitted? ps-number name user-name)
  (let ((result
	 (db-run-query "SELECT submitter"
		       " FROM saved_outputs"
		       " WHERE ps_number = " (db-quote ps-number)
		       " AND name = " (db-quote name)
		       " AND user_name = " (db-quote user-name))))
    (let ((submitted?
	   (and (> (pgsql-n-tuples result) 0)
		(let ((v (pgsql-get-value result 0 0)))
		  (and v
		       (not (string-null? v)))))))
      (pgsql-clear result)
      submitted?)))

(define (db-get-ps-structure)
  (let ((result
	 (db-run-query "SELECT ps_number, ps_part, name"
		       " FROM registered_outputs"
		       " WHERE graded_p"
		       " ORDER BY ps_number, ps_part, name")))
    (let ((n (pgsql-n-tuples result)))
      (do ((i 0 (+ i 1))
	   (items '()
		  (cons (vector (string->number (pgsql-get-value result i 0))
				(pgsql-get-value result i 1)
				(pgsql-get-value result i 2))
			items)))
	  ((= i n)
	   (pgsql-clear result)
	   (ps-structure->tree (reverse! items)))))))

(define (ps-structure->tree items)
  (map (lambda (pset)
	 (cons (vector-ref (car pset) 0)
	       (map (lambda (vs)
		      (cons (vector-ref (car vs) 1)
			    (map (lambda (v) (vector-ref v 2)) vs)))
		    (chop-into-pieces! pset
		      (lambda (a b)
			(string=? (vector-ref a 1) (vector-ref b 1)))))))
       (chop-into-pieces! items
	 (lambda (a b)
	   (= (vector-ref a 0) (vector-ref b 0))))))

(define (chop-into-pieces! items predicate)
  (let loop ((items items) (pieces '()))
    (if (pair? items)
	(receive (head items) (chop-off-head! items predicate)
	  (loop items (cons head pieces)))
	(reverse! pieces))))

(define (chop-off-head! head predicate)
  (let loop ((items (cdr head)) (tail head))
    (if (pair? items)
	(if (predicate (car items) (car head))
	    (loop (cdr items) items)
	    (begin
	      (set-cdr! tail '())
	      (values head items)))
	(values head items))))

;;;; Saved inputs

(define (db-previously-saved-input id)
  (let ((result (db-run-query (saved-inputs-query id '(value submitter) #f))))
    (if (> (pgsql-n-tuples result) 0)
	(let ((value (pgsql-get-value result 0 0))
	      (submitter (pgsql-get-value result 0 1)))
	  (pgsql-clear result)
	  (values value (and submitter (string->symbol submitter))))
	(begin
	  (pgsql-clear result)
	  (values #f #f)))))

(define (db-save-input! id value submitter)
  (case (input-submission-status id #t)
    ((#f)
     (db-run-cmd "INSERT INTO saved_inputs VALUES"
		 " (" (db-quote *user-name*)
		 ", " (db-quote *ps-number*)
		 ", " (db-quote id)
		 ", " (db-quote value)
		 ", " (db-quote submitter)
		 ", " (db-quote (and submitter "NOW"))
		 ")"))
    ((not-submitted)
     (db-run-cmd "UPDATE saved_inputs SET"
		 " value = " (db-quote value)
		 ", submitter = " (db-quote submitter)
		 ", submission_time = " (db-quote (and submitter "NOW"))
		 " WHERE " (saved-inputs-condition id))))
  (db-run-cmd "INSERT INTO input_history VALUES"
	      " (" (db-quote *user-name*)
	      ", " (db-quote *ps-number*)
	      ", " (db-quote id)
	      ", " (db-quote "NOW")
	      ", " (db-quote value)
	      ")"))

(define (input-submission-status id for-update?)
  (let ((result
	 (db-run-query (saved-inputs-query id '(submitter) for-update?))))
    (let ((status
	   (and (> (pgsql-n-tuples result) 0)
		(if (pgsql-get-is-null? result 0 0)
		    'not-submitted
		    'submitted))))
      (pgsql-clear result)
      status)))

(define (saved-inputs-query id fields for-update?)
  (string-append "SELECT " (field-list->db-string fields)
		 " FROM saved_inputs"
		 " WHERE " (saved-inputs-condition id)
		 (if for-update? " FOR UPDATE" "")))

(define (saved-inputs-condition id)
  (string-append "user_name = " (db-quote *user-name*)
		 " AND ps_number = " (db-quote *ps-number*)
		 " AND name = " (db-quote id)))

;;;; Saved outputs

(define (db-previously-saved-output id)
  (let ((result
	 (db-run-query (saved-outputs-query id '(correctness submitter) #f))))
    (if (> (pgsql-n-tuples result) 0)
	(let ((correctness (pgsql-get-value result 0 0))
	      (submitter (pgsql-get-value result 0 1)))
	  (pgsql-clear result)
	  (values correctness (and submitter (string->symbol submitter))))
	(begin
	  (pgsql-clear result)
	  (values #f #f)))))

(define (db-save-output! id correctness submitter late?)
  (case (output-submission-status id #t)
    ((#f)
     (db-run-cmd "INSERT INTO saved_outputs VALUES"
		 " (" (db-quote *user-name*)
		 ", " (db-quote *ps-number*)
		 ", " (db-quote id)
		 ", " (db-quote correctness)
		 ", " (db-quote submitter)
		 ", " (if late? "TRUE" "FALSE")
		 ", " (db-quote (and submitter "NOW"))
		 ")"))
    ((not-submitted)
     (db-run-cmd "UPDATE saved_outputs SET"
		 " correctness = " (db-quote correctness)
		 ", submitter = " (db-quote submitter)
		 ", late_p = " (if late? "TRUE" "FALSE")
		 ", submission_time = " (db-quote (and submitter "NOW"))
		 " WHERE " (saved-outputs-condition id)))))

(define (output-submission-status id for-update?)
  (let ((result
	 (db-run-query (saved-outputs-query id '(submitter) for-update?))))
    (let ((status
	   (and (> (pgsql-n-tuples result) 0)
		(if (pgsql-get-is-null? result 0 0)
		    'not-submitted
		    'submitted))))
      (pgsql-clear result)
      status)))

(define (saved-outputs-query id fields for-update?)
  (string-append "SELECT " (field-list->db-string fields)
		 " FROM saved_outputs"
		 " WHERE " (saved-outputs-condition id)
		 (if for-update? " FOR UPDATE" "")))

(define (saved-outputs-condition id)
  (string-append "user_name = " (db-quote *user-name*)
		 " AND ps_number = " (db-quote *ps-number*)
		 " AND name = " (db-quote id)))

(define (db-get-saved-output user-name ps-number name)
  (let ((result
	 (db-run-query "SELECT correctness, submitter, late_p"
		       " FROM saved_outputs"
		       " WHERE user_name = " (db-quote user-name)
		       " AND ps_number = " (db-quote ps-number)
		       " AND name = " (db-quote name))))
    (if (> (pgsql-n-tuples result) 0)
	(let ((correctness (pgsql-get-value result 0 0))
	      (submitter (pgsql-get-value result 0 1))
	      (late? (string=? (pgsql-get-value result 0 2) "t")))
	  (pgsql-clear result)
	  (values correctness
		  (and submitter (string->symbol submitter))
		  late?))
	(begin
	  (pgsql-clear result)
	  (values #f #f #f)))))

;;;; Persistent values

(define (db-get-persistent-value name default)
  (get-persistent-value name *page-key* default))

(define (db-set-persistent-value! name object)
  (set-persistent-value! name *page-key* object))

(define (db-intern-persistent-value! name get-object)
  (intern-persistent-value! name *page-key* get-object))

(define (db-delete-persistent-value! name)
  (delete-persistent-value! name *page-key*))

(define (db-get-global-value name default)
  (get-persistent-value name global-page-key default))

(define (db-set-global-value! name object)
  (set-persistent-value! name global-page-key object))

(define (db-intern-global-value! name get-object)
  (intern-persistent-value! name global-page-key get-object))

(define (db-delete-global-value! name)
  (delete-persistent-value! name global-page-key))

(define global-page-key
  "*global-page-key*")

(define (get-persistent-value name page-key default)
  (let ((result
	 (db-run-query
	  (persistent-value-query name page-key '(var_value) #f))))
    (let ((string
	   (and (> (pgsql-n-tuples result) 0)
		(pgsql-get-value result 0 0))))
      (pgsql-clear result)
      (if string
	  (read (open-input-string string))
	  default))))

(define (set-persistent-value! name page-key object)
  (let ((value (write-to-string object))
	(result
	 (db-run-query
	  (persistent-value-query name page-key '(var_value) #t))))
    (if (> (pgsql-n-tuples result) 0)
	(let ((same-value? (string=? (pgsql-get-value result 0 0) value)))
	  (pgsql-clear result)
	  (if (not same-value?)
	      (db-run-cmd "UPDATE persistent_values SET"
			  " var_value = "
			  (db-quote value)
			  " WHERE "
			  (persistent-value-condition name page-key))))
	(begin
	  (pgsql-clear result)
	  (db-run-cmd "INSERT INTO persistent_values VALUES"
		      " (" (db-quote *user-name*)
		      ", " (db-quote page-key)
		      ", " (db-quote name)
		      ", " (db-quote value)
		      ")")))))

(define (intern-persistent-value! name page-key get-object)
  (let ((result
	 (db-run-query
	  (persistent-value-query name page-key '(var_value) #t))))
    (if (> (pgsql-n-tuples result) 0)
	(let ((value (pgsql-get-value result 0 0)))
	  (pgsql-clear result)
	  (read (open-input-string value)))
	(begin
	  (pgsql-clear result)
	  (let ((object (get-object)))
	    (db-run-cmd "INSERT INTO persistent_values VALUES"
			" (" (db-quote *user-name*)
			", " (db-quote page-key)
			", " (db-quote name)
			", " (db-quote (write-to-string object))
			")")
	    object)))))

(define (delete-persistent-value! name page-key)
  (db-run-cmd "DELETE FROM persistent_values WHERE "
	      (persistent-value-condition name page-key)))

(define (persistent-value-query name page-key fields for-update?)
  (string-append "SELECT " (field-list->db-string fields)
		 " FROM persistent_values"
		 " WHERE " (persistent-value-condition name page-key)
		 (if for-update? " FOR UPDATE" "")))

(define (persistent-value-condition name page-key)
  (string-append "user_name = " (db-quote *user-name*)
		 " AND file_name = " (db-quote page-key)
		 " AND var_name = " (db-quote name)))

;;;; Clear submitted/late

(define (db-saved-submitters user-name)
  (db-marked-submitters user-name "submitter IS NOT NULL"))

(define (db-late-submitters user-name)
  (db-marked-submitters user-name "late_p"))

(define (db-marked-submitters user-name condition)
  (let ((result
	 (db-run-query "SELECT DISTINCT ps_number, submitter"
		       " FROM saved_outputs"
		       " WHERE user_name = " (db-quote user-name)
		       " AND " condition
		       " ORDER BY ps_number, submitter")))
    (let ((n (pgsql-n-tuples result)))
      (let loop ((i 0) (names '()))
	(if (< i n)
	    (loop (+ i 1)
		  (let ((submitter (pgsql-get-value result i 1)))
		    (if submitter
			(cons (string-append (pgsql-get-value result i 0)
					     "/"
					     submitter)
			      names)
			names)))
	    (begin
	      (pgsql-clear result)
	      (reverse! names)))))))

(define (db-clear-submitter user-name number)
  (receive (ps-number submitter) (parse-problem-number number)
    (db-run-cmd "UPDATE saved_inputs"
		" SET submitter = NULL"
		" WHERE user_name = " (db-quote user-name)
		" AND ps_number = " (db-quote ps-number)
		" AND submitter  = " (db-quote submitter))
    (db-set-output-field user-name ps-number submitter
			 "submitter = NULL")))

(define (db-clear-late-flag user-name number)
  (receive (ps-number submitter) (parse-problem-number number)
    (db-set-output-field user-name ps-number submitter "late_p = FALSE")))

(define (db-set-output-field user-name ps-number submitter assignment)
  (let ((result
	 (db-run-query "UPDATE saved_outputs"
		       " SET " assignment
		       " WHERE user_name = " (db-quote user-name)
		       " AND ps_number = " (db-quote ps-number)
		       " AND submitter  = " (db-quote submitter))))
    (let ((n (pgsql-cmd-tuples result)))
      (pgsql-clear result)
      n)))

;;;; Users

(define (db-known-user? user-name)
  (known-user? user-name #f))

(define (known-user? user-name for-update?)
  (let ((result
	 (db-run-query "SELECT enabled_p"
		       " FROM users"
		       " WHERE user_name = " (db-quote user-name)
		       (if for-update? " FOR UPDATE" ""))))
    (if (> (pgsql-n-tuples result) 0)
	(let ((enabled?
	       (if (string=? (pgsql-get-value result 0 0) "t")
		   #t
		   'disabled)))
	  (pgsql-clear result)
	  enabled?)
	(begin
	  (pgsql-clear result)
	  #f))))

(define (guarantee-known-user user-name)
  (if (not (known-user? user-name #t))
      (error "Unknown user:" user-name)))

(define (db-known-users condition)
  (let ((result
	 (db-run-query "SELECT user_name"
		       " FROM users"
		       (case condition
			 ((enabled) " WHERE enabled_p")
			 ((disabled) " WHERE NOT enabled_p")
			 (else ""))
		       " ORDER BY user_name")))
    (let ((n (pgsql-n-tuples result)))
      (let loop ((i 0) (users '()))
	(if (< i n)
	    (loop (+ i 1) (cons (pgsql-get-value result i 0) users))
	    (begin
	      (pgsql-clear result)
	      (reverse! users)))))))

(define (db-new-user-account user-name first-names last-name password enabled?)
  (if (known-user? user-name #t)
      #f
      (begin
	(db-run-cmd "INSERT INTO users VALUES"
		    " (" (db-quote user-name)
		    ", " (db-quote first-names)
		    ", " (db-quote last-name)
		    ", " (db-quote password)
		    ", " "FALSE"
		    ", " (if enabled? "TRUE" "FALSE")
		    ")")
	#t)))

(define (db-get-user-password user-name)
  (let ((result
	 (db-run-query "SELECT enabled_p, password"
		       " FROM users"
		       " WHERE user_name = " (db-quote user-name))))
    (if (> (pgsql-n-tuples result) 0)
	(let ((password
	       (and (string=? (pgsql-get-value result 0 0) "t")
		    (pgsql-get-value result 0 1))))
	  (pgsql-clear result)
	  password)
	(begin
	  (pgsql-clear result)
	  #f))))

(define (db-change-user-password user-name password)
  (guarantee-known-user user-name)
  (db-run-cmd "UPDATE users"
	      " SET password = " (db-quote password)
	      " WHERE user_name = " (db-quote user-name)))

(define (db-user-real-name user-name)
  (let ((result
	 (db-run-query "SELECT first_names, last_name"
		       " FROM users"
		       " WHERE user_name = " (db-quote user-name))))
    (if (> (pgsql-n-tuples result) 0)
	(let ((first (pgsql-get-value result 0 0))
	      (last (pgsql-get-value result 0 1)))
	  (pgsql-clear result)
	  (values first last))
	(begin
	  (pgsql-clear result)
	  (error "Unknown user:" user-name)
	  (values #f #f)))))

(define (db-set-user-real-name user-name first-names last-name)
  (guarantee-known-user user-name)
  (db-run-cmd "UPDATE users"
	      " SET first_names = " (db-quote first-names)
	      ", last_name = " (db-quote last-name)
	      " WHERE user_name = " (db-quote user-name)))

(define (db-user-enabled? user-name)
  (get-user-flag user-name "enabled_p"))

(define (db-user-administrator? user-name)
  (get-user-flag user-name "administrator_p"))

(define (db-set-user-enabled user-name value)
  (set-user-flag user-name "enabled_p" value))

(define (db-set-user-administrator user-name value)
  (set-user-flag user-name "administrator_p" value))

(define (get-user-flag user-name flag-name)
  (let ((result
	 (db-run-query "SELECT " flag-name
		       " FROM users"
		       " WHERE user_name = " (db-quote user-name))))
    (let ((string
	   (and (> (pgsql-n-tuples result) 0)
		(pgsql-get-value result 0 0))))
      (pgsql-clear result)
      (if (not string)
	  (error "Unknown user:" user-name))
      (string=? string "t"))))

(define (set-user-flag user-name flag-name value)
  (guarantee-known-user user-name)
  (db-run-cmd "UPDATE users"
	      " SET " flag-name " = " (if value "TRUE" "FALSE")
	      " WHERE user_name = " (db-quote user-name)))

(define (db-encrypt-password password)
  (let ((pw-line
	 (call-with-output-string
	   (lambda (port)
	     (let ((status
		    (run-shell-command (string-append "htpasswd -nb foo "
						      password)
				       'output port)))
	       (if (not (= status 0))
		   (error "Non-zero status from htpasswd:" status)))))))
    (if (not (and (string-prefix? "foo:" pw-line)
		  (string-suffix? "\n" pw-line)))
	(error "Unknown result from htpasswd:" pw-line))
    (substring pw-line 4 (fix:- (string-length pw-line) 1))))

(define (db-valid-password? string)
  (and (fix:>= (string-length string) 8)
       (not (string-find-next-char-in-set string char-set:not-password))
       (string-find-next-char-in-set string char-set:lower-case)
       (string-find-next-char-in-set string char-set:upper-case)
       (string-find-next-char-in-set string char-set:numeric)))

(define char-set:password
  (char-set-union char-set:alphanumeric
		  (string->char-set " _-.")))

(define char-set:not-password
  (char-set-invert char-set:password))

(define (db-generate-password)
  (string-append (string (integer->char (+ (char->integer #\A) (random 26))))
		 (string (integer->char (+ (char->integer #\a) (random 26))))
		 (random-digit-string 6)))

(define (random-digit-string n-chars)
  (string-pad-left (number->string (random (expt 10 n-chars))) n-chars #\0))

(define (parse-problem-number string)
  (let ((regs (re-string-match problem-number-regexp string)))
    (if (not regs)
	(error:bad-range-argument string 'parse-problem-number))
    (values (string->number (re-match-extract string regs 1))
	    (re-match-extract string regs 2))))

(define problem-number-regexp
  (rexp-compile
   (let ((int
	  (rexp-sequence (char-set-difference char-set:numeric (char-set #\0))
			 (rexp* char-set:numeric))))
     (rexp-sequence (rexp-string-start)
		    (rexp-group int)
		    "/"
		    (rexp-group (rexp-optional "xdoc_") int (rexp* "." int))
		    (rexp-string-end)))))

(define (field-list->db-string fields)
  (apply string-append
	 (cons (symbol->string (car fields))
	       (map (lambda (value)
		      (string-append ", " (symbol->string value)))
		    (cdr fields)))))