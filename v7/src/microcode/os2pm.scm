#| -*-Scheme-*-

$Id: os2pm.scm,v 1.9 2001/12/20 20:51:16 cph Exp $

Copyright (c) 1995-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Program to generate OS/2 PM interface code.

;;; The Scheme OS/2 Presentation Manager interface is implemented in
;;; its own thread, which means that all operations involving the
;;; interface must by encoded into messages and communicated to the PM
;;; thread through its message queue.  This is reasonably
;;; straightforward, but the overhead for implementing a single
;;; operation is daunting: in addition to the procedure that performs
;;; the operation, the implementer must also write two additional
;;; procedures, three function prototypes, one or two message-type
;;; declarations, one or two message-structure declarations, and one
;;; or two case statements in the message dispatch.  The purpose of
;;; this file is to generate all of the overhead code automatically
;;; from a simple interface definition; the implementer supplies the
;;; definition and the operation's procedure, and this program takes
;;; care of the rest of the details.

;;; The bulk of this file is the program to parse the interface
;;; specifications and to generate the appropriate code.  The
;;; specifications themselves appear on the last page of the file.

;;; To generate the output files, just load this file.  The output
;;; files will be written into the working directory.

(declare (usual-integrations))

(load-option 'HASH-TABLE)
(load-option 'FORMAT)

;;;; Syntax

(define-syntax define-pm-procedure
  (lambda (name . clauses)
    (let ((external-name (if (pair? name) (car name) name))
	  (internal-name (if (pair? name) (cadr name) name)))
      `(BEGIN
	 (HASH-TABLE/PUT! PM-PROCEDURES ',external-name
	   (MAKE-PMP (TRANSLATE-NAME ',external-name)
		     (TRANSLATE-NAME ',internal-name)
		     ,(let ((clause (assq 'VALUE clauses)))
			(if clause
			    (let ((val (cadr clause)))
			      (if (symbol? val)
				  (if (eq? val 'SYNC)
				      `',val
				      `(TRANSLATE-TYPE/NAME
					',`((ID ,val) ,val)))
				  `(TRANSLATE-TYPE/NAME ',val)))
			    '#F))
		     ,(let ((args
			     (let ((clause (assq 'ARGUMENTS clauses)))
			       (if (not clause)
				   (error "ARGUMENTS clause is required:" name))
			       (cdr clause))))
			`(CONS (TRANSLATE-TYPE/NAME
				',(if (symbol? (car args))
				      `((ID ,(car args)) ,(car args))
				      (car args)))
			       (LIST ,@(map (lambda (arg)
					      `(TRANSLATE-TYPE/NAME ',arg))
					    (cdr args)))))))
	 ',external-name))))

(define (translate-type/name tn)
  (cond ((and (pair? tn)
	      (pair? (cdr tn))
	      (null? (cddr tn)))
	 (list (translate-type (car tn))
	       (translate-name (cadr tn))))
	((and (pair? tn)
	      (pair? (cdr tn))
	      (pair? (cddr tn))
	      (null? (cdddr tn)))
	 (list (translate-type (car tn))
	       (translate-name (cadr tn))
	       (translate-name (caddr tn))))
	(else
	 (error "Ill-formed type/name pair:" tn))))

(define (translate-type type)
  (cond ((string? type)
	 type)
	((symbol? type)
	 (let ((abbrev (hash-table/get type-abbreviations type #f)))
	   (if abbrev
	       (translate-type abbrev)
	       (symbol->string type))))
	((and (pair? type)
	      (or (string? (car type))
		  (symbol? (car type)))
	      (pair? (cdr type))
	      (null? (cddr type)))
	 (if (eq? (car type) 'ID)
	     type
	     (list (if (or (string? (car type))
			   (memq (car type) '(POINTER ARRAY)))
		       (car type)
		       (symbol->string (car type)))
		   (translate-type (cadr type)))))
	((and (pair? type)
	      (eq? (car type) 'ARRAY)
	      (pair? (cdr type))
	      (pair? (cddr type))
	      (and (exact-integer? (caddr type))
		   (positive? (caddr type)))
	      (null? (cdddr type)))
	 (list (car type)
	       (translate-type (cadr type))
	       (number->string (caddr type))))
	(else
	 (error "Ill-formed type:" type))))

(define (translate-name name)
  (cond ((string? name)
	 name)
	((symbol? name)
	 (symbol->string name))
	(else
	 (error "Ill-formed name:" name))))

(define (define-type-abbreviation name type)
  (hash-table/put! type-abbreviations name type))

(define type-abbreviations
  (make-eq-hash-table))

(define-type-abbreviation 'boolean 'int)
(define-type-abbreviation 'uchar '(unsigned char))
(define-type-abbreviation 'ushort '(unsigned short))
(define-type-abbreviation 'uint '(unsigned int))
(define-type-abbreviation 'ulong '(unsigned long))

(define (id-type? type) (and (pair? type) (eq? (car type) 'ID)))
(define-integrable id-type-name cadr)

(define (pointer-type? type) (and (pair? type) (eq? (car type) 'POINTER)))
(define (array-type? type) (and (pair? type) (eq? (car type) 'ARRAY)))
(define-integrable subtype cadr)

(define (array-dimension type)
  (and (pair? (cddr type))
       (caddr type)))

(define (variable-length-array? arg)
  (let ((type (pmp-arg-type arg)))
    (and (array-type? type)
	 (not (array-dimension type)))))

;;;; ID Types

(define (define-id internal-root external-root)
  (hash-table/put! id-external-roots
		   internal-root
		   (symbol->string external-root)))

(define (id-internal-root type)
  (symbol->string (id-type-name type)))

(define (id-external-root type)
  (hash-table/get id-external-roots (id-type-name type) #f))

(define id-external-roots
  (make-eq-hash-table))

(define (id-external-type type)
  (list (id-external-root type) "_t"))

(define (id-internal-type type)
  (if (eq? (id-type-name type) 'QID)
      (id-external-type type)
      (list (id-internal-root type) "_t *")))

(define-integrable (id-internal-name arg)
  (pmp-arg-name arg))

(define (id-external-name arg)
  (if (eq? (id-type-name (pmp-arg-type arg)) 'QID)
      (pmp-arg-name arg)
      (list (pmp-arg-name arg) "_id")))

(define (id-internal-expression arg)
  (let ((type (pmp-arg-type arg)))
    (if (eq? (id-type-name type) 'QID)
	(id-external-name arg)
	(list "("
	      (id-external-root type)
	      "_to_"
	      (id-internal-root type)
	      " ("
	      (id-external-name arg)
	      "))"))))

(define (id-external-expression arg)
  (let ((type (pmp-arg-type arg)))
    (if (eq? (id-type-name type) 'QID)
	(id-internal-name arg)
	(list "("
	      (string-upcase (id-internal-root type))
	      "_ID ("
	      (id-internal-name arg)
	      "))"))))

(define (id-qid-expression arg)
  (let ((type (pmp-arg-type arg)))
    (if (eq? (id-type-name type) 'QID)
	(id-internal-name arg)
	(list "("
	      (string-upcase (id-internal-root type))
	      "_QID ("
	      (id-internal-name arg)
	      "))"))))

(define-id 'QID 'QID)
(define-id 'WINDOW 'WID)
(define-id 'PS 'PSID)
(define-id 'BITMAP 'BID)

;;;; PM Procedures

(define pm-procedures
  (make-eq-hash-table))

(define-structure pmp
  (root-name #f read-only #t)
  (internal-name #f read-only #t)
  (value #f read-only #t)
  (arguments #f read-only #t))

(define-integrable pmp-arg-type car)
(define-integrable pmp-arg-name cadr)
(define-integrable (pmp-value? pmp) (pair? (pmp-value pmp)))
(define-integrable (pmp-sync? pmp) (eq? (pmp-value pmp) 'SYNC))

(define (pmp-arg-size-name arg)
  (and (not (null? (cddr arg)))
       (caddr arg)))

(define (pmp-request-struct-name pmp)
  (list "sm_" (pmp-root-name pmp) "_request_t"))

(define (pmp-reply-struct-name pmp)
  (list "sm_" (pmp-root-name pmp) "_reply_t"))

(define (pmp-request-message-name pmp)
  (list "mt_" (pmp-root-name pmp) "_request"))

(define (pmp-reply-message-name pmp)
  (list "mt_" (pmp-root-name pmp) "_reply"))

(define (pmp-external-name pmp)
  (list "OS2_" (pmp-root-name pmp)))

(define (pmp-request-handler-name pmp)
  (list "handle_" (pmp-root-name pmp) "_request"))

(define (for-each-pmp procedure)
  (for-each procedure
	    (sort (hash-table/datum-list pm-procedures)
		  (lambda (x y)
		    (string<? (pmp-root-name x) (pmp-root-name y))))))

;;;; Printing

(define (print tree port)
  (if (list? tree)
      (for-each (lambda (element) (print element port)) tree)
      (display tree port)))

(define (indent n . tree)
  (let ((indent (make-string n #\space)))
    (let at-line-start ((objects (flatten-for-indentation tree)))
      (if (null? objects)
	  '()
	  (cons indent
		(let in-line ((objects objects))
		  (cons (car objects)
			(cond ((eqv? (car objects) #\newline)
			       (at-line-start (cdr objects)))
			      ((null? (cdr objects))
			       '())
			      (else
			       (in-line (cdr objects)))))))))))

(define (indent-following n . tree)
  (let ((indent (make-string n #\space)))
    (let in-line ((objects (flatten-for-indentation tree)))
      (cons (car objects)
	    (cond ((eqv? (car objects) #\newline)
		   (let at-line-start ((objects (cdr objects)))
		     (if (null? objects)
			 '()
			 (cons indent (in-line objects)))))
		  ((null? (cdr objects))
		   '())
		  (else
		   (in-line (cdr objects))))))))

(define (flatten-for-indentation tree)
  (cond ((list? tree)
	 (append-map flatten-for-indentation tree))
	((string? tree)
	 (reveal-embedded-newlines tree))
	(else
	 (list tree))))

(define (reveal-embedded-newlines string)
  (let ((indices (find-embedded-newlines string)))
    (if (null? indices)
	(list string)
	(let loop ((start 0) (indices indices))
	  (if (null? indices)
	      (list (string-tail string start))
	      (cons* (substring string start (car indices))
		     #\newline
		     (loop (fix:+ (car indices) 1) (cdr indices))))))))

(define (find-embedded-newlines string)
  (let ((end (string-length string)))
    (let loop ((start 0))
      (let ((index (substring-find-next-char string start end #\newline)))
	(if index
	    (cons index (loop (fix:+ index 1)))
	    '())))))

(define (first-char-in-tree tree)
  (cond ((list? tree)
	 (and (pair? tree)
	      (or (first-char-in-tree (car tree))
		  (first-char-in-tree (cdr tree)))))
	((string? tree)
	 (and (not (string-null? tree))
	      (string-ref tree 0)))
	((char? tree) tree)
	(else #f)))

;;;; C Syntax Combinators

(define (brace-group . body)
  (list "{" #\newline
	(apply indent 2 body)
	"}" #\newline))

(define (statement . elements)
  (list elements ";" #\newline))

(define (assignment target source)
  (statement target " = " source))

(define (indented-assignment target source)
  (statement target #\newline "  = " (indent-following 4 source)))

(define (function name static? value arguments . body)
  (list (if static? "static " "") value #\newline
	name " " arguments #\newline
	(apply brace-group body)))

(define (funcall function . arguments)
  (list "(" function " " (funcall-arguments arguments) ")"))

(define (indented-funcall function . arguments)
  (list "(" function #\newline (indent 2 (funcall-arguments arguments) ")")))

(define (call function . arguments)
  (statement function " " (funcall-arguments arguments)))

(define (indented-call function . arguments)
  (statement function #\newline (indent 2 (funcall-arguments arguments))))

(define (funcall-arguments arguments)
  (cond ((null? arguments)
	 (list "()"))
	((null? (cdr arguments))
	 (list (guarantee-parentheses (car arguments))))
	(else
	 (let loop ((arguments arguments) (prefix "("))
	   (cons* prefix
		  (car arguments)
		  (if (null? (cdr arguments))
		      (list ")")
		      (loop (cdr arguments) ", ")))))))

(define (guarantee-parentheses expression)
  (if (eqv? #\( (first-char-in-tree expression))
      expression
      (list "(" expression ")")))

(define (cast type expression)
  (list "((" type ") " expression ")"))

;;;; Per-Procedure Output

(define (generate-message-types pmp)
  (cons* "  " (pmp-request-message-name pmp) "," #\newline
	 (if (pmp-value? pmp)
	     (list "  " (pmp-reply-message-name pmp) "," #\newline)
	     '())))

(define (generate-handler-prototype pmp)
  (statement "static void "
	     (pmp-request-handler-name pmp)
	     #\newline "  ("
	     (pmp-request-struct-name pmp)
	     " *)"))

(define (generate-prototype pmp external?)
  (statement (if external? "extern" "static")
	     " "
	     (val-type pmp external?)
	     " "
	     (if external? (pmp-external-name pmp) (pmp-internal-name pmp))
	     #\newline "  "
	     (arg-declarators (pmp-arguments pmp) external? #f)))

(define (generate-message-initializers pmp)
  (indent 2
	  (let ((generate-init
		 (lambda (mn sn)
		   (statement "SET_MSG_TYPE_LENGTH (" mn "," #\newline
			      "                     " sn ")"))))
	    (list (generate-init (pmp-request-message-name pmp)
				 (pmp-request-struct-name pmp))
		  (if (pmp-value? pmp)
		      (generate-init (pmp-reply-message-name pmp)
				     (pmp-reply-struct-name pmp))
		      '())))))

(define (generate-dispatch-case pmp)
  (indent 8
	  "case " (pmp-request-message-name pmp) ":" #\newline
	  (indent 2
		  (indented-call
		   (pmp-request-handler-name pmp)
		   (cast (list (pmp-request-struct-name pmp) " *")
			 "message"))
		  (statement "break"))))

(define (generate-struct-definitions pmp)
  (list (generate-struct-definition
	 (pmp-request-struct-name pmp)
	 (map (lambda (arg)
		(let ((type (pmp-arg-type arg)))
		  (if (array-type? type)
		      (list (arg-type-1 (subtype type))
			    " "
			    (arg-name arg #f)
			    " ["
			    (or (array-dimension type) "1")
			    "]")
		      (arg-declarator arg #f))))
	      (let ((args (pmp-arguments pmp)))
		(let ((array
		       (list-search-positive args
			 variable-length-array?)))
		  (if array
		      (append (delq array args) (list array))
		      args)))))
	(if (pmp-value? pmp)
	    (list #\newline
		  (generate-struct-definition
		   (pmp-reply-struct-name pmp)
		   (list (arg-declarator (pmp-value pmp) #f))))
	    '())))

(define (generate-struct-definition name elements)
  (statement "typedef struct" #\newline
	     "{" #\newline
	     (indent 2
		     (map statement
			  (cons "DECLARE_MSG_HEADER_FIELDS" elements)))
	     "}" " " name))

(define (generate-request-procedure pmp)
  (let ((args (pmp-arguments pmp)))
    (function (pmp-external-name pmp)
	      #f
	      (val-type pmp #t)
	      (arg-declarators args #t #t)
	      (map (lambda (arg)
		     (let ((type (pmp-arg-type arg)))
		       (if (and (id-type? type)
				(not (eq? (id-type-name type) 'QID)))
			   (assignment (arg-declarator arg #f)
				       (id-internal-expression arg))
			   '())))
		   args)
	      (indented-assignment
	       (list (pmp-request-struct-name pmp) " * request")
	       (message-creator pmp
				(pmp-request-struct-name pmp)
				(pmp-request-message-name pmp)
				(request-extra pmp)))
	      (map (lambda (arg) (request-initializer pmp arg)) args)
	      (if (pmp-value? pmp)
		  (let ((val (pmp-value pmp)))
		    (brace-group
		     (indented-assignment
		      (list (pmp-reply-struct-name pmp) " * reply")
		      (indented-funcall
		       "MESSAGE_TRANSACTION"
		       (id-qid-expression (car (pmp-arguments pmp)))
		       "request"
		       (pmp-reply-message-name pmp)))
		     (assignment (arg-declarator val #f)
				 (reply-accessor val))
		     (call "DESTROY_MESSAGE" "reply")
		     (call "return"
			   (if (id-type? (pmp-arg-type val))
			       (id-external-expression val)
			       (arg-name val #f)))))
		  (call (if (pmp-sync? pmp)
			    "SYNC_TRANSACTION"
			    "SIMPLE_TRANSACTION")
			(id-qid-expression (car (pmp-arguments pmp)))
			"request")))))

(define (request-extra pmp)
  (let ((array-arg
	 (list-search-positive (pmp-arguments pmp)
	   variable-length-array?)))
    (and array-arg
	 (let ((size (pmp-arg-size-name array-arg)))
	   (if size
	       (list "(" size " - 1)")
	       (funcall "strlen" (arg-name array-arg #f)))))))

(define (request-initializer pmp arg)
  (if (array-type? (pmp-arg-type arg))
      (let ((source (arg-name arg #t))
	    (target (request-accessor arg))
	    (size (pmp-arg-size-name arg)))
	(if size
	    (call "MEMCPY"
		  target
		  source
		  (list "((sizeof ("
			(arg-type-1 (subtype (pmp-arg-type arg)))
			")) * "
			size
			")"))
	    (call "STRCPY" target source)))
      (assignment (request-accessor arg) (arg-name arg #f))))

(define (generate-request-handler pmp)
  (function (pmp-request-handler-name pmp)
	    #t
	    "void"
	    (list "(" (list (pmp-request-struct-name pmp) " * request") ")")
	    (assignment "qid_t sender" (funcall "MSG_SENDER" "request"))
	    (if (pmp-value? pmp)
		(list (indented-assignment
		       (list (pmp-reply-struct-name pmp) " * reply")
		       (message-creator pmp
					(pmp-reply-struct-name pmp)
					(pmp-reply-message-name pmp)
					#f))
		      (indented-assignment
		       (reply-accessor (pmp-value pmp))
		       (apply indented-funcall
			      (pmp-internal-name pmp)
			      (map (lambda (arg)
				     (request-accessor arg))
				   (pmp-arguments pmp))))
		      (call "DESTROY_MESSAGE" "request")
		      (call "SEND_MESSAGE" "sender" "reply"))
		(list (apply indented-call
			     (pmp-internal-name pmp)
			     (map (lambda (arg) (request-accessor arg))
				  (pmp-arguments pmp)))
		      (call "DESTROY_MESSAGE" "request")
		      (call (if (pmp-sync? pmp) "sync_reply" "simple_reply")
			    "sender")))))

(define (message-creator pmp struct-type message-type extra)
  (if extra
      (funcall "CREATE_MESSAGE_1" message-type extra)
      (funcall "CREATE_MESSAGE" message-type)))

(define (request-accessor arg)
  (message-accessor "request" arg))

(define (reply-accessor arg)
  (message-accessor "reply" arg))

(define (message-accessor message-name arg)
  (list "(" message-name " -> " (arg-name arg #f) ")"))

(define (val-type pmp external?)
  (if (pmp-value? pmp)
      (arg-type (pmp-value pmp) external?)
      "void"))

(define (arg-declarator arg external?)
  (list (arg-type arg external?)
	" "
	(arg-name arg external?)))

(define (arg-declarators args external? names?)
  (if (null? args)
      "(void)"
      (let ((do-arg
	     (lambda (arg)
	       (if names?
		   (arg-declarator arg external?)
		   (arg-type arg external?)))))
	(cons* "("
	       (do-arg (car args))
	       (let loop ((args (cdr args)))
		 (if (null? args)
		     (list ")")
		     (cons* ", "
			    (do-arg (car args))
			    (loop (cdr args)))))))))

(define (arg-type arg external?)
  (let ((type (pmp-arg-type arg)))
    (if (id-type? type)
	(if external?
	    (id-external-type type)
	    (id-internal-type type))
	(arg-type-1 type))))

(define (arg-type-1 type)
  (if (pair? type)
      (case (car type)
	((POINTER ARRAY)
	 (list (arg-type-1 (subtype type)) " *"))
	(else
	 (list (car type) " " (arg-type-1 (subtype type)))))
      type))

(define (arg-name arg external?)
  (let ((name (pmp-arg-name arg)))
    (if (id-type? (pmp-arg-type arg))
	(if external?
	    (id-external-name arg)
	    (id-internal-name arg))
	(pmp-arg-name arg))))

;;;; Top-Level Output

(define (generate-file filename per-pmp)
  (call-with-output-file filename
    (lambda (port)
      (let ((time (get-decoded-time)))
	(format port
		file-header-format-string
		(decoded-time/date-string time)
		(decoded-time/time-string time)
		(current-user-name)
		(decoded-time/year time)))
      (for-each-pmp (lambda (pmp) (print (per-pmp pmp) port))))))

(define file-header-format-string
  "/* -*-C-*-

**** Do not edit this file.  It was generated by a program,
**** on ~A at ~A by ~a.

Copyright (c) ~A Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/
")

(define (write-message-types-file)
  (generate-file "os2pm-mt.h" generate-message-types))

(define (write-external-declarations-file)
  (generate-file "os2pm-ed.h"
    (lambda (pmp)
      (list #\newline
	    (generate-prototype pmp #t)))))

(define (write-internal-declarations-file)
  (generate-file "os2pm-id.h"
    (lambda (pmp)
      (list #\newline
	    (generate-struct-definitions pmp)
	    #\newline
	    (generate-handler-prototype pmp)
	    #\newline
	    (generate-prototype pmp #f)))))

(define (write-message-initializers-file)
  (generate-file "os2pm-mi.h" generate-message-initializers))

(define (write-dispatch-cases-file)
  (generate-file "os2pm-dc.h" generate-dispatch-case))

(define (write-request-procedures-file)
  (generate-file "os2pm-rp.h"
    (lambda (pmp)
      (list #\newline
	    (generate-request-procedure pmp)
	    #\newline
	    (generate-request-handler pmp)))))

(define (write-all-files)
  (write-message-types-file)
  (write-external-declarations-file)
  (write-internal-declarations-file)
  (write-message-initializers-file)
  (write-dispatch-cases-file)
  (write-request-procedures-file))

;;;; Interface Definitions

(define-pm-procedure pm_synchronize
  (value sync)
  (arguments qid))

;;; Windows

(define-pm-procedure window_open
  (value ("wid_t" wid))
  (arguments qid
	     (qid_t event_qid)
	     (ulong flags)
	     ("HMODULE" module)
	     (ulong id)
	     (ulong style)
	     ((array (const char)) title)))

(define-pm-procedure window_close
  (arguments window))

(define-pm-procedure window_show
  (arguments window (boolean showp)))

(define-pm-procedure window_scroll
  (arguments window
	     (short xl)
	     (short xh)
	     (short yl)
	     (short yh)
	     (short x_delta)
	     (short y_delta)))

(define-pm-procedure window_invalidate
  (arguments window (short xl) (short xh) (short yl) (short yh)))

(define-pm-procedure window_set_grid
  (arguments window (ushort x) (ushort y)))

(define-pm-procedure window_activate
  (arguments window))

;;; (define_pm_procedure window_pos ...)

(define-pm-procedure window_set_pos
  (arguments window (short x) (short y)))

;;; (define_pm_procedure window_size ...)
;;; (define_pm_procedure window_frame_size ...)

(define-pm-procedure window_set_size
  (arguments window (ushort x) (ushort y)))

(define-pm-procedure window_focusp
  (value (boolean focusp))
  (arguments window))

(define-pm-procedure window_set_state
  (arguments window (window_state_t state)))

(define-pm-procedure window_set_title
  (arguments window ((array (const char)) title)))

(define-pm-procedure window_update_frame
  (arguments window (ushort flags)))

(define-pm-procedure window_handle_from_id
  (value ("HWND" child))
  (arguments qid ("HWND" parent) (ulong id)))

(define-pm-procedure window_set_capture
  (value ("BOOL" successp))
  (arguments window (int capturep)))

(define-pm-procedure window_query_sys_value
  (value ("LONG" sysval))
  (arguments qid ("HWND" window) ("LONG" id)))

;;; Text Cursors

(define-pm-procedure window_move_cursor
  (arguments window (short x) (short y)))

(define-pm-procedure window_shape_cursor
  (arguments window (ushort width) (ushort height) (ushort style)))

(define-pm-procedure window_show_cursor
  (arguments window (boolean showp)))

;;; Presentation Spaces

(define-pm-procedure create_memory_ps
  (value ps)
  (arguments qid))

(define-pm-procedure destroy_memory_ps
  (arguments ps))

(define-pm-procedure create_bitmap
  (value bitmap)
  (arguments ps (ushort width) (ushort height)))

(define-pm-procedure destroy_bitmap
  (arguments bitmap))

;;; (define_pm_procedure ps_set_bitmap ...)

(define-pm-procedure ps_bitblt
  (arguments ((id ps) target)
	     ((id ps) source)
	     (long npoints)
	     ((array "POINTL" 4) points npoints)
	     (long rop)
	     (ulong options)))

(define-pm-procedure ps_draw_text
  (arguments ps
	     (short x)
	     (short y)
	     ((array (const char)) data size)
	     (ushort size)))

(define-pm-procedure ps_text_width
  (value (ushort width))
  (arguments ps
	     ((array (const char)) data size)
	     (ushort size)))

(define-pm-procedure ps_clear
  (arguments ps (short xl) (short xh) (short yl) (short yh)))

(define-pm-procedure ps_get_foreground_color
  (value ("COLOR" color))
  (arguments ps))

(define-pm-procedure ps_get_background_color
  (value ("COLOR" color))
  (arguments ps))

(define-pm-procedure ps_set_colors
  (arguments ps ("COLOR" foreground) ("COLOR" background)))

(define-pm-procedure ps_move_gcursor
  (arguments ps (short x) (short y)))

(define-pm-procedure ps_draw_line
  (arguments ps (short x) (short y)))

(define-pm-procedure ps_draw_point
  (arguments ps (short x) (short y)))

(define-pm-procedure ps_poly_line
  (value sync)
  (arguments ps
	     (ulong npoints)
	     ((pointer "POINTL") points)))

(define-pm-procedure ps_poly_line_disjoint
  (value sync)
  (arguments ps
	     (ulong npoints)
	     ((pointer "POINTL") points)))

(define-pm-procedure ps_set_line_type
  (arguments ps (long type)))

(define-pm-procedure ps_set_mix
  (arguments ps (long mix)))

(define-pm-procedure ps_query_caps
  (value sync)
  (arguments ps (long start) (long count) ((pointer long) values)))

(define-pm-procedure ps_set_clip_rectangle
  (arguments ps (short xl) (short xh) (short yl) (short yh)))

(define-pm-procedure ps_reset_clip_rectangle
  (arguments ps))

(define-pm-procedure get_bitmap_parameters
  (value sync)
  (arguments bitmap ((pointer "BITMAPINFOHEADER") params)))

(define-pm-procedure ps_get_bitmap_bits
  (value (ulong length))
  (arguments ps
	     (ulong start)
	     (ulong length)
	     ((pointer "BYTE") data)
	     ((pointer "BITMAPINFO2") info)))

(define-pm-procedure ps_set_bitmap_bits
  (value (ulong length))
  (arguments ps
	     (ulong start)
	     (ulong length)
	     ((pointer "BYTE") data)
	     ((pointer "BITMAPINFO2") info)))

;;; Clipboard

(define-pm-procedure clipboard_write_text
  (value sync)
  (arguments qid ((pointer (const char)) text)))

(define-pm-procedure clipboard_read_text
  (value ((pointer (const char)) text))
  (arguments qid))

;;; Menus

(define-pm-procedure menu_create
  (value ("HWND" menu))
  (arguments qid ("HWND" owner) (ushort style) (ushort id)))

(define-pm-procedure menu_destroy
  (value ("BOOL" successp))
  (arguments qid ("HWND" menu)))

(define-pm-procedure menu_insert_item
  (value (ushort position))
  (arguments qid
	     ("HWND" menu)
	     (ushort position)
	     (ushort style)
	     (ushort attributes)
	     (ushort id)
	     ("HWND" submenu)
	     ((pointer char) text)))

(define-pm-procedure menu_remove_item
  (value (ushort length))
  (arguments qid
	     ("HWND" menu)
	     (ushort id)
	     (ushort submenup)
	     (ushort deletep)))

(define-pm-procedure menu_get_item
  (value ((pointer "MENUITEM") item))
  (arguments qid
	     ("HWND" menu)
	     (ushort id)
	     (ushort submenup)))

(define-pm-procedure menu_n_items
  (value (ushort length))
  (arguments qid ("HWND" menu)))

(define-pm-procedure menu_nth_item_id
  (value (ushort id))
  (arguments qid ("HWND" menu) (ushort position)))

(define-pm-procedure menu_get_item_attributes
  (value (ushort attributes))
  (arguments qid
	     ("HWND" menu)
	     (ushort id)
	     (ushort submenup)
	     (ushort mask)))

(define-pm-procedure menu_set_item_attributes
  (value ("BOOL" successp))
  (arguments qid
	     ("HWND" menu)
	     (ushort id)
	     (ushort submenup)
	     (ushort mask)
	     (ushort attributes)))

(define-pm-procedure window_load_menu
  (value ("HWND" menu))
  (arguments window ("HMODULE" module) (ulong id)))

(define-pm-procedure window_popup_menu
  (value ("BOOL" successp))
  (arguments qid
	     ("HWND" parent)
	     ("HWND" owner)
	     ("HWND" menu)
	     (long x)
	     (long y)
	     (long id)
	     (ulong options)))

;;; Font

(define-pm-procedure ps_get_font_metrics
  (value ((pointer font_metrics_t) metrics))
  (arguments ps))

(define-pm-procedure ps_set_font_internal
  (value ((pointer font_metrics_t) metrics))
  (arguments ps
	     (ushort id)
	     ((array (const char)) name)))

(define-pm-procedure window_font_dialog
  (value ((pointer (const char)) spec))
  (arguments window ((pointer (const char)) title)))

;;; Pointers

(define-pm-procedure query_system_pointer
  (value ("HPOINTER" pointer))
  (arguments qid ("HWND" desktop) (long id) ("BOOL" copyp)))

(define-pm-procedure set_pointer
  (value ("BOOL" successp))
  (arguments qid ("HWND" desktop) ("HPOINTER" pointer)))

(define-pm-procedure window_load_pointer
  (value ("HPOINTER" pointer))
  (arguments qid ("HWND" desktop) ("HMODULE" module) (ulong id)))

(define-pm-procedure window_destroy_pointer
  (value ("BOOL" successp))
  (arguments qid ("HPOINTER" icon)))

(define-pm-procedure window_set_icon
  (value ("BOOL" successp))
  (arguments window ("HPOINTER" icon)))

(write-all-files)