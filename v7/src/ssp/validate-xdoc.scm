#| -*-Scheme-*-

$Id: validate-xdoc.scm,v 1.1 2003/12/29 05:24:47 uid67408 Exp $

Copyright 2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; XDOC implementation

(declare (usual-integrations))

;;; **** Belongs in runtime:
(define (count-matching-items items predicate)
  (do ((items items (cdr items))
       (n 0 (if (predicate (car items)) (+ n 1) n)))
      ((not (pair? items)) n)))

(define (validate-xdoc pathname)
  (with-xdoc-expansion-context (pathname->ps-number pathname) pathname
    (lambda (document)
      (let ((root (xml-document-root document)))
	(if (not (xd:xdoc? root))
	    (vx:error root "Root element not <xdoc>."))
	(check-element root 'xdoc)))))

(define (check-element elt local)
  (let ((v (hash-table/get element-checkers local #f)))
    (if (not v)
	(error "Missing element definition:" local))
    (let ((valid-attrs? (vector-ref v 0))
	  (type (vector-ref v 1))
	  (valid-local? (vector-ref v 2))
	  (procedure (vector-ref v 3)))
      (if valid-attrs?
	  (valid-attrs? elt))
      (check-element-content elt type valid-local?)
      (if procedure
	  (procedure elt)))))

(define (check-element-content elt type procedure)
  (case type
    ((empty)
     (if (not (null? (xml-element-contents elt)))
	 (vx:error elt "Empty element has content.")))
    ((element)
     (procedure elt))
    (else
     (for-each (case type
		 ((text)
		  (lambda (item)
		    (if (not (string? item))
			(vx:content-error elt item))))
		 ((html)
		  (lambda (item)
		    (if (xdoc-element? item)
			(vx:content-error elt item))))
		 ((mixed)
		  (lambda (item)
		    (let ((local (xdoc-element-name item)))
		      (if local
			  (if (content-predicate local)
			      (check-element item local)
			      (vx:content-error elt item))))))
		 (else
		  (error "Unknown content type:" type)))
	       (xml-element-contents elt)))))

(define (define-element-checker local type
	  #!optional valid-attrs? valid-local? procedure)
  (let ((valid-attrs? (if (default-object? valid-attrs?) #f valid-attrs?))
	(valid-local? (if (default-object? valid-local?) #f valid-local?))
	(procedure (if (default-object? procedure) #f procedure)))
    (if (and (memq type '(element mixed))
	     (not valid-local?))
	(error "Must supply a name predicate with this content type:" type))
    (hash-table/put! element-checkers
		     local
		     (vector valid-attrs? type valid-local? procedure))))

(define element-checkers
  (make-eq-hash-table))

(define (vx:standard-attrs elt)
  (vx:optional-attr 'class elt vx:nmtokens)
  (vx:optional-attr 'style elt vx:style))

;;;; Containers

(define (vx:container-attrs elt)
  (vx:standard-attrs elt)
  (vx:optional-attr 'id elt vx:id))

(define (problem-element-name? local)
  (or (memq local '(problem answer))
      (answer-element-name? local)))

(define (answer-element-name? local)
  (or (input-checker-element-name? local)
      (switched-output-name? local)
      (button-element-name? local)))

(define-element-checker 'xdoc 'mixed
  (lambda (elt)
    (vx:container-attrs elt)
    (vx:optional-attr 'number-format elt vx:procedure-name)
    (vx:optional-attr 'problem-separator elt vx:boolean)
    (vx:required-attr 'problem-set elt vx:nonnegative-integer)
    (vx:optional-attr 'first-problem elt vx:problem-number)
    (vx:optional-attr 'form-url elt vx:url))
  (lambda (local)
    (or (problem-element-name? local)
	(memq local '(due-date head))))
  (lambda (elt)
    (if (> (count-matching-items (xml-element-contents elt) xd:due-date?) 1)
	(vx:error elt "Multiple xd:due-date elements."))))

(define-element-checker 'head 'html)

(define-element-checker 'due-date 'empty
  (lambda (elt)
    (vx:standard-attrs elt)
    (vx:optional-attr 'year elt vx:year)
    (vx:required-attr 'month elt vx:month)
    (vx:required-attr 'day elt vx:day)
    (vx:required-attr 'hour elt vx:hour)
    (vx:optional-attr 'minute elt vx:minute)))

(define-element-checker 'problem 'mixed
  (lambda (elt)
    (vx:container-attrs elt)
    (vx:optional-attr 'number-format elt vx:procedure-name)
    (vx:optional-attr 'number-type elt vx:number-type)
    (vx:optional-attr 'problem-separator elt vx:boolean))
  (lambda (local)
    (problem-element-name? local)))

(define-element-checker 'answer 'element
  (lambda (elt)
    (vx:container-attrs elt))
  (lambda (local)
    (or (answer-element-name? local)
	(input-element-name? local)
	(eq? local 'label))))

(define-element-checker 'label 'html
  (lambda (elt)
    (vx:standard-attrs elt)))

;;;; Inputs

(define (input-element-name? local)
  (memq local '(checkbox menu radio-buttons text true-false)))

(define (vx:input-attrs elt)
  (vx:standard-attrs elt)
  (vx:optional-attr 'width elt vx:positive-integer))

(define-element-checker 'text 'empty
  (lambda (elt)
    (vx:input-attrs elt)))

(define-element-checker 'menu 'element
  (lambda (elt)
    (vx:input-attrs elt)
    (vx:optional-attr 'size elt vx:positive-integer))
  (lambda (local)
    (eq? local 'menuitem)))

(define-element-checker 'menuitem 'text)

(define-element-checker 'true-false 'empty
  (lambda (elt)
    (vx:input-attrs elt)))

(define-element-checker 'checkbox 'empty
  (lambda (elt)
    (vx:input-attrs elt)))

(define-element-checker 'radio-buttons 'element
  (lambda (elt)
    (vx:input-attrs elt))
  (lambda (local)
    (eq? local 'radio-entry)))

(define-element-checker 'radio-entry 'html
  (lambda (elt)
    (vx:input-attrs elt)
    (vx:required-attr 'value elt vx:nmtoken)))

;;;; Input checkers

(define (input-checker-element-name? local)
  (memq local '(boolean check-input check-inputs menuindex number)))

(define (vx:unary-checker-attrs elt)
  (vx:optional-attr 'id elt vx:id)
  (vx:optional-attr 'source elt vx:idref))

(define (vx:n-ary-checker-attrs elt)
  (vx:optional-attr 'id elt vx:id)
  (vx:optional-attr 'sources elt vx:idrefs))

(define-element-checker 'check-input 'empty
  (lambda (elt)
    (vx:unary-checker-attrs elt)
    (vx:optional-attr 'expected elt vx:cdata)
    (vx:optional-attr 'checkable elt vx:boolean)
    (vx:required-attr 'name elt vx:procedure-name)))

(define-element-checker 'check-inputs 'empty
  (lambda (elt)
    (vx:n-ary-checker-attrs elt)
    (vx:optional-attr 'expected elt vx:cdata)
    (vx:optional-attr 'checkable elt vx:boolean)
    (vx:required-attr 'name elt vx:procedure-name)))

(define-element-checker 'number 'empty
  (lambda (elt)
    (vx:unary-checker-attrs elt)
    (vx:required-attr 'expected elt vx:number)
    (vx:optional-attr 'checkable elt vx:boolean)
    (vx:optional-attr 'tolerance elt vx:number)))

(define-element-checker 'boolean 'empty
  (lambda (elt)
    (vx:unary-checker-attrs elt)
    (vx:required-attr 'expected elt vx:boolean)))

(define-element-checker 'menuindex 'empty
  (lambda (elt)
    (vx:unary-checker-attrs elt)
    (vx:required-attr 'expected elt vx:positive-integer)))

;;;; Switched elements

(define (switched-output-name? local)
  (memq local '(case expected-value explain hint when)))

(define (vx:switched-output-attrs elt)
  (vx:standard-attrs elt)
  (vx:optional-attr 'source elt vx:idref))

(define-element-checker 'explain 'html
  (lambda (elt)
    (vx:switched-output-attrs elt)))

(define-element-checker 'hint 'html
  (lambda (elt)
    (vx:switched-output-attrs elt)))

(define-element-checker 'expected-value 'empty
  (lambda (elt)
    (vx:switched-output-attrs elt)))

(define-element-checker 'when 'html
  (lambda (elt)
    (vx:switched-output-attrs elt)
    (vx:required-attr 'condition elt
		      (lambda (string)
			(vx:test (lambda (string)
				   (or (string=? string "submitted")
				       (string=? string "not-submitted")))
				 string
				 "condition")))))

(define-element-checker 'case 'element
  (lambda (elt)
    (vx:standard-attrs elt))
  (lambda (local)
    (or (input-checker-element-name? local)
	(eq? local 'refer)
	(eq? local 'choice)
	(eq? local 'default)))
  (lambda (elt)
    (if (not (case-element-children? (xml-element-contents elt)))
	(vx:error elt "Invalid arrangement of child elements."))))

(define-element-checker 'refer 'empty
  (lambda (elt)
    (vx:required-attr 'source elt vx:idref)))

(define-element-checker 'choice 'html
  (lambda (elt)
    (vx:required-attr 'values elt vx:nmtokens)))

(define-element-checker 'default 'html)

;;;; Buttons

(define (button-element-name? local)
  (memq local '(check-button submit-button)))

(define (vx:button-attrs elt)
  (vx:standard-attrs elt)
  (vx:optional-attr 'scope elt vx:idref))

(define-element-checker 'check-button 'empty
  (lambda (elt)
    (vx:button-attrs elt)))

(define-element-checker 'submit-button 'empty
  (lambda (elt)
    (vx:button-attrs elt)))

;;;; Attribute tests

(define (vx:required-attr name elt test)
  (let ((attr (%find-attribute name (xml-element-attributes elt))))
    (if attr
	(vx:check-attr test attr elt)
	(vx:error "Missing required attribute: " name elt))))

(define (vx:optional-attr name elt test)
  (let ((attr (%find-attribute name (xml-element-attributes elt))))
    (if attr
	(vx:check-attr test attr elt))))

(define (vx:check-attr test attr elt)
  (let ((desc (test (xml-attribute-value attr))))
    (if desc
	(vx:error elt
		  "Attribute "
		  (xml-attribute-name attr)
		  " value should be "
		  desc
		  ":"
		  (xml-attribute-value attr)))))

(define ((vx:tester desc predicate) string)
  (if (predicate string)
      #f
      desc))

(define (vx:number-tester desc predicate)
  (vx:tester desc
    (lambda (string)
      (predicate (string->number string)))))

(define (vx:index-tester desc k l)
  (vx:number-tester desc
    (lambda (n)
      (and (exact-integer? n)
	   (<= k n l)))))

(define vx:cdata (vx:tester "XML string" xml-char-data?))
(define vx:id (vx:tester "ID" string-is-xml-name?))
(define vx:idref (vx:tester "ID reference" string-is-xml-name?))
(define vx:nmtoken (vx:tester "XML token" string-is-xml-nmtoken?))

(define vx:idrefs
  (vx:tester "ID references"
    (lambda (string)
      (for-all? (burst-string string char-set:whitespace #t)
	string-is-xml-name?))))

(define vx:nmtokens
  (vx:tester "XML tokens"
    (lambda (string)
      (for-all? (burst-string string char-set:whitespace #t)
	string-is-xml-nmtoken?))))

(define vx:boolean
  (vx:tester "true or false"
    (lambda (string)
      (or (string=? string "true")
	  (string=? string "false")))))

(define vx:style
  (vx:tester "style sheet"
    (lambda (string)
      string
      #t)))

(define vx:url
  (vx:tester "URL"
    (lambda (string)
      string
      #t)))

(define vx:number
  (vx:number-tester "number" number?))

(define vx:nonnegative-integer
  (vx:number-tester "non-negative integer" exact-nonnegative-integer?))

(define vx:positive-integer
  (vx:number-tester "positive integer" exact-positive-integer?))

(define vx:minute (vx:index-tester "minute" 0 59))
(define vx:hour (vx:index-tester "hour" 0 59))
(define vx:day (vx:index-tester "day of month" 1 31))
(define vx:month (vx:index-tester "month" 1 12))
(define vx:year (vx:number-tester "year" exact-nonnegative-integer?))

(define vx:problem-number
  (vx:tester "problem number"
    (lambda (string)
      (re-string-match "\\`\\([1-9][0-9]*.\\)*[1-9][0-9]*\\'" string))))

(define vx:number-type
  (vx:tester "problem-number format type"
    (lambda (string)
      (or (string=? string "dl")
	  (string=? string "ol")
	  (string=? string "ul")
	  (string=? string "none")))))

(define vx:procedure-name
  (vx:tester "procedure name" xdoc-procedure-name?))

(define (vx:content-error elt item)
  (vx:error elt "Illegal content: " item))

(define (vx:error elt msg . msg-items)
  (error:xdoc-validation elt (cons msg msg-items)))

(define condition-type:xdoc-validation-error
  (make-condition-type 'xdoc-validation-error
      condition-type:warning
      '(element message-items)
    (lambda (condition port)
      (write-string "Error validating " port)
      (write (xdoc-validation-error/element condition) port)
      (write-string ": " port)
      (let loop ((items (xdoc-validation-error/message-items condition)))
	(if (pair? items)
	    (begin
	      (write-string (car items) port)
	      (if (pair? (cdr items))
		  (begin
		    (write (cadr items) port)
		    (loop (cddr items))))))))))

(define xdoc-validation-error/element
  (condition-accessor condition-type:xdoc-validation-error 'element))

(define xdoc-validation-error/message-items
  (condition-accessor condition-type:xdoc-validation-error 'message-items))

(define error:xdoc-validation
  (condition-signaller condition-type:xdoc-validation-error
		       '(element message-items)
		       standard-warning-handler))