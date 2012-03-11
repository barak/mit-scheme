#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; XML-RPC

(declare (usual-integrations))

(define (xml-rpc uri request #!optional headers)
  (let ((response
	 (http-post uri
		    `(,@(if (default-object? headers) '() headers)
		      ,(make-http-header 'CONTENT-TYPE "text/xml"))
		    (xml->octets (->request request 'XML-RPC)))))
    (if (not (= 200 (http-response-status response)))
	(error "HTTP error:" (http-response-reason response)))
    (xml-rpc:parse-response (read-xml (http-message-body-port response)))))

(define (->request request caller)
  (cond ((or (xml-document? request)
	     (xml-element? request))
	 request)
	((list? request)
	 (apply xml-rpc:request request))
	(else
	 (error:wrong-type-argument request "XML-RPC request" caller))))

(define (xml-rpc:request name . objects)
  (rpc-elt:method-call (rpc-elt:method-name name)
		       (rpc-elt:params (map (lambda (object)
					      (encode-param object))
					    objects))))

(define (xml-rpc:response object)
  (rpc-elt:method-response (rpc-elt:params (encode-param object))))

(define (xml-rpc:fault code string)
  (rpc-elt:method-response
   (rpc-elt:fault
    (rpc-elt:value
     (rpc-elt:struct
      (rpc-elt:member (rpc-elt:name '|faultCode|)
		      (rpc-elt:value (rpc-elt:int (number->string code))))
      (rpc-elt:member (rpc-elt:name '|faultString|)
		      (rpc-elt:value (rpc-elt:string string))))))))

(define (xml-rpc:simple-fault code message . irritants)
  (xml-rpc:fault code
		 (call-with-output-string
		  (lambda (port)
		    (format-error-message message irritants port)))))

(define (xml-rpc:condition->fault code condition)
  (xml-rpc:fault code (condition/report-string condition)))

(define (xml-rpc:parse-request document)
  (fluid-let ((*document* document)
	      (*caller* 'xml-rpc:parse-request))
    (let ((elt (xml-document-root document)))
      (require (xml-name=? (xml-element-name elt) '|methodCall|))
      (values (let ((s (content-string (named-child '|methodName| elt))))
		(require (re-string-match "\\`[a-zA-Z0-9_.:/]+\\'" s))
		(utf8-string->symbol s))
	      (let ((elt (%named-child 'params elt)))
		(if elt
		    (parse-params elt)
		    '()))))))

(define (xml-rpc:parse-response document)
  (fluid-let ((*document* document)
	      (*caller* 'xml-rpc:parse-response))
    (let ((elt (xml-document-root document)))
      (require (xml-name=? (xml-element-name elt) '|methodResponse|))
      (let ((elt (single-child elt)))
	(cond ((xml-name=? (xml-element-name elt) 'params)
	       (let ((objects (parse-params elt)))
		 (require (and (pair? objects) (null? (cdr objects))))
		 (car objects)))
	      ((xml-name=? (xml-element-name elt) 'fault)
	       (let ((alist (decode-value (single-named-child 'value elt))))
		 (require (and (alist? alist) (= (length alist) 2)))
		 (let ((p1 (or (assq '|faultCode| alist) (lose)))
		       (p2 (or (assq '|faultString| alist) (lose))))
		   (require (exact-integer? (cdr p1)))
		   (require (string? (cdr p2)))
		   (error:xml-rpc-fault (cdr p1) (cdr p2)))))
	      (else (lose)))))))

(define (parse-params elt)
  (map (lambda (elt)
	 (decode-value (single-named-child 'value elt)))
       (named-children 'param elt)))

(define *document*)
(define *caller*)

(define (require boolean)
  (if (not boolean)
      (lose)))

(define (lose)
  (error:bad-xml-rpc-message *document* *caller*))

(define condition-type:bad-xml-rpc-message
  (make-condition-type 'bad-xml-rpc-message condition-type:error
      '(document caller)
    (lambda (condition port)
      (write-string "Malformed XML-RPC message:" port)
      (newline port)
      (write-xml (access-condition condition 'document) port))))

(define error:bad-xml-rpc-message
  (condition-signaller condition-type:bad-xml-rpc-message
		       '(document caller)
		       standard-error-handler))

(define condition-type:xml-rpc-fault
  (make-condition-type 'xml-rpc-fault condition-type:error '(code string)
    (lambda (condition port)
      (write-string "Remote procedure call signalled code " port)
      (write (access-condition condition 'code) port)
      (write-string ":" port)
      (newline port)
      (write-string (access-condition condition 'string) port))))

(define error:xml-rpc-fault
  (condition-signaller condition-type:xml-rpc-fault
		       '(code string)
		       standard-error-handler))

(define (named-children name elt)
  (let loop ((items (xml-element-contents elt)))
    (if (pair? items)
	(let ((item (car items))
	      (rest (loop (cdr items))))
	  (if (xml-element? item)
	      (begin
		(require (xml-name=? (xml-element-name item) name))
		(cons item rest))
	      (begin
		(require
		 (or (xml-whitespace-string? item)
		     (xml-comment? item)))
		rest)))
	'())))

(define (all-children elt)
  (let loop ((items (xml-element-contents elt)))
    (if (pair? items)
	(let ((item (car items))
	      (rest (loop (cdr items))))
	  (if (xml-element? item)
	      (cons item rest)
	      (begin
		(require
		 (or (xml-whitespace-string? item)
		     (xml-comment? item)))
		rest)))
	'())))

(define (named-child name elt)
  (let ((child (%named-child name elt)))
    (require child)
    child))

(define (%named-child name elt)
  (find-matching-item (xml-element-contents elt)
    (lambda (item)
      (and (xml-element? item)
	   (xml-name=? (xml-element-name item) name)))))

(define (single-child elt)
  (let ((children (all-children elt)))
    (require (and (pair? children) (null? (cdr children))))
    (car children)))

(define (single-named-child name elt)
  (let ((child (single-child elt)))
    (require (xml-name=? (xml-element-name child) name))
    child))

(define (decode-value elt)
  (let ((items (xml-element-contents elt)))
    (if (and (pair? items)
	     (string? (car items))
	     (null? (cdr items)))
	(car items)
	(let ((object (decode-value-1 (single-child elt))))
	  (if *xml-rpc:decode-value-handler*
	      (*xml-rpc:decode-value-handler* object)
	      object)))))

(define (decode-value-1 elt)
  (case (xml-element-name elt)
    ((boolean)
     (let ((s (content-string elt)))
       (cond ((string=? s "0") #f)
	     ((string=? s "1") #t)
	     (else (lose)))))
    ((nil)
     #!default)
    ((|dateTime.iso8601|)
     (safe-call xml-rpc-iso8601-string->decoded-time (content-string elt)))
    ((double)
     (let ((x (string->number (content-string elt))))
       (require (and x (flo:flonum? x)))
       x))
    ((i4 int)
     (let ((n (string->number (content-string elt))))
       (require (and n
		     (exact-integer? n)
		     (<= #x-80000000 n #x7fffffff)))
       n))
    ((string)
     (content-string elt))
    ((base64)
     (safe-call (lambda (string)
		  (call-with-output-string
		    (lambda (port)
		      (call-with-decode-base64-output-port port #f
			(lambda (port)
			  (write-string string port))))))
		(content-string elt)))
    ((array)
     (map (lambda (elt) (decode-value elt))
	  (named-children 'value (single-named-child 'data elt))))
    ((struct)
     (map (lambda (elt)
	    (cons (utf8-string->symbol
		   (content-string (named-child 'name elt)))
		  (decode-value (named-child 'value elt))))
	  (named-children 'member elt)))
    (else (lose))))

(define (content-string elt)
  (let ((items (xml-element-contents elt)))
    (require
     (and (pair? items)
	  (string? (car items))
	  (null? (cdr items))))
    (car items)))

(define (safe-call procedure . arguments)
  (let ((value (ignore-errors (lambda () (apply procedure arguments)))))
    (require (not (condition? value)))
    value))

(define *xml-rpc:decode-value-handler* #f)

(define (encode-param object)
  (rpc-elt:param (encode-value object)))

(define (encode-value object)
  (rpc-elt:value
   (let ((object
	  (if *xml-rpc:encode-value-handler*
	      (*xml-rpc:encode-value-handler* object)
	      object)))
     (cond ((and (exact-integer? object)
		 (<= #x-80000000 object #x7fffffff))
	    (rpc-elt:int (number->string object)))
	   ((flo:flonum? object)
	    ;; Probably not right -- formatting issues
	    (rpc-elt:double (number->string object)))
	   ((boolean? object)
	    (rpc-elt:boolean (if object "1" "0")))
	   ((default-object? object)
	    (rpc-elt:nil))
	   ((string? object)
	    (encode-string object))
	   ((symbol? object)
	    (encode-string (symbol->utf8-string object)))
	   ((decoded-time? object)
	    (rpc-elt:date-time (decoded-time->xml-rpc-iso8601-string object)))
	   ((and (pair? object)
		 (list-of-type? object
		   (lambda (item)
		     (and (pair? item)
			  (symbol? (car item))))))
	    (rpc-elt:struct
	     (map (lambda (item)
		    (rpc-elt:member
		     (rpc-elt:name (symbol->utf8-string (car item)))
		     (encode-value (cdr item))))
		  (cdr object))))
	   ((list? object)
	    (rpc-elt:array (rpc-elt:data (map encode-value object))))
	   (else
	    (error:wrong-type-argument object
				       "an XML-RPC object"
				       'encode-value))))))

(define (encode-string string)
  (if (and (utf8-string-valid? string)
	   (string-of-xml-chars? string))
      string
      (rpc-elt:base64
       (call-with-output-string
	 (lambda (port)
	   (let ((context (encode-base64:initialize port #f)))
	     (encode-base64:update context string 0 (string-length string))
	     (encode-base64:finalize context)))))))

(define *xml-rpc:encode-value-handler* #f)

(define (rpc-elt name empty?)
  (let ((make-elt
	 (standard-xml-element-constructor name
					   (null-xml-namespace-uri)
					   empty?)))
    (if empty?
	(lambda ()
	  (make-elt))
	(lambda contents
	  (apply make-elt (xml-attrs) contents)))))

(define rpc-elt:array (rpc-elt 'array #f))
(define rpc-elt:base64 (rpc-elt 'base64 #f))
(define rpc-elt:boolean (rpc-elt 'boolean #f))
(define rpc-elt:data (rpc-elt 'data #f))
(define rpc-elt:date-time (rpc-elt '|dateTime.iso8601| #f))
(define rpc-elt:double (rpc-elt 'double #f))
(define rpc-elt:fault (rpc-elt 'fault #f))
(define rpc-elt:i4 (rpc-elt 'i4 #f))
(define rpc-elt:int (rpc-elt 'int #f))
(define rpc-elt:member (rpc-elt 'member #f))
(define rpc-elt:method-call (rpc-elt '|methodCall| #f))
(define rpc-elt:method-name (rpc-elt '|methodName| #f))
(define rpc-elt:method-response (rpc-elt '|methodResponse| #f))
(define rpc-elt:name (rpc-elt 'name #f))
(define rpc-elt:nil (rpc-elt 'nil #t))
(define rpc-elt:param (rpc-elt 'param #f))
(define rpc-elt:params (rpc-elt 'params #f))
(define rpc-elt:string (rpc-elt 'string #f))
(define rpc-elt:struct (rpc-elt 'struct #f))
(define rpc-elt:value (rpc-elt 'value #f))