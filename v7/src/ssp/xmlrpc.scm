#| -*-Scheme-*-

$Id: xmlrpc.scm,v 1.6 2005/01/11 03:43:46 cph Exp $

Copyright 2003,2004,2005 Massachusetts Institute of Technology

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

;;;; MIT/GNU Scheme XML-RPC implementation (requires mod-lisp)

(declare (usual-integrations))

(define-subtree-handler "/xmlrpc/" 'text/xml
  (lambda (pathname port)
    (if (eq? (http-request-method) 'post)
	(let ((entity (http-request-entity)))
	  (if entity
	      (let ((document (read-xml (open-input-string entity))))
		(if document
		    (write-xml (xml-rpc:process-request document pathname)
			       port)
		    (http-status-response 400 "Ill-formed XML entity")))
	      (http-status-response 400 "Missing XML entity")))
	(begin
	  (http-status-response 405 "XML-RPC requires POST method.")
	  (http-response-header 'allow "POST")))))

(define (xml-rpc:process-request document pathname)
  (let ((result
	 (ignore-errors
	  (lambda ()
	    (receive (name params) (xml-rpc:parse-request document)
	      (let ((handler (xml-rpc:get-method-handler pathname name)))
		(if (not handler)
		    (error "Unknown method name:" name))
		(xml-rpc:response
		 (with-working-directory-pathname (directory-pathname pathname)
		   (lambda ()
		     (apply handler params))))))))))
    (if (condition? result)
	(xml-rpc:fault 1 (condition/report-string result))
	result)))

(define (xml-rpc:get-method-handler pathname name)
  (let ((methods (make-string-hash-table)))
    (let ((environment (make-expansion-environment pathname)))
      (environment-define environment 'define-xmlrpc-method
	(lambda (name handler)
	  (hash-table/put! methods name handler)))
      (fluid-let ((load/suppress-loading-message? #t))
	(load pathname environment)))
    (hash-table/get methods name #f)))

(define (xml-rpc:response object)
  (rpc-elt:method-response
   (rpc-elt:params
    (rpc-elt:param
     (rpc-elt:value (xml-rpc:encode-object object))))))

(define (xml-rpc:fault code message . irritants)
  (let ((message
	 (call-with-output-string
	  (lambda (port)
	    (format-error-message message irritants port)))))
    (rpc-elt:method-response
     (rpc-elt:fault
      (rpc-elt:value
       (rpc-elt:struct
	(rpc-elt:member (rpc-elt:name "faultCode")
			(rpc-elt:value (rpc-elt:int (number->string code))))
	(rpc-elt:member (rpc-elt:name "faultString")
			(rpc-elt:value (rpc-elt:string message)))))))))

(define (xml-rpc:parse-request document)
  (let ((elt (xml-document-root document))
	(lose
	 (lambda ()
	   (error:bad-range-argument (xml->string document) #f))))
    (if (not (xml-name=? (xml-element-name elt) '|methodCall|))
	(lose))
    (values (let ((s
		   (xml-rpc:content-string
		    (xml-rpc:named-child '|methodName| elt lose)
		    lose)))
	      (if (not (re-string-match "\\`[a-zA-Z0-9_.:/]+\\'" s))
		  (lose))
	      s)
	    (let ((elt (xml-rpc:named-child 'params elt #f)))
	      (if elt
		  (xml-rpc:parse-params elt lose)
		  '())))))

(define (xml-rpc:parse-params elt lose)
  (map (lambda (elt)
	 (xml-rpc:decode-value (xml-rpc:single-named-child 'value elt lose)
			       lose))
       (xml-rpc:named-children 'param elt lose)))

(define (xml-rpc:named-children name elt lose)
  (let loop ((items (xml-element-contents elt)))
    (if (pair? items)
	(let ((item (car items))
	      (rest (loop (cdr items))))
	  (if (xml-element? item)
	      (begin
		(if (not (xml-name=? (xml-element-name item) name))
		    (lose))
		(cons item rest))
	      (begin
		(if (not (or (xml-whitespace-string? item)
			     (xml-comment? item)))
		    (lose))
		rest)))
	'())))

(define (xml-rpc:children elt lose)
  (let loop ((items (xml-element-contents elt)))
    (if (pair? items)
	(let ((item (car items))
	      (rest (loop (cdr items))))
	  (if (xml-element? item)
	      (cons item rest)
	      (begin
		(if (not (or (xml-whitespace-string? item)
			     (xml-comment? item)))
		    (lose))
		rest)))
	'())))

(define (xml-rpc:named-child name elt lose)
  (or (find-matching-item (xml-element-contents elt)
	(lambda (item)
	  (and (xml-element? item)
	       (xml-name=? (xml-element-name item) name))))
      (and lose (lose))))

(define (xml-rpc:single-child elt lose)
  (let ((children (xml-rpc:children elt lose)))
    (if (not (and (pair? children)
		  (null? (cdr children))))
	(lose))
    (car children)))

(define (xml-rpc:single-named-child name elt lose)
  (let ((child (xml-rpc:single-child elt lose)))
    (if (not (xml-name=? (xml-element-name child) name))
	(lose))
    child))

(define (xml-rpc:decode-object elt lose)
  (case (xml-element-name elt)
    ((boolean)
     (let ((s (xml-rpc:content-string elt lose)))
       (cond ((string=? s "0") #f)
	     ((string=? s "1") #t)
	     (else (lose)))))
    ((|dateTime.iso8601|)
     (safe-call lose
		iso8601-string->decoded-time
		(xml-rpc:content-string elt lose)))
    ((double)
     (let ((x (string->number (xml-rpc:content-string elt lose))))
       (if (not (and x (flo:flonum? x)))
	   (lose))
       x))
    ((i4 int)
     (let ((n (string->number (xml-rpc:content-string elt lose))))
       (if (not (and n
		     (exact-integer? n)
		     (<= #x-80000000 n #x7fffffff)))
	   (lose))
       n))
    ((string)
     (xml-rpc:content-string elt lose))
    ((base64)
     (safe-call lose
		(lambda (string)
		  (call-with-output-string
		    (lambda (port)
		      (call-with-decode-base64-output-port port #f
			(lambda (port)
			  (write-string string port))))))
		(xml-rpc:content-string elt lose)))
    ((array)
     (map (lambda (elt) (xml-rpc:decode-value elt lose))
	  (xml-rpc:named-children 'value
				  (xml-rpc:single-named-child 'data elt lose)
				  lose)))
    ((struct)
     (map (lambda (elt)
	    (cons (utf8-string->symbol (xml-rpc:named-child 'name elt lose))
		  (xml-rpc:decode-value (xml-rpc:named-child 'value elt lose)
					lose)))
	  (xml-rpc:named-children 'member elt lose)))
    (else (lose))))

(define (xml-rpc:content-string elt lose)
  (let ((items (xml-element-contents elt)))
    (if (not (and (pair? items)
		  (string? (car items))
		  (null? (cdr items))))
	(lose))
    (car items)))

(define (safe-call lose procedure . arguments)
  (let ((value (ignore-errors (lambda () (apply procedure arguments)))))
    (if (condition? value)
	(lose)
	value)))

(define (xml-rpc:decode-value elt lose)
  (let ((items (xml-element-contents elt)))
    (if (and (pair? items)
	     (string? (car items))
	     (null? (cdr items)))
	(car items)
	(xml-rpc:decode-object (xml-rpc:single-child elt lose) lose))))

(define (xml-rpc:encode-object object)
  (cond ((and (exact-integer? object)
	      (<= #x-80000000 object #x7fffffff))
	 (rpc-elt:int (number->string object)))
	((flo:flonum? object)
	 ;; Probably not right -- formatting issues
	 (rpc-elt:double (number->string object)))
	((boolean? object)
	 (rpc-elt:boolean (if object "1" "0")))
	((string? object)
	 (if (utf8-string-valid? object)
	     (rpc-elt:string object)
	     (call-with-output-string
	       (lambda (port)
		 (let ((context (encode-base64:initialize port #f)))
		   (encode-base64:update context
					 object
					 0
					 (string-length object))
		   (encode-base64:finalize context))))))
	((decoded-time? object)
	 (rpc-elt:date-time (decoded-time->iso8601-string object)))
	((and (pair? object)
	      (list-of-type? object
		(lambda (item)
		  (and (pair? item)
		       (symbol? (car item))))))
	 (rpc-elt:struct
	  (map (lambda (item)
		 (rpc-elt:member
		  (rpc-elt:name (symbol->utf8-string (car item)))
		  (xml-rpc:encode-value (cdr item))))
	       (cdr object))))
	((list? object)
	 (rpc-elt:array (rpc-elt:data (map xml-rpc:encode-value object))))
	(else
	 (error:wrong-type-argument object
				    "an XML-RPC object"
				    'xml-rpc:encode-object))))

(define (xml-rpc:encode-value v)
  (rpc-elt:value (xml-rpc:encode-object v)))

(define (rpc-elt name)
  (let ((make-elt
	 (standard-xml-element-constructor name (null-xml-namespace-iri) #f)))
    (lambda contents
      (apply make-elt #f contents))))

(define rpc-elt:array (rpc-elt 'array))
(define rpc-elt:base64 (rpc-elt 'base64))
(define rpc-elt:boolean (rpc-elt 'boolean))
(define rpc-elt:data (rpc-elt 'data))
(define rpc-elt:date-time (rpc-elt '|dateTime.iso8601|))
(define rpc-elt:double (rpc-elt 'double))
(define rpc-elt:fault (rpc-elt 'fault))
(define rpc-elt:i4 (rpc-elt 'i4))
(define rpc-elt:int (rpc-elt 'int))
(define rpc-elt:member (rpc-elt 'member))
(define rpc-elt:method-call (rpc-elt '|methodCall|))
(define rpc-elt:method-name (rpc-elt '|methodName|))
(define rpc-elt:method-response (rpc-elt '|methodResponse|))
(define rpc-elt:name (rpc-elt 'name))
(define rpc-elt:param (rpc-elt 'param))
(define rpc-elt:params (rpc-elt 'params))
(define rpc-elt:string (rpc-elt 'string))
(define rpc-elt:struct (rpc-elt 'struct))
(define rpc-elt:value (rpc-elt 'value))