#| -*-Scheme-*-

$Id: xmlrpc.scm,v 1.13 2005/02/19 04:35:45 cph Exp $

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

;;;; XML-RPC content handler

(declare (usual-integrations))

(define (xml-rpc:subtree-handler pathname port)
  (if (eq? (http-request-method) 'post)
      (let ((entity (http-request-entity)))
	(if entity
	    (let ((document (read-xml (open-input-string entity))))
	      (if document
		  (write-xml (process-xmlrpc-request document pathname) port)
		  (http-status-response 400 "Ill-formed XML entity")))
	    (http-status-response 400 "Missing XML entity")))
      (begin
	(http-status-response 405 "XML-RPC requires POST method.")
	(http-response-header 'allow "POST"))))

(define (process-xmlrpc-request document pathname)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler (list condition-type:error)
	 (lambda (condition)
	   (k (xml-rpc:condition->fault 1 condition)))
       (lambda ()
	 (receive (name params) (xml-rpc:parse-request document)
	   (let ((handler (get-xmlrpc-method-handler pathname name)))
	     (if (not handler)
		 (error "Unknown method name:" name))
	     (xml-rpc:response
	      (with-working-directory-pathname (directory-pathname pathname)
		(lambda ()
		  (apply handler params)))))))))))

(define (get-xmlrpc-method-handler pathname name)
  (let ((methods (make-eq-hash-table)))
    (let ((environment (make-expansion-environment pathname)))
      (environment-define environment 'define-xmlrpc-method
	(lambda (name handler)
	  (hash-table/put! methods name handler)))
      (fluid-let ((load/suppress-loading-message? #t))
	(load pathname environment)))
    (hash-table/get methods name #f)))