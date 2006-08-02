#| -*-Scheme-*-

$Id: http-client.scm,v 14.1 2006/08/02 16:27:09 riastradh Exp $

Copyright 2006 Taylor R. Campbell

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

;;;; HTTP 1.0 Client Abstraction

(declare (usual-integrations))

(define (call-with-http-response:entity-request method request-uri
            header-fields content
          receiver)
  (receive (request-uri host port) (decompose-http-request-uri request-uri)
    (call-with-http-connection host port
      (lambda (connection)
        (send-http-request connection method request-uri header-fields content)
        (receiver (receive-http-response connection)
                  (http-connection/socket connection))))))

(define (default-http-uri-authority)
  ;++ implement a nice hook here
  #f)

(define (decompose-http-request-uri request-uri)
  (cond ((or (uri-authority request-uri)
             (default-http-uri-authority))
         => (lambda (authority)
              (values (make-uri #f      ;No scheme
                                #f      ;No authority
                                (uri-path request-uri)
                                (uri-query request-uri)
                                (uri-fragment request-uri))
                      (uri-authority-host authority)
                      (or (uri-authority-port authority)
                          "www"))))
        (else
         (error "Can't figure out what host to send HTTP request to:"
                request-uri))))

(define (call-with-http-response:get request-uri header-fields receiver)
  (call-with-http-response:entity-request 'GET request-uri header-fields #f
    receiver))

(define (call-with-http-response:post request-uri header-fields content
          receiver)
  (call-with-http-response:entity-request 'POST request-uri header-fields
      content
    receiver))

(define (http-head request-uri header-fields)
  (call-with-http-response:entity-request 'HEAD request-uri header-fields #f
    (lambda (http-response input-port)
      input-port                        ;ignore
      http-response)))

(define (http-get request-uri header-fields)
  (call-with-http-response:get request-uri header-fields
    (lambda (http-response input-port)
      (values http-response (read-http-entity http-response input-port)))))

(define (http-post request-uri header-fields content)
  (call-with-http-response:post request-uri header-fields content
    (lambda (http-response input-port)
      (values http-response (read-http-entity http-response input-port)))))

(define (read-http-entity http-response port)
  (or (let ((header-fields (http-response/header-fields http-response)))
        (cond ((rfc822:first-header-field 'CONNECTION header-fields)
               => (lambda (header-field)
                    (and (string-ci=? (rfc822:header-field-value header-field)
                                      "close")
                         (read-all port))))
              ((rfc822:first-header-field 'CONTENT-LENGTH header-fields)
               => (lambda (header-field)
                    (cond ((number->string
                            (rfc822:header-field-value header-field)
                            10)
                           => (lambda (content-length)
                                (read-string-of-length content-length port)))
                          (else #f))))
              (else #f)))
      (begin
        (warn "Unable to determine entity length of response:" http-response)
        #f)))

;;;; HTTP Connections

;++ implement persistent connection pool

(define-structure (http-connection
                   (conc-name http-connection/))
  host
  port
  socket
  ;; marked-for-close?         ; set to true if `Connection: close'
  )

(define (call-with-http-connection host port receiver)
  (let* ((connection (open-http-connection host port))
         (value (receiver connection)))
    (close-http-connection connection)
    value))

(define (open-http-connection host port)
  (guarantee-string host 'OPEN-HTTP-CONNECTION)
  ;++ We'd like to be able to handle other named ports, but we can't.
  (if (not (or (equal? port "www")
               (and (integer? port)
                    (exact? port)
                    (<= 0 port 65535))))
      (error:wrong-type-argument port
                                 "Internet port number"
                                 'OPEN-HTTP-CONNECTION))
  (make-http-connection host port (open-tcp-stream-socket host port)))

(define (http-connection-open? connection)
  (let ((socket (http-connection/socket connection)))
    (and (channel-open? (port/input-channel socket))
         (channel-open? (port/output-channel socket)))))

(define (close-http-connection connection)
  (close-port (http-connection/socket connection)))

(define (http-connection/host-string connection)
  (let ((host (http-connection/host connection))
        (port (http-connection/port connection)))
    (if (equal? port "www")
        host
        (string-append host ":" (number->string port 10)))))

(define (send-http-request connection method request-uri header-fields content)
  (write-http-request method
                      request-uri
                      (rfc822:adjoin-header-fields
                       `((HOST ,(http-connection/host-string connection)))
                       header-fields
                       (if (string? content)
                           `((CONTENT-LENGTH 
                              ,(number->string (string-length content)
                                               10)))
                           '()))
                      content
                      (http-connection/socket connection)))

(define (receive-http-response connection)
  (read-http-response (http-connection/socket connection)))

;;;; HTTP Requests

(define (write-http-request method request-uri header-fields content port)
  (write-http-request-line method request-uri port)
  (rfc822:write-header-fields header-fields port)
  (write-http-content content port)
  (flush-output port))

(define (write-http-request-line method request-uri port)
  (write-http-method method port)
  (write-char #\space port)
  (write-http-request-uri request-uri port)
  (write-char #\space port)
  (write-string "HTTP/" port)
  (write-http-version http-version port)
  (newline port))

(define (write-http-method method port)
  (write-string (cond ((symbol? method) (string-upcase (symbol-name method)))
                      ((string? method) method)
                      (else
                       (error:wrong-type-datum method "HTTP request method")))
                port))

(define (write-http-request-uri request-uri port)
  (cond ((eq? '*  request-uri) (write-char #\* port))
        ((uri?    request-uri) (write-uri request-uri port))
        ((string? request-uri) (write-string request-uri port))
        ((and (pair? request-uri)
              (list-of-type? request-uri string?))
         (for-each (lambda (path-component)
                     (write-char #\/ port)
                     (write-string path-component port))
                   request-uri))
        (else
         (error:wrong-type-datum request-uri "HTTP request URI"))))

(define (write-http-content content port)
  (cond ((procedure? content) (content port))
        ((string? content) (write-string content port))
        ((not content) unspecific)
        (else (error:wrong-type-datum content "HTTP content"))))

;;;; HTTP Responses

(define-structure (http-response
                   (conc-name http-response/))
  version
  status-type
  status-code
  reason
  header-fields
  )

(define (http-response/first-header-field http-response name)
  (rfc822:first-header-field (http-response/header-fields http-response)
                             name))

(define (http-response/all-header-fields http-response name)
  (rfc822:all-header-fields (http-response/header-fields http-response)
                            name))

(define (read-http-response port)
  (receive (http-version status-type status-code reason)
      (read-http-status-line port)
    (let ((header-fields (rfc822:read-header-fields port)))
      (make-http-response http-version
                          status-type
                          status-code
                          reason
                          header-fields))))

(define (read-http-status-line port)
  (let ((vector (http-parser:status-line (input-port->parser-buffer port))))
    (let ((http-version (vector-ref vector 0))
          (status-code (vector-ref vector 1))
          (reason (vector-ref vector 2)))
      (values http-version
              (case (quotient status-code 100)
                ((1) 'INFORMATIONAL)
                ((2) 'SUCCESS)
                ((3) 'REDIRECTION)
                ((4) 'CLIENT-ERROR)
                ((5) 'SERVER-ERROR)
                (else #f))
              status-code
              reason))))

(define http-parser:status-line
  (*parser
   (seq "HTTP/"
        http-parser:version
        #\space
        http-parser:status-code
        #\space
        (match (* (not-char #\newline)))
        ;; This is optional for the bizarre potential usage of this
        ;; parser outside of the HTTP client.
        (? #\newline))))

(define http-parser:version
  (*parser
   (encapsulate (lambda (vector)
                  (make-http-version
                   (string->number (vector-ref vector 0) 10)
                   (string->number (vector-ref vector 1) 10)))
     (seq (match (+ (char-set char-set:numeric)))
          "."
          (match (+ (char-set char-set:numeric)))))))

(define http-parser:status-code
  (*parser
   (map (lambda (status-code)
          (string->number status-code 10))
        (match (n*n 3 (char-set char-set:numeric))))))

;;;; HTTP Version

(define (make-http-version major minor)
  (guarantee-exact-nonnegative-integer major 'MAKE-HTTP-VERSION)
  (guarantee-exact-nonnegative-integer minor 'MAKE-HTTP-VERSION)
  (cons major minor))

(define (http-version? object)
  (and (pair? object)
       (exact-nonnegative-integer? (car object))
       (exact-nonnegative-integer? (cdr object))))

(define-guarantee http-version "HTTP version")

(define (http-version=? a b)
  (guarantee-http-version a 'HTTP-VERSION=?)
  (guarantee-http-version b 'HTTP-VERSION=?)
  (and (= (car a) (car b))
       (= (cdr a) (cdr b))))

(define (http-version<? a b)
  (guarantee-http-version a 'HTTP-VERSION<?)
  (guarantee-http-version b 'HTTP-VERSION<?)
  (or (< (car a) (car b))
      (and (= (car a) (car b))
           (< (cdr a) (cdr b)))))

(define (write-http-version version port)
  (write (car version) port)
  (write-char #\. port)
  (write (cdr version) port))

(define http-version (make-http-version 1 0))

;;;; Random Utilities

(define (read-all input-port)
  (call-with-output-string
    (lambda (output-port)
      (let ((buffer (string-allocate #x100)))
        (let loop ()
          (let ((octets (read-string! buffer input-port)))
            (if (fix:> octets 0)
                (begin
                  (write-substring buffer 0 octets output-port)
                  (loop)))))))))

(define (read-string-of-length length input-port)
  (let* ((string (string-allocate length))
         (octets (read-substring! string 0 length input-port)))
    (if (fix:< octets length)
        (string-prefix string octets)
        string)))

;;;; RFC 822 Header Fields

;;; This should be moved into the run-time library along with Edwin's
;;; RFC 822 support, and something ought to be done about RFC 2822.
;;; Some day.

(define (rfc822:header-field? obj)
  (and (pair? obj)
       (symbol? (car obj))
       (let* ((name (symbol-name (car obj)))
              (length (string-length name)))
         (and (> length 0)
              (rfc822:header-field-name? name 0 length)))
       (pair? (cdr obj))
       (string? (cadr obj))
       (null? (cddr obj))))

(define-guarantee rfc822:header-field "RFC 822 header field")

(define (rfc822:make-header-field name value) (list name value))
(define (rfc822:header-field-name header) (car header))
(define (rfc822:header-field-value header) (cadr header))

(define (rfc822:first-header-field name header-fields)
  (assq name header-fields))

(define (rfc822:all-header-fields name header-fields)
  (keep-matching-items header-fields
    (lambda (header-field)
      (eq? (rfc822:header-field-name header-field)
           name))))

(define (rfc822:adjoin-header-fields left header-fields right)
  (let ((clean (lambda (other-header-fields)
                 (delete-matching-items other-header-fields
                   (lambda (header-field)
                     (and (rfc822:first-header-field
                           (rfc822:header-field-name header-field)
                           header-fields)
                          #t))))))
    (append (clean left) header-fields (clean right))))

;;;;; RFC 822 Header Field Output

(define (rfc822:header-field->string header-field)
  (call-with-output-string
    (lambda (port)
      (rfc822:write-header-field header-field port))))

(define (rfc822:header-fields->string header-fields)
  (call-with-output-string
    (lambda ()
      (rfc822:write-header-fields header-fields port))))

(define (rfc822:write-header-field header-field port)
  (rfc822:write-header-field-name (rfc822:header-field-name header-field) port)
  (write-string ": " port)
  (let* ((value (rfc822:header-field-value header-field))
         (end (string-length value)))
    (let loop ((start 0))
      (cond ((substring-find-next-char value start end #\newline)
             => (lambda (index)
                  (write-substring value start index port)
                  (newline port)
                  (write-char #\space port)
                  (loop (fix:+ index 1))))
            (else
             (write-substring value start end port)
             (newline port))))))

(define (rfc822:write-header-field-name name port)
  (let* ((name (if (symbol? name)
                   (symbol-name name)
                   name))
         (end (string-length name)))
    (if (not (char-alphabetic? (string-ref name 0)))
        (write-string name port)
        (let loop ((start 0))
          (write-char (char-upcase (string-ref name start)) port)
          (cond ((substring-find-next-char name (fix:+ start 1) end #\-)
                 => (lambda (index)
                      (write-substring name
                                       (fix:+ start 1)
                                       (fix:+ index 1)
                                       port)
                      (loop (fix:+ index 1))))
                (else
                 (write-substring name (fix:+ start 1) end port)))))))

(define (rfc822:write-header-fields header-fields port)
  (for-each (lambda (header-field)
              (rfc822:write-header-field header-field port))
            header-fields)
  (newline port))

;;;;; RFC 822 Header Field Input

(define (rfc822:string->header-fields string)
  (vector->list
   (rfc822:parser:header-fields
    (string->parser-buffer string))))

(define (rfc822:read-header-fields input-port)
  (vector->list
   (rfc822:parser:header-fields
    (input-port->parser-buffer input-port))))

(define rfc822:parser:header-fields
  (*parser
   (seq (* (seq rfc822:parser:header-field #\newline))
        #\newline)))

(define rfc822:parser:header-field
  (*parser
   (encapsulate (lambda (vector)
                  (rfc822:make-header-field
                   (vector-ref vector 0)
                   (decorated-string-append
                    "" (string #\newline) ""
                    (map string-trim
                         (subvector->list vector 1 (vector-length vector))))))
     (seq (map intern (match rfc822:matcher:header-field-name))
          ":"
          (match rfc822:matcher:header-field-line-content)
          (* (match rfc822:matcher:header-field-continuation-line))))))

(define rfc822:matcher:header-field-name
  (*matcher (* (char-set rfc822:char-set:header-constituents))))

(define rfc822:char-set:header-constituents
  (char-set-difference (ascii-range->char-set 33 127)
		       (char-set #\:)))

(define rfc822:matcher:header-field-line-content
  (*matcher (* (not-char #\newline))))

(define rfc822:matcher:header-field-continuation-line
  (*matcher
   (seq #\newline
        (+ (char-set rfc822:char-set:lwsp))
        rfc822:matcher:header-field-line-content)))

(define rfc822:char-set:lwsp (char-set #\space #\tab))
