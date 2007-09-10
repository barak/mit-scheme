#| -*-Scheme-*-

$Id: imail-mime.scm,v 1.8 2007/09/10 17:18:24 riastradh Exp $

Copyright 2005 Taylor Campbell

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

;;;; IMAIL mail reader: MIME parser

(declare (usual-integrations))

(define-method mime-message-body-structure ((message <message>))
  (or (get-property message 'MIME-MESSAGE-BODY-STRUCTURE #f)
      (cond ((mime:get-version-header message)
             => (lambda (version-string)
                  (if (mime:version-1.0? version-string)
                      (let ((body-structure
                             (mime:parse-body-structure message)))
                        (store-property! message
                                         'MIME-MESSAGE-BODY-STRUCTURE
                                         body-structure)
                        body-structure)
                      (error "MIME version not 1.0:"
                             version-string
                             message))))
            (else #f))))

(define (mime:parse-body-structure message)
  (let ((content-type (mime:get-content-type message))
        (encoding (mime:get-content-transfer-encoding message)))
    (let ((type (car content-type))
          (subtype (cadr content-type))
          (parameters (cddr content-type)))
      ((let ((top-level (assq type mime:media-parsers))
	     (default mime:basic-media-parser))
	 (cond ((not top-level) default)
	       ((assq subtype (cddr top-level)) => cdr)
	       ((cadr top-level))
	       (else default)))
       message type subtype parameters encoding))))

(define (mime:get-content-type message)
  (parse-first-named-header message
                            "Content-Type"
                            mime:default-content-type
                            mime:parse-content-type))

(define (mime:get-content-transfer-encoding message)
  (named-mime-encoding
   (or (parse-first-named-header message
                                 "Content-Transfer-Encoding"
                                 mime:default-encoding
                                 mime:parse-encoding)
       '7BIT)))

(define-method write-mime-message-body-part
    ((message <message>) selector cache? port)
  cache?
  (if (not (pair? selector))
      (write-message-body message port)
      (let ((lose
             (lambda ()
               (error "Invalid message MIME body selector:"
                      selector
                      message))))
        (let loop ((selector selector)
                   (part (mime-message-body-structure message)))
          (let ((item (car selector))
                (selector (cdr selector)))
            (cond ((exact-nonnegative-integer? item)
                   (if (not (mime-body-multipart? part))
                       (error "Selecting part of non-multipart:"
                              part
                              selector))
                   (let ((subpart
                          (list-ref (mime-body-multipart-parts part)
                                    item)))
                     (if (pair? selector)
                         (loop selector subpart)
                         (begin
                           (if (message? subpart)
                               (begin
                                 (write-header-fields
                                  (message-header-fields subpart)
                                  port)
                                 (newline port)))
                           (write-message-body subpart port)))))
                  ((not (pair? selector))
                   (case item
                     ((TEXT)
                      (write-message-body part port))
                     ((HEADER)
                      (write-header-fields (message-header-fields part)
                                           port))
                     (else (lose))))
                  (else (lose))))))))

;;;; MIME-Version Header Field

(define (mime:get-version-header message)
  (get-first-header-field-value (message-header-fields message)
                                "MIME-Version"
                                ;; No error if not found.
                                #f))

(define (mime:version-1.0? string)
  (let ((tokens (mime:string->non-ignored-tokens string)))
    (let loop ((in tokens)
               (out '()))
      (if (pair? in)
          (let ((token (car in))
                (in (cdr in)))
            (cond ((string? token)
                   (loop in (cons token out)))
                  ((char? token)
                   (loop in (cons (string token) out)))
                  (else #f)))
          (string=? (apply string-append (reverse! out))
                    "1.0")))))

(define mime:media-parsers '())

;++ What about top-level media types whose subtypes are mandated to
;++ have common syntax?

(define (define-mime-media-parser type subtype parser)
  (cond ((assq type mime:media-parsers)
         => (lambda (top-level)
              (if subtype
                  (let ((subtype-parsers (cddr top-level)))
                    (cond ((assq subtype subtype-parsers)
                           => (lambda (sub-level)
                                (warn "Replacing MIME parser:"
                                      (symbol type '/ subtype)
                                      (cdr sub-level)
                                      (error-irritant/noise " with")
                                      parser)
                                (set-cdr! top-level parser)))
                          (else
                           (set-cdr! (cdr top-level)
                                     (cons (cons subtype parser)
                                           subtype-parsers)))))
                  (begin
                    (if (cadr top-level)
                        (warn "Replacing default MIME parser:"
                              type
                              (cadr top-level)
                              (error-irritant/noise " with")
                              parser))
                    (set-car! (cdr top-level) parser)))))
        (else
         (set! mime:media-parsers
               (cons (cons type
                           (if subtype
                               (list #f (cons subtype parser))
                               (list parser)))
                     mime:media-parsers))
	 unspecific)))

(define-class <message-part> ()
  (string define accessor)
  (start  define accessor)
  (end    define accessor))

(define-method message-length ((message <message-part>))
  (- (message-part-end message)
     (message-part-start message)))

(define-method message-body ((message <message-part>))
  (values (message-part-string message)
          (message-part-start  message)
          (message-part-end    message)))

(define-method write-message-body ((message <message-part>) port)
  (write-substring (message-part-string message)
                   (message-part-start  message)
                   (message-part-end    message)
                   port))

(define-class (<mime-body-basic-part>
               (constructor make-mime-body-basic-part
                            (string
                             start end
                             type subtype parameters
                             id description
                             encoding
                             n-octets
                             md5
                             disposition language)))
    (<mime-body-basic> <message-part>))

;;; This is the default media parser, equivalent to a Content-Type of
;;; APPLICATION/OCTET-STREAM.

(define mime:basic-media-parser
  (lambda (message type subtype parameters encoding)
    (receive (string start end) (message-body message)
      (make-mime-body-basic-part
       string start end
       type subtype parameters
       (mime:get-content-id message)
       (mime:get-content-description message)
       (mime-encoding/name encoding)
       (- end start)
       (ignore-errors (lambda () (md5-substring string start end))
                      (lambda (condition) condition #f))
       (mime:get-content-disposition message)
       (mime:get-content-language message)))))

;;; This is unnecessary, but it's nice to make things explicit.

(define-mime-media-parser 'APPLICATION 'OCTET-STREAM
  mime:basic-media-parser)

(define-class (<mime-body-text-part>
               (constructor make-mime-body-text-part
                            (string
                             start end
                             subtype parameters
                             id description
                             encoding
                             n-octets n-lines
                             md5
                             disposition language)))
    (<mime-body-text> <message-part>))

(define-mime-media-parser 'TEXT #f
  (lambda (message type subtype parameters encoding)
    type                                ;ignore
    (receive (string start end) (message-body message)
      (make-mime-body-text-part
       string start end
       subtype parameters
       (mime:get-content-id message)
       (mime:get-content-description message)
       (mime-encoding/name encoding)
       (- end start)                             ;Octets
       (substring-n-newlines string start end)   ;Lines
       (ignore-errors (lambda () (md5-substring string start end))
                      (lambda (condition) condition #f))
       (mime:get-content-disposition message)
       (mime:get-content-language message)))))

;;;; Multipart Media

(define-mime-media-parser 'MULTIPART #f
  (lambda (message type subtype parameters encoding)
    type                                ;ignore
    (mime:parse-multipart message subtype parameters encoding)))

(define-mime-media-parser 'MULTIPART 'DIGEST
  (lambda (message type subtype parameters encoding)
    type                                ;ignore
    (fluid-let ((mime:default-content-type '(MESSAGE RFC822)))
      (mime:parse-multipart message subtype parameters encoding))))

(define (mime:parse-multipart message subtype parameters encoding)
  (let* ((parts (mime:parse-multipart-subparts message parameters
                                               encoding))
         (enclosure (make-mime-body-multipart
                     subtype parameters
                     parts
                     (mime:get-content-disposition message)
                     (mime:get-content-language message))))
    (for-each (lambda (part)
                (set-mime-body-enclosure! part enclosure))
              parts)
    enclosure))

(define (mime:parse-multipart-subparts message parameters encoding)
  (let ((boundary (mime:get-boundary parameters message)))
    (let ((do-it (lambda (body start end)
                   (mime:parse-parts
                    body
                    (mime:multipart-message-parts body start end
                                                  boundary)))))
      (if (mime-encoding/identity? message)
          (call-with-values (lambda () (message-body message))
            do-it)
          (let ((body
                 (call-with-output-string
                   (lambda (output-port)
                     (call-with-mime-decoding-output-port
                         encoding output-port #t
                       (lambda (output-port)
                         (write-message-body message output-port)))))))
            (do-it body 0 (string-length body)))))))

(define (mime:get-boundary parameters message)
  (cond ((assq 'BOUNDARY parameters)
         => (lambda (probe)
              (string-append "--" (cdr probe))))
        (else
         (error "MIME multipart message has no boundary:"
                message))))

(define (mime:multipart-message-parts string start end boundary)
  (let ((boundary-length (string-length boundary)))

    (define (loop part-start search-start parts)
      (cond ((substring-search-forward boundary string
                                       search-start end)
             => (lambda (boundary-start)
                  (let ((boundary-end
                         (+ boundary-start boundary-length)))
                    (if (or (zero? boundary-start)
                            (char=? (string-ref string
                                                (- boundary-start 1))
                                    #\newline))
                        (continue part-start
                                  (if (zero? boundary-start)
                                      0
                                      (- boundary-start 1))
                                  boundary-end
                                  parts)
                        (loop part-start boundary-end parts)))))
            (else (lose parts))))

    (define (continue part-start part-end boundary-end parts)
      (cond ((and (>= end (+ boundary-end 2))
                  (char=? #\- (string-ref string boundary-end))
                  (char=? #\- (string-ref string (+ boundary-end 1))))
             (win (cons (cons part-start part-end) parts)))
            ((skip-lwsp-until-newline string boundary-end end)
             => (lambda (next-line-start)
                  (loop next-line-start
                        next-line-start
                        (cons (cons part-start part-end) parts))))
            (else
             (loop part-start boundary-end parts))))

    (define (win parts)
      (cdr (reverse! parts)))

    (define (lose parts)
      (cdr (reverse! parts)))

    (loop start start '())))

;;;;; MIME Part Messages

(define-class (<message-part-message>
               (constructor make-message-part-message
                            (header-fields string start end)))
    ;** Do not rearrange this!  The MESSAGE-BODY method on
    ;** <MESSAGE-PART> must be more given precedence over that on
    ;** <MESSAGE>!
    (<message-part> <message>))

(define (mime:parse-part string header-start header-end body-end)
  (mime:parse-body-structure
   (make-message-part-message (lines->header-fields
                               (substring->lines string header-start
                                                 header-end))
                              string
                              (+ header-end 1)
                              body-end)))

(define (mime:parse-parts body parts)
  (map (lambda (part)
         (mime:parse-body-structure
          (let ((start (car part))
                (end (cdr part)))
            (cond ((char=? #\newline (string-ref body start))
                   ;; If it starts with a blank line, there are no
                   ;; headers.
                   (make-message-part-message '() body (+ start 1) end))
                  ((substring-search-forward "\n\n" body start end)
                   => (lambda (header-end)
                        (make-message-part-message
                         (lines->header-fields
                          (substring->lines body start
                                            ;; Add trailing newline.
                                            (+ header-end 1)))
                         body
                         ;; Skip the two newlines.
                         (+ header-end 2)
                         end)))
                  (else
                   ;; Grossly assume that the absence of a blank line
                   ;; means there are no headers.
                   (make-message-part-message '() body start end))))))
       parts))

;;;; Content-Type Header Fields

(define mime:default-content-type '(TEXT PLAIN (CHARSET . "us-ascii")))

(define (mime:parse-content-type string)
  (let ((tokens (mime:string->non-ignored-tokens string)))
    (if (pair? tokens)
        (let ((type (car tokens))
              (tokens (cdr tokens)))
          (if (and (string? type)
                   (pair? tokens))
              (let ((slash (car tokens))
                    (tokens (cdr tokens)))
                (if (and (eqv? slash #\/)
                         (pair? tokens))
                    (let ((subtype (car tokens))
                          (tokens (cdr tokens)))
                      (if (string? subtype)
                          (cons* (intern type)
                                 (intern subtype)
                                 (mime:parse-parameters tokens
							"Content-Type"))
                          #f))
                    #f))
              #f))
        #f)))

;;;; Other Content-... Fields

(define mime:default-encoding '7BIT)

(define (mime:parse-encoding encoding)
  (let ((tokens (mime:string->non-ignored-tokens encoding)))
    (if (and (pair? tokens)
             (string? (car tokens))
             (null? (cdr tokens)))
        (intern (car tokens))
        #f)))

(define (mime:get-content-id message)
  (parse-first-named-header message "Content-ID" #f rfc822:parse-msg-id))

(define (mime:get-content-description message)
  (parse-first-named-header message "Content-Description" #f
                            mime:parse-encoded-header-value))

(define (mime:parse-encoded-header-value value)
  ;++ implement
  value)

(define (mime:get-content-disposition message)
  (parse-first-named-header message "Content-Disposition" #f
                            mime:parse-disposition))

(define (mime:parse-disposition disposition)
  (let ((tokens (mime:string->non-ignored-tokens disposition)))
    (if (pair? tokens)
        (let ((type (car tokens))
              (tokens (cdr tokens)))
          (if (string? type)
              (cons (intern type)
                    (mime:parse-parameters tokens
                                           "Content-Disposition"))
              #f))
        #f)))

(define (mime:get-content-language message)
  ;++ implement
  #f)

;;;; Extended RFC 822 Tokenizer

(define mime:special-chars
  (char-set #\( #\) #\< #\> #\@
	    #\, #\; #\: #\\ #\"
	    #\/ #\[ #\] #\? #\=))

;;; STRING->TOKENS includes whitespace & parenthesis comments;
;;; STRING->NON-IGNORED-TOKENS omits them.

(define mime:string->tokens
  (rfc822:string-tokenizer mime:special-chars #t))

(define mime:string->non-ignored-tokens
  (rfc822:string-tokenizer mime:special-chars #f))

;;; Too bad the parser language works only on strings; it would be
;;; nice to be able to use it for general tokens, like RFC822 tokens.

(define (mime:parse-parameters tokens header-name)
  (let ((lose (lambda (tokens)
                (warn (string-append "Malformed " header-name
                                     " parameter tokens:")
                      tokens)
                '())))
    (let recur ((tokens tokens))
      (if (pair? tokens)
          (let ((lose (lambda () (lose tokens))))
            (let ((semi (car tokens))
                  (tokens (cdr tokens)))
              (if (and (eqv? semi #\;)
                       (pair? tokens))
                  (let ((attribute (car tokens))
                        (tokens (cdr tokens)))
                    (if (pair? tokens)
                        (let ((equals (car tokens))
                              (tokens (cdr tokens)))
                          (if (and (eqv? equals #\=)
                                   (pair? tokens))
                              (cons (cons (intern attribute)
                                          (rfc822:unquote-string
                                           (car tokens)))
                                    (recur (cdr tokens)))
                              (lose)))
                        (lose)))
                  (lose))))
          '()))))
