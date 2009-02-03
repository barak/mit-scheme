#| -*-Scheme-*-

$Id: imail-mime.scm,v 1.13 2009/02/03 01:16:52 riastradh Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology
Copyright (C) 2005, 2006, 2007, 2008 Taylor R. Campbell

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

;;;; MIME Entities

;;; Any kind of object can be a MIME entity, provided that it
;;; implements MIME-ENTITY-BODY-STRUCTURE.  A default method is
;;; provided if it instead implements MIME-ENTITY-HEADER-FIELDS and
;;; either MIME-ENTITY-BODY-SUBSTRING or WRITE-ENTITY-MIME-BODY, which
;;; yield the literal text of the entity's body without decoding or
;;; interpretation.  MIME-ENTITY-BODY-STRUCTURE should return a
;;; <MIME-BODY> instance.
;;;
;;; The reason that we do not have a specific class for MIME entities
;;; is that many objects are implicitly MIME entities, such as RFC
;;; (2)822 messages, whose header may contain MIME header fields and
;;; whose body may be a MIME body, but which may otherwise have other
;;; structure unrelated to MIME.

(define-generic mime-entity? (object))
(define-generic mime-entity-header-fields (mime-entity))
(define-generic mime-entity-body-structure (mime-entity))
(define-generic mime-entity-body-substring (mime-entity))
(define-generic write-mime-entity-body (mime-entity port))

(define-method mime-entity? (object) object #f)

(define-guarantee mime-entity "MIME entity")

(define-method mime-entity-body-substring (mime-entity)
  (guarantee-mime-entity mime-entity 'MIME-ENTITY-BODY-SUBSTRING)
  (let ((string
	 (call-with-output-string
	   (lambda (output-port)
	     (write-mime-entity-body mime-entity output-port)))))
    (values string 0 (string-length string))))

(define-method write-mime-entity-body (mime-entity port)
  (guarantee-mime-entity mime-entity 'WRITE-MIME-ENTITY-BODY)
  (receive (string start end) (mime-entity-body-substring mime-entity)
    (write-substring string start end port)))

;;;; MIME Bodies

;;; A MIME body is an instance of a subclass of <MIME-BODY>.  It must
;;; implement MIME-BODY-TYPE, MIME-BODY-SUBTYPE,
;;; MIME-BODY-HEADER-FIELDS, and either MIME-BODY-SUBSTRING or
;;; WRITE-MIME-BODY.

(define-class <mime-body> (<property-mixin>)
  (parameters define accessor)
  (disposition define accessor)
  (language define accessor)
  (enclosure define standard initial-value #f))

(define-generic mime-body-type (body))
(define-generic mime-body-subtype (body))
(define-generic mime-body-header-fields (body))
(define-generic mime-body-substring (mime-body))
(define-generic write-mime-body (mime-body port))

(define-method mime-body-substring ((body <mime-body>))
  (let ((string
         (call-with-output-string
           (lambda (output-port)
             (write-mime-body body output-port)))))
    (values string 0 (string-length string))))

(define-method write-mime-body ((body <mime-body>) port)
  (receive (string start end) (mime-body-substring body)
    (write-substring string start end port)))

(define (mime-body-type-string body)
  (string-append (symbol->string (mime-body-type body))
                 "/"
                 (symbol->string (mime-body-subtype body))))

(define (mime-body-parameter body key default)
  (let ((entry (assq key (mime-body-parameters body))))
    (if entry
        (cdr entry)
        default)))

(define (mime-body-disposition-filename body)
  (let ((disposition (mime-body-disposition body)))
    (and disposition
         (let ((entry (assq 'FILENAME (cdr disposition))))
           (and entry
                (cdr entry))))))

(define-method write-instance ((body <mime-body>) port)
  (write-instance-helper 'MIME-BODY body port
    (lambda ()
      (write-char #\space port)
      (write-string (mime-body-type-string body) port))))

(define (mime-body-enclosed? b1 b2)
  (or (eq? b1 b2)
      (let ((enclosure (mime-body-enclosure b1)))
        (and enclosure
             (mime-body-enclosed? enclosure b2)))))

(define-class <mime-body-substring> ()
  (header-fields accessor mime-body-header-fields)
  (string define accessor)
  (start define accessor)
  (end define accessor))

(define-method mime-body-substring ((body <mime-body-substring>))
  (values (mime-body-substring-string body)
          (mime-body-substring-start body)
          (mime-body-substring-end body)))

(define-class <mime-body-one-part> (<mime-body>)
  (id define accessor)
  (description define accessor)
  (encoding define accessor)
  (n-octets define accessor)
  ;++ This is a random artefact of the IMAP.  We don't use it.
  (md5 define accessor))

(define-class <mime-body-basic> (<mime-body-one-part>)
  (type accessor mime-body-type)
  (subtype accessor mime-body-subtype))

(define-class (<mime-body-basic-substring>
               (constructor (header-fields
                             string start end type subtype parameters id
                             description encoding n-octets md5 disposition
                             language)))
    (<mime-body-basic> <mime-body-substring>))

(define-class <mime-body-text> (<mime-body-one-part>)
  (subtype accessor mime-body-subtype)
  (n-lines define accessor))

(define-method mime-body-type ((body <mime-body-text>)) body 'TEXT)

(define-class (<mime-body-text-substring>
               (constructor (header-fields
                             string start end subtype parameters id description
                             encoding n-octets n-lines md5 disposition
                             language)))
    (<mime-body-text> <mime-body-substring>))

(define-class <mime-body-message> (<mime-body-one-part>)
  (envelope define accessor)            ;<mime-envelope> instance
  (body define accessor)                ;<mime-body> instance
  (n-lines define accessor))

(define-method mime-body-type ((body <mime-body-message>)) body 'MESSAGE)
(define-method mime-body-subtype ((body <mime-body-message>)) body 'RFC822)

(define-generic mime-body-message-header-fields (mime-body-message))

;;; In a <MIME-BODY-MESSAGE-SUBSTRING> instance, the HEADER-FIELDS
;;; slot contains the MIME header fields for the enclosure, and the
;;; substring contains the complete RFC 822 message, including header
;;; and body.

(define-class (<mime-body-message-substring>
               (constructor (header-fields
			     message-header-fields
                             string start end parameters id description
                             encoding envelope body n-octets n-lines md5
                             disposition language)))
    (<mime-body-message> <mime-body-substring>)
  (message-header-fields accessor mime-body-message-header-fields))

(define-class (<mime-envelope>
               (constructor (date subject from sender reply-to to cc bcc
                                  in-reply-to message-id)))
    ()
  (date define accessor)
  (subject define accessor)
  (from define accessor)
  (sender define accessor)
  (reply-to define accessor)
  (to define accessor)
  (cc define accessor)
  (bcc define accessor)
  (in-reply-to define accessor)
  (message-id define accessor))

(define-class (<mime-address> (constructor (name source-route mailbox host)))
    ()
  (name define accessor)
  (source-route define accessor)
  (mailbox define accessor)
  (host define accessor))

(define-class <mime-body-multipart> (<mime-body>)
  (subtype accessor mime-body-subtype)
  (parts define accessor))

(define-method mime-body-type ((body <mime-body-multipart>)) body 'MULTIPART)

(define-class (<mime-body-multipart-substring>
	       (constructor (header-fields
			     string start end
			     subtype parameters parts disposition language)))
    (<mime-body-multipart> <mime-body-substring>))

;;;; MIME Parser

(define-method mime-entity-body-structure (entity)
  (and (mime-entity? entity)
       (let ((header-fields (mime-entity-header-fields entity)))
         (and header-fields
              (let ((version (mime:get-version-string header-fields)))
                (and version
                     (mime:version-1.0? version)
                     (receive (string start end)
                         (mime-entity-body-substring entity)
                       (mime:parse-body-structure header-fields
                                                  string
                                                  start
                                                  end))))))))

;;; In MIME entities that have properties, we cache the body
;;; structures, but weakly, because they may involve very large
;;; strings not already stored in the entity, if parts of the body
;;; require decoding.  This should almost be an around method (if SOS
;;; supported such things), but in some cases, such as IMAP messages,
;;; caching is already handled by another mechanism.  So this is
;;; really useful only for use with the default MIME parser.

(define-method mime-entity-body-structure ((entity <property-mixin>))
  (define (next store)
    (let ((body-structure (call-next-method entity)))
      (store body-structure)
      body-structure))
  (let ((cache (get-property entity 'MIME-ENTITY-BODY-STRUCTURE #f)))
    (if cache
        (let ((body-structure (weak-car cache)))
          (if (weak-pair/car? cache)
              body-structure
              (next (lambda (value) (weak-set-car! cache value)))))
        (next (lambda (value)
                (store-property! entity
                                 'MIME-ENTITY-BODY-STRUCTURE
                                 (weak-cons value '())))))))

(define (mime:parse-body-structure header-fields string start end)
  (let ((content-type (mime:get-content-type header-fields)))
    (let ((type (car content-type))
          (subtype (cadr content-type))
          (parameters (cddr content-type)))
      ((let ((top-level (assq type mime:media-parsers))
             (default mime:basic-media-parser))
         (cond ((not top-level) default)
               ((assq subtype (cddr top-level)) => cdr)
               ((cadr top-level))
               (else default)))
       header-fields string start end type subtype parameters))))

(define (mime:get-version-string header-fields)
  (get-first-header-field-value header-fields "MIME-Version" #f))

(define (mime:version-1.0? string)
  (let ((tokens (mime:string->non-ignored-tokens string)))
    (let loop ((in tokens) (out '()))
      (if (pair? in)
          (let ((token (car in)) (in (cdr in)))
            (cond ((string? token) (loop in (cons token out)))
                  ((char? token) (loop in (cons (string token) out)))
                  (else #f)))
          (string=? "1.0" (apply string-append (reverse! out)))))))

(define mime:media-parsers '())

(define (define-mime-media-parser type subtype parser)
  (guarantee-interned-symbol type 'DEFINE-MIME-MEDIA-PARSER)
  (if subtype
      (guarantee-interned-symbol subtype 'DEFINE-MIME-MEDIA-PARSER))
  (guarantee-procedure-of-arity
   parser
   (length '(HEADER-FIELDS STRING START END TYPE SUBTYPE PARAMETERS))
   'DEFINE-MIME-MEDIA-PARSER)
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
                                (set-cdr! sub-level parser)))
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

(define (substring-header&body-bounds string start end)
  (cond ((= start end)
         (values start start start start))
        ((char=? #\newline (string-ref string start))
         (values start start (+ start 1) end))
        (else
         (let ((index (substring-search-forward "\n\n" string start end)))
           (if index
               (values start (+ index 1) (+ index 2) end)
               (values start end end end))))))

(define (mime:parse-entity string start end)
  (receive (header-start header-end body-start body-end)
      (substring-header&body-bounds string start end)
    (mime:parse-body-structure
     (lines->header-fields (substring->lines string header-start header-end))
     string
     body-start
     body-end)))

;;; This is the default media parser, equivalent to a Content-Type of
;;; APPLICATION/OCTET-STREAM.

(define mime:basic-media-parser
  (lambda (header-fields string start end type subtype parameters)
    (make-mime-body-basic-substring
     header-fields string start end
     type subtype parameters
     (mime:get-content-id header-fields)
     (mime:get-content-description header-fields)
     (mime:get-content-transfer-encoding header-fields)
     (- end start)
     (ignore-errors (lambda () (md5-substring string start end))
                    (lambda (condition) condition #f))
     (mime:get-content-disposition header-fields)
     (mime:get-content-language header-fields))))

;;; This is unnecessary, but it's nice to make things explicit.

(define-mime-media-parser 'APPLICATION 'OCTET-STREAM
  mime:basic-media-parser)

(define-mime-media-parser 'TEXT #f
  (lambda (header-fields string start end type subtype parameters)
    type                                ;ignore
    (make-mime-body-text-substring
     header-fields string start end
     subtype parameters
     (mime:get-content-id header-fields)
     (mime:get-content-description header-fields)
     (mime:get-content-transfer-encoding header-fields)
     (- end start)
     (substring-n-newlines string start end)
     (ignore-errors (lambda () (md5-substring string start end))
                    (lambda (condition) condition #f))
     (mime:get-content-disposition header-fields)
     (mime:get-content-language header-fields))))

(define-mime-media-parser 'MESSAGE 'RFC822
  (lambda (header-fields string start end type subtype parameters)
    type subtype                        ;ignore
    (let ((body (mime:parse-entity string start end)))
      ((lambda (enclosure)
         (set-mime-body-enclosure! body enclosure)
         enclosure)
       (make-mime-body-message-substring
        header-fields (mime-body-header-fields body) string start end
        parameters
        (mime:get-content-id header-fields)
        (mime:get-content-description header-fields)
        (mime:get-content-transfer-encoding header-fields)
        (mime:parse-envelope
         (receive (header-start header-end body-start body-end)
             (substring-header&body-bounds string start end)
           body-start body-end          ;ignore
           (lines->header-fields
            (substring->lines string header-start header-end))))
        body
        (- end start)
        (substring-n-newlines string start end)
        (ignore-errors (lambda () (md5-substring string start end))
                       (lambda (condition) condition #f))
        (mime:get-content-disposition header-fields)
        (mime:get-content-language header-fields))))))

(define (mime:parse-envelope header-fields)
  (make-mime-envelope
   (get-first-header-field-value header-fields "date" #f)
   (get-first-header-field-value header-fields "subject" #f)
   (parse-first-named-header header-fields "from" #f mime:parse-addresses)
   (parse-first-named-header header-fields "sender" #f mime:parse-addresses)
   (parse-first-named-header header-fields "reply-to" #f mime:parse-addresses)
   (parse-first-named-header header-fields "to" #f mime:parse-addresses)
   (parse-first-named-header header-fields "cc" #f mime:parse-addresses)
   (parse-first-named-header header-fields "bcc" #f mime:parse-addresses)
   (get-first-header-field-value header-fields "in-reply-to" #f)
   (get-first-header-field-value header-fields "message-id" #f)))

;++ Provisional crock.  No group address or source route syntax.

(define (mime:parse-addresses string)
  (let* ((tokens (rfc822:string->tokens string))
         (result (rfc822:parse-list tokens #\, rfc822:parse-address)))
    (and result
         (let ((addresses (car result)) (tokens (cdr result)))
           (and (not (pair? tokens)) addresses)))))

(define (rfc822:parse-address tokens)
  (or (rfc822:parse-name-addr tokens)
      (rfc822:parse-addr-spec tokens)))

(define (rfc822:parse-name-addr tokens)
  (define (finish name mailbox host tokens)
    (cons (make-mime-address name #f mailbox host) tokens))
  (let loop ((tokens tokens) (name-tokens '()))
    (and (pair? tokens)
         (cond ((eqv? (car tokens) #\<)
                (let ((name (rfc822:tokens->string (reverse name-tokens)))
                      (result (rfc822:parse-angle-addr tokens)))
                  (and result
                       (let ((local-part (caar result))
                             (domain (cadar result))
                             (tokens (cdr result)))
                         (let ((result
                                (rfc822:parse-comment-names name tokens)))
                           (and (pair? result)
                                (let ((name (car result))
                                      (tokens (cdr result)))
                                  (finish name local-part domain tokens))))))))
               (else
                (and (or (eqv? (car tokens) #\space)
                         (and (string? (car tokens))
                              (not (char=? #\[ (string-ref (car tokens) 0)))))
                     (loop (cdr tokens)
                           (cons (car tokens) name-tokens))))))))

(define (rfc822:parse-comment-names name tokens)
  (define (finish names tokens)
    (cons (rfc822:tokens->string (reverse (map string-trim names))) tokens))
  (let loop ((tokens tokens)
             (names (if (string-null? name) '() (list name))))
    (if (not (pair? tokens))
        (finish names tokens)
        (let ((token (car tokens)))
          (if (and (string? token) (char=? #\( (string-ref token 0)))
              (loop (cdr tokens)
                    (cons (if (pair? names)
                              (substring token 1 (- (string-length token) 1))
                              token)
                          names))
              (finish names tokens))))))

(define (rfc822:parse-angle-addr tokens)
  (and (pair? tokens)
       (eqv? #\< (car tokens))
       (let ((result (rfc822:parse-addr-spec (cdr tokens))))
         (and (pair? result)
              (let ((addr-spec (car result)) (tokens (cdr result)))
                (and (pair? tokens)
                     (eqv? #\> (car tokens))
                     (cons addr-spec (cdr tokens))))))))

(define (rfc822:parse-addr-spec tokens)
  (let ((result (rfc822:parse-list tokens #\. rfc822:parse-word)))
    (and (pair? result)
         (let ((local-part (decorated-string-append "" "." "" (car result)))
               (tokens (cdr result)))
           (and (pair? tokens)
                (eqv? #\@ (car tokens))
                (let ((result (rfc822:parse-domain (cdr tokens))))
                  (and (pair? result)
                       (let ((domain
                              (decorated-string-append "" "." "" (car result)))
                             (tokens
                              (cdr result)))
                         (cons (list local-part domain) tokens)))))))))

;;;; Multipart Media

(define-mime-media-parser 'MULTIPART #f
  (lambda (header-fields string start end type subtype parameters)
    type                                ;ignore
    (mime:parse-multipart header-fields string start end subtype parameters)))

(define-mime-media-parser 'MULTIPART 'DIGEST
  (lambda (header-fields string start end type subtype parameters)
    type                                ;ignore
    (fluid-let ((mime:default-content-type '(MESSAGE RFC822)))
      (mime:parse-multipart header-fields string start end
                            subtype parameters))))

(define (mime:parse-multipart header-fields string start end
                              subtype parameters)
  (let ((boundary (mime:get-boundary parameters)))
    (and boundary
         (let ((parts
                (mime:parse-multipart-parts header-fields string start end
                                            boundary)))
           (and parts
                (let* ((enclosure
                        (make-mime-body-multipart-substring
                         header-fields string start end
			 subtype parameters parts
                         (mime:get-content-disposition header-fields)
                         (mime:get-content-language header-fields))))
                  (for-each (lambda (part)
                              (set-mime-body-enclosure! part enclosure))
                            parts)
                  enclosure))))))

(define (mime:parse-multipart-parts header-fields string start end boundary)
  (let ((encoding
         (named-mime-encoding
          (mime:get-content-transfer-encoding header-fields))))
    (if (mime-encoding/identity? encoding)
        (mime:parse-multipart-parts-1 string start end boundary)
        ((lambda (body)
           (mime:parse-multipart-parts-1 body 0 (string-length body) boundary))
         (call-with-output-string
           (lambda (output-port)
             (call-with-mime-decoding-output-port encoding output-port #t
               (lambda (output-port)
                 (write-substring string start end output-port)))))))))

(define (mime:get-boundary parameters)
  (let ((parameter (assq 'BOUNDARY parameters)))
    (and parameter
         (string-append "--" (cdr parameter)))))

(define (mime:parse-multipart-parts-1 string start end boundary)
  (let ((boundary-length (string-length boundary)))

    (define (loop part-start search-start parts)
      (cond ((substring-search-forward boundary string search-start end)
             => (lambda (boundary-start)
                  (let ((boundary-end (+ boundary-start boundary-length)))
                    (if (boundary-start? boundary-start)
                        (continue part-start
                                  ;; Slurp in the preceding newline.
                                  (if (= boundary-start start)
                                      start
                                      (- boundary-start 1))
                                  boundary-end
                                  parts)
                        (loop part-start boundary-end parts)))))
            (else (lose parts))))

    (define (continue part-start part-end boundary-end parts)
      (cond ((last-boundary-end? boundary-end)
             (win (cons (cons part-start part-end) parts)))
            ((skip-lwsp-until-newline string boundary-end end)
             => (lambda (next-line-start)
                  (loop next-line-start
                        next-line-start
                        (cons (cons part-start part-end) parts))))
            (else
             (loop part-start boundary-end parts))))

    (define (boundary-start? boundary-start)
      ;; It's not a boundary start unless it is the start of a line.
      (or (= boundary-start start)
          (char=? (string-ref string (- boundary-start 1)) #\newline)))

    (define (last-boundary-end? boundary-end)
      (and (>= end (+ boundary-end 2))
           (char=? #\- (string-ref string boundary-end))
           (char=? #\- (string-ref string (+ boundary-end 1)))))

    (define (win parts)
      (map (lambda (start.end)
             (mime:parse-entity string (car start.end) (cdr start.end)))
           ;; Strip the leading text, which is not a proper part --
           ;; usually it is just a message to the effect that this is
           ;; a MIME-formatted message which your mail reader can't
           ;; read.
           (cdr (reverse! parts))))

    (define (lose parts)
      ;; If we got at least one part and the leading text, then win
      ;; with that much -- at least we sha'n't be discarding any
      ;; information, since the last part will include the rest of the
      ;; message that we weren't able to parse.
      (if (and (pair? parts)
               (pair? (cdr parts)))
          (win parts)
          #f))

    (loop start start '())))

;;;; MIME Header Fields

(define (mime:get-content-type header-fields)
  (parse-first-named-header header-fields
                            "Content-Type"
                            mime:default-content-type
                            mime:parse-content-type))

(define mime:default-content-type '(TEXT PLAIN (CHARSET . "us-ascii")))

(define (mime:parse-content-type string)
  (let ((tokens (mime:string->non-ignored-tokens string)))
    (if (pair? tokens)
        (let ((type (car tokens)) (tokens (cdr tokens)))
          (if (and (string? type) (pair? tokens))
              (let ((slash (car tokens)) (tokens (cdr tokens)))
                (if (and (eqv? slash #\/) (pair? tokens))
                    (let ((subtype (car tokens)) (tokens (cdr tokens)))
                      (if (string? subtype)
                          (cons* (intern type)
                                 (intern subtype)
                                 (mime:parse-parameters tokens))
                          #f))
                    #f))
              #f))
        #f)))

(define (mime:get-content-transfer-encoding header-fields)
  (or (parse-first-named-header header-fields
                                "Content-Transfer-Encoding"
                                mime:default-encoding
                                mime:parse-encoding)
      mime:default-encoding))

(define mime:default-encoding '7BIT)

(define (mime:parse-encoding encoding)
  (let ((tokens (mime:string->non-ignored-tokens encoding)))
    (if (and (pair? tokens)
             (string? (car tokens))
             (null? (cdr tokens)))
        (intern (car tokens))
        #f)))

(define (mime:get-content-id header-fields)
  (parse-first-named-header header-fields "Content-ID" #f rfc822:parse-msg-id))

(define (mime:get-content-description header-fields)
  (parse-first-named-header header-fields
                            "Content-Description"
                            #f
                            mime:parse-encoded-header-value))

(define (mime:parse-encoded-header-value value)
  ;++ implement
  value)

(define (mime:get-content-disposition header-fields)
  (parse-first-named-header header-fields
                            "Content-Disposition"
                            #f
                            mime:parse-disposition))

(define (mime:parse-disposition disposition)
  (let ((tokens (mime:string->non-ignored-tokens disposition)))
    (if (pair? tokens)
        (let ((type (car tokens)) (tokens (cdr tokens)))
          (if (string? type)
              (cons (intern type)
                    (mime:parse-parameters tokens))
              #f))
        #f)))

(define (mime:get-content-language header-fields)
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

(define (mime:parse-parameters tokens)
  (let recur ((tokens tokens))
    (if (pair? tokens)
        (let ((semi (car tokens)) (tokens (cdr tokens)))
          (if (and (eqv? semi #\;) (pair? tokens))
              (let ((attribute (car tokens)) (tokens (cdr tokens)))
                (if (pair? tokens)
                    (let ((equals (car tokens)) (tokens (cdr tokens)))
                      (if (and (eqv? equals #\=)
                               (pair? tokens)
                               (string? (car tokens)))
                          (cons (cons (intern attribute)
                                      (rfc822:unquote-string (car tokens)))
                                (recur (cdr tokens)))
                          '()))
                    '()))
              '()))
        '())))

;;;; MIME Encoding Registry

(define-structure (mime-encoding
                   (conc-name mime-encoding/)
                   (print-procedure
                    (standard-unparser-method 'MIME-ENCODING
                      (lambda (encoding output-port)
                        (write-char #\space output-port)
                        (write (mime-encoding/name encoding) output-port))))
                   (constructor %make-mime-encoding))
  (name                          #f read-only #t)
  (identity?                     #f read-only #t)
  (encoder-initializer           #f read-only #t)
  (encoder-finalizer             #f read-only #t)
  (encoder-updater               #f read-only #t)
  (decoder-initializer           #f read-only #t)
  (decoder-finalizer             #f read-only #t)
  (decoder-updater               #f read-only #t)
  (decoding-port-maker           #f read-only #t)
  (caller-with-decoding-port     #f read-only #t))

(define-guarantee mime-encoding "MIME codec")

(define mime-encodings
  (make-eq-hash-table))

(define (define-mime-encoding name
          encode:initialize encode:finalize encode:update
          decode:initialize decode:finalize decode:update
          make-port call-with-port)
  (hash-table/put!
   mime-encodings
   name
   (%make-mime-encoding name #f
                        encode:initialize encode:finalize encode:update
                        decode:initialize decode:finalize decode:update
                        make-port call-with-port))
  name)

(define (define-identity-mime-encoding name)
  (hash-table/put! mime-encodings
                   name
                   (%make-mime-encoding name #t
                                        (lambda (port text?) text? port)
                                        output-port/flush-output
                                        output-port/write-string
                                        (lambda (port text?) text? port)
                                        output-port/flush-output
                                        output-port/write-string
                                        (lambda (port text?) text? port)
                                        (lambda (port text? generator)
                                          text?
                                          (generator port)))))

(define (known-mime-encoding? name)
  (and (hash-table/get mime-encodings name #f)
       #t))

(define (named-mime-encoding name)
  (or (hash-table/get mime-encodings name #f)
      (let ((encoding (make-unknown-mime-encoding name)))
        (hash-table/put! mime-encodings name encoding)
        encoding)))

(define (make-unknown-mime-encoding name)
  (let ((lose (lambda args args (error "Unknown MIME encoding name:" name))))
    (%make-mime-encoding name #f lose lose lose lose lose lose lose lose)))

(define (call-with-mime-decoding-output-port encoding port text? generator)
  ((mime-encoding/caller-with-decoding-port
    (if (symbol? encoding)
        (named-mime-encoding encoding)
        (begin
          (guarantee-mime-encoding encoding
                                   'CALL-WITH-MIME-DECODING-OUTPUT-PORT)
          encoding)))
   port text? generator))

(define-identity-mime-encoding '7BIT)
(define-identity-mime-encoding '8BIT)
(define-identity-mime-encoding 'BINARY)

;; Next two are random values sometimes used by Outlook.
(define-identity-mime-encoding '7-BIT)
(define-identity-mime-encoding '8-BIT)

(define-mime-encoding 'QUOTED-PRINTABLE
  encode-quoted-printable:initialize
  encode-quoted-printable:finalize
  encode-quoted-printable:update
  decode-quoted-printable:initialize
  decode-quoted-printable:finalize
  decode-quoted-printable:update
  make-decode-quoted-printable-port
  call-with-decode-quoted-printable-output-port)

(define-mime-encoding 'BASE64
  encode-base64:initialize
  encode-base64:finalize
  encode-base64:update
  decode-base64:initialize
  decode-base64:finalize
  decode-base64:update
  make-decode-base64-port
  call-with-decode-base64-output-port)

(define-mime-encoding 'BINHEX40
  #f #f #f                              ;No BinHex encoder.
  decode-binhex40:initialize
  decode-binhex40:finalize
  decode-binhex40:update
  make-decode-binhex40-port
  call-with-decode-binhex40-output-port)
