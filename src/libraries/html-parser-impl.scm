;; html-parser.scm -- SSAX-like tree-folding html parser
;; Copyright (c) 2003-2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; MIT/GNU Scheme adaptations Copyright (C) 2023 Chris Hanson.

;;> A permissive HTML parser supporting scalable streaming with a
;;> folding interface.  This copies the interface of Oleg Kiselyov's
;;> SSAX parser, as well as providing simple convenience utilities.
;;> It correctly handles all invalid HTML, inserting "virtual"
;;> starting and closing tags as needed to maintain the proper tree
;;> structure needed for the foldts down/up logic.  A major goal of
;;> this parser is bug-for-bug compatibility with the way common web
;;> browsers parse HTML.

;;> Procedure: make-html-parser . keys

;;   Returns a procedure of two arguments, an initial seed and an
;;   optional input port, which parses the HTML document from the port
;;   with the callbacks specified in the plist KEYS (using normal,
;;   quoted symbols, for portability and to avoid making this a
;;   macro).  The following callbacks are recognized:
;;
;;   START: TAG ATTRS SEED VIRTUAL?
;;       fdown in foldts, called when a start-tag is encountered.
;;     TAG:         tag name
;;     ATTRS:       tag attributes as a alist
;;     SEED:        current seed value
;;     VIRTUAL?:    #t iff this start tag was inserted to fix the HTML tree
;;
;;   END: TAG ATTRS PARENT-SEED SEED VIRTUAL?
;;       fup in foldts, called when an end-tag is encountered.
;;     TAG:         tag name
;;     ATTRS:       tag attributes of the corresponding start tag
;;     PARENT-SEED: parent seed value (i.e. seed passed to the start tag)
;;     SEED:        current seed value
;;     VIRTUAL?:    #t iff this end tag was inserted to fix the HTML tree
;;
;;   TEXT: TEXT SEED
;;       fhere in foldts, called when any text is encountered.  May be
;;       called multiple times between a start and end tag, so you need
;;       to string-append yourself if desired.
;;     TEXT:        entity-decoded text
;;     SEED:        current seed value
;;
;;   COMMENT: TEXT SEED
;;       fhere on comment data
;;
;;   DECL: NAME ATTRS SEED
;;       fhere on declaration data
;;
;;   PROCESS: LIST SEED
;;       fhere on process-instruction data
;;
;;   ENTITY: NAME-OR-NUM SEED
;;       convert an entity with a given name, or number via &#NNNN;
;;       to a string
;;
;;   In addition, entity-mappings may be overriden with the ENTITIES:
;;   keyword.

;; Procedure: html->sxml [port]
;;   Returns the SXML representation of the document from PORT, using
;;   the default parsing options.

;; Procedure: html-strip [port]
;;   Returns a string representation of the document from PORT with all
;;   tags removed.  No whitespace reduction or other rendering is done.

;; Example:
;;
;;   The parser for html-strip could be defined as:
;;
;; (make-html-parser
;;   'text:  (lambda (text seed) (display text)))
;;
;;   Also see the parser for html->sxml.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text parsing utils

(define (read-while pred #!optional in)
  (let ((in (if (default-object? in) (current-input-port) in))
        (builder (string-builder)))
    (let lp ()
      (let ((c (peek-char in)))
        (when (and (not (eof-object? c)) (pred c))
          (builder (read-char in))
          (lp))))
    (builder)))

(define (read-until pred #!optional in)
  (let ((in (if (default-object? in) (current-input-port) in))
        (builder (string-builder)))
    (let lp ()
      (let ((c (peek-char in)))
        (unless (or (eof-object? c) (pred c))
          (builder (read-char in))
          (lp))))
    (builder)))

;; Generates a KMP reader that works on ports, returning the text read
;; up until the search string (or the entire port if the search string
;; isn't found).  This is O(n) in the length of the string returned,
;; as opposed to the find-string-from-port? in SSAX which uses
;; backtracking for an O(nm) algorithm.  This is hard-coded to
;; case-insensitively match, since that's what we need for HTML.  A
;; more general utility would abstract the character matching
;; predicate and possibly provide a limit on the length of the string
;; read.
(define (make-string-reader/ci str)
  (let* ((len (string-length str))
         (vec (make-vector len 0)))
    (when (fx>? len 0)
      (vector-set! vec 0 -1))
    (let lp ((i 2) (j 0))
      (when (fx<? i len)
        (cond ((char-ci=? (string-ref str (- i 1)) (string-ref str j))
               (vector-set! vec i (fx+ j 1))
               (lp (fx+ i 1) (fx+ j 1)))
              ((fx>? j 0)
               (lp i (vector-ref vec j)))
              (else
               (vector-set! vec i 0)
               (lp (fx+ i 1) j)))))
    (lambda (#!optional in)
      (let ((in (if (default-object? in) (current-input-port) in))
            (builder (string-builder)))

        (define (backfill! i)
          (do ((j 0 (fx+ j 1)))
              ((not (fx<? j i)))
            (builder (string-ref str j))))

        (let lp ((i 0))
          (when (fx<? i len)
            (let ((c (peek-char in)))
              (cond ((eof-object? c)
                     (backfill! i))
                    ((char-ci=? c (string-ref str i))
                     (read-char in)
                     (lp (fx+ i 1)))
                    (else
                     (let* ((i2 (vector-ref vec i))
                            (i3 (if (fx=? -1 i2) 0 i2)))
                       (when (fx>? i i3)
                         (backfill! (- i i3)))
                       (when (fx=? -1 i2)
                         (builder (read-char in)))
                       (lp i3)))))))
        (builder)))))

(define (skip-whitespace #!optional in)
  (read-while char-whitespace? in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-specific readers

(define (char-alphanumeric? c)
  (or (char-alphabetic? c) (char-numeric? c)))

(define (char-hex-numeric? c)
  (or (char-numeric? c)
      (memv (char-downcase c) '(#\a #\b #\c #\d #\e #\f))))

(define (read-identifier #!optional in)
  (read-while char-alphanumeric? in))

(define (read-integer #!optional in)
  (read-while char-numeric? in))

(define (read-hex-integer #!optional in)
  (read-while char-hex-numeric? in))

(define (read-entity in)
  (read-char in)
  (cond ((eqv? (peek-char in) #\#)
         (read-char in)
         (cond ((char-numeric? (peek-char in))
                (let* ((str (read-integer in))
                       (num (string->number str)))
                  (when (eqv? (peek-char in) #\;)
                    (read-char in))
                  (cons 'entity num)))
               ((memv (peek-char in) '(#\x #\X))
                (read-char in)
                (let* ((str (read-hex-integer in))
                       (num (string->number str 16)))
                  (when (eqv? (peek-char in) #\;)
                    (read-char in))
                  (cons 'entity num)))
               (else
                (cons 'text "&#"))))
        ((char-alphabetic? (peek-char in))
         (let ((name (read-identifier in)))
           (when (eqv? (peek-char in) #\;)
             (read-char in))
           (cons 'entity name)))
        (else
         (cons 'text "&"))))

(define (read-quoted in entities)
  (let ((terminator (read-char in)))
    (let lp ((res '()))
      (cond ((eof-object? (peek-char in))
             (reverse res))
            ((eqv? terminator (peek-char in))
             (read-char in)             ; discard terminator
             (reverse res))
            ((eqv? #\& (peek-char in))
             (let ((x (read-entity in)))
               (lp (cons (or (and (eq? 'entity (car x))
                                  (get-entity entities (cdr x)))
                             (cdr x))
                         res))))
            (else
             (lp
              (cons (read-until (lambda (c)
                                  (or (eqv? #\& c)
                                      (eqv? terminator c)))
                                in)
                    res)))))))

(define (read-pi in)
  (let ((tag (read-identifier in)))
    (skip-whitespace in)
    (list (if (equal? tag "") #f (string->symbol (string-downcase tag)))
          (read-pi-body in))))

(define read-pi-body
  (make-string-reader/ci "?>"))

(define read-comment
  (make-string-reader/ci "-->"))

(define (tag-char? c)
  (and (char? c)
       (or (char-alphanumeric? c)
           (memv c '(#\- #\+ #\* #\_ #\:)))))

(define (read-attrs in entities)
  (let loop ((attrs '()))
    (skip-whitespace in)
    (let ((c (peek-char in)))
      (cond ((or (eof-object? c) (eqv? c #\>))
             (read-char in)
             (list #f (reverse attrs)))
            ((eqv? c #\/)
             (read-char in)
             (if (not (eqv? #\> (read-char in)))
                 (error "Tag has isolated slash"))
             (list #t (reverse attrs)))
            ((eqv? c #\")
             (read-char in)
             (loop attrs))
            ((not (tag-char? c))
             (list #f (reverse attrs)))
            (else
             (let ((name (read-while tag-char? in)))
               (if (string=? name "")
                   (loop attrs)
                   (let ((name (string->symbol (string-downcase name))))
                     (cond ((eqv? (peek-char in) #\=)
                            (read-char in)
                            (let ((value
                                   (if (memv (peek-char in) '(#\" #\'))
                                       (apply string-append
                                              (read-quoted in entities))
                                       (read-until
                                        (lambda (c)
                                          (or (char-whitespace? c)
                                              (memv c '(#\' #\" #\< #\>))))
                                        in))))
                              (loop (cons (list name value) attrs))))
                           (else
                            (loop (cons (list name) attrs))))))))))))

(define (read-start in entities)
  (let ((tag (string->symbol (string-downcase (read-while tag-char? in)))))
    (cons tag (read-attrs in entities))))

(define (read-end in)
  (let ((tag (read-while tag-char? in)))
    (cond ((equal? tag "")
           (read-until (lambda (c) (eqv? c #\>)) in)
           (read-char in)
           #f)
          (else
           ;; discard closing attrs
           (read-attrs in '())
           (string->symbol (string-downcase tag))))))

(define (read-decl in entities)
  (let loop ((res '()))
    (skip-whitespace in)
    (let ((c (peek-char in)))
      (cond ((eof-object? c)
             (reverse res))
            ((eqv? c #\")
             (loop (cons (read-quoted in entities) res)))
            ((eqv? c #\>)
             (read-char in)
             (reverse res))
            ((eqv? c #\<)
             (read-char in)
             (if (eqv? (peek-char in) #\!) (read-char in))
             (loop (cons (read-decl in entities) res)))
            ((tag-char? c)
             (loop (cons (string->symbol (read-while tag-char? in)) res)))
            (else
             (read-char in)
             (loop res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the parser

(define (entities? object)
  (list-of-type? object
                 (lambda (x)
                   (and (pair? x)
                        (string? (car x))
                        (string? (cdr x))))))

(define *default-entities*
  '(("amp" . "&")
    ("quot" . "\"")
    ("lt" . "<")
    ("gt" . ">")
    ("apos" . "'")
    ("nbsp" . " ")))

(define (get-entity entities name)
  (cond ((number? name) (string (integer->char name)))
        ((string->number name) name)
        ((assoc name entities) => cdr)
        (else #f)))

(define (tag-levels? object)
  (list-of-type? object
                 (lambda (x)
                   (or (symbol? x)
                       (list-of-type? x symbol?)))))

;; span's and div's can be used at any level
(define *tag-levels*
  '(html (head body) table (thead tbody) tr (th td) p (b i u s)))

(define (list-of-symbols? object)
  (list-of-type? object symbol?))

(define *unnestables*
  '(p li td tr))

(define *bodyless*
  '(img hr br meta link))

(define *literals*
  '(script xmp))

(define *terminators*
  '(plaintext))

(define (tag-level tag-levels tag)
  (let lp ((ls tag-levels) (i 0))
    (if (pair? ls)
        (if (if (pair? (car ls))
                (memq tag (car ls))
                (eq? tag (car ls)))
            i
            (lp (cdr ls) (fx+ i 1)))
        (fx+ i 1000))))

(define read-cdata
  (make-string-reader/ci "]]>"))

(define (read-html-token #!optional in entities)
  (let ((in (if (default-object? in) (current-input-port) in))
        (entities (if (default-object? entities) '() entities)))
    (let ((c (peek-char in)))
      (if (eof-object? c)
          (cons 'eof c)
          (case c
            ((#\<)
             (read-char in)
             (case (peek-char in)
               ((#\!)
                (read-char in)
                (cond ((eqv? #\[ (peek-char in))
                       (read-char in)
                       (let lp ((check '(#\C #\D #\A #\T #\A #\[))
                                (acc '(#\[ #\! #\<)))
                         (cond ((null? check)
                                (cons 'text (read-cdata in)))
                               ((let ((c (peek-char in)))
                                  (and (not (eof-object? c))
                                       (char-ci=? c (car check))))
                                (lp (cdr check) (cons (read-char in) acc)))
                               (else
                                (cons 'text (list->string (reverse acc)))))))
                      ((and (eqv? #\- (peek-char in))
                            (begin
                              (read-char in)
                              (eqv? #\- (peek-char in))))
                       (read-char in)
                       (cons 'comment (read-comment in)))
                      (else
                       (cons 'decl (read-decl in entities)))))
               ((#\?)
                (read-char in)
                (cons 'process (read-pi in)))
               ((#\/)
                (read-char in)
                (cons 'end (read-end in)))
               (else
                ;; start tags must immediately be followed by an
                ;; alphabetic character, or we just treat the < as text
                (if (and (char? (peek-char in))
                         (char-alphabetic? (peek-char in)))
                    (let ((res (read-start in entities)))
                      (if (cadr res)
                          (cons 'start/end (cons (car res) (cddr res)))
                          (cons 'start (cons (car res) (cddr res)))))
                    (cons 'text "<")))))
            ((#\&)
             (read-entity in))
            (else
             (cons 'text
                   (read-until (lambda (c) (or (eqv? c #\<) (eqv? c #\&)))
                               in))))))))

;; "<![CDATA["
;; "<!--"
;; "<!"
;; "<?"
;; "</"
;; "<"

(define html-parser-options
  (keyword-option-parser
   (list (list 'start: procedure? (lambda () default-start))
         (list 'end: procedure? (lambda () default-end))
         (list 'text: procedure? (lambda () default-text))
         (list 'decl: procedure? (lambda () default-decl))
         (list 'process: procedure? (lambda () default-text))
         (list 'comment: procedure? (lambda () default-text))
         (list 'entities: entities? (lambda () *default-entities*))
         (list 'tag-levels: tag-levels? (lambda () *tag-levels*))
         (list 'unnestables: list-of-symbols? (lambda () *unnestables*))
         (list 'bodyless: list-of-symbols? (lambda () *bodyless*))
         (list 'literals: list-of-symbols? (lambda () *literals*))
         (list 'terminators: list-of-symbols? (lambda () *terminators*))
         (list 'entity: procedure? (lambda () #f)))))

(define (default-start t a s v)
  (declare (ignore t a v))
  s)

(define (default-end t a p s v)
  (declare (ignore t a p v))
  s)

(define (default-text t s)
  (declare (ignore t))
  s)

(define (default-decl t a s)
  (declare (ignore t a))
  s)

(define (make-html-parser . keylist)
  (let-values
      (((start end text decl process comment entities tag-levels unnestables
               bodyless literals terminators entity)
        (html-parser-options keylist 'make-html-parser)))
    (let ((literals
           (map (lambda (x)
                  (cons x
                        (make-string-reader/ci
                         (string-append "</" (symbol->string x) ">"))))
                literals))
          (entity
           (or entity
               (lambda (t s)
                 (text (or (get-entity entities t)
                           (string-append "&" t ";"))
                       s)))))

      (define (entity->string sxml seed out)
        (if (pair? sxml)
            (if (eq? 'entity (car sxml))
                (entity->string (entity (cdr sxml) seed) seed out)
                (for-each (lambda (x)
                            (entity->string x seed out))
                          sxml))
            (display sxml out)))

      (define (fix-attrs ls seed)
        (map (lambda (x)
               (cons (car x)
                     (if (pair? (cdr x))
                         (list (call-with-output-string
                                 (lambda (out)
                                   (entity->string (cadr x) seed out))))
                         (cdr x))))
             ls))

      (define (fix-decl ls seed)
        (map (lambda (x)
               (if (pair? x)
                   (call-with-output-string
                     (lambda (out)
                       (entity->string x seed out)))
                   x))
             ls))

      (lambda (seed #!optional in)
        (let ((in
               (cond ((default-object? in) (current-input-port))
                     ((string? in) (open-input-string in))
                     (else in))))
          (let lp ((tok (read-html-token in entities))
                   (seed seed)
                   (seeds '())
                   (tags '()))
            (case (car tok)
              ((eof)                      ; close all open tags
               (let eof-lp ((t tags) (s seeds) (seed seed))
                 (if (pair? t)
                     (eof-lp (cdr t)
                             (cdr s)
                             (end (caar t)
                                  (cadar t)
                                  (car s)
                                  seed
                                  'eof))
                     seed)))
              ((start/end)
               (let* ((tag (cadr tok))
                      (rest
                       (cons (fix-attrs (caddr tok) seed)
                             (cdddr tok)))
                      (tok (cons tag rest)))
                 (lp `(end . ,tag)
                     (start tag (car rest) seed #f)
                     (cons seed seeds)
                     (cons tok tags))))
              ((start)
               (let* ((tag (cadr tok))
                      (rest
                       (cons (fix-attrs (caddr tok) seed)
                             (cdddr tok)))
                      (tok (cons tag rest)))
                 (cond ((memq tag terminators)
                        (lp `(text
                              . ,(read-until (lambda (c)
                                               (declare (ignore c))
                                               #f)
                                             in))
                            (start tag (car rest) seed #f)
                            (cons seed seeds)
                            (cons tok tags)))
                       ((assq tag literals)
                        => (lambda (lit)
                             (let ((body ((cdr lit) in))
                                   (seed2 (start tag (car rest) seed #f)))
                               (lp `(end . ,tag)
                                   (if (equal? "" body)
                                       seed2
                                       (text body seed2))
                                   (cons seed seeds)
                                   (cons tok tags)))))
                       ((memq tag bodyless)
                        (lp `(end . ,tag)
                            (start tag (car rest) seed #f)
                            (cons seed seeds)
                            (cons tok tags)))
                       ((and (pair? tags)
                             (eq? tag (caar tags))
                             (memq tag unnestables))
                        ;; <p> ... <p> implies siblings, not nesting
                        (let ((seed2
                               (end tag
                                    (cadar tags)
                                    (car seeds)
                                    seed
                                    'sibling)))
                          (lp (read-html-token in entities)
                              (start tag (car rest) seed #f)
                              (cons seed2 (cdr seeds))
                              (cons tok (cdr tags)))))
                       (else
                        (lp (read-html-token in entities)
                            (start tag (car rest) seed #f)
                            (cons seed seeds)
                            (cons tok tags))))))
              ((end)
               (cond ((not (cdr tok)) ;; nameless closing tag
                      (lp (read-html-token in entities)
                          seed
                          seeds
                          tags))
                     ((and (pair? tags) (eq? (cdr tok) (caar tags)))
                      (lp (read-html-token in entities)
                          (end (cdr tok)
                               (fix-attrs (cadar tags) seed)
                               (car seeds)
                               seed
                               #f)
                          (cdr seeds)
                          (cdr tags)))
                     (else
                      (let ((this-level (tag-level tag-levels (cdr tok)))
                            (expected-level
                             (if (pair? tags)
                                 (tag-level tag-levels (caar tags))
                                 -1)))
                        (cond ((fx<? this-level expected-level)
                               ;; higher-level tag, forcefully close
                               ;; preceding tags
                               (lp tok
                                   (end (caar tags)
                                        (fix-attrs (cadar tags) seed)
                                        (car seeds)
                                        seed
                                        'parent-closed)
                                   (cdr seeds)
                                   (cdr tags)))
                              ((and (fx=? this-level expected-level)
                                    (pair? (cdr tags)))
                               ;; equal, interleave (close prec tag, close this,
                               ;; re-open prec)
                               ;; <b><i></b> => <b><i></i></b><i>
                               ;;                     ^^^^    ^^^
                               ;; XXXX handle backups > 1 here
                               (let* ((seed2
                                       (end (caar tags)
                                            (cadar tags)
                                            (car seeds)
                                            seed
                                            'interleave))
                                      (seed3
                                       (end (caadr tags)
                                            (cadadr tags)
                                            (cadr seeds)
                                            seed2
                                            #f)))
                                 (let ((tok2 (read-html-token in entities)))
                                   (cond ((and (eq? 'end (car tok2))
                                               (eq? (caar tags) (cdr tok2)))
                                          ;; simple case where the closing tag
                                          ;; immediately follows
                                          (lp (read-html-token in entities)
                                              seed3
                                              (cddr seeds)
                                              (cddr tags)))
                                         (else
                                          (lp tok2
                                              (start (caar tags)
                                                     (cadar tags)
                                                     seed3
                                                     'interleave)
                                              (cons seed3 (cddr seeds))
                                              (cons (car tags)
                                                    (cddr tags))))))))
                              (else
                               ;; spurious end for a lower-level tag, add
                               ;; imaginary start
                               (lp (read-html-token in entities)
                                   (end (cdr tok)
                                        '()
                                        seed
                                        (start (cdr tok) '() seed 'no-start)
                                        #f)
                                   seeds
                                   tags)))))))
              ((text)
               (lp (read-html-token in entities)
                   (text (cdr tok) seed)
                   seeds
                   tags))
              ((entity)
               (lp (read-html-token in entities)
                   (entity (cdr tok) seed)
                   seeds
                   tags))
              ((comment)
               (lp (read-html-token in entities)
                   (comment (cdr tok) seed)
                   seeds
                   tags))
              ((decl)
               (lp (read-html-token in entities)
                   (decl (cadr tok) (fix-decl (cddr tok) seed) seed)
                   seeds
                   tags))
              ((process)
               (lp (read-html-token in entities)
                   (process (cdr tok) seed)
                   seeds
                   tags))
              (else
               (error "invalid token:" tok)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple conversions

(define html->sxml
  (let ((parse
         (make-html-parser
          'start:
          (lambda (tag attrs seed virtual?)
            (declare (ignore tag attrs seed virtual?))
            '())
          'end:
          (lambda (tag attrs parent-seed seed virtual?)
            (declare (ignore virtual?))
            `((,tag
               ,@(if (pair? attrs)
                     `((@ ,@attrs) ,@(reverse seed))
                     (reverse seed)))
              ,@parent-seed))
          'decl: (lambda (tag attrs seed) `((*DECL* ,tag ,@attrs) ,@seed))
          'process: (lambda (attrs seed) `((*PI* ,@attrs) ,@seed))
          'comment: (lambda (text seed) `((*COMMENT* ,text) ,@seed))
          'text: cons)))
    (lambda (#!optional in)
      (cons '*TOP* (reverse (parse '() in))))))

(define (html-tag->string tag attrs)
  (let ((builder (string-builder)))
    (html-tag->string! tag attrs builder)
    (builder)))

(define (html-tag->string! tag attrs builder)
  (builder "<")
  (builder (symbol->string tag))
  (for-each (lambda (attr)
              (builder " ")
              (html-attr->string! attr builder))
            attrs)
  (builder ">"))

(define (html-attr->string attr)
  (let ((builder (string-builder)))
    (html-attr->string! attr builder)
    (builder)))

(define (html-attr->string! attr builder)
  (let ((name (symbol->string (car attr))))
    (builder name)
    (builder "=\"")
    (if (pair? (cdr attr))
        (html-escape! (cadr attr) builder)
        (builder name)))
  (builder "\""))

(define (html-escape string)
  (let ((builder (string-builder)))
    (html-escape! string builder)
    (builder)))

(define (html-escape! string builder)
  (string-for-each (lambda (char)
                     (builder
                      (let ((esc (assq char html-character-escapes)))
                        (if esc (cdr esc) char))))
                   string))

(define html-character-escapes
  '((#\< . "&lt;")
    (#\> . "&gt;")
    (#\& . "&amp;")
    (#\" . "&quot;")
    (#\' . "&apos;")))

(define (sxml->html sxml)
  (let ((builder (string-builder)))
    (sxml->html! sxml builder)
    (builder)))

(define (sxml->html! sxml builder)

  (define (special x)
    (builder
     (cond ((string? x) x)
           ((symbol? x) (symbol->string x))
           (else (error "Unknown special element:" x)))))

  (let loop ((sxml sxml))
    (cond ((pair? sxml)
           (let ((tag (car sxml)))
             (if (symbol? tag)
                 (case tag
                   ((*PI*)
                    (builder "<?")
                    (if (cadr sxml)
                        (builder (symbol->string (cadr sxml))))
                    (builder (cadr sxml))
                    (builder "?>"))
                   ((*DECL*)
                    (builder "<!")
                    (when (pair? (cdr sxml))
                      (special (cadr sxml))
                      (for-each (lambda (x)
                                  (builder " ")
                                  (special x))
                                (cddr sxml)))
                    (builder ">"))
                   ((*COMMENT*)
                    (builder "<!--")
                    (for-each builder (cdr sxml))
                    (builder "-->"))
                   ((*TOP*)
                    (for-each loop (cdr sxml)))
                   (else
                    (let ((rest (cdr sxml)))
                      (cond ((and (pair? rest)
                                  (pair? (car rest))
                                  (eq? '@ (caar rest)))
                             (html-tag->string! tag (cdar rest) builder)
                             (for-each loop (cdr rest))
                             (builder "</")
                             (builder (symbol->string tag))
                             (builder ">"))
                            (else
                             (html-tag->string! tag '() builder)
                             (for-each loop rest)
                             (builder "</")
                             (builder (symbol->string tag))
                             (builder ">"))))))
                 (for-each loop sxml))))
          ((null? sxml))
          (else
           (html-escape! (if (string? sxml)
                             sxml
                             (call-with-output-string
                               (lambda (out)
                                 (display sxml out))))
                         builder)))))

(define (sxml-display-as-html sxml #!optional out)
  (write-string (sxml->html sxml) out))

;; just strips tags, no whitespace handling or formatting
(define (html-strip #!optional in)
  (let ((builder (string-builder)))
    ((make-html-parser
      'text: (lambda (text seed)
               (declare (ignore seed))
               (builder text)))
     (cons #f #f)
     in)
    (builder)))