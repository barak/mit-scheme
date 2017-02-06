#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Unicode character database conversion

;;; Parses the XML format UCD and generates code for interesting
;;; tables.

;;; Stage one, needs large stack (100000 works OK) and works better with
;;; large-ish heap.
;;;
;;; (load-option 'xml)
;;; (define ucd (read-xml-file "path/to/ucd.all.grouped.xml"))
;;; (write-standard-property-files ucd "tmp/path/XXX")

;;; Stage two, uses normal sizes:
;;;
;;; (load ".../src/etc/ucd-converter")
;;; (generate-standard-property-tables "tmp/path/XXX")

;;;; Raw UCD property tables

(define ucd-prop-names
  '("CI"
    "CWCF"
    "CWCM"
    "CWKCF"
    "CWL"
    "CWT"
    "CWU"
    "Cased"
    "Lower"
    "NFKC_CF"
    "OLower"
    "OUpper"
    "Upper"
    "cf"
    "gc"
    "lc"
    "scf"
    "slc"
    "stc"
    "suc"
    "tc"
    "uc"))

(define (write-standard-property-files document root-name)
  (call-with-output-file (prop-file-name root-name "index")
    (lambda (port)
      (write (ucd-description document) port)
      (for-each (lambda (prop-name)
                  (newline port)
                  (write prop-name port))
                ucd-prop-names)))
  (for-each (lambda (prop-name)
              (let ((entries
                     (single-repertoire-property (string->symbol prop-name)
                                                 document)))
                (call-with-output-file (prop-file-name root-name prop-name)
                  (lambda (port)
                    (for-each (lambda (p)
                                (write-line p port))
                              entries)))))
            ucd-prop-names))

(define (read-standard-property-files root-name)
  (let ((index (read-file (prop-file-name root-name "index"))))
    (cons (car index)
          (map (lambda (prop-name)
                 (cons prop-name
                       (read-file (prop-file-name root-name prop-name))))
               (cdr index)))))

(define (prop-file-name root-name suffix)
  (ustring-append (->namestring root-name)
                  "-"
                  (ustring-downcase suffix)
                  ".scm"))

;;;; UCD property extraction

(define (single-repertoire-property name document)

  (define (walk-elts elts group-value alist k)
    (if (pair? elts)
        (walk-elt (car elts)
                  group-value
                  alist
                  (lambda (alist)
                    (walk-elts (cdr elts) group-value alist k)))
        (k alist)))

  (define (walk-elt elt group-value alist k)
    (let ((elt-name (xml-name->symbol (xml-element-name elt))))
      (case elt-name
        ((group)
         (walk-elts (xml-element-children elt)
                    (or (attribute-value name elt)
                        group-value)
                    alist
                    k))
        ((char)
         (k (cons (cons (cp-attribute elt)
                        (or (attribute-value name elt)
                            group-value))
                  alist)))
        ((reserved noncharacter surrogate)
         (k alist))
        (else
         (error "Unrecognized repertoire element:" elt)))))

  (walk-elts (repertoire-elts document) #f '() merge-property-alist))

(define (merge-property-alist alist)
  (let ((sorted
         (sort alist
               (lambda (p1 p2)
                 (< (cpr-start (car p1))
                    (cpr-start (car p2)))))))
    (let loop ((alist sorted))
      (if (and (pair? alist)
               (pair? (cdr alist)))
          (let ((p1 (car alist))
                (p2 (cadr alist)))
            (if (and (cprs-adjacent? (car p1) (car p2))
                     (if (cdr p1)
                         (and (cdr p2)
                              (ustring=? (cdr p1) (cdr p2)))
                         (not (cdr p2))))
                (begin
                  (set-car! alist
                            (cons (merge-cprs (car p1) (car p2))
                                  (cdr p1)))
                  (set-cdr! alist (cddr alist))
                  (loop alist))
                (loop (cdr alist))))))
    (insert-undefined-ranges sorted)))

(define (insert-undefined-ranges alist)
  (let loop ((alist alist) (last-end 0))
    (if (pair? alist)
        (let* ((cpr (caar alist))
               (tail (cons (car alist) (loop (cdr alist) (cpr-end cpr)))))
          (if (< last-end (cpr-start cpr))
              (cons (cons (make-cpr last-end (cpr-start cpr)) #f)
                    tail)
              tail))
        (if (< last-end char-code-limit)
            (list (cons (make-cpr last-end char-code-limit) #f))
            '()))))

(define (repertoire-elts document)
  (xml-element-children
   (xml-element-child 'repertoire (xml-document-root document))))

(define (xml-element-children elt)
  (filter xml-element? (xml-element-content elt)))

(define (attribute-value name elt)
  (let ((attr
         (find (lambda (attr)
                 (xml-name=? name (xml-attribute-name attr)))
               (xml-element-attributes elt))))
    (and attr
         (let ((value (xml-attribute-value attr)))
           (and (fix:> (ustring-length value) 0)
                value)))))

(define (cp-attribute elt)
  (let ((cp (attribute-value 'cp elt)))
    (if cp
        (string->number cp 16 #t)
        (cons (string->number (attribute-value 'first-cp elt) 16 #t)
              (+ 1 (string->number (attribute-value 'last-cp elt) 16 #t))))))

(define (ucd-description document)
  (let ((content
         (xml-element-content
          (xml-element-child 'description (xml-document-root document)))))
    (if (not (and (pair? content)
                  (ustring? (car content))
                  (null? (cdr content))))
        (error "Unexpected description content:" content))
    (car content)))

(define (xml-element->sexp elt)
  (cons (cons (xml-name->symbol (xml-element-name elt))
              (map (lambda (attr)
                     (list (xml-name->symbol (xml-attribute-name attr))
                           (xml-attribute-value attr)))
                   (xml-element-attributes elt)))
        (map xml-element->sexp
             (filter xml-element?
                     (xml-element-content elt)))))

;;;; Code-point ranges

(define (make-cpr start end)
  (guarantee index-fixnum? start)
  (guarantee index-fixnum? end)
  (if (not (fix:< start end))
      (error "Start must be less than end:" start end))
  (if (fix:= start (fix:- end 1))
      start
      (cons start end)))

(define (cpr? object)
  (or (index-fixnum? object)
      (and (pair? object)
           (index-fixnum? (car object))
           (index-fixnum? (cdr object))
           (fix:< (car object) (cdr object)))))

(define (cpr-start cpr)
  (guarantee cpr? cpr)
  (if (pair? cpr)
      (car cpr)
      cpr))

(define (cpr-end cpr)
  (guarantee cpr? cpr)
  (if (pair? cpr)
      (cdr cpr)
      (fix:+ cpr 1)))

(define (cpr= cpr1 cpr2)
  (and (fix:= (cpr-start cpr1) (cpr-start cpr2))
       (fix:= (cpr-end cpr1) (cpr-end cpr2))))

(define (cpr-size cpr)
  (fix:- (cpr-end cpr) (cpr-start cpr)))

(define (merge-cpr-list cprs)
  (if (pair? cprs)
      (if (and (pair? (cdr cprs))
               (cprs-adjacent? (car cprs) (cadr cprs)))
          (merge-cpr-list
           (cons (merge-cprs (car cprs) (cadr cprs))
                 (cddr cprs)))
          (cons (car cprs)
                (merge-cpr-list (cdr cprs))))
      '()))

(define (cprs-adjacent? cpr1 cpr2)
  (fix:= (cpr-end cpr1) (cpr-start cpr2)))

(define (merge-cprs cpr1 cpr2)
  (if (not (cprs-adjacent? cpr1 cpr2))
      (error "Can't merge non-adjacent cprs:" cpr1 cpr2))
  (make-cpr (cpr-start cpr1)
            (cpr-end cpr2)))

;;;; Code-point range prefix encoding

(define (split-prop-alist-by-prefix alist)
  (append-map (lambda (p)
                (let ((value (cdr p)))
                  (map (lambda (cpr)
                         (cons (cpr->prefix cpr) value))
                       (split-cpr-by-prefix (car p)))))
              alist))

(define (cpr->prefix cpr)
  (receive (p n) (compute-low-prefix (cpr-start cpr) (fix:- (cpr-end cpr) 1))
    (unsigned-integer->bit-string (fix:- 21 n) p)))

(define (split-cpr-by-prefix cpr)
  (let loop ((low (cpr-start cpr)) (high (fix:- (cpr-end cpr) 1)))
    (if (fix:<= low high)
        (receive (ll lh) (low-bracket low high)
          (receive (hl hh) (high-bracket low high)
            (if (fix:< low hl)
                (if (fix:< lh high)
                    (append (loop low lh)
                            (loop (fix:+ lh 1) (fix:- hl 1))
                            (loop hl high))
                    (append (loop low (fix:- hl 1))
                            (loop hl high)))
                (if (fix:< lh high)
                    (append (loop low lh)
                            (loop (fix:+ lh 1) high))
                    (list (make-cpr low (fix:+ high 1)))))))
        '())))

(define (low-bracket low high)
  (receive (p n) (compute-low-prefix low high)
    (bracket p n)))

(define (high-bracket low high)
  (receive (p n) (compute-high-prefix low high)
    (bracket p n)))

(define (bracket p n)
  (let ((low (fix:lsh p n)))
    (values low
            (fix:or low (fix:- (fix:lsh 1 n) 1)))))

(define (compute-low-prefix low high)
  (let loop
      ((low low)
       (high high)
       (n 0))
    (if (and (fix:< low high)
             (fix:= 0 (fix:and 1 low)))
        (loop (fix:lsh low -1)
              (fix:lsh high -1)
              (fix:+ n 1))
        (values low n))))

(define (compute-high-prefix low high)
  (let loop
      ((low low)
       (high high)
       (n 0))
    (if (and (fix:< low high)
             (fix:= 1 (fix:and 1 high)))
        (loop (fix:lsh low -1)
              (fix:lsh high -1)
              (fix:+ n 1))
        (values high n))))

;;;; Stratification of dispatch tables

(define (stratify-prop-alist alist slices)
  (let loop ((alist alist) (slices slices))
    (if (pair? slices)
        (stratify-prop-alist-1 alist
                               (car slices)
                               (lambda (alist)
                                 (loop alist (cdr slices))))
        '())))

(define (stratify-prop-alist-1 alist n-bits continue)
  (cons n-bits
        (let loop ((alist alist))
          (if (pair? alist)
              (if (< n-bits (bit-string-length (caar alist)))
                  (let ((p1 (prefix-head (caar alist) n-bits)))
                    (let gather
                        ((alist (cdr alist))
                         (tails
                          (list (cons (prefix-tail (caar alist) n-bits)
                                      (cdar alist)))))
                      (if (and (pair? alist)
                               (prefix-match? p1 (caar alist)))
                          (gather (cdr alist)
                                  (cons (cons (prefix-tail (caar alist) n-bits)
                                              (cdar alist))
                                        tails))
                          (cons (cons p1 (continue (reverse! tails)))
                                (loop alist)))))
                  (cons (car alist)
                        (loop (cdr alist))))
              '()))))

(define (prefix-match? p1 p2)
  (let ((n1 (bit-string-length p1))
        (n2 (bit-string-length p2)))
    (if (<= n1 n2)
        (bit-string=? p1 (prefix-head p2 n1))
        (bit-string=? (prefix-head p1 n2) p2))))

(define (prefix-head s n-bits)
  (bit-substring s
                 (- (bit-string-length s) n-bits)
                 (bit-string-length s)))

(define (prefix-tail s n-bits)
  (bit-substring s 0 (- (bit-string-length s) n-bits)))

(define (compute-stratification-costs alists slices)
  (map (lambda (alist)
         (cons (car alist)
               (compute-stratification-cost
                (split-prop-alist-by-prefix (cdr alist))
                slices)))
       alists))

(define (compute-stratification-cost alist slices)
  (let loop ((alist alist) (slices slices))
    (if (pair? slices)
        (compute-stratification-cost-1 alist
                                       (car slices)
                                       (lambda (alist)
                                         (loop alist (cdr slices))))
        0)))

(define (compute-stratification-cost-1 alist n-bits continue)
  (+ (expt 2 n-bits)
     (let loop ((alist alist))
       (if (pair? alist)
           (if (< n-bits (bit-string-length (caar alist)))
               (let ((p1 (prefix-head (caar alist) n-bits)))
                 (let gather
                     ((alist (cdr alist))
                      (tails
                       (list (cons (prefix-tail (caar alist) n-bits)
                                   (cdar alist)))))
                   (if (and (pair? alist)
                            (< n-bits (bit-string-length (caar alist)))
                            (bit-string=? p1
                                          (prefix-head (caar alist) n-bits)))
                       (gather (cdr alist)
                               (cons (cons (prefix-tail (caar alist) n-bits)
                                           (cdar alist))
                                     tails))
                       (+ (continue (reverse! tails))
                          (loop alist)))))
               (loop (cdr alist)))
           0))))

(define (count-nodes stratified)
  (fold (lambda (p1 p2)
          (cons (+ (car p1) (car p2))
                (+ (cdr p1) (cdr p2))))
        '(1 . 0)
        (map (lambda (entry)
               (if (pair? (cdr entry))
                   (count-nodes (cdr entry))
                   '(0 . 1)))
             (cdr stratified))))

;;;; Code generator

(define mit-scheme-root-pathname
  (merge-pathnames "../../" (directory-pathname (current-load-pathname))))

(define copyright-file-name
  (merge-pathnames "dist/copyright.scm" mit-scheme-root-pathname))

(define output-file-root
  (merge-pathnames "src/runtime/ucd-table" mit-scheme-root-pathname))

(define (generate-standard-property-tables prop-root)
  (generate-property-tables (read-standard-property-files prop-root)
                            output-file-root))

(define (generate-property-tables std-prop-alists root-name)
  (for-each (lambda (p)
              (let ((exprs (generate-property-table (car p) (cdr p))))
                (call-with-output-file (prop-file-name root-name (car p))
                  (lambda (port)
                    (write-table-header (car p)
                                        (car std-prop-alists)
                                        port)
                    (pp (car exprs) port)
                    (for-each (lambda (exprs)
                                (newline port)
                                (pp exprs port))
                              (cdr exprs))))))
            (cdr std-prop-alists)))

(define (write-table-header prop-name ucd-version port)
  (call-with-input-file copyright-file-name
    (lambda (ip)
      (let loop ()
        (let ((char (read-char ip)))
          (if (not (eof-object? char))
              (begin
                (write-char char port)
                (loop)))))))
  (write-string ";;;; UCD property: " port)
  (write-string prop-name port)
  (newline port)
  (newline port)
  (write-string ";;; Generated from " port)
  (write-string ucd-version port)
  (write-string " UCD at " port)
  (write-string (universal-time->local-iso8601-string (get-universal-time))
                port)
  (newline port)
  (newline port)
  (write-string "(declare (usual-integrations))" port)
  (newline port)
  (write-char #\page port)
  (newline port))

(define (generate-property-table prop-name prop-alist)
  (let ((stratified-entries
         (stratify-prop-alist (split-prop-alist-by-prefix prop-alist)
                              '(5 8 4 4)))
        (maker (entries-maker))
        (entry-count 0)
        (unique-entry-count 0)
        (byte-count 0))

    (define (make-value-code value)
      (lambda (sv-name table-name)
        `(,sv-name ,table-name ,value)))

    (define (make-node-code n-bits offset indexes)
      (receive (bytes-per-entry coder)
          (or (try-linear indexes)
              (try-8-bit-direct indexes)
              (try-8-bit-spread indexes)
              (try-16-bit-direct indexes)
              (try-16-bit-spread indexes)
              (error "Dispatch won't fit in 16 bits:" indexes))
        (count-entries! indexes bytes-per-entry)
        (lambda (sv-name table-name)
          `(((vector-ref ,table-name
                         ,(coder
                           `(fix:and ,(- (expt 2 n-bits) 1)
                                     ,(code:rsh sv-name offset))))
             ,sv-name
             ,table-name)))))

    (define (count-entries! indexes bytes-per-entry)
      (let ((n (length indexes))
            (u (length (delete-duplicates indexes eqv?))))
        (set! entry-count (+ entry-count n))
        (set! unique-entry-count (+ unique-entry-count u))
        (set! byte-count (+ byte-count (* n bytes-per-entry))))
      unspecific)

    (let ((make-entry (maker 'make-entry)))
      (generate-code stratified-entries
                     (lambda (n-bits offset indexes)
                       (make-entry (make-node-code n-bits offset indexes)))
                     (lambda (value)
                       (make-entry (make-value-code value)))))

    (let ((root-entry ((maker 'get-root-entry)))
          (table-entries ((maker 'get-table-entries))))
      (report-table-statistics prop-name
                               entry-count
                               unique-entry-count
                               byte-count
                               (length table-entries))
      (generate-top-level (ustring-downcase prop-name)
                          root-entry
                          table-entries))))

(define (report-table-statistics prop-name entry-count unique-entry-count
                                 byte-count n-entries)
  (with-notification
   (lambda (port)
     (write-string "UCD property " port)
     (write-string prop-name port)
     (write-string ": dispatch tables = " port)
     (write entry-count port)
     (write-string " entries (" port)
     (write unique-entry-count port)
     (write-string " unique), " port)
     (write byte-count port)
     (write-string " bytes; object table = " port)
     (write n-entries port)
     (write-string " words" port))))

(define (generate-top-level prop-name root-entry table-entries)
  (let ((table-name (symbol "ucd-" prop-name "-entries"))
        (entry-names
         (map (lambda (index)
                (symbol "ucd-" prop-name "-entry-" index))
              (iota (length table-entries)))))

    `((define (,(symbol "ucd-" prop-name "-value") sv)
        ,@(root-entry 'sv table-name))

      (define ,table-name)
      ,@(generate-table-initializers table-name entry-names)

      ,@(map (lambda (name entry)
               `(define (,name sv table)
                  ,@(entry 'sv 'table)))
             entry-names
             table-entries))))

(define (generate-table-initializers table-name entries)
  (let ((root-name (symbol "initialize-" table-name))
        (groups
         (let split-items
             ((items
               (map cons
                    (iota (length entries))
                    entries)))
           (let ((n-items (length items)))
             (if (<= n-items 100)
                 (list items)
                 (let ((split (quotient n-items 2)))
                   (append (split-items (list-head items split))
                           (split-items (list-tail items split)))))))))
    (let ((group-names
           (map (lambda (index)
                  (symbol root-name "-" index))
                (iota (length groups)))))
      `((define (,root-name)
          (set! ,table-name (make-vector ,(length entries)))
          ,@(map (lambda (name)
                   `(,name))
                 group-names))
        ,@(map (lambda (name group)
                 `(define (,name)
                    ,@(map (lambda (p)
                             `(vector-set! ,table-name ,(car p) ,(cdr p)))
                           group)))
               group-names
               groups)))))

(define (try-linear indexes)
  (and (pair? indexes)
       (pair? (cdr indexes))
       (let ((slope (- (cadr indexes) (car indexes))))
         (let loop ((indexes (cdr indexes)))
           (if (pair? (cdr indexes))
               (and (= slope (- (cadr indexes) (car indexes)))
                    (loop (cdr indexes)))
               (linear-coder slope indexes))))))

(define (linear-coder slope indexes)
  (values 0
          (lambda (index-code)
            (if (< slope 0)
                (code:+ (last indexes) (code:* (- slope) index-code))
                (code:+ (car indexes) (code:* slope index-code))))))

(define (try-8-bit-direct indexes)
  (and (< (apply max indexes) #x100)
       (8-bit-spread-coder 0 indexes)))

(define (try-8-bit-spread indexes)
  (let ((base (apply min indexes)))
    (and (< (- (apply max indexes) base) #x100)
         (8-bit-spread-coder base indexes))))

(define (8-bit-spread-coder base indexes)
  (values 1
          (lambda (index-code)
            (code:+ base
                    `(bytevector-u8-ref ',(apply bytevector
                                                 (map (lambda (index)
                                                        (- index base))
                                                      indexes))
                                        ,index-code)))))

(define (try-16-bit-direct indexes)
  (and (< (apply max indexes) #x10000)
       (16-bit-spread-coder 0 indexes)))

(define (try-16-bit-spread indexes)
  (let ((base (apply min indexes)))
    (and (< (- (apply max indexes) base) #x10000)
         (16-bit-spread-coder base indexes))))

(define (16-bit-spread-coder base indexes)
  (values 2
          (lambda (index-code)
            (code:+ base
                    `(bytevector-u16le-ref ',(make-u16-vector
                                              (map (lambda (index)
                                                     (- index base))
                                                   indexes))
                                           ,index-code)))))

(define (make-u16-vector u16s)
  (let ((bv (make-bytevector (* 2 (length u16s)))))
    (for-each (lambda (u16 index)
                (bytevector-u16le-set! bv (* 2 index) u16))
              u16s
              (iota (length u16s)))
    bv))

(define (code:+ a b)
  (cond ((eqv? 0 a) b)
        ((eqv? 0 b) a)
        (else `(fix:+ ,a ,b))))

(define (code:* a b)
  (cond ((or (eqv? 0 a) (eqv? 0 b)) 0)
        ((eqv? 1 a) b)
        ((eqv? 1 b) a)
        (else `(fix:* ,a ,b))))

(define (code:rsh a n)
  (if (= n 0)
      a
      `(fix:lsh ,a ,(- n))))

(define (entries-maker)
  (let ((next-index 0)
        (entries '()))

    (define (make-entry entry)
      (let ((index next-index))
        (set! next-index (+ next-index 1))
        (set! entries (cons entry entries))
        index))

    (lambda (operator)
      (case operator
        ((make-entry) make-entry)
        ((get-table-entries) (lambda () (reverse (cdr entries))))
        ((get-root-entry) (lambda () (car entries)))
        (else (error "Unknown operator:" operator))))))

(define (generate-code stratified-entries make-node make-value)
  (let ((value-table (make-equal-hash-table)))

    (define (intern-value value)
      (hash-table-intern! value-table value (lambda () (make-value value))))

    (let loop ((entries stratified-entries) (n-max 21))
      (let ((n-bits (car entries)))
        (make-node n-bits (- n-max n-bits)
          (append-map (lambda (entry)
                        (make-list (expt 2
                                         (- n-bits
                                            (bit-string-length (car entry))))
                                   (if (pair? (cdr entry))
                                       (loop (cdr entry) (- n-max n-bits))
                                       (intern-value (cdr entry)))))
                      (cdr entries)))))))