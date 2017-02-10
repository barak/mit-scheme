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
;;; (load ".../ucd-converter")
;;; (write-standard-property-files ucd)

;;; Stage two, uses normal sizes:
;;;
;;; (load ".../ucd-converter")
;;; (generate-standard-property-tables)

(define this-directory
  (directory-pathname (current-load-pathname)))

(define mit-scheme-root-pathname
  (merge-pathnames "../../" this-directory))

;;;; Raw UCD attribute tables

(define raw-directory
  (pathname-as-directory (merge-pathnames "ucd-raw-props" this-directory)))

(define (raw-file-name name)
  (merge-pathnames (ustring-append name ".scm") raw-directory))

(define (read-ucd-property-metadata)
  (let ((properties (read-file (raw-file-name "names"))))
    (for-each (lambda (metadata)
		(if (not (and (list? metadata)
			      (= 3 (length metadata))
			      (string? (car metadata))
			      (symbol? (cadr metadata))
			      (property-type? (caddr metadata))))
		    (error "Ill-formed property metadata record:" metadata)))
	      properties)
    properties))

(define (property-type? object)
  (or (simple-property-type? object)
      (and (pair? object)
	   (symbol? (car object))
	   (list? (cdr object))
	   (case (car object)
	     ((enum)
	      (or (every string? (cdr object))
		  (every (lambda (elt)
			   (and (pair? elt)
				(string? (car elt))
				(or (boolean? (cdr elt))
				    (symbol? (cdr elt)))))
			 (cdr object))))
	     ((regex)
	      (and (= 2 (length object))
		   (string? (cadr object))))
	     ((or)
	      (and (= 3 (length object))
		   (string? (cadr object))
		   (simple-property-type? (caddr object))))
	     (else #f)))))

(define (simple-property-type? object)
  (case object
    ((boolean byte code-point code-point* code-point+ exact-rational
	      list-of-script string)
     #t)
    (else #f)))

(define ucd-property-metadata
  (read-ucd-property-metadata))

(define all-ucd-prop-names
  (map car ucd-property-metadata))

(define (write-standard-property-files document)
  (let ((ucd-version (ucd-description document)))
    (call-with-output-file (ucd-version-file-name)
      (lambda (port)
	(write-line ucd-version port)))
    (for-each (lambda (prop-name)
		(write-prop-file prop-name ucd-version document))
	      all-ucd-prop-names)))

(define (write-prop-file prop-name ucd-version document)
  (with-notification (lambda (port)
                       (write-string "Writing property " port)
                       (write-string prop-name port))
    (lambda ()
      (let ((entries
             (single-repertoire-property (string->symbol prop-name)
                                         document)))
        (call-with-output-file (prop-file-name prop-name)
          (lambda (port)
            (write-copyright-and-title prop-name ucd-version port)
            (for-each (lambda (p)
                        (write-line p port))
                      entries)))))))

(define (write-copyright-and-title prop-name ucd-version port)
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
  (newline port))

(define (read-ucd-version-file)
  (car (read-file (ucd-version-file-name))))

(define (read-prop-file prop-name)
  (read-file (prop-file-name prop-name)))

(define (ucd-version-file-name)
  (raw-file-name "version"))

(define (prop-file-name prop-name)
  (raw-file-name (ustring-append "prop-" prop-name)))

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

(define (make-cpr start #!optional end)
  (guarantee-index-fixnum start 'make-cpr)
  (let ((end
	 (if (default-object? end)
	     (fix:+ start 1)
	     (begin
	       (guarantee-index-fixnum end 'make-cpr)
	       (if (not (fix:< start end))
		   (error:bad-range-argument end 'make-cpr))
	       end))))
    (if (fix:= start (fix:- end 1))
	start
	(cons start end))))

(define (cpr? object)
  (or (index-fixnum? object)
      (and (pair? object)
           (index-fixnum? (car object))
           (index-fixnum? (cdr object))
           (fix:< (car object) (cdr object)))))

(define (cpr-start cpr)
  (if (pair? cpr)
      (car cpr)
      cpr))

(define (cpr-end cpr)
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

(define (rebase-cpr cpr base)
  (make-cpr (fix:- (cpr-start cpr) base)
	    (fix:- (cpr-end cpr) base)))

;;;; Code-point range prefix encoding

(define (split-prop-alist-by-prefix alist)
  (append-map (lambda (p)
                (let ((value (cdr p)))
                  (map (lambda (cpr)
                         (cons cpr value))
                       (split-cpr-by-prefix (car p)))))
              alist))

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

;;;; Code generator

(define copyright-file-name
  (merge-pathnames "dist/copyright.scm" mit-scheme-root-pathname))

(define output-file-root
  (merge-pathnames "src/runtime/ucd-table" mit-scheme-root-pathname))

(define (generate-standard-property-tables)
  (for-each generate-property-table
	    '("Alpha"
	      "Lower"
	      "Upper"
	      "WSpace"
	      "gc"
	      "nt"
	      "slc"
	      "suc")))

(define (generate-property-table prop-name)
  (let ((exprs (generate-property-table-code prop-name))
	(ucd-version (read-ucd-version-file)))
    (parameterize ((param:pp-forced-x-size 1000))
      (call-with-output-file (prop-table-file-name prop-name)
	(lambda (port)
	  (write-copyright-and-title prop-name ucd-version port)
	  (write-code-header port)
	  (print-code-expr (car exprs) port)
	  (for-each (lambda (expr)
		      (newline port)
		      (print-code-expr expr port))
		    (cdr exprs)))))))

(define (prop-table-file-name prop-name)
  (ustring-append (->namestring output-file-root)
		  "-"
		  (ustring-downcase prop-name)
		  ".scm"))

(define (write-code-header port)
  (write-string "(declare (usual-integrations))" port)
  (newline port)
  (write-char #\page port)
  (newline port))

(define (print-code-expr expr port)
  (if (and (pair? expr)
           (eq? 'comment (car expr))
	   (pair? (cdr expr))
           (null? (cddr expr)))
      (if output-comments?
	  (begin
	    (write-string ";;; " port)
	    (display (cadr expr) port)))
      (pp expr port)))

(define output-comments? #f)

(define (generate-property-table-code prop-name)
  (let ((prop-alist (read-prop-file prop-name))
	(maker (entries-maker))
        (entry-count 0)
        (unique-entry-count 0)
        (byte-count 0)
	(convert-value (value-converter prop-name)))

    (define (make-value-code value)
      (lambda (offsets-name sv-name table-name)
	offsets-name
        (values #f #f `(,sv-name ,table-name ,(convert-value value)))))

    (define (make-node-code n-bits offset indexes)
      (receive (bytes-per-entry offsets-expr coder)
          (or (try-linear indexes)
              (try-8-bit-direct indexes)
              (try-8-bit-spread indexes)
              (try-16-bit-direct indexes)
              (try-16-bit-spread indexes)
              (error "Dispatch won't fit in 16 bits:" indexes))
        (count-entries! indexes bytes-per-entry)
        (lambda (offsets-name sv-name table-name)
	  (values indexes
                  offsets-expr
		  `(((vector-ref ,table-name
				 ,(coder offsets-name
				    (lambda (shift)
				      `(fix:and ,(* (expt 2 shift)
						    (- (expt 2 n-bits) 1))
						,(code:rsh sv-name
							   (- offset shift))))))
		     ,sv-name
		     ,table-name))))))

    (define (count-entries! indexes bytes-per-entry)
      (let ((n (length indexes))
            (u (length (delete-duplicates indexes eqv?))))
        (set! entry-count (+ entry-count n))
        (set! unique-entry-count (+ unique-entry-count u))
        (set! byte-count (+ byte-count (* n bytes-per-entry))))
      unspecific)

    (let ((table (make-equal-hash-table))
	  (make-entry (maker 'make-entry)))

      ;; Make sure that the leaf nodes are at the beginning of the table.
      (for-each (lambda (value)
		  (hash-table/intern! table value
				      (lambda ()
					(make-entry (make-value-code value)))))
		(map cdr prop-alist))

      (let loop
          ((entries (expand-ranges (slice-prop-alist prop-alist '(5 8 8))))
           (n-max 21))
	(hash-table/intern! table entries
	  (lambda ()
	    (make-entry
	     (let* ((n-bits (car entries))
		    (n-max* (- n-max n-bits)))
	       (make-node-code n-bits n-max*
		 (map (lambda (entry)
                        (loop entry n-max*))
                      (cdr entries)))))))))

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
     (write-string "/" port)
     (write unique-entry-count port)
     (write-string " entries, " port)
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

    `(,@(generate-entry-definition (symbol "ucd-" prop-name "-value")
                                   root-entry
                                   'sv
                                   table-name
                                   '(sv))

      ,@(append-map (lambda (name entry)
                      (generate-entry-definition name entry
                                                 'sv 'table '(sv table)))
                    entry-names
                    table-entries)

      (define ,table-name)
      ,@(generate-table-initializers table-name entry-names))))

(define (generate-entry-definition name entry sv-name table-name arg-names)
  (receive (comment offsets-expr body) (entry 'offsets sv-name table-name)
    (let ((defn
            (if offsets-expr
                `(define-deferred ,name
                   (let ((offsets ,offsets-expr))
                     (named-lambda (,name ,@arg-names)
                       ,@body)))
                `(define (,name ,@arg-names)
                   ,@body))))
      (if comment
          (list `(comment ,comment) defn)
          (list defn)))))

(define (generate-table-initializers table-name entries)
  (let ((groups
         (let split-items
             ((items
               (map cons
                    (iota (length entries))
                    entries)))
           (let ((n-items (length items)))
             (if (<= n-items 100)
                 (list items)
		 (append (split-items (list-head items 100))
			 (split-items (list-tail items 100))))))))
    (let ((group-names
           (map (lambda (index)
                  (symbol "initialize-" table-name "-" index))
                (iota (length groups)))))
      `((add-boot-init!
	 (lambda ()
	   (set! ,table-name (make-vector ,(length entries)))
	   ,@(map (lambda (name)
		    `(,name))
		  group-names)))
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
         (let loop ((indexes* (cdr indexes)))
           (if (pair? (cdr indexes*))
               (and (= slope (- (cadr indexes*) (car indexes*)))
                    (loop (cdr indexes*)))
               (linear-coder slope indexes))))))

(define (linear-coder slope indexes)
  (values 0
	  #f
          (lambda (offsets-name make-index-code)
	    offsets-name
	    (let ((make-offset
		   (lambda (slope)
		     (let ((power
			    (find (lambda (i)
				    (= slope (expt 2 i)))
				  (iota 8 1))))
		       (if power
			   (make-index-code power)
			   (code:* slope (make-index-code 0)))))))
	      (if (< slope 0)
		  (code:- (car indexes) (make-offset (- slope)))
		  (code:+ (car indexes) (make-offset slope)))))))

(define (try-8-bit-direct indexes)
  (and (< (apply max indexes) #x100)
       (8-bit-spread-coder 0 indexes)))

(define (try-8-bit-spread indexes)
  (let ((base (apply min indexes)))
    (and (< (- (apply max indexes) base) #x100)
         (8-bit-spread-coder base indexes))))

(define (8-bit-spread-coder base indexes)
  (values 1
	  `(bytevector
	    ,@(map (lambda (index)
		     (- index base))
		   indexes))
          (lambda (offsets-name make-index-code)
            (code:+ base
                    `(bytevector-u8-ref ,offsets-name
					,(make-index-code 0))))))

(define (try-16-bit-direct indexes)
  (and (< (apply max indexes) #x10000)
       (16-bit-spread-coder 0 indexes)))

(define (try-16-bit-spread indexes)
  (let ((base (apply min indexes)))
    (and (< (- (apply max indexes) base) #x10000)
         (16-bit-spread-coder base indexes))))

(define (16-bit-spread-coder base indexes)
  (values 2
	  `(bytevector
	    ,@(append-map (lambda (index)
			    (let ((delta (- index base)))
			      (list (remainder delta #x100)
				    (quotient delta #x100))))
			  indexes))
          (lambda (offsets-name make-index-code)
            (code:+ base
		    `(bytevector-u16le-ref ,offsets-name
					   ,(make-index-code 1))))))

(define (code:+ a b)
  (cond ((eqv? 0 a) b)
        ((eqv? 0 b) a)
        (else `(fix:+ ,a ,b))))

(define (code:- a b)
  (cond ((eqv? 0 b) a)
        (else `(fix:- ,a ,b))))

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

(define (expand-ranges stratified)
  (if (list? stratified)
      (let ((elements*
             (append-map (lambda (element)
                           (make-list (car element)
                                      (expand-ranges (cdr element))))
                         stratified)))
        (cons (count->bits (length elements*))
              elements*))
      stratified))

(define (count->bits count)
  (let loop ((bits 0) (n 1))
    (if (fix:< n count)
        (loop (fix:+ bits 1)
              (fix:lsh n 1))
        bits)))

(define (slice-prop-alist alist slices)
  (let loop ((alist alist) (slices (reverse slices)))
    (if (pair? slices)
        (loop (slice-by-bits alist (car slices))
              (cdr slices))
        (cdar alist))))

(define (slice-by-bits alist n-bits)
  (let ((step (fix:lsh 1 n-bits)))
    (let loop ((tail alist) (splits '()) (start 0))
      (if (pair? tail)
	  (receive (head tail* end) (slice-prop-alist-at tail start step)
	    (loop tail*
		  (cons (cons (make-cpr (fix:quotient start step)
					(fix:quotient end step))
                              (if (fix:= 1 (length head))
                                  (cdar head)
                                  (map (lambda (entry)
                                         (cons (cpr-size (car entry))
                                               (cdr entry)))
                                       head)))
			splits)
		  end))
	  (reverse! splits)))))

(define (slice-prop-alist-at alist start step)
  (let loop ((head '()) (tail alist) (end (fix:+ start step)))
    (if (pair? tail)
	(let ((entry (car tail)))
	  (let ((cpr (car entry)))
	    (cond ((fix:>= (cpr-start cpr) end)
		   (values (reverse! head) tail end))
		  ((fix:<= (cpr-end cpr) end)
		   (loop (cons entry head) (cdr tail) end))
		  (else
		   (let ((end*
			  (if (pair? head)
			      end
			      (fix:+ end
				     (fix:* (fix:quotient (fix:- (cpr-end cpr)
								 end)
							  step)
					    step)))))
		     (receive (entry1 entry2)
			 (split-entry-at cpr (cdr entry) end*)
		       (values (reverse! (cons entry1 head))
			       (if entry2
				   (cons entry2 (cdr tail))
				   (cdr tail))
			       end*)))))))
	(values (reverse! head) tail end))))

(define (split-entry-at cpr value cp)
  (if (fix:< cp (cpr-end cpr))
      (values (cons (make-cpr (cpr-start cpr) cp) value)
	      (cons (make-cpr cp (cpr-end cpr)) value))
      (values (cons cpr value)
	      #f)))

;;;; Value conversions

(define (enum-converter name translations)
  (lambda (value)
    (if value
	(let ((p
	       (find (lambda (p)
		       (ustring=? value (car p)))
		     translations)))
	  (if (not p)
	      (error (ustring-append "Illegal " name " value:") value))
	  (cdr p))
	(default-object))))

(define converter:category
  (enum-converter "category"
		  '(("Lu" . letter:uppercase)
		    ("Ll" . letter:lowercase)
		    ("Lt" . letter:titlecase)
		    ("Lm" . letter:modifier)
		    ("Lo" . letter:other)
		    ("Mn" . mark:nonspacing)
		    ("Mc" . mark:spacing-combining)
		    ("Me" . mark:enclosing)
		    ("Nd" . number:decimal-digit)
		    ("Nl" . number:letter)
		    ("No" . number:other)
		    ("Pc" . punctuation:connector)
		    ("Pd" . punctuation:dash)
		    ("Ps" . punctuation:open)
		    ("Pe" . punctuation:close)
		    ("Pi" . punctuation:initial-quote)
		    ("Pf" . punctuation:final-quote)
		    ("Po" . punctuation:other)
		    ("Sm" . symbol:math)
		    ("Sc" . symbol:currency)
		    ("Sk" . symbol:modifier)
		    ("So" . symbol:other)
		    ("Zs" . separator:space)
		    ("Zl" . separator:line)
		    ("Zp" . separator:paragraph)
		    ("Cc" . other:control)
		    ("Cf" . other:format)
		    ("Cs" . other:surrogate)
		    ("Co" . other:private-use)
		    ("Cn" . other:not-assigned))))

(define converter:boolean
  (enum-converter "boolean"
		  '(("N" . #f)
		    ("Y" . #t))))

(define converter:numeric-type
  (enum-converter "numeric-type"
		  '(("None" . #f)
		    ("De" . decimal)
		    ("Di" . digit)
		    ("Nu" . numeric))))

(define (converter:single-code-point value)
  (cond ((not value) (default-object))
	((ustring=? value "#") #f)
	((string->number value 16)
	 => (lambda (cp)
	      (if (not (unicode-code-point? cp))
		  (error "Illegal code-point value:" value))
	      cp))
	(else (error "Illegal code-point value:" value))))

(define (converter:zero-or-more-code-points value)
  (convert-code-points value #t))

(define (converter:one-or-more-code-points value)
  (convert-code-points value #f))

(define (convert-code-points value zero-ok?)
  (cond ((not value) (default-object))
	((ustring=? value "#") #f)
	((ustring=? value "")
	 (if (not zero-ok?)
	     (error "At least one code point required:"  value))
	 '())
	(else
	 (map (lambda (part)
		(let ((cp (string->number part 16 #t)))
		  (if (not (unicode-code-point? cp))
		      (error "Illegal code-points value:" value))
		  cp))
	      (code-points-splitter value)))))

(define code-points-splitter
  (string-splitter #\space #f))

(define (value-converter prop-name)
  (let ((p
	 (find (lambda (p)
		 (ustring=? prop-name (car p)))
	       value-converters)))
    (if (not p)
	(error "Unsupported property:" prop-name))
    (maybe-quote (cdr p))))

;;; Converted values must be constant expressions.
(define (maybe-quote converter)
  (lambda (value)
    (let ((converted (converter value)))
      (if (or (symbol? converted)
	      (list? converted)
	      (vector? converted))
	  `',converted
	  converted))))

(define value-converters
  (list (cons "Alpha" converter:boolean)
	(cons "CI" converter:boolean)
	(cons "CWCF" converter:boolean)
	(cons "CWCM" converter:boolean)
	(cons "CWKCF" converter:boolean)
	(cons "CWL" converter:boolean)
	(cons "CWT" converter:boolean)
	(cons "CWU" converter:boolean)
	(cons "Cased" converter:boolean)
	(cons "Lower" converter:boolean)
	(cons "NFKC_CF" converter:zero-or-more-code-points)
	(cons "OLower" converter:boolean)
	(cons "OUpper" converter:boolean)
	(cons "Upper" converter:boolean)
	(cons "WSpace" converter:boolean)
	(cons "cf" converter:one-or-more-code-points)
	(cons "gc" converter:category)
	(cons "lc" converter:one-or-more-code-points)
	(cons "nt" converter:numeric-type)
	(cons "scf" converter:single-code-point)
	(cons "slc" converter:single-code-point)
	(cons "stc" converter:single-code-point)
	(cons "suc" converter:single-code-point)
	(cons "tc" converter:one-or-more-code-points)
	(cons "uc" converter:one-or-more-code-points)))