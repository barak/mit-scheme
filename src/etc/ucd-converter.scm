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

;;;; UCD property metadata

(define (well-formed-metadata-spec? object)
  (and (list? object)
       (= 3 (length object))
       (string? (car object))
       (symbol? (cadr object))
       (property-type? (caddr object))))

(define (property-type? object)
  (or (simple-type? object)
      (unmapped-enum-type? object)
      (mapped-enum-type? object)
      (regex-type? object)
      (or-type? object)))

(define (simple-type? object)
  (case object
    ((boolean byte code-point code-point? code-point* code-point+
	      list-of-script rational-or-nan string)
     #t)
    (else #f)))

(define (unmapped-enum-type? object)
  (and (list? object)
       (>= (length object) 2)
       (eq? 'enum (car object))
       (every string? (cdr object))))

(define (unmapped-enum-type-names enum-type)
  (cdr enum-type))

(define (mapped-enum-type? object)
  (and (list? object)
       (>= (length object) 2)
       (eq? 'enum (car object))
       (every enum-value-map? (cdr object))))

(define (mapped-enum-type-translations enum-type)
  (cdr enum-type))

(define (enum-value-map? object)
  (and (pair? object)
       (string? (car object))
       (or (boolean? (cdr object))
	   (symbol? (cdr object)))))

(define (regex-type? object)
  (and (list? object)
       (= 2 (length object))
       (eq? 'regex (car object))
       (string? (cadr object))))

(define (or-type? object)
  (and (list? object)
       (= 3 (length object))
       (eq? 'or (car object))
       (string? (cadr object))
       (simple-type? (caddr object))))

(define this-directory
  (directory-pathname (current-load-pathname)))

(define mit-scheme-root-pathname
  (merge-pathnames "../../" this-directory))

(define raw-directory
  (pathname-as-directory (merge-pathnames "ucd-raw-props" this-directory)))

(define (raw-file-name name)
  (merge-pathnames (string-append name ".scm") raw-directory))

(define (read-ucd-property-metadata)
  (let ((properties (read-file (raw-file-name "names"))))
    (map (lambda (metadata)
	   (if (not (well-formed-metadata-spec? metadata))
	       (error "Ill-formed property metadata record:" metadata))
	   (make-metadata (car metadata)
			  (cadr metadata)
			  (caddr metadata)))
	 properties)))

(define-record-type <metadata>
    (make-metadata name full-name type-spec)
    metadata?
  (name metadata-name)
  (full-name metadata-full-name)
  (type-spec metadata-type-spec))

(define ucd-property-metadata
  (read-ucd-property-metadata))

(define (prop-metadata prop-name)
  (let ((metadata
	 (find (lambda (metadata)
		 (string=? prop-name (metadata-name metadata)))
	       ucd-property-metadata)))
    (if (not metadata)
	(error "Unknown property name:" prop-name))
    metadata))

;;;; Raw UCD attribute tables

(define (write-standard-property-files document)
  (let ((ucd-version (ucd-description document)))
    (call-with-output-file (ucd-version-file-name)
      (lambda (port)
	(write-line ucd-version port)))
    (for-each (lambda (metadata)
		(write-prop-file (metadata-name metadata) ucd-version document))
              ucd-property-metadata)))

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
  (write-string " (" port)
  (display (metadata-full-name (prop-metadata prop-name)) port)
  (write-string ")" port)
  (newline port)
  (newline port)
  (write-string ";;; Generated from " port)
  (write-string ucd-version port)
  (newline port)
  (newline port))

(define (read-ucd-version-file)
  (car (read-file (ucd-version-file-name))))

(define (read-prop-file prop-name)
  (read-file (prop-file-name prop-name)))

(define (ucd-version-file-name)
  (raw-file-name "version"))

(define (prop-file-name prop-name)
  (raw-file-name (string-append "prop-" prop-name)))

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
        ((char reserved noncharacter surrogate)
         (k (cons (cons (cp-attribute elt)
                        (or (attribute-value name elt)
                            group-value))
                  alist)))
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
                              (string=? (cdr p1) (cdr p2)))
                         (not (cdr p2))))
                (begin
                  (set-car! alist
                            (cons (merge-cprs (car p1) (car p2))
                                  (cdr p1)))
                  (set-cdr! alist (cddr alist))
                  (loop alist))
                (loop (cdr alist))))))
    (detect-undefined-ranges sorted)))

(define (detect-undefined-ranges alist)
  (let loop ((alist alist) (last-end 0))
    (if (pair? alist)
        (let* ((cpr (caar alist)))
          (if (< last-end (cpr-start cpr))
              (warn "Missing range:" (make-cpr last-end (cpr-start cpr))))
          (loop (cdr alist) (cpr-end cpr)))
        (if (< last-end char-code-limit)
            (warn "Missing range:" (make-cpr last-end char-code-limit)))))
  alist)

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
           (and (fix:> (string-length value) 0)
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
                  (string? (car content))
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
  (guarantee index-fixnum? start 'make-cpr)
  (let ((end
	 (if (default-object? end)
	     (fix:+ start 1)
	     (begin
	       (guarantee index-fixnum? end 'make-cpr)
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

(define (expand-cpr cpr)
  (iota (cpr-size cpr) (cpr-start cpr)))

;;;; Code generator

(define copyright-file-name
  (merge-pathnames "dist/copyright.scm" mit-scheme-root-pathname))

(define output-file-root
  (merge-pathnames "src/runtime/ucd-table" mit-scheme-root-pathname))

(define (generate-standard-property-tables)
  (for-each generate-property-table
	    '("Alpha"
	      "CWCF"
	      "CWL"
	      "CWU"
	      "Cased"
	      "GCB"
	      "Lower"
	      "NFD_QC"
	      "Upper"
	      "WB"
	      "WSpace"
	      "ccc"
	      "cf"
	      "dm"
	      "gc"
	      "lc"
	      "nt"
	      "nv"
	      "scf"
	      "slc"
	      "suc"
	      "tc"
	      "uc")))

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
  (string-append (->namestring output-file-root)
		  "-"
		  (string-downcase prop-name)
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
      (begin
	(write-string ";;; " port)
	(display (cadr expr) port))
      (pp expr port)))

(define (generate-property-table-code prop-name)
  (let* ((metadata (prop-metadata prop-name))
	 (generator (metadata->code-generator metadata)))
    (generator prop-name
	       metadata
	       (read-prop-file prop-name)
	       (symbol "ucd-" (string-downcase prop-name) "-value"))))

(define (metadata->code-generator metadata)
  (let ((name (metadata-name metadata))
	(type-spec (metadata-type-spec metadata)))
    (cond ((string=? name "GCB") code-generator:gcb)
	  ((string=? name "NFC_QC") code-generator:nfc-qc)
	  ((string=? name "NFKC_QC") code-generator:nfc-qc)
	  ((string=? name "WB") code-generator:wb)
	  ((string=? name "gc") code-generator:gc)
	  ((string=? name "nt") code-generator:nt)
	  ((eq? type-spec 'boolean) code-generator:boolean)
	  ((eq? type-spec 'byte) code-generator:byte)
	  ((eq? type-spec 'code-point) code-generator:code-point)
	  ((eq? type-spec 'code-point*) code-generator:code-point*)
	  ((eq? type-spec 'code-point+) code-generator:code-point+)
	  ((eq? type-spec 'rational-or-nan) code-generator:rational-or-nan)
	  (else (error "Unsupported metadata:" metadata)))))

(define (code-generator:boolean prop-name metadata prop-alist proc-name)
  (declare (ignore proc-name))
  (let* ((full-name (metadata-full-name metadata))
	 (char-set-name (symbol "char-set:" full-name)))
    `((define (,(symbol "char-" full-name "?") char)
	(char-in-set? char ,char-set-name))
      (define-deferred ,char-set-name
	(char-set*
	 ',(filter-map (lambda (value-map)
			 (and (equal? "Y" (cdr value-map))
			      (car value-map)))
		       prop-alist))))))

(define (code-generator:byte prop-name metadata prop-alist proc-name)
  ((hashed-code-generator value-manager:byte)
   prop-name metadata prop-alist proc-name))

(define (code-generator:code-point prop-name metadata prop-alist proc-name)
  ((hashed-code-generator value-manager:code-point)
   prop-name metadata prop-alist proc-name))

(define (code-generator:code-point* prop-name metadata prop-alist proc-name)
  ((hashed-code-generator value-manager:code-points)
   prop-name metadata prop-alist proc-name))

(define (code-generator:code-point+ prop-name metadata prop-alist proc-name)
  ((hashed-code-generator value-manager:code-points)
   prop-name metadata prop-alist proc-name))

(define (code-generator:gc prop-name metadata prop-alist proc-name)
  ((trie-code-generator (mapped-enum-value-manager #f metadata) '(5 8 8))
   prop-name metadata prop-alist proc-name))

(define (code-generator:gcb prop-name metadata prop-alist proc-name)
  ((trie-code-generator (unmapped-enum-value-manager #f metadata) '(5 8 8))
   prop-name metadata prop-alist proc-name))

(define (code-generator:nfc-qc prop-name metadata prop-alist proc-name)
  ((hashed-code-generator (mapped-enum-value-manager "Y" metadata))
   prop-name metadata prop-alist proc-name))

(define (code-generator:nt prop-name metadata prop-alist proc-name)
  ((hashed-code-generator (mapped-enum-value-manager "None" metadata))
   prop-name metadata prop-alist proc-name))

(define (code-generator:rational-or-nan prop-name metadata prop-alist proc-name)
  ((hashed-code-generator value-manager:rational-or-nan)
   prop-name metadata prop-alist proc-name))

(define (code-generator:wb prop-name metadata prop-alist proc-name)
  ((trie-code-generator (unmapped-enum-value-manager #f metadata) '(5 8 8))
   prop-name metadata prop-alist proc-name))

(define (value-manager default-string converter
		       #!optional runtime-default runtime-converter)
  (make-value-manager default-string
		      converter
		      (if (default-object? runtime-default)
			  (let ((value
				 (and default-string
				      (converter default-string))))
			    (lambda (char-expr)
			      char-expr
			      value))
			  runtime-default)
		      (if (default-object? runtime-converter)
			  (lambda (sv-expr) sv-expr)
			  runtime-converter)))

(define-record-type <value-manager>
    (make-value-manager default-string
			converter
			runtime-default
			runtime-converter)
    value-manager?
  (default-string value-manager-default-string)
  (converter value-manager-converter)
  (runtime-default value-manager-runtime-default)
  (runtime-converter value-manager-runtime-converter))

(define (string->cp string)
  (let ((cp (string->number string 16)))
    (if (not (unicode-code-point? cp))
	(error "Illegal code-point value:" string))
    cp))

(define value-manager:code-point
  (value-manager "#"
		 string->cp
		 (lambda (char-expr) char-expr)
		 (lambda (sv-expr) `(integer->char ,sv-expr))))

(define value-manager:code-points
  (value-manager "#"
		 (let ((splitter
			(string-splitter 'delimiter #\space
					 'allow-runs? #f)))
		   (lambda (value)
		     (if (string=? "" value)
			 '()
			 (map string->cp (splitter value)))))
		 (lambda (char-expr) `(list ,char-expr))
		 (lambda (svs-expr) `(map integer->char ,svs-expr))))

(define value-manager:byte
  (value-manager "0"
		 (lambda (string)
		   (let ((n (string->number string 10)))
		     (if (not (and (index-fixnum? n) (fix:<= n 254)))
			 (error "Illegal ccc value:" string))
		     n))))

(define value-manager:rational-or-nan
  (value-manager "NaN"
		 (lambda (string)
		   (if (string=? string "NaN")
		       #f
		       (let ((n (string->number string 10)))
			 (if (not (exact-rational? n))
			     (error "Illegal rational value:" string))
			 n)))))

(define (unmapped-enum-value-manager default-string metadata)
  (value-manager default-string
		 (enum-converter metadata
				 (let ((names
					(unmapped-enum-type-names
					 (metadata-type-spec metadata))))
				   (map cons
					names
					(iota (length names)))))))

(define (mapped-enum-value-manager default-string metadata)
  (value-manager default-string
		 (enum-converter metadata
				 (mapped-enum-type-translations
				  (metadata-type-spec metadata)))))

(define (enum-converter metadata translations)
  (let ((name (symbol->string (metadata-full-name metadata))))
    (lambda (value)
      (if value
	  (let ((p
		 (find (lambda (p)
			 (string=? value (car p)))
		       translations)))
	    (if (not p)
		(error (string-append "Illegal " name " value:") value))
	    (cdr p))
	  (default-object)))))

(define (hashed-code-generator value-manager)
  (let ((default-string (value-manager-default-string value-manager))
	(value-converter (value-manager-converter value-manager))
	(default-value (value-manager-runtime-default value-manager))
	(runtime-converter (value-manager-runtime-converter value-manager)))
    (lambda (prop-name metadata prop-alist proc-name)
      (let ((table-name (symbol "char-map:" (metadata-full-name metadata)))
	    (mapping
	     (append-map (lambda (p)
			   (map (let ((value (value-converter (cdr p))))
				  (lambda (cp)
				    (cons cp value)))
				(expand-cpr (car p))))
			 (remove (lambda (p)
				   (and default-string
					(string=? default-string (cdr p))))
				 prop-alist))))
	(with-notification
	 (lambda (port)
	   (write-string "UCD property " port)
	   (write-string prop-name port)
	   (write-string ": table pairs = " port)
	   (write (length mapping) port)))
	`((define (,proc-name char)
	    (hash-table-ref ,table-name char
			    (lambda () ,(default-value 'char))))
	  (define-deferred ,table-name
	    (let ((table (make-non-pointer-hash-table)))
	      (for-each (lambda (p)
			  (hash-table-set! table
					   (integer->char (car p))
					   ,(runtime-converter '(cdr p))))
			',mapping)
	      table)))))))

(define (trie-code-generator value-manager slices)
  (let ((default-string (value-manager-default-string value-manager))
	(value-converter (value-manager-converter value-manager))
	(default-value (value-manager-runtime-default value-manager))
	(runtime-converter (value-manager-runtime-converter value-manager)))
    (lambda (prop-name metadata prop-alist proc-name)
      (let ((maker (entries-maker))
	    (stats (trie-stats)))

	(define (make-value-code value)
	  (if (and default-string (string=? value default-string))
	      (lambda (offsets-name sv-name table-name)
		offsets-name
		(values #f #f
			`((declare (ignore ,table-name))
			  ,(default-value sv-name))))
	      (let ((value*
		     (let ((converted (value-converter value)))
		       (if (or (symbol? converted)
			       (list? converted)
			       (vector? converted))
			   `',converted
			   converted))))
		(lambda (offsets-name sv-name table-name)
		  offsets-name
		  (values #f #f
			  `((declare (ignore ,sv-name ,table-name))
			    ,(runtime-converter value*)))))))

	(define (make-node-code n-bits offset indexes)
	  (receive (bytes-per-entry offsets-expr coder)
	      (or (try-linear indexes)
		  (try-8-bit-direct indexes)
		  (try-8-bit-spread indexes)
		  (try-16-bit-direct indexes)
		  (try-16-bit-spread indexes)
		  (error "Dispatch won't fit in 16 bits:" indexes))
	    ((stats 'record!) indexes bytes-per-entry)
	    (lambda (offsets-name sv-name table-name)
	      (values indexes
		      offsets-expr
		      `(((vector-ref ,table-name
				     ,(coder offsets-name
					(lambda (shift)
					  `(fix:and ,(* (expt 2 shift)
							(- (expt 2 n-bits) 1))
						    ,(code:rsh sv-name
							       (- offset
								  shift))))))
			 ,sv-name
			 ,table-name))))))

	(let ((table (make-equal-hash-table))
	      (make-entry (maker 'make-entry)))

	  ;; Make sure that the leaf nodes are at the beginning of the table.
	  (for-each (lambda (value)
		      (hash-table/intern! table value
			(lambda ()
			  (make-entry (make-value-code value)))))
		    (map cdr prop-alist))

	  (let loop
	      ((entries (expand-ranges (slice-prop-alist prop-alist slices)))
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
	  ((stats 'report) prop-name (length table-entries))
	  (generate-top-level (string-downcase prop-name)
			      root-entry table-entries proc-name))))))

(define (generate-top-level prop-name root-entry table-entries proc-name)
  (let ((table-name (symbol "ucd-" prop-name "-entries"))
        (entry-names
         (map (lambda (index)
                (symbol "ucd-" prop-name "-entry-" index))
              (iota (length table-entries)))))

    `(,@(generate-entry-definition proc-name root-entry 'sv table-name
				   '(char)
				   (lambda (body)
				     `((let ((sv (char->integer char)))
					 ,@body))))

      ,@(append-map (lambda (name entry)
                      (generate-entry-definition name entry 'sv 'table
						 '(sv table)
						 (lambda (body) body)))
                    entry-names
                    table-entries)

      (define ,table-name)
      ,@(generate-table-initializers table-name entry-names))))

(define (generate-entry-definition name entry sv-name table-name
				   arg-names wrap-body)
  (receive (comment offsets-expr body) (entry 'offsets sv-name table-name)
    (let ((defn
           (if offsets-expr
	       `(define-deferred ,name
		  (let ((offsets ,offsets-expr))
		    (named-lambda (,name ,@arg-names)
		      ,@(wrap-body body))))
	       `(define (,name ,@arg-names)
		  ,@(wrap-body body)))))
      (if (and comment generate-node-comments?)
          (list `(comment ,comment) defn)
          (list defn)))))

(define generate-node-comments? #f)

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

(define (trie-stats)
  (let ((entry-count 0)
	(unique-entry-count 0)
	(byte-count 0))

    (define (record! indexes bytes-per-entry)
      (let ((n (length indexes))
	    (u (length (delete-duplicates indexes eqv?))))
	(set! entry-count (+ entry-count n))
	(set! unique-entry-count (+ unique-entry-count u))
	(set! byte-count (+ byte-count (* n bytes-per-entry))))
      unspecific)

    (define (report prop-name n-entries)
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

    (lambda (operator)
      (case operator
	((record!) record!)
	((report) report)
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