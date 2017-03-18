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

(declare (usual-integrations))

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
  (declare (ignore prop-name proc-name))
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
  ((trie-code-generator value-manager:byte)
   prop-name metadata prop-alist proc-name))

(define (code-generator:code-point prop-name metadata prop-alist proc-name)
  ((trie-code-generator value-manager:code-point)
   prop-name metadata prop-alist proc-name))

(define (code-generator:code-point* prop-name metadata prop-alist proc-name)
  ((trie-code-generator value-manager:code-points)
   prop-name metadata prop-alist proc-name))

(define (code-generator:code-point+ prop-name metadata prop-alist proc-name)
  ((trie-code-generator value-manager:code-points)
   prop-name metadata prop-alist proc-name))

(define (code-generator:gc prop-name metadata prop-alist proc-name)
  ((trie-code-generator (mapped-enum-value-manager #f metadata))
   prop-name metadata prop-alist proc-name))

(define (code-generator:gcb prop-name metadata prop-alist proc-name)
  ((trie-code-generator (unmapped-enum-value-manager #f metadata))
   prop-name metadata prop-alist proc-name))

(define (code-generator:nfc-qc prop-name metadata prop-alist proc-name)
  ((trie-code-generator (mapped-enum-value-manager "Y" metadata))
   prop-name metadata prop-alist proc-name))

(define (code-generator:nt prop-name metadata prop-alist proc-name)
  ((trie-code-generator (mapped-enum-value-manager "None" metadata))
   prop-name metadata prop-alist proc-name))

(define (code-generator:rational-or-nan prop-name metadata prop-alist proc-name)
  ((trie-code-generator value-manager:rational-or-nan)
   prop-name metadata prop-alist proc-name))

(define (code-generator:wb prop-name metadata prop-alist proc-name)
  ((trie-code-generator (unmapped-enum-value-manager #f metadata))
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
		 (lambda (char-expr) `(string ,char-expr))
		 (lambda (svs-expr) `(string* (map integer->char ,svs-expr)))))

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
	(runtime-default (value-manager-runtime-default value-manager))
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
			    (lambda () ,(runtime-default 'char))))
	  (define-deferred ,table-name
	    (let ((table (make-non-pointer-hash-table)))
	      (for-each (lambda (p)
			  (hash-table-set! table
					   (integer->char (car p))
					   ,(runtime-converter '(cdr p))))
			',mapping)
	      table)))))))

(define (inversion-map-generator value-manager)
  (let ((default-string (value-manager-default-string value-manager))
	(value-converter (value-manager-converter value-manager))
	(runtime-default (value-manager-runtime-default value-manager))
	(runtime-converter (value-manager-runtime-converter value-manager)))
    (lambda (prop-name metadata prop-alist proc-name)
      (let ((table-name (symbol "char-map:" (metadata-full-name metadata)))
	    (pairs
	     (remove (lambda (p)
		       (and default-string
			    (string=? default-string (cdr p))))
		     prop-alist)))
	(with-notification
	 (lambda (port)
	   (write-string "UCD property " port)
	   (write-string prop-name port)
	   (write-string ": table pairs = " port)
	   (write (length pairs) port)))
	(let ((keys
	       (list->vector
		(append-map (lambda (p)
			      (let ((cpr (car p)))
				(list (cpr-start cpr)
				      (cpr-end cpr))))
			    pairs)))
	      (values
	       (list->vector
		(map (lambda (p)
		       (value-converter (cdr p)))
		     pairs))))
	  `((define (,proc-name char)
	      (inversion-map-ref ,table-name
				 char
				 (lambda () ,(runtime-default 'char))))
	    (define-deferred ,table-name
	      (make-inversion-map ',keys
				  (vector-map (lambda (value)
						,(runtime-converter 'value))
					      ',values)))))))))

(define (trie-code-generator value-manager #!optional bit-slices)
  (let ((bit-slices (if (default-object? bit-slices) '(5 4 4 4 4) bit-slices))
        (convert-value (make-trie-value-converter value-manager))
	(runtime-default (value-manager-runtime-default value-manager))
	(runtime-converter (value-manager-runtime-converter value-manager)))
    (lambda (prop-name metadata prop-alist proc-name)
      (declare (ignore metadata))
      (let ((tables
	     (build-trie-tables bit-slices convert-value runtime-converter
                                prop-name prop-alist)))
	(with-notification
	 (lambda (port)
	   (write-string "UCD property " port)
	   (write-string prop-name port)
	   (write-string ": tables = " port)
	   (write (apply + (map trie-table-size tables)) port)
	   (write-string " bytes" port)))
	`((define (,proc-name char)
            ,(let ((accesses
                    `(let ((sv (char->integer char)))
                       ,(generate-accesses prop-name 'sv tables)))
                   (default (runtime-default 'char)))
               (if default
                   `(or ,accesses ,default)
                   accesses)))
	  ,@(map (lambda (table)
		   `(define-deferred ,(trie-table-name table)
		      ,(trie-table-expr table)))
                 tables))))))

(define (make-trie-value-converter value-manager)
  (let ((default-string (value-manager-default-string value-manager))
	(value-converter (value-manager-converter value-manager)))
    (lambda (value)
      (if (and default-string (string=? value default-string))
	  #f
          (value-converter value)))))

(define (generate-accesses prop-name sv-expr tables)
  (define (generate-access table index)
    ((trie-table-accessor table)
     (trie-table-name table)
     (if (trie-table-scale table)
	 (code:or (code:lsh index (trie-table-scale table))
		  (code:and (trie-table-mask table)
			    (code:rsh sv-expr (trie-table-offset table))))
	 index)))

  (let loop ((tables tables) (expr 0))
    (if (pair? tables)
	(loop (cdr tables)
	      (generate-access (car tables) expr))
	expr)))

(define (code:and m a)
  (cond ((not m) a)
        ((index-fixnum? a) (fix:and m a))
        (else `(fix:and ,m ,a))))

(define (code:or a b)
  (cond ((eqv? 0 a) b)
        ((eqv? 0 b) a)
        (else `(fix:or ,a ,b))))

(define (code:lsh a n)
  (cond ((index-fixnum? a) (fix:lsh a n))
        ((= n 0) a)
        (else `(fix:lsh ,a ,n))))

(define (code:rsh a n)
  (code:lsh a (- n)))

(define (build-trie-tables bit-slices convert-value runtime-converter
                           prop-name prop-alist)
  (define (loop suffix offset scale bit-slices tables)
    (let ((mask
	   (and (fix:> suffix 0)
		(fix:- (fix:lsh 1 (car bit-slices)) 1)))
	  (offset (fix:- offset (car bit-slices))))
      (if (pair? (cdr tables))
	  (receive (size table-expr accessor)
	      (choose-index-format (car tables) (cadr tables) (cadr bit-slices))
	    (cons (make-trie-table (make-name suffix)
				   size
				   table-expr
				   accessor
				   mask
				   offset
				   scale)
		  (loop (fix:+ suffix 1)
			offset
			(cadr bit-slices)
			(cdr bit-slices)
			(cdr tables))))
	  (receive (index table) (maybe-split-trie-value-table (car tables))
	    (let ((converted-values (map convert-value (vector->list table))))
	      (if index
		  (receive (size table-expr accessor)
		      (choose-index-format index table 0)
		    (list (make-trie-table (make-name suffix)
					   size
					   table-expr
					   accessor
					   mask
					   offset
					   scale)
			  (receive (size table-expr accessor)
			      (choose-value-format converted-values
                                                   runtime-converter)
			    (make-trie-table (make-name (fix:+ suffix 1))
					     size
					     table-expr
					     accessor
					     #f
					     #f
					     #f))))
		  (receive (size table-expr accessor)
		      (choose-value-format converted-values runtime-converter)
		    (list (make-trie-table (make-name suffix)
					   size
					   table-expr
					   accessor
					   mask
					   offset
					   scale)))))))))

  (define (make-name suffix)
    (symbol "ucd-" prop-name "-table-" suffix))

  (loop 0
	21
	1
	bit-slices
	(prop-alist->tables prop-alist
			    (reverse
			     (map (lambda (bit-slice)
				    (fix:lsh 1 bit-slice))
				  (cdr bit-slices))))))

(define-record-type <trie-table>
    (make-trie-table name size expr accessor mask offset scale)
    trie-table?
  (name trie-table-name)
  (size trie-table-size)
  (expr trie-table-expr)
  (accessor trie-table-accessor)
  (mask trie-table-mask)
  (offset trie-table-offset)
  (scale trie-table-scale))

(define (choose-index-format index table bit-slice)
  (let ((n (fix:lsh (vector-length table) (fix:- 0 bit-slice))))
    (cond ((fix:< n #x100)
	   (values (+ 2 (vector-length index))
                   `(apply bytevector ',(vector->list index))
		   (lambda (bv-expr i-expr)
		     `(bytevector-u8-ref ,bv-expr ,i-expr))))
	  ((fix:< n #x10000)
	   (values (+ 2 (* 2 (vector-length index)))
                   `(apply bytevector-u16be ',(vector->list index))
		   (lambda (bv-expr i-expr)
		     `(bytevector-u16be-ref ,bv-expr
					    (fix:lsh ,i-expr 1)))))
	  (else
	   (error "Table too large:" n)))))

(define (choose-value-format converted-values runtime-converter)
  (values (+ 1 (* 8 (length converted-values)))
          (let ((conversion (runtime-converter 'converted)))
            (if (eq? 'converted conversion)
                `(list->vector ',converted-values)
                `(list->vector
                  (map (lambda (converted)
                         ,(if (memq #f converted-values)
                              `(and converted ,conversion)
                              conversion))
                       ',converted-values))))
	  (lambda (v-expr i-expr)
	    `(vector-ref ,v-expr ,i-expr))))

(define (generate-trie-table-builder make-table combine-tables values)
  (let ((tables
	 (let loop ((values values) (n (length values)))
	   (if (<= n 100)
	       (list `(,make-table ,@values))
	       (cons `(,make-table ,@(list-head values 100))
		     (loop (list-tail values 100)
			   (- n 100)))))))
    (if (= 1 (length tables))
	(car tables)
	`(,combine-tables ,@tables))))

(define (maybe-split-trie-value-table table)
  (let ((distinct-values
	 (list->vector
	  (delete-duplicates! (vector->list table)
			      string=?))))
    (let ((n-values (vector-length distinct-values))
	  (table-size (vector-length table)))
      ;; Check whether two tables would use less space than one.
      (if (< (+ (if (< n-values #x100)
		    table-size
		    (* 2 table-size))
		(* 8 n-values))
	     (* 8 table-size))
	  (values (vector-map (lambda (value)
				(do ((i 0 (fix:+ i 1)))
				    ((string=? (vector-ref distinct-values i)
					       value)
				     i)))
			      table)
		  distinct-values)
	  (values #f table)))))

(define (prop-alist->tables prop-alist slice-steps)
  (let loop
      ((slices (slice-prop-alist prop-alist (car slice-steps)))
       (slice-steps (cdr slice-steps))
       (tables '()))
    (receive (indices table) (generate-index-from-slices slices)
      (if (pair? slice-steps)
	  (loop (slice-list indices (car slice-steps))
		(cdr slice-steps)
		(cons (list->vector table) tables))
	  (cons* (list->vector indices)
		 (list->vector table)
		 tables)))))

(define (generate-index-from-slices slices)
  (let ((table (make-equal-hash-table)))
    (let loop ((slices slices) (next-index 0) (indices '()))
      (let ((next (slices)))
	(if (pair? next)
	    (let ((index
                   (hash-table-intern! table
                                       (car next)
                                       (lambda () next-index))))
	      (loop (cdr next)
		    (if (fix:= index next-index)
			(fix:+ next-index 1)
			next-index)
		    (cons index indices)))
	    (values (reverse! indices)
		    (append-map car
                                (sort (hash-table->alist table)
                                      (lambda (a b)
                                        (fix:< (cdr a) (cdr b)))))))))))

(define (slice-list items step)
  (let loop ((items items))
    (lambda ()
      (if (pair? items)
	  (cons (list-head items step)
		(loop (list-tail items step)))
	  '()))))

(define (slice-prop-alist alist step)
  (let loop ((alist alist) (start 0))
    (lambda ()
      (if (pair? alist)
	  (let ((end (fix:+ start step)))
	    (receive (head tail) (slice-prop-alist-at alist start end)
	      (cons head
		    (loop tail end))))
	  '()))))

(define (slice-prop-alist-at alist start end)
  (let loop ((tail alist) (head '()))
    (if (pair? tail)
	(let ((entry (car tail)))
	  (let ((cpr (car entry))
		(value (cdr entry)))

            (define (cons-head n head)
              (if (fix:> n 0)
                  (cons-head (fix:- n 1) (cons value head))
                  head))

            (if (fix:< end (cpr-end cpr))
                (values (reverse!
                         (cons-head (fix:- end (cpr-start cpr))
                                    head))
                        (cons (cons (make-cpr end (cpr-end cpr))
                                    value)
                              (cdr tail)))
                (loop (cdr tail)
                      (cons-head (cpr-size cpr)
                                 head)))))
	(values (reverse! head) tail))))