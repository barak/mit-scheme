;;; -*-Scheme-*-
;;;
;;; $Id: xml-parser.scm,v 1.2 2001/07/05 20:47:41 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; XML parser

;;; Comments of the form [N] refer to production rules in the XML 1.0
;;; standard, second edition, 6 October 2000.  Each such comment marks
;;; the code that corresponds to that rule.

;;; **** TO DO ****
;;; * Attribute-value normalization (p. 29).

(declare (usual-integrations))

;;;; Utilities

(define char-set:xml-char		;[2], loose UTF-8
  ;; The upper range of this alphabet would normally be #xFE, but XML
  ;; doesn't use any characters larger than #x10FFFF, so the largest
  ;; byte that can be seen is #xF4.
  (char-set-union (char-set #\tab #\linefeed #\return)
		  (ascii-range->char-set #x20 #xF5)))

(define char-set:char-data
  (char-set-difference char-set:xml-char (char-set #\< #\&)))

(define-*parser-macro S			;[3]
  `(NOISE (+ (ALPHABET CHAR-SET:XML-WHITESPACE))))

(define-*parser-macro S?
  `(NOISE (* (ALPHABET CHAR-SET:XML-WHITESPACE))))

(define char-set:xml-whitespace
  (char-set #\space #\tab #\return #\linefeed))

(define-*parser-macro (bracket description open close . body)
  (let ((v (generate-uninterned-symbol)))
    `(WITH-POINTER ,v
       (SEQ ,open
	    ,@body
	    (ALT ,close
		 (SEXP
		  (LAMBDA (BUFFER)
		    BUFFER
		    (ERROR
		     ,(if (string? description)
			  (string-append "Unterminated " description " at")
			  `(STRING-APPEND "Unterminated " ,description " at"))
		     (PARSER-BUFFER-POSITION-STRING ,v)))))))))

(define-*parser-macro (sbracket description open close . body)
  `(BRACKET ,description (NOISE (STRING ,open)) (NOISE (STRING ,close))
     ,@body))

(define-*parser-macro (require-success message body)
  `(ALT ,body
	(SEXP
	 (LAMBDA (BUFFER)
	   (ERROR ,(if (string? message)
		       (string-append message " at")
		       `(STRING-APPEND ,message " at"))
		  (PARSER-BUFFER-POSITION-STRING BUFFER))))))

(define (make-xml-char-reference n)
  (if (not (or (= n #x9)
	       (= n #xA)
	       (= n #xD)
	       (<= #x20 n #xD7FF)
	       (<= #xE000 n #xFFFD)
	       (<= #x10000 n #x10FFFF)))
      (error "Disallowed Unicode character code:" n))
  (integer->unicode-string n))

(define (integer->unicode-string n)
  (let ((initial-char
	 (lambda (n offset)
	   (integer->char
	    (fix:or (fix:and (fix:lsh #xFF (fix:+ n 1)) #xFF)
		    (fix:lsh n -6)))))
	(subsequent-char
	 (lambda (offset)
	   (integer->char
	    (fix:or #x80
		    (fix:and (fix:lsh n (fix:- 0 offset))
			     #x3F))))))
    (declare (integrate-operator initial-char subsequent-char))
    (if (not (and (<= 0 n) (< n #x80000000)))
	(error:bad-range-argument n 'INTEGER->UNICODE-STRING))
    (cond ((< n #x00000080)
	   (string (integer->char n)))
	  ((< n #x00000800)
	   (string (initial-char 5 6)
		   (subsequent-char 6)))
	  ((< n #x00010000)
	   (string (initial-char 4 12)
		   (subsequent-char 12)
		   (subsequent-char 6)))
	  ((< n #x00200000)
	   (string (initial-char 3 18)
		   (subsequent-char 18)
		   (subsequent-char 12)
		   (subsequent-char 6)))
	  ((< n #x04000000)
	   (string (initial-char 2 24)
		   (subsequent-char 24)
		   (subsequent-char 18)
		   (subsequent-char 12)
		   (subsequent-char 6)))
	  (else
	   (string (initial-char 1 30)
		   (subsequent-char 30)
		   (subsequent-char 24)
		   (subsequent-char 18)
		   (subsequent-char 12)
		   (subsequent-char 6))))))

(define (normalize-line-endings string)
  (if (string-find-next-char string #\return)
      (let ((end (string-length string)))
	(let ((step-over-eol
	       (lambda (index)
		 (fix:+ index
			(if (and (fix:< (fix:+ index 1) end)
				 (char=? (string-ref string (fix:+ index 1))
					 #\linefeed))
			    2
			    1)))))
	  (let ((n
		 (let loop ((start 0) (n 0))
		   (let ((index
			  (substring-find-next-char string start end
						    #\return)))
		     (if index
			 (loop (step-over-eol index)
			       (fix:+ n (fix:+ (fix:- index start) 1)))
			 (fix:+ n (fix:- end start)))))))
	    (let ((string* (make-string n)))
	      (let loop ((start 0) (start* 0))
		(let ((index
		       (substring-find-next-char string start end
						 #\return)))
		  (if index
		      (let ((start*
			     (substring-move! string start index
					      string* start*)))
			(string-set! string* start* #\newline)
			(loop (step-over-eol index)
			      (fix:+ start* 1)))
		      (substring-move! string start end string* start*))))
	      string*))))
      string))

;;;; Top level

(define parse-xml-document		;[1,22]
  (*parser
   (transform
       (lambda (v)
	 (make-xml-document (vector-ref v 0)
			    (vector-ref v 1)
			    (vector-ref v 2)
			    (vector-ref v 3)
			    (vector-ref v 4)
			    (vector-ref v 5)))
     (complete
      (seq (alt (top-level parse-declaration)
		(values #f))
	   (encapsulate vector->list
	     (* (top-level parse-misc)))
	   (alt (seq (top-level parse-dtd)
		     (encapsulate vector->list
		       (* (top-level parse-misc))))
		(values #f '()))
	   (top-level parse-element)
	   (encapsulate vector->list
	     (* (top-level parse-misc))))))))

(define parse-misc			;[27]
  (*parser
   (alt parse-comment
	parse-processing-instructions
	(element-transform normalize-line-endings
	  (match (+ (alphabet char-set:xml-whitespace)))))))

(define parse-declaration		;[23,24,32,80]
  (*parser
   (encapsulate (lambda (v) (transform-declaration (vector-ref v 0)))
     (sbracket "XML declaration" "<?xml" "?>"
       parse-attribute-list))))

(define (transform-declaration attributes)
  (let ((finish
	 (lambda (version encoding standalone)
	   (if (not (and (string? version)
			 (match-xml-version (string->parser-buffer version))))
	       (error "Malformed XML version:" version))
	   (if (and encoding
		    (not (and (string? encoding)
			      (match-encoding
			       (string->parser-buffer encoding)))))
	       (error "Malformed encoding attribute:" encoding))
	   (if (and standalone
		    (not (member standalone '("yes" "no"))))
	       (error "Malformed standalone attribute:" standalone))
	   (make-xml-declaration version
				 encoding
				 (equal? standalone "yes")))))
    (let loop
	((attributes attributes)
	 (names '("version" "encoding" "standalone"))
	 (results '()))
      (if (pair? names)
	  (if (pair? attributes)
	      (if (string=? (symbol-name (caar attributes)) (car names))
		  (loop (cdr attributes)
			(cdr names)
			(cons (cdar attributes) results))
		  (loop attributes
			(cdr names)
			(cons #f results)))
	      (let loop ((names names) (results results))
		(if (pair? names)
		    (loop (cdr names) (cons #f results))
		    (finish (caddr results) (cadr results) (car results)))))
	  (begin
	    (if (pair? attributes)
		(error "Extra attributes in XML declaration:" attributes))
	    (finish (caddr results) (cadr results) (car results)))))))

(define match-xml-version		;[26]
  (let ((a (char-set-union char-set:alphanumeric (string->char-set "_.:-"))))
    (*matcher (+ (alphabet a)))))

(define match-encoding			;[81]
  (let ((a (char-set-union char-set:alphanumeric (string->char-set "_.-"))))
    (*matcher
     (seq (alphabet char-set:alphabetic)
	  (* (alphabet a))))))

;;;; Elements

(define (parse-element buffer)		;[39]
  (let ((p (get-parser-buffer-pointer buffer)))
    (let ((v (parse-start-tag buffer)))
      (and v
	   (vector
	    (make-xml-element
	     (vector-ref v 0)
	     (vector-ref v 1)
	     (if (string=? (vector-ref v 2) ">")
		 (let loop ((elements '#()))
		   (let ((v* (parse-end-tag buffer)))
		     (if v*
			 (begin
			   (if (not (eq? (vector-ref v 0) (vector-ref v* 0)))
			       (error "Mismatched start tag at"
				      (parser-buffer-position-string p)))
			   (vector->list elements))
			 (let ((v* (parse-content buffer)))
			   (if (not v*)
			       (error "Unterminated start tag at"
				      (parser-buffer-position-string p)))
			   (if (equal? v* '#(""))
			       (error "Unknown content at"
				      (parser-buffer-position-string buffer)))
			   (loop (vector-append elements v*))))))
		 '())))))))

(define parse-start-tag			;[40,44]
  (*parser
   (bracket "start tag"
       (seq (noise (string "<")) maybe-parse-name)
       (match (alt (string ">") (string "/>")))
     parse-attribute-list)))

(define parse-end-tag			;[42]
  (*parser
   (sbracket "end tag" "</" ">"
     parse-name
     S?)))

(define parse-content			;[43]
  (*parser
   (transform
       (lambda (v)
	 (let ((elements (vector->list v)))
	   (coalesce-strings! elements)
	   (list->vector elements)))
     (seq parse-char-data
	  (* (seq (alt parse-element
		       parse-reference
		       parse-cdata-section
		       parse-processing-instructions
		       parse-comment)
		  parse-char-data))))))

;;;; Other markup

(define (bracketed-region-parser description start end)
  (let ((parser
	 (terminated-region-parser description char-set:xml-char end)))
    (*parser (sbracket description start end parser))))

(define (terminated-region-parser description alphabet . ends)
  (let ((matcher
	 (lambda (buffer)
	   (let loop ()
	     (if (and (not (there-exists? ends
			     (lambda (end)
			       (match-parser-buffer-string-no-advance buffer
								      end))))
		      (match-parser-buffer-char-in-set buffer alphabet))
		 (loop)
		 #t)))))
    (*parser (element-transform normalize-line-endings (match matcher)))))

(define parse-char-data			;[14]
  (terminated-region-parser "character data" char-set:char-data "]]>"))

(define parse-comment			;[15]
  (let ((parser (bracketed-region-parser "comment" "<!--" "-->")))
    (*parser (element-transform make-xml-comment parser))))

(define parse-cdata-section		;[18,19,20,21]
  (bracketed-region-parser "CDATA section" "<![CDATA[" "]]>"))

(define parse-processing-instructions	;[16,17]
  (let ((description "processing instructions")
	(start "<?")
	(end "?>"))
    (let ((parse-body
	   (terminated-region-parser description char-set:xml-char end)))
      (*parser
       (encapsulate
	   (lambda (v)
	     (make-xml-processing-instructions (vector-ref v 0)
					       (vector-ref v 1)))
	 (sbracket description start end
	   (with-pointer ns
	     (transform
		 (lambda (v)
		   (if (string-ci=? (symbol-name (vector-ref v 0)) "xml")
		       (error "Illegal PI name at"
			      (parser-buffer-position-string ns)))
		   v)
	       parse-name))
	   parse-body))))))

;;;; Names and references

(define parse-name
  (*parser (require-success "malformed XML name" maybe-parse-name)))

(define maybe-parse-name		;[5]
  (*parser
   (element-transform xml-intern
     (match (seq (alphabet char-set:name-initial)
		 (* (alphabet char-set:name-subsequent)))))))

(define parse-name-token
  (*parser
   (require-success "malformed XML name token"
     maybe-parse-name-token)))

(define maybe-parse-name-token		;[7]
  (*parser
   (element-transform xml-intern
     (match (+ (alphabet char-set:name-subsequent))))))

(define char-set:name-initial
  (char-set-union char-set:alphabetic
		  (string->char-set "_:")
		  (ascii-range->char-set #x80 #xF5)))

(define char-set:name-subsequent	;[4], loose UTF-8
  (char-set-union char-set:alphanumeric
		  (string->char-set ".-_:")
		  (ascii-range->char-set #x80 #xF5)))

(define parse-reference			;[66,67,68]
  (let ((predefined
	 (list (cons (xml-intern "lt") "<")
	       (cons (xml-intern "gt") ">")
	       (cons (xml-intern "amp") "&")
	       (cons (xml-intern "quot") "\"")
	       (cons (xml-intern "apos") "'"))))
    (*parser
     (sbracket "reference" "&" ";"
       (alt (seq (noise (string "#"))
		 (alt (element-transform
			  (lambda (s)
			    (make-xml-char-reference (string->number s 10)))
			(match (+ (alphabet char-set:numeric))))
		      (element-transform
			  (lambda (s)
			    (make-xml-char-reference (string->number s 16)))
			(seq (noise (string "x"))
			     (match (+ (alphabet "0-9a-fA-f")))))))
	    (element-transform
		(lambda (name)
		  (let ((entry (assq name predefined)))
		    (if entry
			(cdr entry)
			(make-xml-entity-reference name))))
	      parse-name))))))

(define parse-parameter-entity-reference ;[69]
  (*parser
   (element-transform make-xml-parameter-entity-reference
     (sbracket "parameter-entity reference" "%" ";"
       parse-name))))

;;;; Attributes

(define parse-attribute-list
  (*parser
   (encapsulate vector->list
     (seq (* parse-attribute)
	  S?))))

(define parse-attribute			;[41,25]
  (*parser
   (encapsulate (lambda (v) (cons (vector-ref v 0) (vector-ref v 1)))
     (seq S
	  maybe-parse-name
	  S?
	  (require-success "missing attribute separator"
	    (noise (string "=")))
	  S?
	  parse-attribute-value))))

(define (attribute-value-parser alphabet parse-reference)
  (let ((a1 (char-set-difference alphabet (char-set #\")))
	(a2 (char-set-difference alphabet (char-set #\'))))
    (*parser
     (encapsulate
	 (lambda (v)
	   (let ((elements (vector->list v)))
	     (coalesce-strings! elements)
	     (if (and (pair? elements)
		      (null? (cdr elements)))
		 (car elements)
		 elements)))
       (alt (sbracket "attribute value" "\"" "\""
	      (* (alt (match (+ (alphabet a1)))
		      parse-reference)))
	    (sbracket "attribute value" "'" "'"
	      (* (alt (match (+ (alphabet a2)))
		      parse-reference))))))))

(define parse-entity-value		;[9]
  (attribute-value-parser
   (char-set-difference char-set:xml-char (char-set #\% #\&))
   (*parser
    (alt parse-reference
	 parse-parameter-entity-reference))))

(define parse-attribute-value		;[10]
  (let ((parser (attribute-value-parser char-set:char-data parse-reference)))
    (*parser (require-success "malformed attribute value" parser))))

(define (coalesce-strings! elements)
  (do ((elements elements (cdr elements)))
      ((not (pair? elements)))
    (if (and (string? (car elements))
	     (pair? (cdr elements))
	     (string? (cadr elements)))
	(begin
	  (set-car! elements
		    (string-append (car elements)
				   (cadr elements)))
	  (set-cdr! elements (cddr elements))))))

;;;; Document-type declarations

(define parse-dtd			;[28]
  (*parser
   (encapsulate
       (lambda (v)
	 (make-xml-dtd (vector-ref v 0)
		       (vector-ref v 1)
		       (vector-ref v 2)))
     (sbracket "document-type declaration" "<!DOCTYPE" ">"
       (require-success "Malformed document type"
	 (seq S
	      parse-name
	      (alt (seq S
			parse-external-id)
		   (values #f))
	      S?
	      (alt (seq (encapsulate vector->list
			  (sbracket "internal DTD" "[" "]"
			    (* (alt parse-markup-decl
				    parse-decl-separator))))
			S?)
		   (values #f))))))))

(define parse-markup-decl		;[29]
  (*parser
   (alt parse-element-decl
	parse-attlist-decl
	parse-entity-decl
	parse-notation-decl
	parse-processing-instructions
	parse-comment)))

(define parse-decl-separator		;[28a]
  (*parser
   (alt parse-parameter-entity-reference
	S)))

(define parse-external-subset		;[30]
  (*parser
   (seq (? parse-text-decl)
	parse-external-subset-decl)))

(define parse-external-subset-decl	;[31]
  (*parser
   (* (alt parse-markup-decl
	   parse-conditional-section
	   parse-decl-separator))))

(define parse-element-decl		;[45]
  (letrec
      ((parse-children			;[47,49,50]
	(*parser
	 (encapsulate encapsulate-suffix
	   (seq (sbracket "element-declaration type" "(" ")"
		  S?
		  (alt (encapsulate (lambda (v) (cons 'ALT (vector->list v)))
			 (seq parse-cp
			      (+ (seq S?
				      (noise (string "|"))
				      S?
				      parse-cp))))
		       (encapsulate (lambda (v) (cons 'SEQ (vector->list v)))
			 (seq parse-cp
			      (* (seq S?
				      (noise (string ","))
				      S?
				      parse-cp)))))
		  S?)
		(? (match (alphabet "?*+")))))))

       (parse-cp			;[48]
	 (*parser
	  (alt (encapsulate encapsulate-suffix
		 (seq maybe-parse-name
		      (? (match (alphabet "?*+")))))
	       parse-children)))

       (encapsulate-suffix
	(lambda (v)
	  (if (fix:= (vector-length v) 1)
	      (vector-ref v 0)
	      (list (xml-intern (vector-ref v 1))
		    (vector-ref v 0))))))

    (*parser
     (encapsulate
	 (lambda (v)
	   (make-xml-element-declaration (vector-ref v 0)
					 (vector-ref v 1)))
       (sbracket "element declaration" "<!ELEMENT" ">"
	 S
	 parse-name
	 S
	 ;;[46]
	 (alt (element-transform xml-intern (match (string "EMPTY")))
	      (element-transform xml-intern (match (string "ANY")))
	      ;;[51]
	      (encapsulate (lambda (v) (cons 'MIX (vector->list v)))
		(alt (sbracket "element-declaration type" "(" ")"
		       S?
		       (noise (string "#PCDATA"))
		       S?)
		     (sbracket "element-declaration type" "(" ")*"
		       S?
		       (noise (string "#PCDATA"))
		       (* (seq S?
			       (noise (string "|"))
			       S?
			       parse-name))
		       S?)))
	      parse-children))))))

(define parse-attlist-decl		;[52,53]
  (*parser
   (encapsulate
       (lambda (v)
	 (make-xml-attribute-declaration (vector-ref v 0)
					 (vector-ref v 1)))
     (sbracket "attribute-list declaration" "<!ATTLIST" ">"
       S
       parse-name
       (encapsulate vector->list
	 (* (encapsulate vector->list
	      (seq S
		   maybe-parse-name
		   S
		   ;;[54,57]
		   (alt (element-transform xml-intern
			  ;;[55,56]
			  (alt (match (string "CDATA"))
			       (match (string "IDREFS"))
			       (match (string "IDREF"))
			       (match (string "ID"))
			       (match (string "ENTITY"))
			       (match (string "ENTITIES"))
			       (match (string "NMTOKENS"))
			       (match (string "NMTOKEN"))))
			;;[58]
			(encapsulate
			    (lambda (v)
			      (cons 'NOTATION (vector->list v)))
			  (bracket "notation type"
			      (seq (noise (string "NOTATION"))
				   S
				   (noise (string "(")))
			      (noise (string ")"))
			    S?
			    parse-name
			    (* (seq S?
				    (noise (string "|"))
				    S?
				    parse-name))
			    S?))
			;;[59]
			(encapsulate
			    (lambda (v)
			      (cons 'ENUMERATED (vector->list v)))
			  (sbracket "enumerated type" "(" ")"
			    S?
			    parse-name-token
			    (* (seq S?
				    (noise (string "|"))
				    S?
				    parse-name-token))
			    S?)))
		   S
		   ;;[60]
		   (alt (element-transform xml-intern
			  (alt (match (string "#REQUIRED"))
			       (match (string "#IMPLIED"))))
			(encapsulate vector->list
			  (seq (element-transform xml-intern
				 (match (string "#FIXED")))
			       S
			       parse-attribute-value))
			(element-transform (lambda (v) (list 'DEFAULT v))
			  parse-attribute-value))))))
       S?))))

(define parse-conditional-section	;[61]
  (*parser
   (alt parse-include-section
	parse-ignore-section)))

(define-integrable conditional-start "<![")
(define-integrable conditional-end "]]>")

(define parse-include-section		;[62]
  (*parser
   (encapsulate
       (lambda (v)
	 (make-xml-include-section (vector->list v)))
     (bracket "include section"
	 (seq (noise (string conditional-start))
	      S?
	      (noise (string "INCLUDE"))
	      S?
	      (noise (string "[")))
	 (noise (string conditional-end))
       parse-external-subset-decl))))

(define parse-ignore-section		;[63]
  (*parser
   (encapsulate
       (lambda (v)
	 (make-xml-ignore-section (vector->list v)))
     (bracket "ignore section"
	 (seq (noise (string conditional-start))
	      S?
	      (noise (string "IGNORE"))
	      S?
	      (noise (string "[")))
	 (noise (string conditional-end))
       (* parse-ignore-section-contents)))))

(define parse-ignore-section-contents	;[64,65]
  (let ((parser
	 (terminated-region-parser "ignore section" char-set:xml-char
				   conditional-start conditional-end)))
    (*parser
     (seq parser
	  (* (seq (sbracket "ignore section" conditional-start conditional-end
		    parse-ignore-section-contents)
		  parser))))))

(define parse-entity-decl		;[70,71,72,73,74]
  (*parser
   (sbracket "entity declaration" "<!ENTITY" ">"
     (seq S
	  (alt (encapsulate
		   (lambda (v)
		     (make-xml-parameter-entity-declaration (vector-ref v 0)
							    (vector-ref v 1)))
		 (seq (noise (string "%"))
		      S
		      parse-name
		      S
		      (alt parse-entity-value
			   parse-external-id)))
	       (encapsulate
		   (lambda (v)
		     (make-xml-entity-declaration (vector-ref v 0)
						  (vector-ref v 1)))
		 (seq parse-name
		      S
		      (alt parse-entity-value
			   (seq parse-external-id
				(? parse-ndata-decl))))))
	  S?))))

(define parse-external-id		;[75]
  (*parser
   (alt (encapsulate
	    (lambda (v)
	      (make-xml-external-id #f (vector-ref v 0)))
	  (seq (noise (string "SYSTEM"))
	       S
	       parse-system-literal))
	(encapsulate
	    (lambda (v)
	      (make-xml-external-id (vector-ref v 0) (vector-ref v 1)))
	  (seq (noise (string "PUBLIC"))
	       S
	       parse-public-id-literal
	       S
	       parse-system-literal)))))

(define (string-parser description alphabet)
  (let ((a1 (char-set-difference alphabet (char-set #\")))
	(a2 (char-set-difference alphabet (char-set #\'))))
    (*parser
     (alt (sbracket description "\"" "\"" (match (* (alphabet a1))))
	  (sbracket description "'" "'" (match (* (alphabet a2))))))))

(define parse-public-id-literal		;[12,13]
  (string-parser
   "public-ID literal"
   (char-set-union char-set:alphanumeric
		   (string->char-set " \r\n-'()+,./:=?;!*#@$_%"))))

(define parse-system-literal		;[11]
  (string-parser "system literal" char-set:xml-char))

(define parse-ndata-decl		;[76]
  (*parser
   (seq S
	(noise (string "NDATA"))
	S
	parse-name)))

(define parse-notation-decl		;[82,83]
  (*parser
   (sbracket "notation declaration" "<!NOTATION" ">"
     S
     parse-name
     S
     (alt parse-external-id
	  (encapsulate
	      (lambda (v)
		(make-xml-external-id (vector-ref v 0) #f))
	    (seq (noise (string "PUBLIC"))
		 S
		 parse-public-id-literal)))
     S?)))

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'encapsulate 1)
;;; Eval: (scheme-indent-method 'transform 1)
;;; Eval: (scheme-indent-method 'element-transform 1)
;;; Eval: (scheme-indent-method 'require-success 1)
;;; Eval: (scheme-indent-method 'bracket 3)
;;; Eval: (scheme-indent-method 'sbracket 3)
;;; End:
