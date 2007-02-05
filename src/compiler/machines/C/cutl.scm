#| -*-Scheme-*-

$Id: cutl.scm,v 1.10 2007/01/21 23:19:54 riastradh Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; C back-end utilities
;;; package: (compiler)

(declare (usual-integrations))

(define (back-end:+ x y)
  (cond ((and (number? x) (number? y)) (+ x y))
	((and (number? y) (= y 0)) x)
	((and (number? x) (= x 0)) y)
	(else (c:+ x y))))

(define (back-end:- x y)
  (cond ((and (number? x) (number? y)) (- x y))
	((and (number? y) (= y 0)) x)
	((equal? x y) 0)
	(else (c:- x y))))

(define (back-end:* x y)
  (cond ((and (number? x) (number? y)) (* x y))
	((and (number? y) (= y 1)) x)
	((and (number? y) (= y 0)) 0)
	((and (number? x) (= x 1)) y)
	((and (number? x) (= x 0)) 0)
	(else (c:* x y))))

(define (back-end:quotient x y)
  (cond ((and (number? x) (number? y)) (quotient x y))
	((and (number? y) (= y 1)) x)
	((and (number? x) (= x 0)) 0)
	((equal? x y) 1)
	(else (c:/ x y))))

(define (back-end:expt x y)
  (cond ((and (number? x) (number? y)) (expt x y))
	((and (number? x) (or (= x 0) (= x 1))) x)
	((and (number? y) (= y 0)) 1)
	((and (number? y) (= y 1)) x)
	((and (number? x) (= x 2)) (c:<< 1 y))
	(else (error "back-end:expt: Cannot exponentiate:" x y))))

(define (back-end:= x y)
  (cond ((and (number? x) (number? y)) (= x y))
	(else (equal? x y))))

(define (back-end:< x y)
  ;; This is a lie, but it is used only in places where #f is the
  ;; correct default.
  (cond ((and (number? x) (number? y)) (< x y))
	(else #f)))

;;;; Output abstraction

(define-record-type <c:line>
    (c:%make-line indentation text)
    c:line?
  (indentation c:line-indentation)
  (text c:line-text))

(define-guarantee c:line "C line")

(define (c:line . items)
  (c:%make-line 0 (apply string-append items)))

(define (c:line-items items)
  (if (pair? items)
      (if (pair? (cdr items))
	  (apply string-append (map c:line-item items))
	  (c:line-item (car items)))
      ""))

(define (c:line-item item)
  (cond ((string? item) item)
	((char? item) (string item))
	((symbol? item) (symbol-name item))
	((number? item) (number->string item))
	((decoded-time? item) (decoded-time->iso8601-string item))
	((not item) "false")
	((eq? item #t) "true")
	(else (error:wrong-type-argument item "C line item" 'C:LINE-ITEM))))

(define (c:make-line indentation text)
  (c:%make-line (if (or (string-null? text)
			(string-prefix? "#" text)
			(string-prefix? "\f" text))
		    0
		    indentation)
		text))

(define (c:write-line line port)
  (let ((qr
	 (integer-divide (* (max 0 (c:line-indentation line))
			    c:indentation-delta)
			 c:indentation-tab-width)))
    (let ((n (integer-divide-quotient qr)))
      (do ((i 0 (+ i 1)))
	  ((not (< i n)))
	(write-char #\tab port)))
    (let ((n (integer-divide-remainder qr)))
      (do ((i 0 (+ i 1)))
	  ((not (< i n)))
	(write-char #\space port))))
  (write-string (c:line-text line) port)
  (newline port))

(define c:indentation-delta 2)
(define c:indentation-tab-width 8)

(define (c:blank-line? line)
  (string-null? (c:line-text line)))

(define-record-type <c:group>
    (c:%make-group lines)
    c:group?
  (lines c:group-lines))

(define-guarantee c:group "C group")

(define (c:group . items)
  (c:group* items))

(define (c:group* items)
  (if (and (pair? items)
	   (c:group? (car items))
	   (null? (cdr items)))
      (car items)
      (c:make-group
       (append-map (lambda (item)
		     (cond ((c:line? item) (list item))
			   ((c:group? item) (c:group-lines item))
			   ((not item) '())
			   (else (error:not-c:line item 'C:GROUP*))))
		   items))))

(define c:make-group
  (let ((empty (c:%make-group '())))
    (lambda (lines)
      (if (null? lines)
	  empty
	  (c:%make-group lines)))))

(define (c:group-length group)
  (length (c:group-lines group)))

(define (c:indent . items) (c:indent* items))
(define (c:exdent . items) (c:exdent* items))

(define (c:indent* items) (c:%indent (c:group* items) 1))
(define (c:exdent* items) (c:%indent (c:group* items) -1))

(define (c:%indent item delta)
  (let ((indent-line
	 (lambda (line)
	   (c:make-line (+ (c:line-indentation line) delta)
			(c:line-text line)))))
    (cond ((c:line? item)
	   (indent-line item))
	  ((c:group? item)
	   (c:make-group (map indent-line (c:group-lines item))))
	  (else
	   (error:not-c:line item 'C:%INDENT)))))

(define (c:write-group group port)
  (cond ((c:line? group) (c:write-line group port))
	((c:group? group)
	 (let loop ((lines (c:group-lines group)) (prev #f))
	   (if (pair? lines)
	       (let ((line (car lines))
		     (lines (cdr lines)))
		 (if (and (c:label-line? line)
			  (not (and prev
				    (or (c:label-line? prev)
					(c:blank-line? prev)))))
		     (newline port))
		 (c:write-line line port)
		 (loop lines line)))))
	(else (error:not-c:group group 'C:WRITE-GROUP))))

(define (c:label-line? line)
  (or (string-prefix? "DEFLABEL " (c:line-text line))
      (string-prefix? "INVOKE_INTERFACE_TARGET_" (c:line-text line))
      (string=? "INVOKE_PRIMITIVE_TARGET" (c:line-text line))))

(define (c:comment . content)
  (string-append "/* " (c:preserve-comment (c:line-items content)) " */"))

(define (c:preserve-comment comment)
  (cond ((string-search-forward "*/" comment)
         => (lambda (index)
              (call-with-output-string
                (lambda (port)
                  (let ((end (string-length comment)))
                    (let loop ((start 0) (index index))
                      (write-substring comment start index port)
                      (write-string "*\\/" port)
                      (let ((index (+ index 2)))
                        (cond ((substring-search-forward "*/" comment index end)
                               => (lambda (index*) (loop index index*)))
                              (else
                               (write-substring comment index end port))))))))))
        (else comment)))

(define (c:string . content)
  (string-append "\"" (c:line-items content) "\""))

(define (c:parens . content)
  (string-append "(" (c:line-items content) ")"))

(define (c:struct-init . exprs)
  (string-append "{ " (c:comma-list exprs) " }"))

(define (c:comma-list exprs)
  (decorated-string-append "" ", " "" (map c:line-item exprs)))

(define (c:hex n)
  (string-append "0x" (number->string n 16)))

(define (c:page)
  (c:line "\f"))

(define (c:brace-group . items)
  (c:brace-group* items))

(define (c:brace-group* items)
  (c:group (c:line "{")
	   (c:indent* items)
	   (c:line "}")))

(define (c:code-section . items) (apply c:ifndef "WANT_ONLY_DATA" items))
(define (c:data-section . items) (apply c:ifndef "WANT_ONLY_CODE" items))

(define (c:ifndef symbol . items)
  (c:group (c:line "#ifndef " (c:var symbol))
	   (c:line)
	   (c:group* items)
	   (c:line)
	   (c:line "#endif " (c:comment "!" symbol))))

(define (c:include name)
  (c:line "#include "
	  (if (and (string-prefix? "<" name)
		   (string-suffix? ">" name))
	      name
	      (c:string name))))

(define (c:define symbol val)
  (c:line "#define " (c:var symbol) " " (c:expr val)))

(define (c:fn static? rtype name adecls . items)
  (c:group (c:line (if static? "static " "")
		   (c:type rtype))
	   (c:line name
		   " "
		   (if (null? adecls)
		       "(void)"
		       (c:parens
			(c:comma-list (map (lambda (p)
					     (string-append (c:type (car p))
							    " "
							    (c:var (cdr p))))
					   adecls)))))
	   (c:brace-group* items)))

(define (c:=  var val) (c:line (c:expr var)  " = " (c:expr val) ";"))
(define (c:+= var val) (c:line (c:expr var) " += " (c:expr val) ";"))
(define (c:-= var val) (c:line (c:expr var) " -= " (c:expr val) ";"))
(define (c:*= var val) (c:line (c:expr var) " *= " (c:expr val) ";"))
(define (c:/= var val) (c:line (c:expr var) " /= " (c:expr val) ";"))

(define (c:goto label)
  (c:line "goto " (c:var label) ";"))

(define (c:label label)
  (c:exdent (c:line (c:call "DEFLABEL" label))))

(define (c:return expr)
  (c:line "return " (c:pexpr expr) ";"))

(define (c:scall function . args)
  (c:line (apply c:call function args) ";"))

(define (c:ecall function . args)
  (c:parens (apply c:call function args)))

(define (c:call function . args)
  (string-append (c:expr function)
		 " "
		 (let ((args (map c:expr args)))
		   (if (and (pair? args)
			    (null? (cdr args))
			    (c:%parenthesized? (car args)))
		       (car args)
		       (c:parens (c:comma-list args))))))

(define (c:switch expr . cases)
  (c:group (c:line "switch " (c:pexpr expr))
	   (c:indent (c:brace-group* cases))))

(define (c:case tag . items)
  (c:group (c:exdent
	    (c:line (if tag
			(string-append "case " (c:line-item tag))
			"default")
		    ":"))
	   (c:group* items)))

(define (c:if-goto pred label)
  (c:group (c:line "if " (c:pexpr pred))
	   (c:indent (c:goto label))))

(define (c:while expr . body)
  (c:group (c:line "while " (c:pexpr expr))
	   (c:indent (c:brace-group* body))))

(define (c:cast type expr)
  (let ((type (c:type type))
	(expr (c:expr expr)))
    (let ((p
	   (and (c:%decimal? expr)
		(assoc type c:decimal-suffixes))))
      (if p
	  (string-append expr (cdr p))
	  (string-append "((" type ") " expr ")")))))

(define c:decimal-suffixes
  '(("long" . "L")
    ("unsigned" . "U")
    ("unsigned long" . "UL")))

(define (c:%decimal? e)
  (let ((n (string-length e)))
    (let loop
	((i
	  (if (or (string-prefix? "-" e)
		  (string-prefix? "+" e))
	      1
	      0)))
      (if (fix:< i n)
	  (and (char-set-member? c:decimal-chars (string-ref e i))
	       (loop (fix:+ i 1)))
	  #t))))

(define c:decimal-chars
  (ascii-range->char-set (char->integer #\0)
			 (+ (char->integer #\9) 1)))

(define (c:type type)
  (or (and (symbol? type)
	   (let ((p (assq type type-abbrevs)))
	     (and p
		  (cdr p))))
      (c:line-item type)))

(define type-abbrevs
  (let ((types
	 (let ((types '(char short int long float double)))
	   `(,@(map (lambda (t)
		      (cons t (symbol-name t)))
		    types)
	     ,@(map (lambda (t)
		      (cons (symbol 'u t)
			    (string-append "unsigned " (symbol-name t))))
		    types)
	     (sobj . "SCHEME_OBJECT")))))
    `(,@types
      ,@(map (lambda (p)
	       (cons (symbol (car p) '*)
		     (string-append (cdr p) " *")))
	     types))))

(define (c:decl type var #!optional val)
  (c:line (c:type type) " " (c:var var)
	  (if (default-object? val) "" (string-append " = " (c:expr val)))
	  ";"))

(define (c:var item)
  (cond ((string? item) item)
	((symbol? item) (symbol-name item))
	(else (error:wrong-type-argument item "C variable" 'C:VAR))))

(define (c:array-decl type name dim items)
  (let ((lines (list-copy items)))
    (if (pair? lines)
	(let loop ((lines lines))
	  (if (pair? (cdr lines))
	      (begin
		(set-car! lines (c:line (c:line-item (car lines)) ","))
		(loop (cdr lines)))
	      (set-car! lines (c:line (c:line-item (car lines)))))))
    (c:group (c:line (c:type type) " " (c:var name) " [" (c:expr dim) "] =")
	     (c:indent (c:group (c:line "{")
				(c:indent (c:group* lines))
				(c:line "};"))))))

(define (c:expr expr)
  (let ((expr (c:line-item expr)))
    (if (or (c:%identifier? expr)
	    (string->number expr)
	    (c:%parenthesized? expr)
	    (and (string-prefix? "\"" expr)
		 (string-suffix? "\"" expr)))
	expr
	(string-append "(" expr ")"))))

(define (c:pexpr expr)
  (let ((expr (c:line-item expr)))
    (if (c:%parenthesized? expr)
	expr
	(string-append "(" expr ")"))))

(define (c:%identifier? e)
  (let ((n (string-length e)))
    (let loop ((i 0))
      (if (fix:< i n)
	  (and (char-set-member? c:identifier-chars (string-ref e i))
	       (loop (fix:+ i 1)))
	  #t))))

(define c:identifier-chars
  (char-set-union (ascii-range->char-set (char->integer #\A)
					 (+ (char->integer #\Z) 1))
		  (ascii-range->char-set (char->integer #\a)
					 (+ (char->integer #\z) 1))
		  (ascii-range->char-set (char->integer #\0)
					 (+ (char->integer #\9) 1))
		  (char-set #\_)))

(define (c:%parenthesized? e)
  (and (string-prefix? "(" e)
       (string-suffix? ")" e)))

(define (c:predec expr)
  (string-append "--" (c:expr expr)))

(define (c:preinc expr)
  (string-append "++" (c:expr expr)))

(define (c:postdec expr)
  (string-append (c:expr expr) "--"))

(define (c:postinc expr)
  (string-append (c:expr expr) "++"))

(define (c:aref array index)
  (string-append "(" (c:expr array) " [" (c:expr index) "])"))

(define (c:aptr array index)
  (c:& (c:aref array index)))

(define (c:?: a b c . rest)
  (apply string-append
	 "("
	 (c:expr a)
	 " ? "
	 (c:expr b)
	 " : "
	 (c:expr c)
	 (let loop ((exprs rest))
	   (if (pair? exprs)
	       (begin
		 (if (not (pair? (cdr exprs)))
		     (error "C:?: requires even number of args."))
		 (cons* " ? "
			(c:expr (car exprs))
			" : "
			(c:expr (cadr exprs))
			(loop (cddr exprs))))
	       (list ")")))))

(define (c:unary op a)
  (string-append "(" (c:line-item op) " " (c:expr a) ")"))

(define (c:! a)
  (c:unary "!" a))

(define (c:~ a)
  (c:unary "~" a))

(define (c:binary-infix op a b)
  (string-append "(" (c:expr a) " " (c:line-item op) " " (c:expr b) ")"))

(define (c:== a b)
  (c:binary-infix "==" a b))

(define (c:!= a b)
  (c:binary-infix "==" a b))

(define (c:> a b)
  (c:binary-infix ">" a b))

(define (c:>= a b)
  (c:binary-infix ">=" a b))

(define (c:< a b)
  (c:binary-infix "<" a b))

(define (c:<= a b)
  (c:binary-infix "<=" a b))

(define (c:\| a b)
  (c:binary-infix "|" a b))

(define (c:^ a b)
  (c:binary-infix "^" a b))

(define (c:&~ a b)
  (c:binary-infix "&~" a b))

(define (c:/ a b)
  (c:binary-infix "/" a b))

(define (c:<< a b)
  (c:binary-infix "<<" a b))

(define (c:>> a b)
  (c:binary-infix ">>" a b))

(define (c:ubinary op a b)
  (if (default-object? b)
      (c:unary op a)
      (c:binary-infix op a b)))

(define (c:& a #!optional b)
  (c:ubinary "&" a b))

(define (c:* a #!optional b)
  (c:ubinary "*" a b))

(define (c:+ a #!optional b)
  (c:ubinary "+" a b))

(define (c:- a #!optional b)
  (c:ubinary "-" a b))

(define (c:make-object type datum)
  (c:ecall "MAKE_OBJECT" type datum))

(define (c:make-pointer-object type address)
  (c:ecall "MAKE_POINTER_OBJECT" type address))

(define (c:object-type expr)
  (c:ecall "OBJECT_TYPE" expr))

(define (c:object-datum expr)
  (c:ecall "OBJECT_DATUM" expr))

(define (c:object-address expr)
  (c:ecall "OBJECT_ADDRESS" expr))