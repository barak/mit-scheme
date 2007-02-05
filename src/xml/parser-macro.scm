#| -*-Scheme-*-

$Id: parser-macro.scm,v 1.11 2007/01/05 21:19:29 cph Exp $

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

;;;; XML parser macros

(declare (usual-integrations))

(define-*matcher-macro S `(+ (CHAR-SET CHAR-SET:XML-WHITESPACE)))
(define-*parser-macro S `(NOISE S))

(define-*matcher-macro S? `(* (CHAR-SET CHAR-SET:XML-WHITESPACE)))
(define-*parser-macro S? `(NOISE S?))

(define-*parser-macro (bracket description open close . body)
  (let ((v (generate-uninterned-symbol)))
    `(WITH-POINTER ,v
       (SEQ ,open
	    ,@body
	    (ALT ,close
		 (SEXP
		  (LAMBDA (BUFFER)
		    BUFFER
		    (PERROR
		     ,v
		     ,(if (string? description)
			  (string-append "Malformed " description)
			  `(STRING-APPEND "Malformed " ,description))))))))))

(define-*parser-macro (sbracket description open close . body)
  `(BRACKET ,description (NOISE (STRING ,open)) (NOISE (STRING ,close))
     ,@body))

(define-*parser-macro (require-success message body)
  `(ALT ,body (SEXP (LAMBDA (BUFFER) (PERROR BUFFER ,message)))))