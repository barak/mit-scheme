-*-Scheme-*-

$Id: parser-macro.scm,v 1.6 2003/01/26 06:33:09 cph Exp $

Copyright 2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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
			  (string-append "Unterminated " description)
			  `(STRING-APPEND "Unterminated "
					  ,description))))))))))

(define-*parser-macro (sbracket description open close . body)
  `(BRACKET ,description (NOISE (STRING ,open)) (NOISE (STRING ,close))
     ,@body))

(define-*parser-macro (require-success message body)
  `(ALT ,body (SEXP (LAMBDA (BUFFER) (PERROR BUFFER ,message)))))