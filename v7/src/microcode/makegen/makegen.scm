#| -*-Scheme-*-

$Id: makegen.scm,v 1.5 2003/02/13 19:56:29 cph Exp $

Copyright 2000,2001,2003 Massachusetts Institute of Technology

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

;;;; Generate "Makefile.in" from template.

(declare (usual-integrations))

(load-option 'REGULAR-EXPRESSION)
(load-option 'SYNCHRONOUS-SUBPROCESS)

(define (generate-makefile template deps-filename makefile)
  (let ((file-lists
	 (map (lambda (pathname)
		(cons (pathname-name pathname)
		      (read-file pathname)))
	      (list-transform-positive (directory-read "makegen/")
		(lambda (pathname)
		  (re-string-match "^files-.+\\.scm$"
				   (file-namestring pathname)))))))
    (call-with-input-file template
      (lambda (input)
	(call-with-output-file makefile
	  (lambda (output)
	    (write-string "# This file automatically generated from " output)
	    (write-string (file-namestring template) output)
	    (newline output)
	    (write-string "# on " output)
	    (write-string (universal-time->string (get-universal-time)) output)
	    (write-string "." output)
	    (newline output)
	    (newline output)
	    (let loop ((column 0))
	      (let ((char (read-char input)))
		(if (not (eof-object? char))
		    (if (and (char=? #\@ char)
			     (eqv? #\( (peek-char input)))
			(let ((command (read input)))
			  (if (eqv? #\@ (peek-char input))
			      (read-char input)
			      (error "Missing @ at end of command:" command))
			  (loop (interpret-command command column
						   file-lists deps-filename
						   output)))
			(begin
			  (write-char char output)
			  (loop
			   (if (char=? #\newline char)
			       0
			       (+ column 1))))))))))))))

(define (interpret-command command column file-lists deps-filename output)
  (let ((malformed (lambda () (error "Malformed command:" command))))
    (if (not (and (pair? command)
		  (symbol? (car command))
		  (list? (cdr command))))
	(malformed))
    (let ((guarantee-nargs
	   (lambda (n)
	     (if (not (= n (length (cdr command))))
		 (malformed)))))
      (let ((write-suffixed
	     (lambda (suffix)
	       (guarantee-nargs 1)
	       (let ((entry (assoc (cadr command) file-lists)))
		 (if (not entry)
		     (malformed))
		 (write-items (map (lambda (file) (string-append file suffix))
				   (cdr entry))
			      column
			      output)
		 0))))
      (case (car command)
	((WRITE-SOURCES)
	 (write-suffixed ".c"))
	((WRITE-OBJECTS)
	 (write-suffixed ".o"))
	((WRITE-DEPENDENCIES)
	 (guarantee-nargs 0)
	 (write-dependencies file-lists deps-filename output))
	(else
	 (error "Unknown command:" command)))))))

(define (write-dependencies file-lists deps-filename output)
  (maybe-update-dependencies
   deps-filename
   (sort (append-map (lambda (file-list)
		       (map (lambda (base) (string-append base ".c"))
			    (cdr file-list)))
		     file-lists)
     string<?))
  (call-with-input-file deps-filename
    (lambda (input)
      (let ((buffer (make-string 4096)))
	(let loop ()
	  (let ((n (read-substring! buffer 0 4096 input)))
	    (if (> n 0)
		(begin
		  (write-substring buffer 0 n output)
		  (loop)))))))))

(define (maybe-update-dependencies deps-filename source-files)
  (if (let ((mtime (file-modification-time deps-filename)))
	(or (not mtime)
	    (there-exists? source-files
	      (lambda (source-file)
		(> (file-modification-time source-file) mtime)))))
      (let ((rules (map generate-rule source-files)))
	(call-with-output-file deps-filename
	  (lambda (output)
	    (let loop ((rules rules))
	      (if (pair? rules)
		  (begin
		    (write-rule (car rules) output)
		    (if (pair? (cdr rules))
			(begin
			  (newline output)
			  (loop (cdr rules))))))))))))

(define (generate-rule filename)
  (parse-rule
   (unbreak-lines
    (call-with-output-string
     (lambda (port)
       (run-shell-command (string-append "gcc -M -DMIT_SCHEME " filename)
			  'OUTPUT port))))))

(define (unbreak-lines string)
  (let ((indexes (string-search-all "\\\n" string)))
    (let ((n (length indexes))
	  (end (string-length string)))
      (let ((result (make-string (- end (* 2 n)))))
	(let loop ((start 0) (indexes indexes) (rstart 0))
	  (if (pair? indexes)
	      (begin
		(substring-move! string start (car indexes) result rstart)
		(loop (+ (car indexes) 2)
		      (cdr indexes)
		      (+ rstart (- (car indexes) start))))
	      (substring-move! string start end result rstart)))
	result))))

(define (parse-rule rule)
  (let ((items (burst-string rule char-set:whitespace #t)))
    (if (not (string-suffix? ":" (car items)))
	(error "Missing rule target:" rule))
    (cons* (string-head (car items) (- (string-length (car items)) 1))
	   (cadr items)
	   (sort (list-transform-negative (cddr items) pathname-absolute?)
	     string<?))))

(define (write-rule rule port)
  (write-string (car rule) port)
  (write-string ": " port)
  (write-items (cdr rule) (+ (string-length (car rule)) 2) port))

(define (write-items items start-column port)
  (let loop ((items* items) (column start-column))
    (if (pair? items*)
	(let ((column
	       (if (eq? items* items)
		   column
		   (begin
		     (write-string " " port)
		     (+ column 1))))
	      (delta (string-length (car items*))))
	  (let ((new-column (+ column delta)))
	    (if (>= new-column 78)
		(begin
		  (write-string "\\\n\t" port)
		  (write-string (car items*) port)
		  (loop (cdr items*) (+ 8 delta)))
		(begin
		  (write-string (car items*) port)
		  (loop (cdr items*) new-column)))))
	column)))