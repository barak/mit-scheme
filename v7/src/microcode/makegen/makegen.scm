#| -*-Scheme-*-

$Id: makegen.scm,v 1.23 2007/05/06 00:01:01 cph Exp $

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

;;;; Generate "Makefile.in" from template.

(declare (usual-integrations))

(load-option 'REGULAR-EXPRESSION)
(load-option 'SYNCHRONOUS-SUBPROCESS)

(define (generate-makefile)
  (generate-liarc-files)
  (let ((file-lists
	 (map (lambda (pathname)
		(cons (pathname-name pathname)
		      (read-file pathname)))
	      (keep-matching-items (directory-read "makegen/")
		(lambda (pathname)
		  (re-string-match "^files-.+\\.scm$"
				   (file-namestring pathname)))))))
    (call-with-input-file "makegen/Makefile.in.in"
      (lambda (input)
	(call-with-output-file "Makefile.in"
	  (lambda (output)
	    (write-header output)
	    (let loop ()
	      (let ((char (read-char input)))
		(if (not (eof-object? char))
		    (if (and (char=? #\@ char)
			     (eqv? #\( (peek-char input)))
			(let ((command (read input)))
			  (if (eqv? #\@ (peek-char input))
			      (read-char input)
			      (error "Missing @ at end of command:" command))
			  (interpret-command command file-lists output)
			  (loop))
			(begin
			  (write-char char output)
			  (loop))))))))))))

(define (write-header output)
  (write-string "# This file automatically generated at " output)
  (write-string (universal-time->local-iso8601-string (get-universal-time))
		output)
  (write-string "." output)
  (newline output)
  (newline output))

(define (generate-liarc-files)
  (generate-liarc-variables)
  (generate-liarc-rules))

(define (generate-liarc-variables)
  (call-with-output-file "liarc-vars"
    (lambda (output)
      (write-header output)
      (write-rule "LIARC_HEAD_FILES"
		  "="
		  (cddr (generate-rule "liarc-gendeps.c"))
		  output)
      (newline output)
      (let ((files (liarc-static-files)))
	(write-rule "LIARC_SOURCES" "=" (files+suffix files ".c") output)
	(newline output)
	(write-rule "LIARC_OBJECTS" "=" (files+suffix files ".o") output)
	(newline output))
      (write-rule "LIARC_BOOT_BUNDLES"
		  "="
		  (files+suffix '("sf" "compiler" "star-parser" "cref") ".so")
		  output)
      (let ((bundles (liarc-bundles)))
	(write-rule "LIARC_BUNDLES"
		    "="
		    (bundles+suffix bundles ".so")
		    output)
	(write-rule "LIARC_BUNDLE_CLEAN_FILES"
		    "="
		    (cons "$(LIARC_BUNDLES)"
			  (append (bundles+suffix bundles "-init.h")
				  (bundles+suffix bundles "-init.c")
				  (bundles+suffix bundles "-init.o")))
		    output)))))

(define (generate-liarc-rules)
  (call-with-output-file "liarc-rules"
    (lambda (output)
      (write-header output)
      (call-with-input-file "makegen/liarc-base-rules"
	(lambda (input)
	  (let loop ()
	    (let ((char (read-char input)))
	      (if (not (eof-object? char))
		  (begin (write-char char output)
			 (loop)))))))
      (for-each (lambda (bundle)
		  (newline output)
		  (let ((files
			 (append (append-map package-description-files
					     (cadr bundle))
				 (enumerate-directories (cddr bundle))))
			(init-root (string-append (car bundle) "-init")))
		    (write-rule (string-append (car bundle) ".so")
				":"
				(files+suffix files ".o")
				output)
		    (write-command output
				   "../etc/c-bundle.sh"
				   "library"
				   init-root
				   (files+suffix files ".c"))
		    (write-command output
				   "$(COMPILE_MODULE)"
				   "-c"
				   (string-append init-root ".c"))
		    (write-command output
				   "$(LINK_MODULE)"
				   (string-append init-root ".o")
				   "$^")
		    (write-command output
				   "rm"
				   "-f"
				   (map (lambda (suffix)
					  (string-append init-root suffix))
					'(".h" ".c" ".o")))))
		(liarc-bundles)))))

(define (write-command port program . args)
  (write-char #\tab port)
  (write-string program port)
  (write-items (flatten-items args) port)
  (newline port))

(define (flatten-items items)
  (append-map (lambda (item)
		(if (list? item)
		    (flatten-items item)
		    (list item)))
	      items))

(define (liarc-static-files)
  (append '("utabmd")
	  (append-map package-description-files
		      (read-file "makegen/pkds-liarc.scm"))
	  (enumerate-directories (read-file "makegen/dirs-liarc.scm"))))

(define (liarc-bundles)
  (read-file "makegen/bundles-liarc.scm"))

(define (enumerate-directories specs)
  (map (lambda (path)
	 (enough-namestring (pathname-new-type path #f)))
       (append-map (lambda (spec)
		     (let ((dir (pathname-as-directory (car spec))))
		       (if (file-directory? dir)
			   (delete-matching-items
			       (directory-read (merge-pathnames "*.scm" dir))
			     (lambda (path)
			       (member (pathname-name path) (cdr spec))))
			   (begin
			     (warn "Can't read directory:" dir)
			     '()))))
		   specs)))

(define os-pkd-suffixes '("unx" "w32" "os2"))

(define (package-description-files descriptor)
  (receive (filename suffixes)
      (if (pair? descriptor)
	  (values (car descriptor) (cdr descriptor))
	  (values descriptor os-pkd-suffixes))
    (map (lambda (suffix)
	   (string-append filename "-" suffix))
	 suffixes)))

(define (interpret-command command file-lists output)
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
		 (let ((files (files+suffix (cdr entry) suffix)))
		   (if (pair? files)
		       (begin
			 (write-string (car files) output)
			 (write-items (cdr files) output))))))))
      (case (car command)
	((WRITE-SOURCES)
	 (write-suffixed ".c"))
	((WRITE-OBJECTS)
	 (write-suffixed ".o"))
	((WRITE-DEPENDENCIES)
	 (guarantee-nargs 0)
	 (write-dependencies file-lists "Makefile.deps" output))
	(else
	 (error "Unknown command:" command)))))))

(define (files+suffix files suffix)
  (map (lambda (file)
	 (string-append file suffix))
       files))

(define (bundles+suffix bundles suffix)
  (files+suffix (map car bundles) suffix))

(define (write-rule lhs op rhs port)
  (write-string lhs port)
  (write-string " " port)
  (write-string op port)
  (write-items rhs port)
  (newline port))

(define (write-items items port)
  (for-each (lambda (item)
	      (write-string " " port)
	      (write-item item port))
	    items))

(define (write-item item port)
  (if (>= (+ (output-port/column port)
	     (string-length item))
	  78)
      (write-string "\\\n\t  " port))
  (write-string item port))

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
	    (for-each (lambda (rule)
			(write-rule (car rule) ":" (cdr rule) output))
		      rules))))))

(define (generate-rule filename)
  (parse-rule
   (unbreak-lines
    (with-string-output-port
     (lambda (port)
       (run-shell-command (string-append "./makegen-cc " filename)
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