#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Generate the microcode makefile fragment and the C back end rules.

(declare (usual-integrations))

(load-option 'REGULAR-EXPRESSION)
(load-option 'SYNCHRONOUS-SUBPROCESS)
(load (merge-pathnames "../../etc/utilities"
		       (directory-pathname (current-load-pathname))))

(define (generate-fragments)
  (generate-sources-am)
  (generate-liarc-am))

;;;; The microcode's Makefile.am includes makegen/sources.am, and for the
;;;; C back end makegen/liarc.am.  automake derives the object list and
;;;; the header dependencies itself, so the source lists are all that is
;;;; left to generate.  No timestamp goes in the output: it is under
;;;; version control, and a header that changed on every run would show
;;;; up as a diff each time.

(define (write-generated-header output)
  (write-string
   "## Generated from makegen/*.scm by makegen/makegen.scm." output)
  (newline output)
  (write-string "## Do not edit." output)
  (newline output))

(define microcode-source-groups
  '(("files-core" . "MICROCODE_CORE_SOURCES")
    ("files-os-prim" . "MICROCODE_OS_PRIM_SOURCES")
    ("files-unix" . "MICROCODE_UNIX_SOURCES")))

(define (generate-sources-am)
  (call-with-output-file "makegen/sources.am"
    (lambda (output)
      (write-generated-header output)
      (for-each (lambda (group)
                  (newline output)
                  (write-macro output
                               (cdr group)
                               (files+suffix
                                (read-file
                                 (string-append "makegen/" (car group) ".scm"))
                                ".c")))
                microcode-source-groups))))


(define (generate-liarc-am)
  (call-with-output-file "makegen/liarc.am"
    (lambda (output)
      (write-generated-header output)
      (newline output)
      (write-macro output
		   "LIARC_HEAD_FILES"
		   (cddr (generate-rule "liarc-gendeps.c")))
      (newline output)
      (let ((files (liarc-static-files)))
	(write-macro output "LIARC_C_FILES" (files+suffix files ".c"))
	(newline output)
	(write-macro output "LIARC_OBJECTS" (files+suffix files ".o"))
	(newline output))
      (write-macro output
		   "LIARC_BOOT_BUNDLES"
		   (files+suffix '("sf" "compiler" "star-parser" "cref") ".so"))
      (let ((bundles (liarc-bundles)))
	(write-macro output
		     "LIARC_BUNDLES"
		     (bundles+suffix bundles ".so"))
	(write-macro output
		     "LIARC_BUNDLE_CLEAN_FILES"
		     (cons "$(LIARC_BUNDLES)"
			   (append (bundles+suffix bundles "-init.h")
				   (bundles+suffix bundles "-init.c")
				   (bundles+suffix bundles "-init.o"))))))))

(define (bundles+suffix bundles suffix)
  (files+suffix (map car bundles) suffix))

(define (liarc-static-files)
  (append (append-map package-description-files
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
			   (remove (lambda (path)
				     (member (pathname-name path) (cdr spec)))
				   (directory-read
				    (merge-pathnames "*.scm" dir)))
			   (begin
			     (warn "Can't read directory:" dir)
			     '()))))
		   specs)))

(define os-pkd-suffixes '("unx"))

(define (package-description-files descriptor)
  (receive (filename suffixes)
      (if (pair? descriptor)
	  (values (car descriptor) (cdr descriptor))
	  (values descriptor os-pkd-suffixes))
    (map (lambda (suffix)
	   (string-append filename "-" suffix))
	 suffixes)))

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
	   (sort (remove pathname-absolute? (cddr items))
		 string<?))))