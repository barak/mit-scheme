#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Build Utilities
;;; package: (ffi build)

(define (add-plugin name project infodir scmlibdir scmdocdir)
  ;; For plugin postinst scripts: register.
  (update-plugin 'add name project infodir scmlibdir scmdocdir))

(define (remove-plugin name project infodir scmlibdir scmdocdir)
  ;; For plugin prerm scripts: de-register.
  (update-plugin 'remove name project infodir scmlibdir scmdocdir))

(define (update-plugin operation name project infodir scmlibdir scmdocdir)
  (let ((infodir (normal-dirname infodir))
	(scmlibdir (normal-dirname scmlibdir))
	(scmdocdir (normal-dirname scmdocdir)))
    (if scmlibdir
	(update-plugin-lib operation name project scmlibdir))
    (if scmdocdir
	(update-plugin-doc operation name project infodir scmdocdir))))

(define (delete-plugin-list)
  ;; For the prerm script: delete the database of plugins (plugins.scm
  ;; file in the system library directory).
  (let ((filename
	 (merge-pathnames "plugins.scm" (system-library-directory-pathname))))
    (if (file-exists? filename)
	(delete-file filename))))

(define (update-plugin-indices project infodir scmlibdir scmdocdir)
  ;; For the postinst script: re-initialize the optiondb, Info and
  ;; HTML indices using the list of currently installed plugins.  (The
  ;; indices are presumed clobbered by the core upgrade.)
  (let ((infodir (normal-dirname infodir))
	(scmlibdir (normal-dirname scmlibdir))
	(scmdocdir (normal-dirname scmdocdir)))
    (if scmlibdir
	(update-optiondb (read-plugins-file scmlibdir)
			 scmlibdir))
    (if scmdocdir
	(let ((plugins (read-plugins-file scmdocdir)))
	  (update-info-index project plugins infodir scmdocdir)
	  (update-html-index project plugins scmdocdir)))))

(define (normal-dirname dirname)
  (and dirname
       (not (string-null? dirname))
       (->namestring (pathname-as-directory dirname))))

(define (update-plugin-lib operation name project scmlibdir)
  (declare (ignore project))
  (if (file-exists? (merge-pathnames "optiondb.scm" scmlibdir))
      ;; NOT in dpkg-buildpackage's chroot
      (update-optiondb (update-plugins-file operation name scmlibdir)
		       scmlibdir)))

(define (update-plugin-doc operation name project infodir scmdocdir)
  (let ((plugins (update-plugins-file operation name scmdocdir)))
    (update-info-index project plugins infodir scmdocdir)
    (update-html-index project plugins scmdocdir)))

(define (read-plugins-file dir)
  (let ((filename (merge-pathnames "plugins.scm" dir)))
    (if (file-exists? filename)
	(call-with-input-file filename read)
	'())))

(define (update-plugins-file operation name dir)
  (let ((filename (merge-pathnames "plugins.scm" dir)))
    (if (file-exists? filename)
	(rewrite-file filename
	  (lambda (in out)
	    (case operation
	      ((add)
	       (let ((new (cons name (delete! name (read in)))))
		 (write new out)
		 new))
	      ((remove)
	       (let ((new (delete! name (read in))))
		 (write new out)
		 new))
	      (else
	       (error "Unexpected plugin-list operation:" operation)))))
	(case operation
	  ((add)
	   (let ((new (list name)))
	     (call-with-exclusive-output-file filename
	       (lambda (out)
		 (write new out)))
	     new))
	  ((remove)
	   (warn "plugin list not found:" filename)
	   '())
	  (else
	   (error "Unexpected plugin-list operation:" operation))))))

(define (update-optiondb plugins scmlibdir)
  (let ((filename (string scmlibdir"optiondb.scm")))
    (if (file-exists? filename)		;i.e. NOT in dpkg-buildpackage chroot
	(rewrite-file
	 filename
	 (lambda (in out)
	   (copy-to+line "(further-load-options" in out)
	   (write-string (string ";;; DO NOT EDIT the remainder of this file."
				 "  Any edits will be clobbered."
				 "\n") out)
	   (for-each
	     (lambda (name)
	       (write-string "\n(define-load-option '" out)
	       (write-string name out)
	       (write-string "\n  (standard-system-loader \"" out)
	       (write-string name out)
	       (write-string "\"))\n" out))
	     (sort plugins string<?))))
	(warn "optiondb not found:" filename))))

(define (update-info-index project plugins infodir scmdocdir)
  (if infodir
      (let ((filename (string infodir project".info")))
	(if (file-exists-or-compressed? filename)
	    (rewrite-file
	     filename
	     (lambda (in out)
	       (copy-to+line "Plugin Manuals" in out)
	       (newline out)
	       (for-each (lambda (plugin)
			   (write-direntry project plugin scmdocdir out))
			 (sort plugins string<?))))
	    (warn "Scheme Info index not found:" filename)))))

(define (write-direntry project plugin scmdocdir out)
  (let ((filename (string scmdocdir"info/"plugin".info")))
    (if (file-exists-or-compressed? filename)
	(call-with-input-file-uncompressed filename
	  (lambda (in)
	    (skip-to-line "START-INFO-DIR-ENTRY" in)
	    (transform-to-line
	     "END-INFO-DIR-ENTRY" in out #f
	     (let* ((str (string "("project"/"))
		    (str-len (string-length str)))
	       (lambda (line)
		 (let ((index (string-search-forward str line)))
		   (if index
		       (string (substring line 0 index)
			       "("scmdocdir"info/"
			       (substring line (fix:+ index str-len)))
		       line))))))))))

(define (update-html-index project plugins scmdocdir)
  (let* ((scmhtmldir (if (file-exists? (string scmdocdir"html/index.html"))
			 (string scmdocdir"html/")
			 scmdocdir))
	 (filename (string scmhtmldir"index.html")))
    (if (file-exists? filename)
	(rewrite-file
	 filename
	 (lambda (in out)
	   (copy-to+line "<ul id=\"plugins\"" in out)
	   (newline out)
	   (write-string (string-append "<!-- DO NOT EDIT this list."
					"  Any edits will be clobbered. -->"
					"\n") out)

	   ;; Write new list.
	   (let ((filenames.titles
		  (sort (html-filenames.titles project plugins scmhtmldir)
			(lambda (a b)
			  (string<? (cdr a) (cdr b))))))
	     (for-each
	       (lambda (filename.title)
		 (write-string "<li><a href=\"" out)
		 (write-string (car filename.title) out)
		 (write-string "\">" out)
		 (write-string (cdr filename.title) out)
		 (write-string "</a></li>\n" out))
	       filenames.titles)
	     (if (null? filenames.titles)
		 (write-string "<i>None currently installed.</i>\n" out)))

	   ;; Skip old list.
	   (do ((line (read-line in) (read-line in)))
	       ((or (eof-object? line)
		    (string-prefix? "</ul>" line))
		(if (eof-object? line)
		    (error "Premature end of HTML index.")
		    (begin
		      (write-string line out)
		      (newline out)))))

	   ;; Copy the rest.
	   (do ((line (read-line in) (read-line in)))
	       ((eof-object? line))
	     (write-string line out)
	     (newline out))))
	(warn "Scheme html index not found:" filename))))

(define (html-filenames.titles project plugins scmhtmldir)

  (define (existing-file . strings)
    (let ((filename (string* strings)))
      (and (file-exists? filename)
	   filename)))

  (append-map!
   (lambda (plugin)
     (let ((filename
	    (or (existing-file scmhtmldir plugin".html")
		(existing-file scmhtmldir project"-"plugin".html")
		(existing-file scmhtmldir project"-"plugin"/index.html"))))
       (if filename
	   (list (cons filename (read-html-title filename)))
	   '())))
   plugins))

(define (read-html-title filename)
  (let ((patt (compile-regsexp '(seq "<title>"
				     (group title (* (any-char)))
				     "</title>"))))
    (call-with-input-file filename
      (lambda (in)
	(let loop ()
	  (let ((line (read-line in)))
	    (if (eof-object? line)
		(error "Could not find HTML title:" filename)
		(let ((match (regsexp-match-string patt line)))
		  (if (not match)
		      (loop)
		      (match-ref match 'title))))))))))

(define (match-ref match key)
  (let ((entry (assq key (cddr match))))
    (if entry
	(cdr entry)
	(error "Match group not found:" key match))))

(define (copy-to+line prefix in out)
  (transform-to-line prefix in out #t #f))

(define (copy-to-line prefix in out)
  (transform-to-line prefix in out #f #f))

(define (transform-to-line prefix in out inclusive? transform)
  (do ((line (read-line in) (read-line in)))
      ((or (eof-object? line)
	   (string-prefix? prefix line))
       (if (eof-object? line)
	   (error "Copied to eof without seeing line:" prefix))
       (if inclusive?
	   (let ((line* (if transform (transform line) line)))
	     (write-string line* out)
	     (newline out))))
    (write-string (if transform (transform line) line) out)
    (newline out)))

(define (skip-to-line prefix in)
  (do ((line (read-line in) (read-line in)))
      ((or (eof-object? line)
	   (string-prefix? prefix line))
       (if (eof-object? line)
	   (error "Skipped to eof without seeing line:" prefix)))))

(define (rewrite-file filename rewriter)
  (let ((suffix.progs (compressed? filename)))
    (if suffix.progs
	(rewrite-compressed-file filename suffix.progs rewriter)
	(rewrite-simple-file filename rewriter))))

(define (rewrite-simple-file filename rewriter)
  (let ((replacement (replacement-filename filename)))
    (if (file-exists? replacement)
	(delete-file replacement))
    (with-temporary-file
     replacement
     (lambda ()
       (let ((value (call-with-exclusive-output-file
		     replacement
		     (lambda (out)
		       (call-with-input-file filename
			 (lambda (in)
			   (rewriter in out)))))))
	 (rename-file replacement filename)
	 value)))))

(define (rewrite-compressed-file filename suffix.progs rewriter)
  (load-option-quietly 'synchronous-subprocess)
  (let ((compressed (string filename"."(car suffix.progs))))
    (call-with-temporary-file-pathname
     (lambda (uncompressed)
       (un/compress-file (cddr suffix.progs)
			 compressed
			 (->namestring uncompressed))
       (call-with-temporary-file-pathname
	(lambda (transformed)
	  (let ((value
		 (call-with-input-file uncompressed
		   (lambda (in)
		     (call-with-output-file transformed
		       (lambda (out)
			 (rewriter in out)))))))
	    (let ((replacement (replacement-filename filename)))
	      (if (file-exists? replacement)
		  (delete-file replacement))
	      (with-temporary-file
	       replacement
	       (lambda ()
		 (un/compress-file (cadr suffix.progs)
				   (->namestring transformed)
				   replacement)
		 (rename-file replacement compressed))))
	    value)))))))

(define (call-with-input-file-uncompressed filename receiver)
  (let ((suffix.progs (compressed? filename)))
    (if suffix.progs
	(let ((compressed (string filename"."(car suffix.progs))))
	  (call-with-temporary-file-pathname
	   (lambda (uncompressed)
	     (un/compress-file (cddr suffix.progs)
			       compressed
			       (->namestring uncompressed))
	     (call-with-input-file uncompressed receiver))))
	(call-with-input-file filename receiver))))

(define compressed-file-suffixes.progs
  '(("gz" "gzip" . "gunzip")
    ("bz2" "bzip2" . "bunzip2")
    ("Z" "compress" . "uncompress")))

(define (file-exists-or-compressed? filename)
  (or (file-exists? filename)
      (find-compressed-suffix.progs filename)))

(define (compressed? filename)
  (and (not (file-exists? filename))
       (find-compressed-suffix.progs filename)))

(define (find-compressed-suffix.progs filename)
  (find (lambda (suffix.progs)
	  (file-exists? (string filename"."(car suffix.progs))))
	compressed-file-suffixes.progs))

(define (un/compress-file program infile outfile)
  (load-option-quietly 'synchronous-subprocess)
  (let ((cmdline (string program" < "infile" > "outfile)))
    (if (not (zero? (run-shell-command cmdline)))
	(error "File un/compress failed:" cmdline))))

(define (replacement-filename filename)
  (let ((pathname (->pathname filename)))
    (string (directory-namestring pathname)
	    "."(file-namestring pathname)"."(random-alphanumeric-string 6))))

(define (random-alphanumeric-string length)
  (list->string (map (lambda (i) i (random-alphanumeric-character))
		     (iota length))))

(define (random-alphanumeric-character)
  (integer->char
   (let ((n (random 62)))
    (cond ((< n 26) (+ (char->integer #\a) n))
	  ((< n 52) (+ (char->integer #\A) (- n 26)))
	  (else     (+ (char->integer #\0) (- n 52)))))))

(define (load-option-quietly name)
  (if (not (option-loaded? name))
      (let ((kernel
	     (lambda ()
	       (parameterize ((param:suppress-loading-message? #t))
		 (load-option name)))))
	(if (nearest-cmdl/batch-mode?)
	    (kernel)
	    (with-notification
	     (lambda (port)
	       (write-string "Loading " port)
	       (write-string (symbol->string name) port)
	       (write-string " option" port))
	     kernel)))))