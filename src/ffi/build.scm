#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

(define (compile-shim)
  (let ((vals (conf-values (shim-conf) 'COMPILE-SHIM)))
    (let ((prefix (append
		   (filter (lambda (i) (not (string=? "-DMIT_SCHEME" i)))
			   (parse-words (car vals)))
		   (list (string-append "-I" (auxdir)))
		   (parse-words (cadr vals)))))
      (run-command (append prefix (command-line))))))

(define (link-shim)
  (let ((vals (conf-values (shim-conf) 'LINK-SHIM)))
    (let ((prefix (parse-words (car vals)))
	  (suffix (parse-words (cadr vals))))
      (run-command (append prefix (command-line) suffix)))))

(define (install-shim destdir libname)
  (guarantee-string destdir 'INSTALL-SHIM)
  (guarantee-string libname 'INSTALL-SHIM)
  (if (string-find-next-char libname #\/)
      (error "Directory separator, #\/, in library name:" libname))
  (run-command (append (conf-words (shim-conf) 'INSTALL)
		       (list (string-append libname "-shim.so")
			     (string-append libname "-types.bin")
			     (string-append libname "-const.bin")
			     (string-append destdir (auxdir))))))

(define (install-load-option destdir name #!optional directory)
  (guarantee-string destdir 'INSTALL-LOAD-OPTION)
  (guarantee-string name 'INSTALL-LOAD-OPTION)
  (let ((dir (if (default-object? directory)
		 name
		 directory)))
    (guarantee-string dir 'INSTALL-LOAD-OPTION)
    (let ((install (conf-words (shim-conf) 'INSTALL))
	  (auxdir (auxdir)))
      (let ((library-dir (string-append destdir auxdir dir)))
	(run-command (list "rm" "-rf" library-dir))
	(run-command (list "mkdir" library-dir))
	(run-command (list "chmod" "755" library-dir))
	(run-command (append install (files) (list library-dir)))
	(rewrite-file (string-append destdir auxdir "optiondb.scm")
		      (lambda (in out)
			(rewrite-optiondb name dir in out)))))))

(define (files)
  (append-map!
   (lambda (arg)
     (let ((p (->pathname arg)))
       (if (pathname-type p)
	   (if (file-exists? p)
	       (list arg)
	       (error "Could not find file:" p))
	   (let ((files
		  (let ((com (pathname-new-type p "com")))
		    (if (file-exists? com)
			(list (->namestring com)
			      (->namestring (pathname-new-type p "bci")))
			(let ((bin (pathname-new-type p "bin")))
			  (if (file-exists? bin)
			      (list (->namestring bin))
			      (error "Could not find .com nor .bin:" p))))))
		 (ext (pathname-new-type p "ext")))
	     (if (file-exists? ext)
		 (cons (->namestring ext) files)
		 files)))))
   (command-line)))

(define (rewrite-optiondb name dirname in out)
  (do ((line (read-line in) (read-line in)))
      ((eof-object? line))
    (write-string line out)
    (newline out))
  (fresh-line out)
  (newline out)
  (write-string "(define-load-option '" out)
  (write-string name out)
  (newline out)
  (write-string "  (standard-system-loader \"" out)
  (write-string dirname out)
  (write-string "\"))" out))

(define (install-html destdir title)
  (guarantee-string destdir 'INSTALL-HTML)
  (guarantee-string title 'INSTALL-HTML)
  (let ((conf (doc-conf)))
    (let ((install (conf-words conf 'INSTALL))
	  (htmldir (string-append destdir (conf-value conf 'HTMLDIR)))
	  (files (files)))
      (run-command (append install files (list htmldir)))
      (rewrite-file (merge-pathnames "index.html"
				     (pathname-as-directory htmldir))
		    (lambda (in out)
		      (rewrite-html-index (car files) title in out))))))

(define (rewrite-html-index file title in out)

  (define (match line)
    (if (eof-object? line)
	(error "Premature end of HTML documentation index." in))
    (let ((regs (re-string-match "^<li><a href=\"\\(.*\\)\">\\(.*\\)</a></li>$"
				 line)))
      (if (not regs)
	  #f
	  (cons (re-match-extract line regs 1)
		(re-match-extract line regs 2)))))

  (define (write-item file.title)
    (let ((file (car file.title))
	  (title (cdr file.title)))
      (write-string (string-append "<li><a href=\""file"\">"title"</a></li>")
		    out)
      (newline out)))

  (define (copy-prefix)
    (let* ((line (read-line in))
	   (f.t (match line)))
      (if (not f.t)
	  (begin
	    (write-string line out)
	    (newline out)
	    (copy-prefix))
	  f.t)))

  (define (copy-items)
    (let loop ((items (list (copy-prefix))))
      (let* ((line (read-line in))
	     (f.t (match line)))
	(if f.t
	    (loop (cons f.t items))
	    (let ((items (let ((entry (assoc file items)))
			   (if entry
			       (delq! entry items)
			       items))))
	      (for-each write-item
			(sort (cons (cons file title) items)
			      (lambda (f.title1 f.title2)
				(string<? (cdr f.title1)
					  (cdr f.title2)))))
	      line)))))

  (define (copy-suffix line)
    (if (not (eof-object? line))
	(begin
	  (write-string line out)
	  (newline out)
	  (copy-suffix (read-line in)))))

  (copy-suffix (copy-items)))

(define (auxdir)
  (->namestring (system-library-directory-pathname)))

(define (shim-conf)
  (fluid-let ((load/suppress-loading-message? #t))
    (load (system-library-pathname "shim-config.scm"))))

(define (doc-conf)
  (fluid-let ((load/suppress-loading-message? #t))
    (load (string-append (conf-value (shim-conf) 'INFODIR)
				     "mit-scheme-doc-config.scm"))))

(define (conf-values conf name)
  (let ((entry (assq name conf)))
    (if (pair? entry)
	(if (list-of-type? (cdr entry) string?)
	    (cdr entry)
	    (error "Configuration value not a list:" name))
	(error "Configuration value not found:" name))))

(define (conf-value conf name)
  (let ((vals (conf-values conf name)))
    (if (= 1 (length vals))
	(car vals)
	(error "Configuration value not a single string:" name))))

(define (conf-words conf name)
  (let ((val (conf-value conf name)))
    (parse-words val)))

(define (rewrite-file name rewriter)
  (let ((tmp (pathname-new-type name "tmp")))
    (call-with-exclusive-output-file tmp
      (lambda (out)
	(call-with-input-file name
	  (lambda (in)
	    (rewriter in out)))))
    (rename-file tmp name)))

(define (parse-words string)
  (burst-string string char-set:whitespace #t))

(define (run-command command)
  (fresh-line)
  (write-string (decorated-string-append "" " " "" command))
  (newline)
  (let ((code (run-synchronous-subprocess
	       (car command) (cdr command)
	       'working-directory (working-directory-pathname))))
    (if (not (zero? code))
	(error "Process exited with error code:" code command))))