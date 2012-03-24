#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Utility to update copyright and license statements.

(declare (usual-integrations))

(define (translate-standard-dirs root-dir translation)
  (let* ((root-dir (pathname-as-directory root-dir))
	 (target (merge-pathnames "translated-files" root-dir))
	 (target-dir (pathname-as-directory target)))
    (make-directory target)
    (for-each (lambda (dir)
		(translate-directory (merge-pathnames dir root-dir)
				     (merge-pathnames dir target-dir)
				     root-dir
				     translation))
	      '("dist" "doc" "etc" "html" "src" "tests"))))

(define (translate-directory source target root-dir translation)
  (let ((source (pathname-as-directory source))
	(target (pathname-as-directory target)))
    (let loop ((pathnames (list source)))
      (if (pair? pathnames)
	  (let* ((input (car pathnames))
		 (output
		  (merge-pathnames (enough-pathname input source) target)))
	    (if (file-directory? input)
		(loop (if (match-path input root-dir dirs-to-skip)
			  (cdr pathnames)
			  (begin
			    (if (not (file-directory? output))
				(make-directory output))
			    (append (directory-read
				     (pathname-as-directory input))
				    (cdr pathnames)))))
		(begin
		  (if (and (file-regular? input)
			   (not (match-path input root-dir files-to-skip)))
		      (let ((results (translation (read-file-leader input))))
			(if (pair? results)
			    (translate-file-1 input output results)
			    (if (not
				 (match-path input root-dir
					     suppress-warnings-for))
				(begin
				  (write-string "skipping ")
				  (write-string
				   (enough-namestring input root-dir))
				  (newline))))))
		  (loop (cdr pathnames)))))))))

(define (match-path pathname root-dir res)
  (any (let ((ns (enough-namestring pathname root-dir)))
	 (lambda (re)
	   (re-string-match re ns)))
       res))

(define dirs-to-skip
  '("\\(.*/\\)?\\.$"
    "\\(.*/\\)?\\.\\.$"
    "\\(.*/\\)?\\.git$"
    "\\(.*/\\)?CVS$"
    ))

(define files-to-skip
  '("src/microcode/svm1-defns\\.h$"
    ".+\\.png$"
    ".+\\.pdf$"
    "html/.+\\.html$"
    ))

(define suppress-warnings-for
  '("\\(.*/\\)?\\.gitignore$"
    "\\(.*/\\)?CVS/.+$"
    "\\(.*/\\)?ChangeLog$"
    "\\(.*/\\)?COPYING$"
    "\\(.*/\\)?INSTALL$"
    "\\(.*/\\)?LOG$"
    "\\(.*/\\)?Makefile-fragment$"
    "\\(.*/\\)?README$"
    "\\(.*/\\)?TAGS$"
    "\\(.*/\\)?TODO$"
    "src/swat/"
    ".+\\.ico$"
    ".+\\.sh$"
    ".+\\.txt$"))

(define (read-file-leader pathname)
  (call-with-input-file pathname
    (lambda (port)
      (let ((leader (make-string 4096)))
	(string-head leader (read-substring! leader 0 4096 port))))))

(define (translate-file-1 input output results)
  (call-with-input-file input
    (lambda (input)
      (call-with-output-file output
	(lambda (output)
	  (let loop ((results results) (index 0))
	    (if (pair? results)
		(receive (ps pe prefix1 prefix2 new-pps trailer)
		    (apply values (car results))
		  (let* ((n (- ps index))
			 (buffer (make-string n)))
		    (input-port/read-string! input buffer)
		    (output-port/write-substring output buffer 0 n))
		  (input-port/read-string! input (make-string (fix:- pe ps)))
		  (let ((spacer
			 (string-trim-right prefix2 char-set:not-whitespace))
			(do-pp
			 (lambda (pp)
			   (output-port/write-string
			    output
			    (fill-paragraph pp prefix1 prefix2 70)))))
		    (do-pp (car new-pps))
		    (for-each (lambda (pp)
				(write-string spacer output)
				(newline output)
				(do-pp pp))
			      (cdr new-pps))
		    (if trailer
			(begin
			  (newline output)
			  (write-string trailer output)
			  (newline output))))
		  (loop (cdr results) pe))
		(transfer-bytes input output)))))))
  ;;(set-file-times! output #f (file-modification-time input))
  (set-file-modes! output (file-modes input)))

(define (transfer-bytes input output)
  (let ((buffer (make-string 512)))
    (let loop ()
      (let ((n-read (input-port/read-string! input buffer)))
	(if (fix:> n-read 0)
	    (begin
	      (output-port/write-substring output buffer 0 n-read)
	      (loop)))))))

;;;; Copyright translation

(define (translate-copyright leader)
  (let ((end (string-length leader)))
    (let loop ((start 0) (results '()))
      (receive (ls le prefix cmark) (match-copyright leader start end)
	(if ls
	    (let ((le
		   (let loop ((le le))
		     (let ((ls (string-next-line-start leader le)))
		       (if ls
			   (let ((regs
				  (re-substring-match copyright-line-regexp
						      leader ls end)))
			     (if regs
				 (loop (re-match-end-index 0 regs))
				 ls))
			   le)))))
	      (let ((prefix1 (string-replace prefix #\tab #\space)))
		(loop le
		      (cons
		       (list ls
			     le
			     prefix1
			     (if (string=? prefix1 "[")
				 "    "
				 (string-append prefix1 "    "))
			     `(("Copyright"
				,cmark ,@(copyright-years (this-year))
				"Massachusetts" "Institute" "of" "Technology"))
			     #f)
		       results))))
	    (and (pair? results)
		 (reverse results)))))))

(define (match-copyright leader start end)
  (let ((regs
	 (re-substring-search-forward copyright-line-regexp leader start end)))
    (if regs
	(values (re-match-start-index 0 regs)
		(re-match-end-index 0 regs)
		(re-match-extract leader regs 1)
		(let ((cmark (re-match-extract leader regs 3)))
		  (if (or (string-null? cmark)
			  (string-ci=? cmark "(c)"))
		      "(C)"
		      cmark)))
	(values #f #f #f #f))))

(define copyright-line-regexp
  (re-compile-pattern
   (string-append
    "^\\(\\[?\\(.*\\)\\)Copyright \\(@copyright{}\\|&copy;\\|(C)\\|\\) [0-9]+"
    "\\(,\\(\n\\2\\)? *[0-9]+\\)*"
    (decorated-string-append
     "\\(\n\\2\\)? *"
     ""
     ""
     (list "Massachusetts" "Institute" "of" "Technology"))
    " *\\(@\\*\\)?$")
   #t))

(define (copyright-years y0)
  (let loop ((y 1986))
    (if (< y y0)
	(cons (string-append (number->string y) ",")
	      (loop (+ y 1)))
	(list (number->string y)))))

(define (this-year)
  (decoded-time/year (get-decoded-time)))

;;;; License translation

(define (translate-license leader)
  (let ((ks (find-key-string leader old-key-string)))
    (and ks
	 (let* ((ps (string-line-start leader ks))
		(prefix (substring leader ps ks))
		(pe (skip-paragraphs leader ks prefix old-n-paragraphs)))
	   (and pe
		(let ((prefix (string-replace prefix #\tab #\space)))
		  (list (list ps
			      pe
			      prefix
			      prefix
			      new-license
			      (find-trailer leader pe
					    old-license-final-token)))))))))

(define old-key-string "You should have received a copy of")
(define old-n-paragraphs 1)
(define old-license-final-token "USA.")

(define new-license
  (map (lambda (pp) (burst-string pp char-set:whitespace #t))
       '("
You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.")))

(define (find-key-string leader key-string)
  (let* ((n (string-length key-string))
	 (end (fix:- (string-length leader) n))
	 (c (string-ref key-string 0)))
    (and (fix:>= end 0)
	 (let loop ((start 0))
	   (let ((index (substring-find-next-char leader start end c)))
	     (and index
		  (if (substring=? key-string 0 n
				   leader index (fix:+ index n))
		      index
		      (loop (fix:+ index 1)))))))))

(define (find-trailer leader pe final-token)
  (define (skip-whitespace index)
    (let ((index (- index 1)))
      (if (char-whitespace? (string-ref leader index))
	  (skip-whitespace index)
	  (check-word (+ index 1)))))
  (define (check-word end)
    (cond ((test end "|#") "|#")
	  ((test end "*/") "*/")
	  ((test end final-token) #f)
	  (else
	   (write-string "Unknown ending: ")
	   (write-string (substring leader (- end 10) end))
	   (newline)
	   #f)))
  (define (test end string)
    (let ((n (string-length string)))
      (and (> (- end n) 0)
	   (substring=? leader (- end n) end string 0 n))))
  (skip-whitespace pe))

;;;; Text manipulation

(define (string-line-start string index)
  (let ((n (substring-find-previous-char string 0 index #\newline)))
    (if n
	(fix:+ n 1)
	0)))

(define (string-next-line-start string index)
  (let ((n
	 (substring-find-next-char string index (string-length string)
				   #\newline)))
    (and n
	 (fix:+ n 1))))

(define (skip-paragraphs leader index prefix n)
  (if (> n 0)
      (let ((pe (find-paragraph-end leader index prefix)))
	(and pe
	     (skip-paragraphs leader pe prefix (- n 1))))
      index))

(define (find-paragraph-end leader index prefix)
  (let ((end (string-length leader))
	(plen (string-length prefix)))
    (let loop ((start index) (n-lines 0))
      (let ((nl (substring-find-next-char leader start end #\newline)))
	(if (not nl) (error "Can't find end of paragraph."))
	(let ((ls (fix:+ nl 1)))
	  (if (if (fix:> plen 0)
		  (substring-prefix? prefix 0 plen leader ls end)
		  ;; This is for configure.ac:
		  (not (substring-prefix? "])" 0 2 leader ls end)))
	      (let ((i (skip-whitespace leader (fix:+ ls plen))))
		(if (or (fix:= i end)
			(char=? #\newline (string-ref leader i)))
		    ls
		    (loop i (fix:+ n-lines 1))))
	      ls))))))

(define (skip-whitespace leader index)
  (let ((c (string-ref leader index)))
    (if (or (char=? #\newline c)
	    (not (char-whitespace? c)))
	index
	(skip-whitespace leader (+ index 1)))))

(define (fill-paragraph words prefix1 prefix2 limit-column)
  (let ((lines (fill-paragraph-count words prefix1 prefix2 limit-column)))
    (let ((n (fix:+ (length lines) (apply + (map cdr lines)))))
      (let ((buffer (make-string n)))
	(fill-paragraph-write words prefix1 prefix2 lines buffer 0)
	buffer))))

(define (fill-paragraph-count words prefix1 prefix2 limit-column)
  (let ((pc1 (string-length prefix1))
	(pc2 (string-length prefix2)))
    (let loop ((words words) (pc pc1) (lines '()))
      (if (pair? words)
	  (receive (nw nc) (fill-paragraph-line-count words pc limit-column)
	    (loop (list-tail words nw)
		  pc2
		  (cons (cons nw nc) lines)))
	  (reverse! lines)))))

(define (fill-paragraph-line-count words prefix-columns limit-column)
  (let loop
      ((prev-word (car words))
       (words (cdr words))
       (nw 1)
       (nc (fix:+ prefix-columns (string-length (car words)))))
    (if (null? words)
	(values nw nc)
	(let ((nc*
	       (fix:+ nc
		      (fix:+ (if (sentence-end? prev-word) 2 1)
			     (string-length (car words))))))
	  (if (fix:<= nc* limit-column)
	      (loop (car words)
		    (cdr words)
		    (fix:+ nw 1)
		    nc*)
	      (values nw nc))))))

(define (fill-paragraph-write words prefix1 prefix2 lines buffer index)
  (let loop ((words words) (lines lines) (index index) (prefix prefix1))
    (if (pair? lines)
	(receive (words index)
	    (fill-paragraph-line-write words prefix (caar lines) buffer index)
	  (string-set! buffer index #\newline)
	  (loop words (cdr lines) (fix:+ index 1) prefix2))
	index)))

(define (fill-paragraph-line-write words prefix nw buffer index)
  (let loop
      ((index
	(fill-paragraph-word-write
	 (car words)
	 buffer
	 (fill-paragraph-word-write prefix buffer index)))
       (prev-word (car words))
       (words (cdr words))
       (nw (fix:- nw 1)))
    (if (fix:> nw 0)
	(begin
	  (string-set! buffer index #\space)
	  (loop (fill-paragraph-word-write
		 (car words)
		 buffer
		 (if (sentence-end? prev-word)
		     (begin
		       (string-set! buffer (fix:+ index 1) #\space)
		       (fix:+ index 2))
		     (fix:+ index 1)))
		(car words)
		(cdr words)
		(fix:- nw 1)))
	(values words index))))

(define (fill-paragraph-word-write word buffer index)
  (let ((n (string-length word)))
    (substring-move-left! word 0 n buffer index)
    (fix:+ index n)))

(define (sentence-end? word)
  (re-string-match ".+[.?!][]\"')}]*$" word))