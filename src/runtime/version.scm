#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;;; Runtime System Version Information
;;; package: (runtime)

(declare (usual-integrations))

(define copyright-years)

(add-boot-init!
 (lambda ()
   (set! copyright-years
	 (let ((now 2012)
	       (then 1986))
	   (iota (+ (- now then) 1) then)))
   (add-subsystem-identification! "Release" '(9 1 99))
   (snarf-microcode-version!)
   (add-event-receiver! event:after-restore snarf-microcode-version!)
   (add-subsystem-identification! "Runtime" '(15 7))))

(define (snarf-microcode-version!)
  (add-subsystem-identification! "Microcode"
				 (get-microcode-version-numbers)))

(define (write-mit-scheme-copyright #!optional port line-prefix cmark short?)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'WRITE-MIT-SCHEME-COPYRIGHT)))
	(cmark (if (default-object? cmark) "(C)" cmark))
	(line-prefix (if (default-object? line-prefix) "" line-prefix)))
    (write-words (let ((years (map number->string copyright-years)))
		   `("Copyright"
		     ,cmark
		     ,@(if short?
			   '()
			   (map (lambda (s) (string-append s ","))
				(except-last-pair years)))
		     ,(last years)
		     "Massachusetts"
		     "Institute"
		     "of"
		     "Technology"))
		 line-prefix
		 "    "
		 port)))

(define (write-mit-scheme-license #!optional port line-prefix short?)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'WRITE-MIT-SCHEME-LICENSE)))
	(line-prefix (if (default-object? line-prefix) "" line-prefix))
	(short? (if (default-object? short?) #f short?)))
    (let loop
	((paragraphs
	  (split-paragraphs
	   (if short?
	       short-license-statement
	       long-license-statement))))
      (write-words (car paragraphs) line-prefix "" port)
      (if (pair? (cdr paragraphs))
	  (begin
	    (newline port)
	    (write-string (string-trim-right line-prefix) port)
	    (newline port)
	    (loop (cdr paragraphs)))))))

(define long-license-statement
  "This file is part of MIT/GNU Scheme.

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
USA.")

(define short-license-statement
  "This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.")

(define (split-paragraphs text)
  (let loop ((lines (split-into-lines text)) (paras '()))
    (receive (lines para) (next-paragraph lines)
      (if para
	  (loop lines (cons para paras))
	  (reverse! paras)))))

(define (next-paragraph lines)
  (let loop ((lines (skip-blank-lines lines)) (para '()))
    (if (and (pair? lines)
	     (not (blank-line? (car lines))))
	(loop (cdr lines) (cons (car lines) para))
	(values lines
		(if (pair? para)
		    (append-map! split-text-words (reverse! para))
		    #f)))))

(define (skip-blank-lines lines)
  (if (and (pair? lines)
	   (blank-line? (car lines)))
      (skip-blank-lines (cdr lines))
      lines))

(define (blank-line? line)
  (string-null? (string-trim line)))

(define (split-into-lines text)
  (let ((input (open-input-string text)))
    (let loop ((lines '()))
      (let ((line (read-line input)))
	(if (eof-object? line)
	    (reverse! lines)
	    (loop (cons line lines)))))))

(define (split-text-words text)
  (let ((end (string-length text)))

    (define (loop i words)
      (let ((i (skip-white i)))
	(if (fix:< i end)
	    (let ((j (skip-non-white i)))
	      (loop j
		    (cons (substring text i j) words)))
	    words)))

    (define (skip-white i)
      (if (and (fix:< i end)
	       (char-set-member? char-set:whitespace (string-ref text i)))
	  (skip-white (fix:+ i 1))
	  i))

    (define (skip-non-white i)
      (if (and (fix:< i end)
	       (not (char-set-member? char-set:whitespace
				      (string-ref text i))))
	  (skip-non-white (fix:+ i 1))
	  i))

    (reverse! (loop 0 '()))))

(define (write-words words line-prefix indentation port)
  (let ((wrap-column (- (output-port/x-size port) 5))
	(space " "))

    (define (write-first-word words indent?)
      (write-string line-prefix port)
      (if indent? (write-string indentation port))
      (write-string (car words) port)
      (write-rest-words (cdr words)
			(new-column 0
				    line-prefix
				    (if indent? indentation "")
				    (car words))))

    (define (write-rest-words words column)
      (if (pair? words)
	  (let ((column* (new-column column space (car words))))
	    (if (<= column* wrap-column)
		(begin
		  (write-string space port)
		  (write-string (car words) port)
		  (write-rest-words (cdr words) column*))
		(begin
		  (newline port)
		  (write-first-word words #t))))))

    (write-first-word words #f)))

(define (new-column column . strings)
  (let loop ((column column) (strings strings))
    (if (pair? strings)
	(loop (let ((string (car strings)))
		(let ((end (string-length string)))
		  (do ((i 0 (fix:+ i 1))
		       (column column
			       (fix:+ column
				      (let ((c (string-ref string i)))
					(if (char=? c #\tab)
					    (fix:- 8 (fix:remainder column 8))
					    1)))))
		      ((not (fix:< i end)) column))))
	      (cdr strings))
	column)))