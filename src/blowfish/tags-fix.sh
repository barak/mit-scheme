#!/bin/sh
# -*-Scheme-*-
#
# Changes to TAGS:
#   + Punt any generated *-shim.c and *-const.c files.
#   + Re-order the files: .scm first, .[hc] next, whatnot, and .cdecls last.

set -e
: ${MIT_SCHEME_EXE=mit-scheme}
${MIT_SCHEME_EXE} --batch-mode -- ${1+"$@"} <<\EOF
(let ()

  (define-integrable (make-section filename bytecount lines)
    (cons (cons filename bytecount) lines))
  (define-integrable section.filename caar)
  (define-integrable section.bytecount cdar)
  (define-integrable section.lines cdr)

  (define headline-pattern
    (compile-regsexp '(seq (line-start)
			   (group filename (+ (char-not-in #\,)))
			   #\,
			   (group bytecount (+ (char-in numeric)))
			   (line-end))))

  (define (write-section section out)
    (write-string "\f\n" out)
    (write-string (string (section.filename section)
			  #\, (section.bytecount section)
			  "\n")
		  out)
    (for-each (lambda (line) (write-string line out) (newline out))
	      (section.lines section)))

  (define (write-sections sections out)
    (for-each (lambda (section) (write-section section out))
	      (sort sections
		    (lambda (a b)
		      (string<? (section.filename a)
				(section.filename b))))))

  (define (read-section in)
    (let loop ((lines '()))
      (let ((line (read-line in)))
	(if (or (eof-object? line)
		(string=? line "\f"))
	    (reverse! lines)
	    (loop (cons line lines))))))

  (define (rewriter in out)
    (let ((line (read-line in)))
      (cond ((eof-object? line)
	     (error "TAGS file is empty"))
	    ((not (string=? line "\f"))
	     (error "TAGS file does not start with a formfeed:" line))))
    (let loop ((scms '()) (chs '()) (cdecls '()) (rest '()))
      (let ((line (read-line in)))
	(if (eof-object? line)
	    (begin
	      (write-sections scms out)
	      (write-sections chs out)
	      (write-sections rest out)
	      (write-sections cdecls out))
	    (let ((match (regsexp-match-string headline-pattern line)))
	      (if (not match)
		  (error "TAGS file contains a bogus headline:" line))
	      (let ((filename (cdr (assq 'filename (cddr match))))
		    (section (make-section (cdr (assq 'filename (cddr match)))
					   (cdr (assq 'bytecount (cddr match)))
					   (read-section in))))
		(cond ((or (string-suffix? "-shim.c" filename)
			   (string-suffix? "-const.c" filename))
		       (loop scms chs cdecls rest))
		      ((string-suffix? ".scm" filename)
		       (loop (cons section scms) chs cdecls rest))
		      ((or (string-suffix? ".c" filename)
			   (string-suffix? ".h" filename))
		       (loop scms (cons section chs) cdecls rest))
		      ((string-suffix? ".cdecl" filename)
		       (loop scms chs (cons section cdecls) rest))
		      (else
		       (loop scms chs cdecls (cons section rest))))))))))

  (parameterize ((param:suppress-loading-message? #t))
    (load-option 'ffi))
  ((access rewrite-file (->environment '(ffi build)))
   (merge-pathnames "TAGS")
   rewriter))
EOF
