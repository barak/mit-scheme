#!/bin/sh
# -*-Scheme-*-
#
# Chop the generated $1-shim.c and $1-const.c files out of TAGS.

set -e
: ${MIT_SCHEME_EXE=mit-scheme}
${MIT_SCHEME_EXE} --batch-mode -- ${1+"$@"} <<\EOF
(let ((name (car (command-line-arguments))))
  (let ((shim.c-prefix (string-append name "-shim.c,"))
	(const.c-prefix (string-append name "-const.c,")))

    (define (rewriter in out)
      (let loop ((skipping? #f))
	(let ((line (read-line in)))
	  (cond ((eof-object? line)
		 unspecific)
		((string=? line "\f")
		 (let ((next (read-line in)))
		   (cond ((eof-object? next) (error "Bogus TAGS format:" next))
			 ((or (string-prefix? shim.c-prefix next)
			      (string-prefix? const.c-prefix next))
			  (loop #t))
			 (else
			  (write-string line out)
			  (newline out)
			  (write-string next out)
			  (newline out)
			  (loop #f)))))
		(skipping?
		 (loop skipping?))
		(else
		 (write-string line out)
		 (newline out)
		 (loop skipping?))))))

    (parameterize ((param:suppress-loading-message? #t))
      (load-option 'FFI))
    ((access rewrite-file (->environment '(ffi build)))
     (merge-pathnames "TAGS")
     rewriter)))
EOF
