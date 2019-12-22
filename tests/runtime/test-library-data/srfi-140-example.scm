(define-library* (test srfi 140 base)
  (import (drop (scheme base)
		(exports (srfi 140)))
	  (srfi 140))
  (export (exports (scheme base))
	  (exports (srfi 140))))

(define-library* (test srfi 140 char)
  (import (drop (scheme char)
		(exports (srfi 140)))
	  (take (srfi 140)
		(exports (scheme char))))
  (export (exports (scheme char))))

(define-library* (test srfi 140 istrings)
  (define istring-names
    (union list->string
	   reverse-list->string
	   string
	   string-append
	   string-concatenate
	   string-concatenate-reverse
	   string-downcase
	   string-drop
	   string-drop-right
	   string-filter
	   string-foldcase
	   string-join
	   string-map
	   string-map-index
	   string-pad
	   string-pad-right
	   string-remove
	   string-repeat
	   string-replace
	   string-tabulate
	   string-take
	   string-take-right
	   string-titlecase
	   string-trim
	   string-trim-both
	   string-trim-right
	   string-unfold
	   string-unfold-right
	   string-upcase
	   substring
	   utf16->string
	   utf16be->string
	   utf16le->string
	   utf8->string
	   vector->string
	   xsubstring))
  (import (take (srfi 140) istring-names))
  (export istring-names))

(define-library* (test srfi 140 mstrings)
  (import (take (scheme base) define))
  (export (msubstring substring)
	  (xmsubstring xsubstring)
	  ((: ($ (* any)) "->mstring" eos) (1 "->string"))
	  ((: "mstring" ($ (* any)) eos) ("string" 1)))
  (begin
    (define list->mstring)
    (define reverse-list->mstring)
    (define mstring)
    (define mstring-append)
    (define mstring-concatenate)
    (define mstring-concatenate-reverse)
    (define mstring-downcase)
    (define mstring-drop)
    (define mstring-drop-right)
    (define mstring-filter)
    (define mstring-foldcase)
    (define mstring-join)
    (define mstring-map)
    (define mstring-map-index)
    (define mstring-pad)
    (define mstring-pad-right)
    (define mstring-remove)
    (define mstring-repeat)
    (define mstring-replace)
    (define mstring-tabulate)
    (define mstring-take)
    (define mstring-take-right)
    (define mstring-titlecase)
    (define mstring-trim)
    (define mstring-trim-both)
    (define mstring-trim-right)
    (define mstring-unfold)
    (define mstring-unfold-right)
    (define mstring-upcase)
    (define msubstring)
    (define utf16->mstring)
    (define utf16be->mstring)
    (define utf16le->mstring)
    (define utf8->mstring)
    (define vector->mstring)
    (define xmsubstring)))