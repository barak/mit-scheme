(let ((etables (->environment '(runtime ucd-tables)))
      (eglue (->environment '(runtime ucd-glue))))

  (define (table-ref name)
    (environment-lookup etables name))

  (define (glue-ref name)
    (environment-lookup eglue name))

  (define (gc-char-set gc)
    (table-ref (symbol 'char-set:gc= gc)))

  (define (report name new old)
    (write-line (if (char-set=? new old)
		    name
		    (list name
			  'new (char-set->code-points new)
			  'old (char-set->code-points old)))))
  (fresh-line)

  #;
  (for-each
   (lambda (gc)
     (report gc
	     (compute-char-set
	      (lambda (sv)
		(eq? gc (code-point-general-category sv))))
	     (gc-char-set gc)))
   '(letter:uppercase
     letter:lowercase
     letter:titlecase
     letter:modifier
     letter:other
     mark:nonspacing
     mark:spacing-combining
     mark:enclosing
     number:decimal-digit
     number:letter
     number:other
     punctuation:connector
     punctuation:dash
     punctuation:open
     punctuation:close
     punctuation:initial-quote
     punctuation:final-quote
     punctuation:other
     symbol:math
     symbol:currency
     symbol:modifier
     symbol:other
     separator:space
     separator:line
     separator:paragraph
     other:control
     other:format
     other:surrogate
     other:private-use
     other:not-assigned))

  (report 'char-set:unicode
	  (char-set-difference (char-set-invert (char-set))
			       (gc-char-set 'other:surrogate)
			       (gc-char-set 'other:not-assigned))
	  (glue-ref 'char-set:unicode))

  (report 'char-set:symbol-constituent
	  (char-set-difference
	   (char-set-union*
	    (cons (char-set #\x200c #\x200d)
		  (map gc-char-set
		       '(letter:uppercase
			 letter:lowercase
			 letter:titlecase
			 letter:modifier
			 letter:other
			 mark:nonspacing
			 number:letter
			 number:other
			 punctuation:connector
			 punctuation:dash
			 punctuation:other
			 symbol:math
			 symbol:currency
			 symbol:modifier
			 symbol:other
			 other:private-use
			 mark:spacing-combining
			 mark:enclosing
			 number:decimal-digit))))
	   (char-set #\" #\# #\' #\, #\; #\\ #\` #\|))
	  (glue-ref 'char-set:symbol-constituent))

  (report 'char-set:symbol-initial
	  (char-set-difference
	   (char-set-union*
	    (cons (char-set #\x200c #\x200d)
		  (map gc-char-set
		       '(letter:uppercase
			 letter:lowercase
			 letter:titlecase
			 letter:modifier
			 letter:other
			 mark:nonspacing
			 number:letter
			 number:other
			 punctuation:connector
			 punctuation:dash
			 punctuation:other
			 symbol:math
			 symbol:currency
			 symbol:modifier
			 symbol:other
			 other:private-use))))
	   (char-set #\" #\# #\' #\, #\; #\\ #\` #\|))
	  (glue-ref 'char-set:symbol-initial))

  (report 'char-set:normal-printing
	  (char-set-union*
	   (map gc-char-set
		'(letter:uppercase
		  letter:lowercase
		  letter:titlecase
		  letter:modifier
		  letter:other
		  mark:nonspacing
		  mark:spacing-combining
		  mark:enclosing
		  number:decimal-digit
		  number:letter
		  number:other
		  punctuation:connector
		  punctuation:dash
		  punctuation:open
		  punctuation:close
		  punctuation:initial-quote
		  punctuation:final-quote
		  punctuation:other
		  separator:space
		  symbol:math
		  symbol:currency
		  symbol:modifier
		  symbol:other)))
	  (glue-ref 'char-set:normal-printing))

  (report 'char-set:numeric
	  (table-ref 'char-set:nt=decimal)
	  (glue-ref 'char-set:numeric))

  )