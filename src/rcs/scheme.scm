(define (make-bind-script symbol output-file)
  (let ((read-head (access rcs/read-head (->environment '(RCS)))))
    (with-output-to-file output-file
      (lambda ()
	(for-each (lambda (pathname)
		    (let ((head (read-head pathname)))
		      (write-string
		       (string-append "rcs -n" symbol ":" head
				      " -sRel:" head " "
				      (pathname->string pathname)
				      "\n"))))
		  (apply append!
			 (map (lambda (pathname)
				(list-transform-negative
				    (directory-read pathname)
				  (lambda (pathname)
				    (zero? (string-match-backward
					    (->namestring pathname)
					    ",v")))))
			      (map (lambda (directory)
				     (string-append directory "/RCS/"))
				   '("microcode"
				     "microcode/m"
				     "microcode/s"
				     "runtime"
				     "cref"
				     "sf"
				     "compiler"
				     "compiler/back"
				     "compiler/base"
				     "compiler/etc"
				     "compiler/fggen"
				     "compiler/fgopt"
				     "compiler/machines/bobcat"
				     "compiler/machines/mips"
				     "compiler/machines/spectrum"
				     "compiler/machines/vax"
				     "compiler/rtlbase"
				     "compiler/rtlgen"
				     "compiler/rtlopt"
				     "edwin"
				     ;; "documentation"
				     ;; "etc"
				     )))))))))