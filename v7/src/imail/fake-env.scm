(let ((new-child
       (lambda (parent name)
	 (let ((package (name->package parent)))
	   (package/add-child! package
			       name
			       (in-package (package/environment package)
				 (make-environment)))))))
  (new-child '(EDWIN) 'IMAIL)
  (new-child '(EDWIN IMAIL) 'IMAP-response)
  (new-child '(EDWIN IMAIL) 'IMAP-SYNTAX)
  (new-child '(EDWIN IMAIL) 'PARSER)
  (new-child '(EDWIN IMAIL) 'REXP)
  (new-child '(EDWIN IMAIL) 'RFC822)
  (new-child '(EDWIN IMAIL) 'URL))