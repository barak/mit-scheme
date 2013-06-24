(let ((new-child
       (lambda (parent name)
	 (let ((package (name->package parent)))
	   (package/add-child! package
			       name
			       (extend-top-level-environment
				(package/environment package)))))))
  (new-child '(EDWIN) 'IMAIL)
  (new-child '(EDWIN IMAIL) 'IMAP-RESPONSE)
  (new-child '(EDWIN IMAIL) 'IMAP-SYNTAX)
  (new-child '(EDWIN IMAIL) 'PARSER)
  (new-child '(EDWIN IMAIL) 'REXP)
  (new-child '(EDWIN IMAIL) 'URL))