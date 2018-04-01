(let ((new-child
       (lambda (parent name)
	 (let ((package (name->package parent)))
	   (package/add-child! package
			       name
			       (extend-top-level-environment
				(package/environment package)))))))
  (new-child '(edwin) 'imail)
  (new-child '(edwin imail) 'imap-response)
  (new-child '(edwin imail) 'imap-syntax)
  (new-child '(edwin imail) 'parser)
  (new-child '(edwin imail) 'rexp)
  (new-child '(edwin imail) 'url))