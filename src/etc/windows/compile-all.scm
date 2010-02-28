(with-working-directory-pathname "runtime\\"
  (lambda ()
    (load "runtime.sf")
    (load "runtime.cbf")))
(with-working-directory-pathname "win32\\"
  (lambda ()
    (load "win32.sf")
    (load "win32.cbf")))
(with-working-directory-pathname "sf\\"
  (lambda ()
    (load "sf.sf")
    (load "sf.cbf")))
(with-working-directory-pathname "cref\\"
  (lambda ()
    (load "cref.sf")
    (load "cref.cbf")))
(with-working-directory-pathname "compiler\\"
  (lambda ()
    (let ((copy
	   (lambda (name)
	     (let ((from (merge-pathnames name "machines\\i386\\")))
	       (if (file-modification-time<? name from)
		   (begin
		     (delete-file-no-errors name)
		     (copy-file from name)))))))
      (copy "compiler.pkg")
      (copy "make.com")
      (copy "compiler.sf")
      (copy "compiler.cbf"))
    (load "compiler.sf")
    (load "compiler.cbf")))
(load "sos\\compile")
(load "star-parser\\compile")
(load "xml\\compile")
(with-working-directory-pathname "edwin\\"
  (lambda ()
    (load "edwin.sf")
    (load "edwin.cbf")))
(load "imail\\compile")
(load "ssp\\compile")