#| -*-Scheme-*-

Compile the FFI system. |#

(load-option 'CREF)

;; Temporary hack.  Remove when compile-system is in the release.
(if (not (environment-bound? (->environment '()) 'compile-system))
    (let ((butil-env (->environment '(cross-reference)))
	  (global-env (->environment '())))
      (load "../cref/butils" butil-env)
      (environment-link-name global-env butil-env 'compile-system)))

(compile-system "ffi" (directory-pathname (current-load-pathname)))