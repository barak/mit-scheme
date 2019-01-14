#| -*-Scheme-*-

Load the X11-Screen option. |#

(load-option 'x11)
(load-option 'edwin)
(with-loader-base-uri (system-library-uri "x11-screen/")
  (lambda ()
    (load-package-set "x11-screen")))
(add-subsystem-identification! "X11-Screen" '(1 0))

;; Replace stubs in (edwin screen x-screen).
(let ((x (->environment '(edwin screen x-screen)))
      (x11 (->environment '(edwin screen x11-screen))))
  (for-each
    (lambda (name)
      (environment-assign! x name (environment-lookup x11 name)))
    '(make-xterm-screen
      get-xterm-input-operations
      with-editor-interrupts-from-x
      with-x-interrupts-enabled
      with-x-interrupts-disabled)))

;; Replace stubs in (edwin).
(let ((edwin (->environment '(edwin)))
      (x11 (->environment '(edwin screen x11-screen))))
  (for-each
    (lambda (name)
      (environment-assign! edwin name (environment-lookup x11 name)))
    '(os/interprogram-cut
      os/interprogram-paste)))