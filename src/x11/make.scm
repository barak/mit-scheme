#| -*-Scheme-*-

Load the X11 option. |#

(with-loader-base-uri (system-library-uri "x11/")
  (lambda ()
    (load-package-set "x11")))
(add-subsystem-identification! "X11" '(1 3))

((access link! (->environment '(runtime x-graphics))))