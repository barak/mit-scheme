#| -*-Scheme-*-

Load the X11-Screen option. |#

(load-option 'X11)
(load-option 'Edwin)
(with-loader-base-uri (system-library-uri "x11-screen/")
  (lambda ()
    (load-package-set "x11-screen")))
(add-subsystem-identification! "X11-Screen" '(0 1))

;; Reassign (edwin x-commands) bindings created by the define-
;; primitives form.  Reassign them to their replacements in the (x11)
;; package.
(let ((xcom (->environment '(edwin x-commands)))
      (x11 (->environment '(x11))))
  (for-each (lambda (name)
	      (environment-assign! xcom name (environment-lookup x11 name)))
	    '(x-list-fonts
	      x-set-default-font
	      x-window-clear
	      x-window-get-position
	      x-window-get-size
	      x-window-lower
	      x-window-raise
	      x-window-set-background-color
	      x-window-set-border-color
	      x-window-set-border-width
	      x-window-set-cursor-color
	      x-window-set-font
	      x-window-set-foreground-color
	      x-window-set-internal-border-width
	      x-window-set-mouse-color
	      x-window-set-mouse-shape
	      x-window-set-position
	      x-window-set-size
	      x-window-x-size
	      x-window-y-size
	      xterm-reconfigure
	      xterm-set-size
	      xterm-x-size
	      xterm-y-size)))

;; Reassign (edwin screen x-screen) bindings exported to (edwin).
(let ((edwin (->environment '(edwin)))
      (x11 (->environment '(edwin screen x11-screen))))
  (for-each (lambda (name)
	      (environment-assign! edwin name (environment-lookup x11 name)))
	    '(edwin-variable$x-cut-to-clipboard
	      edwin-variable$x-paste-from-clipboard
	      os/interprogram-cut
	      os/interprogram-paste
	      x-root-window-size
	      x-screen-ignore-focus-button?
	      x-selection-timeout
	      xterm-screen/flush!
	      xterm-screen/grab-focus!)))

;; Reassign (edwin screen x-screen) bindings exported to (edwin x-commands).
(let ((edwin (->environment '(edwin x-commands)))
      (x11 (->environment '(edwin screen x11-screen))))
  (for-each (lambda (name)
	      (environment-assign! edwin name (environment-lookup x11 name)))
	    '(screen-display
	      screen-xterm
	      xterm-screen/set-icon-name
	      xterm-screen/set-name)))

;; Remove the X display type.  If it stays on the list, its available?
;; operation will load the prx11 microcode module which contains
;; conflicting definitions for symbols like xterm_open_window.
(let ((env (->environment '(edwin display-type))))
  (set! (access display-types env)
	(filter (lambda (display-type)
		  (not (eq? 'X ((access display-type/name env) display-type))))
		(access display-types env))))