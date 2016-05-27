#| -*-Scheme-*-

Load the X11 option. |#

(with-loader-base-uri (system-library-uri "x11/")
  (lambda ()
    (load-package-set "x11")))
(add-subsystem-identification! "X11" '(0 1))

;; Until the microcode module based X Graphics system is removed,
;; reassign the define-primitives bindings in (runtime x-graphics) to
;; their replacements in (x11).
(let ((x-graphics (->environment '(runtime x-graphics)))
      (x11 (->environment '(x11))))
  (for-each (lambda (name)
	      (environment-assign! x-graphics name
				   (environment-lookup x11 name)))
	    '(
	      x-close-all-displays
	      x-display-descriptor
	      x-display-get-default
	      x-display-process-events
	      x-font-structure
	      x-window-beep
	      x-window-clear
	      x-window-colormap
	      x-window-depth
	      x-window-event-mask
	      x-window-flush
	      x-window-iconify
	      x-window-id
	      x-window-lower
	      x-window-map
	      x-window-query-pointer
	      x-window-raise
	      x-window-set-background-color
	      x-window-set-border-color
	      x-window-set-border-width
	      x-window-set-cursor-color
	      x-window-set-event-mask
	      x-window-set-font
	      x-window-set-foreground-color
	      x-window-set-icon-name
	      x-window-set-input-hint
	      x-window-set-internal-border-width
	      x-window-set-mouse-color
	      x-window-set-mouse-shape
	      x-window-set-name
	      x-window-set-position
	      x-window-set-size
	      ;; x-window-starbase-filename No such primitive!
	      x-window-visual
	      x-window-withdraw
	      x-window-x-size
	      x-window-y-size
	      x-graphics-copy-area
	      x-graphics-drag-cursor
	      x-graphics-draw-arc
	      x-graphics-draw-line
	      x-graphics-draw-lines
	      x-graphics-draw-point
	      x-graphics-draw-points
	      x-graphics-draw-string
	      x-graphics-draw-image-string
	      x-graphics-fill-polygon
	      x-graphics-map-x-coordinate
	      x-graphics-map-y-coordinate
	      x-graphics-move-cursor
	      x-graphics-open-window
	      x-graphics-reconfigure
	      x-graphics-reset-clip-rectangle
	      x-graphics-set-clip-rectangle
	      x-graphics-set-dashes
	      x-graphics-set-fill-style
	      x-graphics-set-function
	      x-graphics-set-line-style
	      x-graphics-set-vdc-extent
	      x-graphics-vdc-extent
	      x-bytes-into-image
	      x-create-image
	      x-destroy-image
	      x-display-image
	      x-get-pixel-from-image
	      x-set-pixel-in-image
	      x-allocate-color
	      x-create-colormap
	      x-free-colormap
	      x-query-color
	      x-set-window-colormap
	      x-store-color
	      x-store-colors
	      x-visual-deallocate)))

;; Check that these (integrated!) constants DO "match" the C
;; constants, just because we can (with the FFI's help).
(let ((x-graphics (->environment '(runtime x-graphics)))
      (x11 (->environment '(x11))))
  (for-each (lambda (name)
	      (if (not (equal? (environment-lookup x-graphics name)
			       (environment-lookup x11 name)))
		  (warn "Incorrect C constant in (runtime x-graphics):" name)))
	    '(event-type:button-down
	      event-type:button-up
	      event-type:configure
	      event-type:enter
	      event-type:focus-in
	      event-type:focus-out
	      event-type:key-press
	      event-type:leave
	      event-type:motion
	      event-type:expose
	      event-type:delete-window
	      event-type:map
	      event-type:unmap
	      event-type:take-focus
	      event-type:visibility
	      number-of-event-types)))