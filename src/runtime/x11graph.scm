#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; X Graphics Interface
;;; package: (runtime x-graphics)

(declare (usual-integrations))

;;; Access to the X11 library is now accomplished with the FFI rather
;;; than a microcode module.  The bindings in this package are linked
;;; to those in the (x11 graphics) package after the plugin is loaded.

(define linked? #f)

(define (x-graphics/available?)
  (and (plugin-available? "x11")
       (or linked?
	   (begin
	     (load-option 'x11)
	     linked?))))

(define (link!)
  (for-each
    (let ((runtime (->environment '(runtime x-graphics)))
	  (x11 (->environment '(x11))))
      (lambda (name)
	(environment-link-name runtime x11 name)))
    names)
  (set! linked? #t))

(define names
  '(create-x-colormap
    create-x-image
    x-character-bounds/ascent
    x-character-bounds/descent
    x-character-bounds/lbearing
    x-character-bounds/rbearing
    x-character-bounds/width
    x-close-all-displays
    x-colormap/allocate-color
    x-colormap/free
    x-colormap/query-color
    x-colormap/store-color
    x-colormap/store-colors
    x-colormap?
    x-display/name
    x-display/properties
    x-font-structure/all-chars-exist?
    x-font-structure/character-bounds
    x-font-structure/default-char
    x-font-structure/direction
    x-font-structure/max-ascent
    x-font-structure/max-bounds
    x-font-structure/max-descent
    x-font-structure/min-bounds
    x-font-structure/name
    x-font-structure/start-index
    x-geometry-string
    x-graphics-default-display-name
    x-graphics-default-geometry
    x-graphics-device-type
    x-graphics/clear
    x-graphics/close-display
    x-graphics/close-window
    x-graphics/color?
    x-graphics/coordinate-limits
    x-graphics/copy-area
    x-graphics/device-coordinate-limits
    x-graphics/disable-keyboard-focus
    x-graphics/discard-events
    x-graphics/display
    x-graphics/drag-cursor
    x-graphics/draw-arc
    x-graphics/draw-circle
    x-graphics/draw-line
    x-graphics/draw-lines
    x-graphics/draw-point
    x-graphics/draw-points
    x-graphics/draw-text
    x-graphics/enable-keyboard-focus
    x-graphics/fill-circle
    x-graphics/flush
    x-graphics/font-structure
    x-graphics/get-colormap
    x-graphics/get-default
    x-graphics/iconify-window
    x-graphics/image-depth
    x-graphics/lower-window
    x-graphics/map-window
    x-graphics/move-cursor
    x-graphics/move-window
    x-graphics/open-display
    x-graphics/open-display?
    x-graphics/open-window?
    x-graphics/query-pointer
    x-graphics/raise-window
    x-graphics/read-button
    x-graphics/read-user-event
    x-graphics/reset-clip-rectangle
    x-graphics/resize-window
    x-graphics/select-user-events
    x-graphics/set-background-color
    x-graphics/set-border-color
    x-graphics/set-border-width
    x-graphics/set-clip-rectangle
    x-graphics/set-colormap
    x-graphics/set-coordinate-limits
    x-graphics/set-drawing-mode
    x-graphics/set-font
    x-graphics/set-foreground-color
    x-graphics/set-icon-name
    x-graphics/set-input-hint
    x-graphics/set-internal-border-width
    x-graphics/set-line-style
    x-graphics/set-mouse-color
    x-graphics/set-mouse-shape
    x-graphics/set-window-name
    x-graphics/visual-info
    x-graphics/window-id
    x-graphics/withdraw-window
    x-graphics:auto-raise?
    x-image/destroy
    x-image/draw
    x-image/draw-subimage
    x-image/fill-from-byte-vector
    x-image/get-pixel
    x-image/height
    x-image/set-pixel
    x-image/width
    x-image?
    x-visual-class:direct-color
    x-visual-class:gray-scale
    x-visual-class:pseudo-color
    x-visual-class:static-color
    x-visual-class:static-gray
    x-visual-class:true-color
    x-visual-info/bits-per-rgb
    x-visual-info/blue-mask
    x-visual-info/class
    x-visual-info/colormap-size
    x-visual-info/depth
    x-visual-info/green-mask
    x-visual-info/red-mask
    x-visual-info/screen
    x-visual-info/visual
    x-visual-info/visual-id))

(define create-x-colormap)
(define create-x-image)
(define x-character-bounds/ascent)
(define x-character-bounds/descent)
(define x-character-bounds/lbearing)
(define x-character-bounds/rbearing)
(define x-character-bounds/width)
(define x-close-all-displays)
(define x-colormap/allocate-color)
(define x-colormap/free)
(define x-colormap/query-color)
(define x-colormap/store-color)
(define x-colormap/store-colors)
(define x-colormap?)
(define x-display/name)
(define x-display/properties)
(define x-font-structure/all-chars-exist?)
(define x-font-structure/character-bounds)
(define x-font-structure/default-char)
(define x-font-structure/direction)
(define x-font-structure/max-ascent)
(define x-font-structure/max-bounds)
(define x-font-structure/max-descent)
(define x-font-structure/min-bounds)
(define x-font-structure/name)
(define x-font-structure/start-index)
(define x-geometry-string)
(define x-graphics-default-display-name)
(define x-graphics-default-geometry)
(define x-graphics-device-type)
(define x-graphics/clear)
(define x-graphics/close-display)
(define x-graphics/close-window)
(define x-graphics/color?)
(define x-graphics/coordinate-limits)
(define x-graphics/copy-area)
(define x-graphics/device-coordinate-limits)
(define x-graphics/disable-keyboard-focus)
(define x-graphics/discard-events)
(define x-graphics/display)
(define x-graphics/drag-cursor)
(define x-graphics/draw-arc)
(define x-graphics/draw-circle)
(define x-graphics/draw-line)
(define x-graphics/draw-lines)
(define x-graphics/draw-point)
(define x-graphics/draw-points)
(define x-graphics/draw-text)
(define x-graphics/enable-keyboard-focus)
(define x-graphics/fill-circle)
(define x-graphics/flush)
(define x-graphics/font-structure)
(define x-graphics/get-colormap)
(define x-graphics/get-default)
(define x-graphics/iconify-window)
(define x-graphics/image-depth)
(define x-graphics/lower-window)
(define x-graphics/map-window)
(define x-graphics/move-cursor)
(define x-graphics/move-window)
(define x-graphics/open-display)
(define x-graphics/open-display?)
(define x-graphics/open-window?)
(define x-graphics/query-pointer)
(define x-graphics/raise-window)
(define x-graphics/read-button)
(define x-graphics/read-user-event)
(define x-graphics/reset-clip-rectangle)
(define x-graphics/resize-window)
(define x-graphics/select-user-events)
(define x-graphics/set-background-color)
(define x-graphics/set-border-color)
(define x-graphics/set-border-width)
(define x-graphics/set-clip-rectangle)
(define x-graphics/set-colormap)
(define x-graphics/set-coordinate-limits)
(define x-graphics/set-drawing-mode)
(define x-graphics/set-font)
(define x-graphics/set-foreground-color)
(define x-graphics/set-icon-name)
(define x-graphics/set-input-hint)
(define x-graphics/set-internal-border-width)
(define x-graphics/set-line-style)
(define x-graphics/set-mouse-color)
(define x-graphics/set-mouse-shape)
(define x-graphics/set-window-name)
(define x-graphics/visual-info)
(define x-graphics/window-id)
(define x-graphics/withdraw-window)
(define x-graphics:auto-raise?)
(define x-image/destroy)
(define x-image/draw)
(define x-image/draw-subimage)
(define x-image/fill-from-byte-vector)
(define x-image/get-pixel)
(define x-image/height)
(define x-image/set-pixel)
(define x-image/width)
(define x-image?)
(define x-visual-class:direct-color)
(define x-visual-class:gray-scale)
(define x-visual-class:pseudo-color)
(define x-visual-class:static-color)
(define x-visual-class:static-gray)
(define x-visual-class:true-color)
(define x-visual-info/bits-per-rgb)
(define x-visual-info/blue-mask)
(define x-visual-info/class)
(define x-visual-info/colormap-size)
(define x-visual-info/depth)
(define x-visual-info/green-mask)
(define x-visual-info/red-mask)
(define x-visual-info/screen)
(define x-visual-info/visual)
(define x-visual-info/visual-id)