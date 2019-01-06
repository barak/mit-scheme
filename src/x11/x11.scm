#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of an x11 plugin for MIT/GNU Scheme.

This plugin is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This plugin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this plugin; if not, write to the Free Software Foundation,
Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

|#

;;;; X11 interface
;;; package: (x11)

(define (import-x11)
  (let ((target-environment (nearest-repl/environment))
	(source-environment (->environment '(x11))))
    (for-each (lambda (name)
		(link-variables target-environment name
				source-environment name))
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
		x-graphics/available?
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
		x-visual-info/visual-id))))