#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/win32/wf_user.scm,v 1.1 1993/09/20 01:13:04 adams Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; win32 foreign functions
;;; package: (win32)

(declare (usual-integrations))


(define  arc)
(define  begin-paint)
(define  bit-blt)
(define  create-brush-indirect)
(define  create-compatible-bitmap)
(define  create-compatible-dc)
(define  create-palette)
(define  create-pen)
(define  create-rect-rgn)
(define  create-solid-brush)
(define  delete-dc)
(define  delete-object)
(define  debug-break)
(define  ellipse)
(define  end-paint)
(define  get-dc)
(define  get-device-caps)
(define  get-nearest-color)
(define  get-nearest-palette-index)
(define  get-rop2)
(define  get-stock-object)
(define  get-window-text-length)
(define  get-system-metrics)
(define  invalidate-rect)
(define  line-to)
(define  load-cursor)
(define  load-icon)
(define  move-to-ex)
(define  polygon)
(define  polyline)
(define  realize-palette)
(define  release-dc)
(define  rectangle)
(define  select-object)
(define  select-palette)
(define  select-clip-rgn)
(define  set-bk-color)
(define  set-bk-mode)
(define  set-cursor)
(define  set-focus)
(define  set-pixel)
(define  set-text-align)
(define  set-text-color)
(define  set-rop2)
(define  set-stretch-blt-mode)
(define  set-window-pos)
(define  stretch-blt)
(define  text-out)
(define  update-colors)
(define  update-window)

(define (init-wf_user!)

  (set!  arc
    (windows-procedure
	(Arc (hdc hdc) (leftrect int) (toprect int) (rightrect int) (bottomrect int)
	     (xstartarc int) (ystartarc int) (xendarc int) (yendarc int))
      bool gdi32.dll "Arc"))

  (set!  begin-paint
    (windows-procedure (begin-paint (hwnd hwnd) (ps paintstruct))
      hdc user32.dll "BeginPaint"))

  (set!  bit-blt
    (windows-procedure
	(bit-blt (dest hdc) (xdest int) (ydest int) (width int) (height int)
		 (src hdc) (xsrc int) (ysrc int) (rop dword))
      bool gdi32.dll "BitBlt"))

  (set!  create-brush-indirect
    (windows-procedure (create-brush-indirect (logbrush unchecked))
      hbrush gdi32.dll "CreateBrushIndirect"))

  (set!  create-compatible-bitmap
    (windows-procedure
	(create-compatible-bitmap (hdc hdc) (width int) (height int))
      hbitmap gdi32.dll "CreateCompatibleBitmap"))

  (set!  create-compatible-dc
    (windows-procedure (create-compatible-dc (hdc hdc))
      hdc gdi32.dll "CreateCompatibleDC"))

  (set!  create-palette
    (windows-procedure (create-palette (logpalette unchecked))
      hpalette gdi32.dll "CreatePalette"))

  (set!  create-pen
    (windows-procedure (create-pen (style int) (width int) (color colorref))
      hpen gdi32.dll "CreatePen"))

  (set!  create-rect-rgn
    (windows-procedure
	(create-rect-rgn (left int) (top int) (right int) (bottom int))
      hrgn gdi32.dll "CreateRectRgn"))

  (set!  create-solid-brush
    (windows-procedure (create-solid-brush (color colorref))
      hbrush gdi32.dll "CreateSolidBrush"))

  (set!  delete-dc
    (windows-procedure (delete-dc (hdc hdc)) bool gdi32.dll "DeleteDC"))

  (set!  delete-object
    (windows-procedure (delete-object (handle handle))
      bool gdi32.dll "DeleteObject"))

  (set!  debug-break
    (windows-procedure (debug-break) unchecked kernel32.dll "DebugBreak"))

  (set!  ellipse
    (windows-procedure
	(ellipse (hdc hdc) (left int) (top int) (right int) (bottom int))
      bool gdi32.dll "Ellipse"))

  (set!  end-paint
    (windows-procedure (end-paint (hwnd hwnd) (ps paintstruct))
      bool user32.dll "EndPaint"))

  (set!  get-dc
    (windows-procedure (get-dc (hwnd hwnd)) hdc user32.dll "GetDC"))

  (set!  get-device-caps
    (windows-procedure (get-device-caps (hdc hdc) (index int))
      int user32.dll "GetDeviceCaps"))

  (set!  get-nearest-color
    (windows-procedure (get-nearest-color (hdc hdc) (color colorref))
      colorref gdi32.dll "GetNearestColor"))

  (set!  get-nearest-palette-index
    (windows-procedure
	(get-nearest-palette-index (pal hpalette) (color colorref))
      uint gdi32.dll "GetNearestPaletteIndex"))

  (set!  get-rop2
    (windows-procedure (get-rop2 (hdc hdc)) int gdi32.dll "GetROP2"))

  (set!  get-stock-object
    (windows-procedure (get-stock-object (object int))
      hgdiobj gdi32.dll "GetStockObject"))

  (set!  get-window-text-length
    (windows-procedure (get-window-text-length (hdc hdc))
      int user32.dll "GetWindowTextLengthA"))

  (set!  get-system-metrics
    (windows-procedure (get-system-metrics (index int))
      int user32.dll "GetSystemMetrics"))

  (set!  invalidate-rect
    (windows-procedure (invalidate-rect (hwnd hwnd) (rect rect) (erase? bool))
      bool user32.dll "InvalidateRect"))

  (set!  line-to
    (windows-procedure (line-to (hdc hdc) (x int) (y int))
      bool gdi32.dll "LineTo"))

  (set!  load-cursor
    (windows-procedure (load-cursor (inst hinstance) (id resource-id))
      hcursor user32.dll "LoadCursorA"))

  (set!  load-icon
    (windows-procedure (load-icon (inst hinstance) (id resource-id))
      hicon user32.dll "LoadIconA"))

  (set!  move-to-ex
    (windows-procedure (move-to-ex (hdc hdc) (x int) (y int) (point unchecked))
      bool gdi32.dll "MoveToEx"))

  (set!  polygon
    (windows-procedure (polygon (hdc hdc) (points unchecked) (count int))
      bool gdi32.dll "Polygon"))

  (set!  polyline
    (windows-procedure (polyline (hdc hdc) (points unchecked) (count int))
      bool gdi32.dll "Polyline"))

  (set!  rectangle
    (windows-procedure
	(rectangle (hdc hdc) (left int) (top int) (right int) (bottom int))
      bool gdi32.dll "Rectangle"))

  (set!  realize-palette
    (windows-procedure (realize-palette (hdc hdc))
      uint gdi32.dll "RealizePalette"))

  (set!  release-dc
    (windows-procedure (release-dc (hwnd hwnd) (hdc hdc))
      int user32.dll "ReleaseDC"))

  (set!  select-object
    (windows-procedure (select-object (hdc hdc) (obj hgdiobj))
      hgdiobj gdi32.dll "SelectObject"))

  (set!  select-palette
    (windows-procedure
	(select-palette (hdc hdc) (pal hpalette) (force-background? bool))
      hpalette gdi32.dll "SelectPalette"))

  (set!  select-clip-rgn
    (windows-procedure (select-clip-rgn (hdc hdc) (region hrgn))
      int gdi32.dll "SelectClipRgn"))

  (set!  set-bk-color
    (windows-procedure (set-bk-color (hdc hdc) (color colorref))
      colorref gdi32.dll "SetBkColor"))

  (set!  set-bk-mode
    (windows-procedure (set-bk-mode (hdc hdc) (bkmode int))
      int gdi32.dll "SetBkMode"))

  (set!  set-cursor
    (windows-procedure (set-cursor (cur hcursor))
      hcursor user32.dll "SetCursor"))

  (set!  set-focus
    (windows-procedure (set-focus (hwnd hwnd)) hwnd user32.dll "SetFocus"))

  (set!  set-pixel
    (windows-procedure (set-pixel (hdc hdc) (x int) (y int) (color colorref))
      colorref gdi32.dll "SetPixel"))

  (set!  set-rop2
    (windows-procedure (set-rop2 (hdc hdc) (draw-mode int))
      int gdi32.dll "SetROP2"))

  (set!  set-stretch-blt-mode
    (windows-procedure (set-stretch-blt-mode (hdc hdc) (stretch-mode int))
      int gdi32.dll "SetStretchBltMode"))

  (set!  set-text-align
    (windows-procedure (set-text-align (hdc hdc) (mode uint))
      uint gdi32.dll "SetTextAlign"))

  (set!  set-text-color
    (windows-procedure (set-text-color (hdc hdc) (color colorref))
      colorref gdi32.dll "SetTextColor"))

  (set!  set-window-pos
    (windows-procedure
	(set-window-pos (hwnd hwnd) (insert-after hwnd)
			(x int) (y int) (cx int) (cy int) (flags uint))
      bool user32.dll "SetWindowPos"))

  (set!  stretch-blt
    (windows-procedure
	(stretch-blt
	 (dest hdc) (xdest int) (ydest int) (widthdest int) (heightdest int)
	 (src hdc) (xsrc int) (ysrc int) (widthsrc int) (heightsrc int)
	 (rop dword))
      bool gdi32.dll "StretchBlt"))

  (set!  text-out
    (windows-procedure
	(text-out (hdc hdc) (x int) (y int) (text string) (count int))
      bool gdi32.dll "TextOutA"))

  (set!  update-colors
    (windows-procedure (update-colors (hdc hdc))
      bool gdi32.dll "UpdateColors"))

  (set!  update-window
    (windows-procedure (update-window (hwnd hwnd))
      bool user32.dll "UpdateWindow"))

)
