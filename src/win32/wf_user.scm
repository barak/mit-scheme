#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; win32 foreign functions
;;; package: (win32)

(declare (usual-integrations))

(define  adjust-window-rect)
(define  append-menu)
(define  arc)
(define  begin-paint)
(define  bit-blt)
(define  check-menu-item)
(define  close-clipboard)
(define  copy-memory)
(define  create-brush-indirect)
(define  create-compatible-bitmap)
(define  create-compatible-dc)
(define  create-menu)
(define  create-popup-menu)
(define  create-palette)
(define  create-pen)
(define  create-rect-rgn)
(define  create-solid-brush)
(define  debug-break)
(define  delete-dc)
(define  delete-menu)
(define  delete-object)
(define  destroy-menu)
(define  destroy-window)
(define  draw-menu-bar)
(define  ellipse)
(define  empty-clipboard)
(define  enable-menu-item)
(define  end-paint)
(define  get-client-rect)
(define  get-clipboard-data)
(define  get-dc)
(define  get-device-caps)
(define  get-focus)
(define  get-last-error)
(define  get-menu)
(define  get-menu-check-mark-dimensions)
(define  get-menu-item-count)
(define  get-menu-item-id)
(define  get-menu-state)
(define  get-menu-string)
(define  get-module-file-name)
(define  get-nearest-color)
(define  get-nearest-palette-index)
(define  get-rop2)
(define  get-stock-object)
(define  get-sub-menu)
(define  get-system-menu)
(define  get-system-metrics)
(define  get-window-rect)
(define  get-window-text-length)
(define  global-alloc)
(define  global-lock)
(define  global-size)
(define  global-unlock)
(define  hilite-menu-item)
(define  insert-menu)
(define  invalidate-rect)
(define  is-menu?)
(define  line-to)
(define  load-cursor)
(define  load-icon)
(define  load-menu)
(define  load-menu-indirect)
(define  move-to-ex)
(define  modify-menu)
(define  open-clipboard)
(define  polygon)
(define  polyline)
(define  realize-palette)
(define  rectangle)
(define  release-dc)
(define  remove-menu)
(define  select-object)
(define  select-palette)
(define  select-clip-rgn)
(define  set-active-window)
(define  set-bk-color)
(define  set-bk-mode)
(define  set-clipboard-data)
(define  set-cursor)
(define  set-focus)
(define  set-menu)
(define  set-menu-item-bitmaps)
(define  set-pixel)
(define  set-text-align)
(define  set-text-color)
(define  set-rop2)
(define  set-stretch-blt-mode)
(define  set-window-pos)
(define  show-window)
(define  stretch-blt)
(define  text-out)
(define  track-popup-menu)
(define  update-colors)
(define  update-window)

(define (init-wf_user!)

  (set!  arc
    (windows-procedure
	(Arc (hdc hdc)
	     (leftrect int) (toprect int) (rightrect int) (bottomrect int)
	     (xstartarc int) (ystartarc int) (xendarc int) (yendarc int))
      bool gdi32.dll "Arc"))

  (set!  append-menu
    (windows-procedure
	(append-menu (menu hmenu)
		     (mf_flags uint) (kind uint) (newitem unchecked))
      bool user32.dll "AppendMenuA"))

  (set!  begin-paint
    (windows-procedure (begin-paint (hwnd hwnd) (ps paintstruct))
      hdc user32.dll "BeginPaint"))

  (set!  bit-blt
    (windows-procedure
	(bit-blt (dest hdc) (xdest int) (ydest int) (width int) (height int)
		 (src hdc) (xsrc int) (ysrc int) (rop dword))
      bool gdi32.dll "BitBlt"))

  (set!  check-menu-item
    (windows-procedure (check-menu-item (menu hmenu) (item uint) (flags uint))
      dword user32.dll "CheckMenuItem"))

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

  (set!  create-menu
    (windows-procedure (create-menu) hmenu user32.dll "CreateMenu"))

  (set!  create-palette
    (windows-procedure (create-palette (logpalette unchecked))
      hpalette gdi32.dll "CreatePalette"))

  (set!  create-pen
    (windows-procedure (create-pen (style int) (width int) (color colorref))
      hpen gdi32.dll "CreatePen"))

  (set!  create-popup-menu
    (windows-procedure (create-popup-menu) hmenu user32.dll "CreatePopupMenu"))

  (set!  create-rect-rgn
    (windows-procedure
	(create-rect-rgn (left int) (top int) (right int) (bottom int))
      hrgn gdi32.dll "CreateRectRgn"))

  (set!  create-solid-brush
    (windows-procedure (create-solid-brush (color colorref))
      hbrush gdi32.dll "CreateSolidBrush"))

  (set!  debug-break
    (windows-procedure (debug-break) unchecked kernel32.dll "DebugBreak"))

  (set!  delete-dc
    (windows-procedure (delete-dc (hdc hdc)) bool gdi32.dll "DeleteDC"))

  (set!  delete-menu
    (windows-procedure (delete-menu (menu hmenu) (item uint) (flags uint))
      bool user32.dll "DeleteMenu"))

  (set!  delete-object
    (windows-procedure (delete-object (handle handle))
      bool gdi32.dll "DeleteObject"))

  (set!  destroy-menu
    (windows-procedure (destroy-menu (menu hmenu))
      bool user32.dll "DestroyMenu"))

  (set!  destroy-window
    (windows-procedure (destroy-window (window hwnd))
      bool user32.dll "DestroyWindow"))

  (set!  draw-menu-bar
    (windows-procedure (draw-menu-bar (window hwnd))
      bool user32.dll "DrawMenuBar"))

  (set!  ellipse
    (windows-procedure
	(ellipse (hdc hdc) (left int) (top int) (right int) (bottom int))
      bool gdi32.dll "Ellipse"))

  (set!  enable-menu-item
    (windows-procedure (enable-menu-item (menu hmenu) (item uint) (flags uint))
      bool user32.dll "EnableMenuItem"))

  (set!  end-paint
    (windows-procedure (end-paint (hwnd hwnd) (ps paintstruct))
      bool user32.dll "EndPaint"))

  (set!  get-dc
    (windows-procedure (get-dc (hwnd hwnd)) hdc user32.dll "GetDC"))

  (set!  get-device-caps
    (windows-procedure (get-device-caps (hdc hdc) (index int))
      int gdi32.dll "GetDeviceCaps"))

  (set!  get-focus
    (windows-procedure (get-focus) hwnd user32.dll "SetFocus"))

  (set!  get-menu
    (windows-procedure (get-menu (window hwnd))
      hmenu user32.dll "GetMenu"))

  (set!  get-menu-check-mark-dimensions
    (windows-procedure (get-menu-check-mark-dimensions)
      long user32.dll "GetMenuCheckMarkDimensions"))

  (set!  get-menu-item-count
    (windows-procedure (get-menu-item-count (menu hmenu)) 
      int user32.dll "GetMenuItemCount"))

  (set!  get-menu-item-id
    (windows-procedure (get-menu-item-id (menu hmenu) (pos int)) 
      int user32.dll "GetMenuItemID"))

  (set!  get-menu-state
    (windows-procedure (get-menu-state (menu hmenu) (item uint) (flags uint))
      uint user32.dll "GetMenuState"))

  (set!  get-menu-string
    (windows-procedure
	(get-menu-string (menu hmenu) (item uint) (buffer string)
			      (max-chars int) (flags uint))
      int user32.dll "GetMenuStringA"))

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

  (set!  get-sub-menu
    (windows-procedure (get-sub-menu (hmenu hmenu) (pos int))
      hmenu user32.dll "GetSubMenu"))

  (set!  get-system-menu
    (windows-procedure (get-system-menu (window hwnd) (revert? bool))
      hmenu user32.dll "GetSystemMenu"))

  (set!  get-system-metrics
    (windows-procedure (get-system-metrics (index int))
      int user32.dll "GetSystemMetrics"))

  (set!  adjust-window-rect
    (windows-procedure (adjust-window-rect (rect rect)
					   (style dword)
					   (menu? bool))
      bool user32.dll "AdjustWindowRect"))

  (set!  get-client-rect
    (windows-procedure (get-client-rect (window hwnd) (rect rect))
      bool user32.dll "GetClientRect"))

  (set!  get-window-rect
    (windows-procedure (get-window-rect (window hwnd) (rect rect))
      bool user32.dll "GetWindowRect"))

  (set!  get-window-text-length
    (windows-procedure (get-window-text-length (hdc hdc))
      int user32.dll "GetWindowTextLengthA"))

  (set!  hilite-menu-item
    (windows-procedure (hilite-menu-item (hwnd hwnd) (hmenu hmenu)
					 (itemhilite uint) (hilite-flags uint))
      bool user32.dll "HiliteMenuItem"))

  (set!  insert-menu
    (windows-procedure
	(insert-menu (menu hmenu) (item uint) (flags uint)
		     (idnewitem unchecked) (newitem unchecked))
      bool user32.dll "InsertMenuA"))

  (set!  invalidate-rect
    (windows-procedure (invalidate-rect (hwnd hwnd) (rect rect) (erase? bool))
      bool user32.dll "InvalidateRect"))

  (set!  is-menu?
    (windows-procedure (is-menu? (handle hmenu)) bool user32.dll "IsMenu"))

  (set!  line-to
    (windows-procedure (line-to (hdc hdc) (x int) (y int))
      bool gdi32.dll "LineTo"))

  (set!  load-cursor
    (windows-procedure (load-cursor (inst hinstance) (id resource-id))
      hcursor user32.dll "LoadCursorA"))

  (set!  load-icon
    (windows-procedure (load-icon (inst hinstance) (id resource-id))
      hicon user32.dll "LoadIconA"))

  (set!  load-menu
    (windows-procedure (load-menu (inst hinstance) (id resource-id))
      hmenu user32.dll "LoadMenuA"))

  (set!  load-menu-indirect
    (windows-procedure (load-menu-indirect (menu-template unchecked))
      hmenu user32.dll "LoadMenuIndirectA"))

  (set!  move-to-ex
    (windows-procedure (move-to-ex (hdc hdc) (x int) (y int) (point unchecked))
      bool gdi32.dll "MoveToEx"))

  (set!  modify-menu
    (windows-procedure
	(modify-menu (hmenu hmenu) (item uint) (flags uint)
		     (idnewitem uint) (newitem unchecked))
      bool user32.dll "ModifyMenu"))

  (set!  polygon
    (windows-procedure (polygon (hdc hdc) (points unchecked) (count int))
      bool gdi32.dll "Polygon"))

  (set!  polyline
    (windows-procedure (polyline (hdc hdc) (points unchecked) (count int))
      bool gdi32.dll "Polyline"))

  (set!  realize-palette
    (windows-procedure (realize-palette (hdc hdc))
      uint gdi32.dll "RealizePalette"))

  (set!  rectangle
    (windows-procedure
	(rectangle (hdc hdc) (left int) (top int) (right int) (bottom int))
      bool gdi32.dll "Rectangle"))

  (set!  release-dc
    (windows-procedure (release-dc (hwnd hwnd) (hdc hdc))
      int user32.dll "ReleaseDC"))

  (set!  remove-menu
    (windows-procedure (remove-menu (hmenu hmenu) (item uint) (flags uint))
      bool user32.dll "RemoveMenu"))

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

  (set!  set-active-window
    (windows-procedure (set-active-window (hwnd hwnd))
      hwnd user32.dll "SetActiveWindow"))

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

  (set!  set-menu
    (windows-procedure (set-menu (hwnd hwnd) (hmenu hmenu))
      bool user32.dll "SetMenu"))

  (set!  set-menu-item-bitmaps
    (windows-procedure
	(set-menu-item-bitmaps (hmenu hmenu) (item uint) (flags uint)
			       (bm-unchecked hbitmap) (bm-checked hbitmap))
      bool user32.dll "SetMenuItemBitmaps"))

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

  (set!  show-window
    (windows-procedure (show-window (hwnd hwnd) (nCmdShow int))
     bool user32.dll "ShowWindow"))

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

  (set!  track-popup-menu
    (windows-procedure
	(track-popup-menu (hmenu hmenu) (flags uint) (x int) (y int)
			  (reserved int) (hwnd hwnd) (rect rect))
      bool user32.dll "TrackPopupMenu"))

  (set!  update-colors
    (windows-procedure (update-colors (hdc hdc))
      bool gdi32.dll "UpdateColors"))

  (set!  update-window
    (windows-procedure (update-window (hwnd hwnd))
      bool user32.dll "UpdateWindow"))


  (set! open-clipboard
    (windows-procedure (open-clipboard (hwnd hwnd))
      bool user32.dll "OpenClipboard"))

  (set! close-clipboard
    (windows-procedure (close-clipboard)
      bool user32.dll "CloseClipboard"))

  (set! set-clipboard-data
    (windows-procedure (set-clipboard-data (format uint) (hdata handle))
      handle user32.dll "SetClipboardData"))

  (set! get-clipboard-data
    (windows-procedure (get-clipboard-data (format uint))
      handle user32.dll "GetClipboardData"))

  (set! empty-clipboard
    (windows-procedure (empty-clipboard)
      bool user32.dll "EmptyClipboard"))


  (set! global-alloc
    (windows-procedure (global-alloc (fuFlags uint) (cbBytes dword))
      handle kernel32.dll "GlobalAlloc"))

  (set! global-lock
    (windows-procedure (global-lock (hglbMem handle))
      uint kernel32.dll "GlobalLock"))

  (set! global-unlock
    (windows-procedure (global-unlock (hglbMem handle))
      bool kernel32.dll "GlobalUnlock"))

  (set! global-size
    (windows-procedure (global-size (hglbMem handle))
      dword kernel32.dll "GlobalSize"))

  (set! copy-memory
    (windows-procedure (copy-memory (destination unchecked) (source unchecked)
				    (length dword))
      bool kernel32.dll "RtlMoveMemory"))


  (set! get-last-error
    (windows-procedure (get-last-error)
      dword kernel32.dll "GetLastError"))

  (set! get-module-file-name
    (windows-procedure (get-module-file-name (module handle)
					     (name string)
					     (strlen int))
      int kernel32.dll "GetModuleFileNameA"))


  unspecific)
