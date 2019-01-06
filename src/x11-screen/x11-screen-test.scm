#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of an X11-screen plugin for MIT/GNU Scheme.

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

;;;; Test the x11-screen type.

(let ((x11s (->environment '(edwin screen x11-screen))))
  (let ((xterm ((access screen-xterm x11s) (selected-screen)))
	(xd ((access screen-display x11s) (selected-screen)))
	(get-xterm-property (access get-xterm-property x11s))
	(x-atom->symbol (access x-atom->symbol x11s)))
    (display "WM allowed actions:\n")
    (pp (vector-map
	 (lambda (atom) (x-atom->symbol xd atom))
	 (get-xterm-property xterm '_NET_WM_ALLOWED_ACTIONS 'atom #f)))
    (display "WM hints:\n")
    (pp (get-xterm-property xterm 'WM_HINTS 'wm_hints #f))
    (display "WM normal hints:\n")
    (pp (get-xterm-property xterm 'WM_NORMAL_HINTS 'wm_size_hints #f))))