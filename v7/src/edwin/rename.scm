;;; -*-Scheme-*-
;;;
;;; $Id: rename.scm,v 1.7 1999/01/02 06:11:34 cph Exp $
;;;
;;; Copyright (c) 1989-1999 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Edwin Interpackage Renames

(declare (usual-integrations))

(let ((global (->environment '()))
      (edwin (->environment '(edwin)))
      (window (->environment '(edwin window))))
  (let ((g<-e
	 (lambda (g e)
	   (if (not (lexical-unreferenceable? edwin e))
	       (local-assignment global g (lexical-reference edwin e))))))
    (g<-e 'save-editor-files 'debug-save-files))
  (let ((e<-w
	 (lambda (e w)
	   (if (not (lexical-unreferenceable? window w))
	       (lexical-assignment edwin e (lexical-reference window w))))))
    (e<-w 'window? 'buffer-frame?)
    (e<-w 'window-x-size 'buffer-frame-x-size)
    (e<-w 'window-y-size 'buffer-frame-y-size)
    (e<-w 'window-needs-redisplay? 'buffer-frame-needs-redisplay?)))