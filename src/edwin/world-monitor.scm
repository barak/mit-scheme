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

;;;; Create a *World* buffer containing the world report.

(declare (usual-integrations))

(define-command monitor-world
  "Find or create a buffer named *World*, insert a world report into
it, and spawn a thread to update it after every
\\[world-monitor-sleep] seconds."
  ()
  (lambda ()
    (let* ((buffer (find-or-create-buffer "*World*"))
	   (thread (buffer-get buffer 'WORLD-MONITOR #f)))
      (if (not thread)
	  (start-world-monitor buffer))
      (pop-up-buffer buffer #f))))

(define-variable world-monitor-sleep
  "Number of seconds the world monitor should sleep between reports."
  1.
  (lambda (object)
    (and (flo:flonum? object)
	 (flo:< .09 object))))

(define (start-world-monitor buffer)
  (set-buffer-major-mode! buffer (ref-mode-object read-only))
  (local-set-variable! truncate-lines #t buffer)
  (let ((registration #f)
	(report #f))

    (define (new-report)
      (call-with-output-string
	(lambda (port)
	  (world-report port))))

    (define (sleep)
      (sleep-current-thread
       (flo:round->exact
	(flo:* 1000. (ref-variable world-monitor-sleep)))))

    (let ((monitor
	   (create-thread editor-thread-root-continuation
	     (named-lambda (monitor-world)
	       (let loop ()
		 (sleep)
		 (if (buffer-alive? buffer)
		     (begin
		       (if registration
			   (begin
			     (set! report (new-report))
			     (inferior-thread-output! registration)))
		       (loop))
		     (begin
		       (if registration
			   (deregister-inferior-thread! registration))
		       (set! registration #f)
		       (exit-current-thread #t)))))
	     buffer)))
      (buffer-put! buffer 'WORLD-MONITOR monitor)
      (update-world-monitor! buffer (new-report))
      (set-buffer-point! buffer (buffer-start buffer))
      (set! registration
	    (register-inferior-thread!
	     monitor (named-lambda (world-monitor-output!)
		       (update-world-monitor! buffer report)))))))

(define (update-world-monitor! buffer report)
  (let ((saved-windows
	 (map (lambda (window)
		(cons window
		      (cons (mark-temporary-copy (window-point window))
			    (window-start-mark window) ;already temporary
			    )))
	      (buffer-windows buffer)))
	(saved-mark (let ((ring (buffer-mark-ring buffer)))
		      (if (ring-empty? ring)
			  #f
			  (mark-temporary-copy (ring-ref ring 0))))))
    (with-read-only-defeated
     buffer (lambda ()
	      (with-group-undo-disabled
	       (buffer-group buffer)
	       (lambda ()
		 (buffer-widen! buffer)
		 (region-delete! (buffer-region buffer))
		 (insert-string report (buffer-start buffer))
		 (buffer-not-modified! buffer)))))
    (for-each (lambda (item)
		(let ((window (car item))
		      (point (cadr item))
		      (start (cddr item)))
		  (set-window-point! window point)
		  (set-window-start-mark! window start 0)))
	      saved-windows)
    (if saved-mark (set-buffer-mark! buffer saved-mark))))