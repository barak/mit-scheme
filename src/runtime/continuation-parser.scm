#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Continuation Parser
;;; package: (runtime new-continuation-parser)

(declare (usual-integrations))

(add-boot-deps! '(runtime microcode-tables) '(runtime history))

(define (continuation->stack-frame* continuation)
  (parse-control-point (continuation/control-point continuation)
		       (continuation/dynamic-state continuation)
		       (continuation/block-thread-events? continuation)))

(define (parse-control-point control-point dynamic-state block-thread-events?)
  (let ((frames
	 (generator->stream (control-point->frame-generator control-point))))
    (parse-one-frame
     (make-pstate frames dynamic-state block-thread-events? #f #f #f #f))))

(define (parse-one-frame pstate)
  (let ((frames (pstate-frames pstate)))
    (and (stream-pair? frames)
	 (let ((frame (stream-car frames)))
	   (case (cpoint-frame-type frame)
	     ((restore-interrupt-mask cc-restore-interrupt-mask)
	      (parse-one-frame
	       (pstate-new-interrupt-mask pstate
		 (cpoint-frame-ref frame 'interrupt-mask))))
	     ((restore-history restore-dont-copy-history)
	      (parse-one-frame
	       (pstate-new-history pstate
		 (history-transform (cpoint-frame-ref frame 'history))
		 (cpoint-frame-ref frame 'next-history-offset))))
	     ((stack-marker cc-stack-marker)
	      (let ((marker-type (cpoint-frame-ref frame 'marker-type))
		    (marker-instance (cpoint-frame-ref frame 'marker-instance)))
		(cond ((eq? marker-type %translate-to-state-point)
		       (parse-one-frame
			(pstate-new-dynamic-state pstate
			  (merge-dynamic-state (pstate-dynamic-state pstate)
					       marker-instance))))
		      ((eq? marker-type 'set-interrupt-enables!)
		       (parse-one-frame
			(pstate-new-interrupt-mask pstate
			  marker-instance)))
		      ((eq? marker-type 'with-thread-events-blocked)
		       (parse-one-frame
			(pstate-new-block-thread-events? pstate
			  marker-instance)))
		      (else
		       (emit-frame frame pstate)))))
	     (else
	      (emit-frame frame pstate)))))))

(define (emit-frame cpoint-frame pstate)
  (make-stack-frame cpoint-frame
		    pstate
		    (delay (parse-one-frame (pstate-next-frame pstate)))))

(define-record-type <pstate>
    make-pstate
    pstate?
  (frames pstate-frames)
  (dynamic-state pstate-dynamic-state)
  (block-thread-events? pstate-block-thread-events?)
  (interrupt-mask pstate-interrupt-mask)
  (history pstate-history)
  (history-offset pstate-history-offset)
  (previous-type pstate-previous-type))

(define (pstate-next-frame pstate)
  (make-pstate (stream-cdr (pstate-frames pstate))
	       (pstate-dynamic-state pstate)
	       (pstate-block-thread-events? pstate)
	       (pstate-interrupt-mask pstate)
	       (pstate-history pstate)
	       (pstate-history-offset pstate)
	       (cpoint-frame-type (stream-car (pstate-frames pstate)))))

(define (pstate-new-dynamic-state pstate dynamic-state)
  (make-pstate (stream-cdr (pstate-frames pstate))
	       dynamic-state
	       (pstate-block-thread-events? pstate)
	       (pstate-interrupt-mask pstate)
	       (pstate-history pstate)
	       (pstate-history-offset pstate)
	       (pstate-previous-type pstate)))

(define (pstate-new-block-thread-events? pstate block-thread-events?)
  (make-pstate (stream-cdr (pstate-frames pstate))
	       (pstate-dynamic-state pstate)
	       block-thread-events?
	       (pstate-interrupt-mask pstate)
	       (pstate-history pstate)
	       (pstate-history-offset pstate)
	       (pstate-previous-type pstate)))

(define (pstate-new-interrupt-mask pstate interrupt-mask)
  (make-pstate (stream-cdr (pstate-frames pstate))
	       (pstate-dynamic-state pstate)
	       (pstate-block-thread-events? pstate)
	       interrupt-mask
	       (pstate-history pstate)
	       (pstate-history-offset pstate)
	       (pstate-previous-type pstate)))

(define (pstate-new-history pstate history history-offset)
  (make-pstate (stream-cdr (pstate-frames pstate))
	       (pstate-dynamic-state pstate)
	       (pstate-block-thread-events? pstate)
	       (pstate-interrupt-mask pstate)
	       history
	       history-offset
	       (pstate-previous-type pstate)))

;;; Local Variables:
;;; eval: (put 'pstate-new-dynamic-state 'scheme-indent-hook 1)
;;; eval: (put 'pstate-new-block-thread-events? 'scheme-indent-hook 1)
;;; eval: (put 'pstate-new-interrupt-mask 'scheme-indent-hook 1)
;;; eval: (put 'pstate-new-history 'scheme-indent-hook 1)
;;; End:
