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

;;;; World Report
;;; package: (runtime world-report)

(declare (usual-integrations))

(define (world-report #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee textual-output-port? port 'world-report)))
	(now (get-universal-time))
	(cpu (process-time-clock)))
    (write-string "-*-Outline-*-" port)
    (newline port)
    (newline port)
    (write-string "Time:  " port)
    (write-string (universal-time->local-time-string now) port)
    (write-string "  Process: " port)
    (write-padded-flonum (ticks->string cpu) 5 2 port)
    (newline port)
    (write-string "Start: " port)
    (write-string (universal-time->local-time-string time-world-restored) port)
    (write-string "  Up time: " port)
    (write-time-interval (- now time-world-restored) port)
    (newline port)
    (memory-report port)
    (thread-report port)))

(define (ticks->string ticks)
  (parameterize ((param:flonum-printer-cutoff '(absolute 3)))
    (number->string (internal-time/ticks->seconds ticks) 10)))

(define (write-time-interval secs port)
  (let ((min/sec (integer-divide secs 60)))
    (let ((seconds (integer-divide-remainder min/sec))
	  (hr/min (integer-divide (integer-divide-quotient min/sec) 60)))
      (let ((minutes (integer-divide-remainder hr/min))
	    (hours (integer-divide-quotient hr/min)))
	(if (< hours 10) (write-char #\0 port))
	(write-string (number->string hours 10) port)
	(write-char #\: port)
	(if (< minutes 10) (write-char #\0 port))
	(write-string (number->string minutes 10) port)
	(write-char #\: port))
      (if (< seconds 10) (write-char #\0 port))
      (write-string (number->string seconds 10) port))))

(define (memory-report port)
  (newline port)
  (write-string "* Memory" port)
  (newline port)
  (newline port)
  (let ((status (gc-space-status)))
    (let ((bytes/word (vector-ref status 0))
	  (heap (- (vector-ref status 6)     ; heap_alloc_limit
		   (vector-ref status 4)))   ; heap_start
	  (const (- (vector-ref status 3)    ; constant_end
		    (vector-ref status 1)))) ; constant_start
      (let ((width (string-length
		    (number->string (quotient (+ heap const) bytes/word)))))

	(define (write-size prefix high low)
	  (write-string prefix port)
	  (write-padded (number->string (quotient (- (vector-ref status high)
						     (vector-ref status low))
						  bytes/word))
			width port)
	  (write-string " words" port))

	(write-size "Constant: " 3 1) ; constant_end - constant_start
	(newline port)
	(write-size "Heap:     " 6 4) ; heap_alloc_limit - heap_start
	(newline port)
	(write-size "Free:     " 6 5) ; heap_alloc_limit - Free
	(write-string "  " port)
	(write-free-bar status 32 port)
	(newline port)
	(let loop ((i 0)
		   (stats (reverse! (gc-statistics))))
	  (if (and (pair? stats)
		   (fix:< i 3))
	      (begin
		(write-string (gc-statistic->string (car stats)) port)
		(newline port)
		(loop (fix:1+ i) (cdr stats)))))))))

(define (write-padded string width port)
  (let loop ((length (string-length string)))
    (if (< length width)
	(begin
	  (write-char #\space port)
	  (loop (1+ length)))))
  (write-string string port))

(define (write-free-bar status width port)
  (let ((ratio (/
		(- (vector-ref status 6)	  ; heap_alloc_limit
		   (vector-ref status 5))	  ; Free
		(- (vector-ref status 6)	  ; heap_alloc_limit
		   (vector-ref status 4))	  ; heap_start
		)))
    (let ((length (round->exact (* ratio width))))
      (let loop ((n 0))
	(if (< n length)
	    (begin
	      (write-char #\* port)
	      (loop (1+ n)))))
      (let loop ((n length))
	(if (< n width)
	    (begin
	      (write-char #\- port)
	      (loop (1+ n))))))))

(define (thread-report port)
  (newline port)
  (write-string "* Threads" port)
  (newline port)
  (newline port)
  (for-each
    (lambda (item)
      (let ((thread (cdr item)))
	(write-string (write-to-string thread) port)
	(write-char #\tab port)
	(write-state thread port)
	(write-char #\space port)
	(write-time (thread/process-time thread) port)
	(write-string " CPU, " port)
	(write-time (thread/real-time thread) port)
	(write-string " real" port)
	(let ((name (thread-get thread 'name)))
	  (if name
	      (begin
		(write-char #\space port)
		(write name port))))
	(newline port)))
    (sort (map (lambda (t) (cons (hash-object t) t)) (threads-list))
	  (lambda (a b) (< (car a) (car b))))))

(define (write-state thread port)
  (write-string (case (thread-execution-state thread)
		  ((running)	"running")
		  ((dead)	"  dead ")
		  ((waiting)	"waiting")
		  ((stopped)	"stopped")
		  ((running-without-preemption) "RUNNING")
		  (else "   ????"))
		port))

(define (write-time ticks port)
  (write-padded-flonum (ticks->string ticks) 3 3 port))

(define (write-padded-flonum string columns-before-dot zeros-after-dot port)
  (let ((index (string-find-next-char string #\.))
	(length (string-length string)))
    (let loop ((columns index))
      (if (< columns columns-before-dot)
	  (begin
	    (write-char #\space port)
	    (loop (1+ columns)))))
    (write-string string port)
    (let loop ((after-dot (- length (1+ index))))
      (if (< after-dot zeros-after-dot)
	  (begin
	    (write-char #\0 port)
	    (loop (1+ after-dot)))))))