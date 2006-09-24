#| -*-Scheme-*-

$Id: version.scm,v 14.221 2006/09/24 23:55:42 cph Exp $

Copyright 1988,1989,1990,1991,1992,1993 Massachusetts Institute of Technology
Copyright 1995,1996,1997,1998,1999,2000 Massachusetts Institute of Technology
Copyright 2001,2002,2003,2004,2005,2006 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Runtime System Version Information
;;; package: (runtime)

(declare (usual-integrations))

(define (initialize-package!)
  (add-subsystem-identification! "Release" '(7 7 90 "+"))
  (snarf-microcode-version!)
  (add-event-receiver! event:after-restore snarf-microcode-version!)
  (add-subsystem-identification! "Runtime" '(15 7)))

(define (snarf-microcode-version!)
  (add-subsystem-identification! "Microcode"
				 (get-microcode-version-numbers)))