#| -*-Scheme-*-

$Id: version.scm,v 14.188 2000/10/14 00:57:28 cph Exp $

Copyright (c) 1988-2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Runtime System Version Information
;;; package: (runtime)

(declare (usual-integrations))

(define (initialize-package!)
  (add-subsystem-identification! "Release" '(7 5 10))
  (snarf-microcode-version!)
  (add-event-receiver! event:after-restore snarf-microcode-version!)
  (add-subsystem-identification! "Runtime" '(14 184)))

(define (snarf-microcode-version!)
  (add-subsystem-identification! "Microcode"
				 (list microcode-id/version
				       microcode-id/modification)))