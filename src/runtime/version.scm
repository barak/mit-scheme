#| -*-Scheme-*-

$Id: version.scm,v 14.207 2002/03/12 20:33:24 cph Exp $

Copyright (c) 1988-2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Runtime System Version Information
;;; package: (runtime)

(declare (usual-integrations))

(define (initialize-package!)
  (add-subsystem-identification! "Release" '(7 7 0))
  (snarf-microcode-version!)
  (add-event-receiver! event:after-restore snarf-microcode-version!)
  (add-subsystem-identification! "Runtime" '(15 0)))

(define (snarf-microcode-version!)
  (add-subsystem-identification! "Microcode"
				 (list microcode-id/version
				       microcode-id/modification)))