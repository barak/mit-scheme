#| -*-Scheme-*-

$Id: kryptdum.scm,v 1.2 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1994, 1999 Massachusetts Institute of Technology

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

;;;; Encryption/Decryption dummy stubs
;;; package: (runtime krypt)

(declare (usual-integrations))

(define (encrypt string password)
  password
  string)

(define (decrypt string password #!optional password-error checksum-error)
  password password-error checksum-error
  string)