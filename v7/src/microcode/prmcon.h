/* -*-C-*-

$Id: prmcon.h,v 1.6 2003/02/14 18:28:22 cph Exp $

Copyright (c) 1990, 1999, 2000 Massachusetts Institute of Technology

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

*/

#ifndef SCM_PRMCON_H

#define SCM_PRMCON_H

SCHEME_OBJECT EXFUN (continue_primitive, (void));

void EXFUN (suspend_primitive,
	    (int continuation, int reentry_record_length,
	     SCHEME_OBJECT *reentry_record));

void EXFUN (immediate_interrupt, (void));

void EXFUN (immediate_error, (long error_code));

/* The tables below should be built automagically (by Findprim?).
   This is a temporary (or permanent) kludge.
 */

/* For each continuable primitive, there should be a constant,
   and an entry in the table below.

   IMPORTANT: Primitives that can be suspended must use
   PRIMITIVE_CANONICALIZE_CONTEXT at entry!
 */

#define CONT_FASLOAD			0

#define CONT_MAX_INDEX			0

#ifdef SCM_PRMCON_C

SCHEME_OBJECT EXFUN (continue_fasload, (SCHEME_OBJECT *));

static SCHEME_OBJECT EXFUN
  ((* (continuation_procedures [])), (SCHEME_OBJECT *)) = {
  continue_fasload
};

#endif /* SCM_PRMCON_C */

#endif /* SCM_PRMCON_H */
