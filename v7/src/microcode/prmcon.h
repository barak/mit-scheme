/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/prmcon.h,v 1.1 1990/11/21 07:00:30 jinx Rel $

Copyright (c) 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

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

SCHEME_OBJECT EXFUN (continue_fasload, (SCHEME_OBJECT *reentry_record));

static
SCHEME_OBJECT (* (continuation_procedures []))() = {
  continue_fasload
};

#endif /* SCM_PRMCON_C */

#endif /* SCM_PRMCON_H */
