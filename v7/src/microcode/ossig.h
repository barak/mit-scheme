/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/ossig.h,v 1.2 1992/05/05 06:57:20 jinx Exp $

Copyright (c) 1990-1992 Massachusetts Institute of Technology

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

#ifndef SCM_OSSIG_H
#define SCM_OSSIG_H

#include "os.h"

enum interrupt_handler
{
  interrupt_handler_ignore,
  interrupt_handler_default,
  interrupt_handler_unknown,
  interrupt_handler_terminate,
  interrupt_handler_stop,
  interrupt_handler_interactive,
  interrupt_handler_control_b,
  interrupt_handler_control_g,
  interrupt_handler_control_u,
  interrupt_handler_control_x
};

#endif /* SCM_OSSIG_H */
