/* -*-C-*-
   Machine file for DEC Alpha computers.

$Id: alpha.h,v 1.2 1992/09/26 02:46:49 cph Exp $

Copyright (c) 1992 Digital Equipment Corporation

*/

#define PROC_TYPE PROC_TYPE_ALPHA

/* The following crock seems to be needed, since ULTRIX on the vax has
   libg but ULTRIX on the pmax doesn't! 
 */

#define LIB_DEBUG

#define C_SWITCH_MACHINE -Dalpha
