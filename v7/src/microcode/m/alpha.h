/* -*-C-*-
   Machine file for DEC Alpha computers.

$Id: alpha.h,v 1.3 1992/11/18 15:31:25 gjr Exp $

Copyright (c) 1992 Digital Equipment Corporation

*/

#ifndef PROC_TYPE
#define PROC_TYPE PROC_TYPE_ALPHA
#endif /* PROC_TYPE */

/* The following crock seems to be needed, since ULTRIX on the vax has
   libg but ULTRIX on the pmax doesn't! 
 */

#define LIB_DEBUG

#define C_SWITCH_MACHINE -Dalpha
