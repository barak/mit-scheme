/* -*-C-*-
   System file for DEC OSF1

$Id: decosf.h,v 1.2 1992/08/29 12:47:54 jinx Exp $

Copyright (C) 1992  Digital Equipment Corporation */

#define LIB_DEBUG		/* no -lg */

/* AUTOCLOBBER_BUG temporarily defined (until BL10) and release. */

#define C_SWITCH_SYSTEM -std1 -DAUTOCLOBBER_BUG
