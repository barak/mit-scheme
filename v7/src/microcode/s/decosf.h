/* -*-C-*-
   System file for DEC OSF1

$Id: decosf.h,v 1.4 1993/11/21 18:55:14 gjr Exp $

Copyright (C) 1992-1993  Digital Equipment Corporation */

#define LIB_DEBUG		/* no -lg */

/* AUTOCLOBBER_BUG temporarily defined (until BL10) and release. */
/* Apparently it is no longer necessary at BL10 -DAUTOCLOBBER_BUG. */

#define C_SWITCH_SYSTEM -std1

/* These definitions configure the microcode to support dynamic loading. */
#define SOURCES_SYSTEM pruxdld.c
#define OBJECTS_SYSTEM pruxdld.o
