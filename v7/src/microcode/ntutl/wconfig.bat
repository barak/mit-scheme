@echo off
rem $Id: wconfig.bat,v 1.6 2007/04/24 05:32:01 cph Exp $
rem
rem Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
rem     1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
rem     2004, 2005, 2006, 2007 Massachusetts Institute of Technology
rem
rem This file is part of MIT/GNU Scheme.
rem
rem MIT/GNU Scheme is free software; you can redistribute it and/or
rem modify it under the terms of the GNU General Public License as
rem published by the Free Software Foundation; either version 2 of the
rem License, or (at your option) any later version.
rem
rem MIT/GNU Scheme is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
rem General Public License for more details.
rem
rem You should have received a copy of the GNU General Public License
rem along with MIT/GNU Scheme; if not, write to the Free Software
rem Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
rem 02110-1301, USA.
rem
rem Microcode configuration script for Win32 / Watcom C
rem
copy cmpauxmd\i386-ntw.asm cmpauxmd.asm
copy cmpintmd\i386.c cmpintmd.c
copy cmpintmd\i386-config.h cmpintmd-config.h
copy cmpintmd\i386.h cmpintmd.h
copy ntutl\makefile.wcc makefile
copy ntutl\config.h .
copy ntutl\float.h .
copy ntutl\limits.h .
copy ntutl\ntdialog.h .
copy ntutl\scheme32.c .
copy ntutl\scheme32.lbc .
copy ntutl\scheme32.lnk .
copy ntutl\ntdialog.dlg .
copy ntutl\*.ico .
copy ntutl\ntgui.rc .
copy ntutl\gc.cur .
