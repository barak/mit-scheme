@echo off
rem MIT/GNU Scheme microcode configuration script for Win32 / Watcom C
rem
rem Copyright 1995-96 Massachusetts Institute of Technology
rem
rem $Id: wconfig.bat,v 1.4 2003/02/14 18:48:12 cph Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386-ntw.asm cmpauxmd.asm
copy ntutl\makefile.wcc makefile
copy ntutl\*.h .
copy ntutl\*.c .
copy ntutl\*.lbc .
copy ntutl\*.lnk .
copy ntutl\*.dlg .
copy ntutl\*.ico .
copy ntutl\*.rc .
copy ntutl\*.cur .
