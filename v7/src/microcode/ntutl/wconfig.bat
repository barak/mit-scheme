@echo off
rem MIT Scheme microcode configuration script for Win32 / Watcom C
rem
rem Copyright (c) 1995 Massachusetts Institute of Technology
rem
rem $Id: wconfig.bat,v 1.1 1995/10/24 09:34:29 cph Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386-ntw.asm cmpauxmd.asm
copy ntutl\makefile.wcc makefile
copy ntutl\*.h .
copy ntutl\*.c .
copy ntutl\*.mak .
copy ntutl\*.lbc .
copy ntutl\*.dlg .
copy ntutl\*.ico .
copy ntutl\*.rc .
copy ntutl\*.cur .
