@echo off
rem MIT/GNU Scheme microcode configuration script for Windows NT
rem
rem Copyright 1993 Massachusetts Institute of Technology
rem
rem $Id: config.bat,v 1.7 2003/02/14 18:48:12 cph Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386-nt.asm cmpauxmd.asm
copy ntutl\makefile .
copy ntutl\*.c .
copy ntutl\*.h .
copy ntutl\*.lst .
copy ntutl\*.bat .
copy ntutl\*.mak .
copy ntutl\*.def .
copy ntutl\*.dlg .
copy ntutl\*.ico .
copy ntutl\*.rc .
copy ntutl\*.cur .
