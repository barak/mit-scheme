@echo off
rem MIT Scheme microcode configuration script for Windows NT
rem
rem Copyright (c) 1993 Massachusetts Institute of Technology
rem
rem $Id: config.bat,v 1.4 1993/08/21 02:32:58 gjr Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386-nt.asm cmpauxmd.asm
copy ntutl\*.lst .
copy ntutl\*.h .
copy ntutl\makefile .
copy ntutl\*.bat .
copy ntutl\*.mak .
copy ntutl\*.def .
copy ntutl\*.dlg .
copy ntutl\*.ico .
