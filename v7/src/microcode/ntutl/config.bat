@echo off
rem MIT Scheme microcode configuration script for Windows NT
rem
rem Copyright (c) 1993 Massachusetts Institute of Technology
rem
rem $Id: config.bat,v 1.3 1993/07/18 20:30:44 gjr Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386-nt.asm cmpauxmd.asm
copy ntutl\*.lst .
copy ntutl\*.h .
copy ntutl\makefile .
copy ntutl\setenv.bat .
