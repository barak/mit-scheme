@echo off
rem MIT Scheme microcode configuration script for Windows NT
rem
rem Copyright (c) 1993 Massachusetts Institute of Technology
rem
rem $Id: config.bat,v 1.2 1993/06/24 15:18:20 gjr Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386-nt.asm cmpauxmd.asm
copy ntutl\*.lst .
copy ntutl\*.h .
copy ntutl\makefile .
