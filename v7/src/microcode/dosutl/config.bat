rem MIT Scheme microcode configuration script for DOS
rem
rem Copyright (c) 1993 Massachusetts Institute of Technology
rem
rem $Id: config.bat,v 1.1 1993/06/24 07:40:46 gjr Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386-dos.asm cmpauxmd.asm
copy dosutl\*.lst .
copy dosutl\*.h .
copy dosutl\makefile .
