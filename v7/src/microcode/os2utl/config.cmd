@echo off
rem MIT Scheme microcode configuration script for OS/2
rem
rem Copyright (c) 1994 Massachusetts Institute of Technology
rem
rem $Id: config.cmd,v 1.1 1994/11/28 05:26:34 cph Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386-os2.asm cmpauxmd.asm
copy os2utl\makefile .
