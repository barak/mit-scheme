@echo off
rem MIT/GNU Scheme microcode configuration script for OS/2
rem
rem Copyright 1994,1995,2000 Massachusetts Institute of Technology
rem
rem $Id: config.cmd,v 1.5 2003/02/14 18:48:12 cph Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386.m4 cmpauxmd.m4
copy os2utl\makefile .
copy os2utl\config.h .
copy cmpauxmd\asmcvt.c .
echo ***** Read and edit the makefile! *****
