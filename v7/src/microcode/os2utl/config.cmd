@echo off
rem MIT Scheme microcode configuration script for OS/2
rem
rem Copyright (c) 1994 Massachusetts Institute of Technology
rem
rem $Id: config.cmd,v 1.2 1995/10/09 05:58:38 cph Exp $
rem
copy cmpintmd\i386.h cmpintmd.h
copy cmpauxmd\i386.m4 cmpauxmd.m4
copy os2utl\makefile .
echo ***** Read and edit the makefile! *****
