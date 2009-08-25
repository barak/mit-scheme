@echo off
rem Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
rem     1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
rem     2004, 2005, 2006, 2007, 2008, 2009 Massachusetts Institute of
rem     Technology
rem
rem This file is part of MIT/GNU Scheme.
rem
rem MIT/GNU Scheme is free software; you can redistribute it and/or
rem modify it under the terms of the GNU General Public License as
rem published by the Free Software Foundation; either version 2 of the
rem License, or (at your option) any later version.
rem
rem MIT/GNU Scheme is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
rem General Public License for more details.
rem
rem You should have received a copy of the GNU General Public License
rem along with MIT/GNU Scheme; if not, write to the Free Software
rem Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
rem 02110-1301, USA.
rem
rem Microcode configuration script for Windows NT
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
