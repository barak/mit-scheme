#!/bin/sh
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
#     2015, 2016, 2017, 2018, 2019 Massachusetts Institute of
#     Technology
#
# This file is part of MIT/GNU Scheme.
#
# MIT/GNU Scheme is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# MIT/GNU Scheme is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MIT/GNU Scheme; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
# 02110-1301, USA.

# Utility for MIT/GNU Scheme compiler staging.

set -e

. ../etc/functions.sh

if [ $# -ne 2 ]; then
    echo "usage: $0 <command> <tag>"
    exit 1
fi

SUBDIRS="back base fggen fgopt machine rtlbase rtlgen rtlopt"
S="STAGE${2}"

case "${1}" in
make)
    for D in ${SUBDIRS}; do
	( cd "${D}"
	  mkdir "${S}"
	  maybe_mv *.com "${S}/."
	  maybe_mv *.bci "${S}/." )
    done
    ;;
make-cross)
    for D in $SUBDIRS; do
	( cd $D
	  mkdir "$S"
	  maybe_mv *.com "$S"
	  maybe_mv *.bci "$S"
	  maybe_mv *.moc "$S"
	  maybe_mv *.fni "$S" )
    done
    ;;
unmake)
    for D in ${SUBDIRS}; do
	( cd "${D}"
	  if [ -d "${S}" ]; then
	      maybe_mv "${S}"/* .
	      rmdir "${S}"
	  fi )
    done
    ;;
remove)
    for D in ${SUBDIRS}; do
	rm -rf "${D}/${S}"
    done
    ;;
copy)
    for D in ${SUBDIRS}; do
	(cd ${D} && cp "${S}"/* .) || exit 1
    done
    ;;
link)
    for D in ${SUBDIRS}; do
	(cd ${D} && ln "${S}"/* .) || exit 1
    done
    ;;
*)
    echo "$0: Unknown command ${1}"
    exit 1
    ;;
esac
