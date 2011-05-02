#!/bin/sh

# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute
#     of Technology
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

# Processing to simulate m4 accepting definition arguments.

set -e

if [ $# -le 1 ]; then
  printf 'Usage: %s m4 <file/definition> ...\n' >&2
  exit 1
fi

M4="${1}"
shift

TMP_FILE="m4.tmp"

clean ()
{
  rm -f "${TMP_FILE}"
}

run_m4 ()
{
  ${M4} && clean
}

trap clean EXIT INT QUIT TERM
rm -f "${TMP_FILE}"
touch "${TMP_FILE}"

if [ $# = 0 ]
then
  sed -e '/^#/D' | run_m4 | sed -e 's/@/$/g' -e 's/^$//'
else
  SEEN_INPUT=0
  while [ $# != 0 ]; do
    if [ "${1}" = "-P" ]; then
      echo "define(${2})" >> "${TMP_FILE}"
      shift
    else
      SEEN_INPUT=1
      sed -e '/^#/D' < "${1}" >> "${TMP_FILE}"
    fi
    shift
  done
  if [ ${SEEN_INPUT} -eq 0 ]; then
    sed -e '/^#/D' >> "${TMP_FILE}"
  fi
  run_m4 < "${TMP_FILE}" | sed -e 's/@/$/g' -e 's/^$//'
fi

# If m4 was successful, run_m4 has deleted the temporary file.  If
# not, report the failure; exiting will have the effect of running
# `clean', which will delete the temporary file.

if [ -f "${TMP_FILE}" ]; then
  exit 1
fi
