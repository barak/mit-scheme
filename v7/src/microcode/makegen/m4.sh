#!/bin/sh

# $Id: m4.sh,v 1.4 2003/02/14 18:28:31 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
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
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# Processing to simulate m4 accepting definition arguments.

if [ $# = 0 ]
then
  sed -e '/^#/D' | m4 | sed -e 's/@/$/g' -e 's/^$//'
else
  TMP_FILE="m4.tmp"
  SEEN_INPUT=0
  rm -f "${TMP_FILE}"
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
  m4 < "${TMP_FILE}" | sed -e 's/@/$/g' -e 's/^$//'
  rm -f "${TMP_FILE}"
fi
