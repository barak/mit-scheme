#!/bin/sh
#
# Utility to make TAGS file for the MIT/GNU Scheme MICROCODE build
# directory.  The working directory must be the build directory.

REGEX1='/^DEF[A-Z0-9_]*[ \t]*(\("[^"]+"\|[a-zA-Z_][a-zA-Z0-9_]*\)/'
REGEX2='/^DEF[A-Z0-9_]*[ \t]*\(("[^"]+"|[a-zA-Z_][a-zA-Z0-9_]+)/\1/'

etags -r "${REGEX1}" *.[ch] */*.[ch] \
    || etags --regex-C="${REGEX2}" *.[ch] */*.[ch]
