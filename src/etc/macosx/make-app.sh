#!/bin/bash
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009 Massachusetts Institute of
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

# Utility to build a MacOS X application bundle to run Edwin.

set -e

# These values are placeholders; we need to get the right ones.
: ${VERSION_STRING=$(date +%Y%m%d)}
: ${LONG_VERSION_STRING="snapshot ${VERSION_STRING}"}
: ${MACOSX_MIN_VERSION=10.5}
: ${YEAR=$(date +%Y)}
: ${bindir=/usr/local/bin}
: ${libdir=/usr/local/lib}

rm -rf tmp mit-scheme.app

# Build directory structure for bundle.
mkdir mit-scheme.app
mkdir mit-scheme.app/Contents
mkdir mit-scheme.app/Contents/MacOS
mkdir mit-scheme.app/Contents/Resources

# Install into temporary directory, then move contents into bundle.
make install DESTDIR=$(pwd)/tmp

if [[ -z ${MIT_SCHEME_EXE} ]]; then
    for FN in $(ls tmp"${bindir}"); do
	[[ -L tmp${bindir}/${FN} ]] && continue
	if [[ -f tmp${bindir}/${FN} ]]; then
	    MIT_SCHEME_EXE=${FN}
	    break;
	fi
    done
fi
if [[ -z ${AUXDIR} ]]; then
    for FN in $(ls tmp"${libdir}"); do
	if [[ -d tmp${libdir}/${FN} ]]; then
	    AUXDIR=${libdir}/${FN}
	    break
	fi
    done
fi
mv tmp"${bindir}"/"${MIT_SCHEME_EXE}" \
    mit-scheme.app/Contents/Resources/mit-scheme
mv tmp"${AUXDIR}"/macosx-starter mit-scheme.app/Contents/MacOS/.
rm -f tmp"${AUXDIR}"/runtime.com
mv tmp"${AUXDIR}"/* mit-scheme.app/Contents/Resources/.
rm -rf tmp
cp -p etc/macosx/schlogo.icns mit-scheme.app/Contents/Resources/appIcon.icns

# Generate an appropriate Info.plist file.
cat > mit-scheme.app/Contents/Info.plist <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist
          PUBLIC "-//Apple//DTD PLIST 1.0//EN"
	  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>
    <key>CFBundleName</key>
    <string>mit-scheme</string>
    <key>CFBundleDisplayName</key>
    <string>MIT/GNU Scheme</string>
    <key>CFBundleIdentifier</key>
    <string>org.gnu.mit-scheme</string>
    <key>CFBundleVersion</key>
    <string>${VERSION_STRING}</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleSignature</key>
    <string>mgsc</string>
    <key>CFBundleExecutable</key>
    <string>macosx-starter</string>

    <key>CFBundleShortVersionString</key>
    <string>${VERSION_STRING}</string>
    <key>LSMinimumSystemVersion</key>
    <string>${MACOSX_MIN_VERSION}</string>
    <key>NSHumanReadableCopyright</key>
    <string>Copyright (C) ${YEAR} Massachusetts Institute of Technology</string>

    <key>CFBundleGetInfoString</key>
    <string>${LONG_VERSION_STRING}</string>
    <key>CFBundleDevelopmentRegion</key>
    <string>English</string>
    <key>LSHasLocalizedDisplayName</key>
    <false/>
    <key>CFBundleIconFile</key>
    <string>appIcon.icns</string>
  </dict>
</plist>
EOF
