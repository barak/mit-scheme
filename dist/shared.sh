#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010 Massachusetts Institute of
#     Technology
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
# 02110-1301, USA.

set -e
umask 022

TL_DIR=$(pwd)
PROGRAM=${0}
PROJECT_NAME=mit-scheme
SOURCE_TREE=${PROJECT_NAME}

usage ()
{
    echo "usage: ${PROGRAM} TYPE" >&2
    echo "  TYPE must be 'snapshot' to specify today's date" >&2
    echo "    or 'standard' to specify standard release" >&2
    exit 1
}

(( ${#} <= 1 )) || usage
DIST_TYPE=${1:-standard}

[[ -d ${SOURCE_TREE} ]] || usage

get_release ()
{
    fgrep Release "${SOURCE_TREE}"/src/runtime/version.scm \
	| awk 'BEGIN { OFS = "." }
	       NF == 4 { print $3, $4 }
	       NF == 5 { print $3, $4, $5 }
	       NF == 6 { print $3, $4, $5, $6 }
	       NF == 7 { print $3, $4, $5, $6, $7 }' \
	| tr -d \(\)\'\"
}

case ${DIST_TYPE} in
    (snapshot)
    RELEASE=$(date +%Y%m%d)

	;;
    (standard)
    RELEASE=$(get_release)

	;;
    (*)
	usage
	;;
esac

CHANGELOG=changelog.txt
TAR_SUFFIX=.tar.gz

DIST_DIR=${PROJECT_NAME}-${RELEASE}
DOC_IMAGE_DIR=${DIST_DIR}-doc-image

DIST_UCODE=${DIST_DIR}-ucode

OUTPUT_DIR=${TL_DIR}/.out
SRC_OUT=${OUTPUT_DIR}/src
DOC_OUT=${OUTPUT_DIR}/doc
LIARC_OUT=${OUTPUT_DIR}/liarc
BUILD_OUT=${OUTPUT_DIR}/build

CMD_TRACE=
cmd ()
{
    [[ -n ${CMD_TRACE} ]] && echo ">>>>" "${@}"
    "${@}"
}

reset_output_dir ()
{
    my_rm_rf "${OUTPUT_DIR}"
    cmd mkdir "${OUTPUT_DIR}"
}

make_output_dir ()
{
    my_mkdir "${OUTPUT_DIR}"
}

my_cp ()
{
    cmd cp -pR "${@}"
}

my_mv ()
{
    cmd mv "${@}"
}

my_rm_f ()
{
    cmd rm -f "${@}"
}

my_rm_rf ()
{
    cmd rm -rf "${@}"
}

my_rmdir ()
{
    cmd rmdir "${@}"
}

my_mkdir ()
{
    cmd install -d -m 755 "${@}"
}

my_install ()
{
    cmd install -p "${@}"
}

my_install_data ()
{
    cmd my_install -m 644 "${@}"
}

my_tar ()
{
    local TAR_FILE=${1}${TAR_SUFFIX}
    shift
    cmd tar cvzf "${TAR_FILE}" "${@}"
}

my_untar ()
{
    local TAR_FILE=${1}${TAR_SUFFIX}
    shift
    cmd tar xzf "${TAR_FILE}" "${@}"
}

my_configure ()
{
    cmd ./configure "${@}"
}

my_make ()
{
    cmd make "${@}"
}

my_find ()
{
    cmd find "${@}"
}

fixup_perms ()
{
    cmd chmod -R og-w "${1}"
}

make_read_only ()
{
    cmd chmod 444 "${@}"
}

notify ()
{
    echo "${@}"
}

run_command ()
{
    local OUT_FILE=${1}
    local DIR=${2}
    shift 2
    (cd "${DIR}"; cmd "${@}") > "${OUT_FILE}" 2>&1
}

make_tar_file ()
{
    local OUT_FILE=${1}-tar
    local DIR=${2}
    local TAR_FILE=${DIR}${TAR_SUFFIX}
    shift 2
    if (( ${#} < 1 )); then
	set "${DIR}"
    fi
    my_rm_f "${TAR_FILE}"
    my_tar "${DIR}" "${@}" > "${OUT_FILE}" 2>&1
    make_read_only "${TAR_FILE}"
}

unpack_dist_to ()
{
    local DEST=${1}
    guarantee_tar "${DIST_DIR}"
    new_temp_dir "${DEST}"
    unpack_into "${DIST_DIR}" "${DEST}"
}

unpack_dist ()
{
    guarantee_tar "${DIST_DIR}"
    new_temp_file "${DIST_DIR}"
    my_untar "${DIST_DIR}"
}

guarantee_tar ()
{
    guarantee_file "${1}""${TAR_SUFFIX}"
}

guarantee_file ()
{
    if [[ ! -f ${1} ]]; then
	echo "No source file: ${1}"
	exit 1
    fi
}

unpack_doc_image_into ()
{
    unpack_into "${DOC_IMAGE_DIR}" "${1}"
}

unpack_into ()
{
    my_untar "${1}" -C "${2}" --strip-components 1
}

# Keep track of temp files and clean them up.

new_temp_file ()
{
    TEMP_FILES+=("${@}")
    my_rm_rf "${@}"
}

new_temp_dir ()
{
    new_temp_file "${1}"
    my_mkdir "${1}"
}

cleanup_temp_files ()
{
    cd "${TL_DIR}"
    my_rm_rf "${TEMP_FILES[@]}"
    TEMP_FILES=()
}

TEMP_FILES=()
trap cleanup_temp_files EXIT SIGINT SIGQUIT SIGTERM
