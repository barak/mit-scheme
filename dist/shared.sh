#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
#     2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Massachusetts
#     Institute of Technology
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

set -ae
umask 0002

# Capture standard out so we can send messages there even when it's
# redirected.
exec 3>&1

TL_DIR=$(pwd)
PROGRAM=${0}
PROJECT_NAME=mit-scheme

usage ()
{
    ${USAGE_FUNCTION:-standard_usage} >&2
    exit 1
}

standard_usage ()
{
    echo "usage: ${PROGRAM} TYPE"
    standard_args_usage
}

standard_args_usage ()
{
    echo "  TYPE must be 'snapshot' to specify today's date"
    echo "    or 'standard' to specify standard release"
}

standard_args ()
{
    (( ${#} <= 1 )) || usage
    DIST_TYPE=${1:-standard}
}

${ARGS_FUNCTION:-standard_args} "${@}"

CHANGELOG=changelog.txt
TAR_SUFFIX=.tar.gz

OUTPUT_DIR=${TL_DIR}/.out
RELEASE_OUT=${OUTPUT_DIR}/release
SRC_OUT=${OUTPUT_DIR}/src
DOC_OUT=${OUTPUT_DIR}/doc
LIARC_OUT=${OUTPUT_DIR}/liarc
NATIVE_OUT=${OUTPUT_DIR}/native
MACOSX_OUT=${OUTPUT_DIR}/macosx

PLUGINS=(blowfish gdbm edwin imail x11 x11-screen)

set_release_vars ()
{
    local -r SOURCE_TREE=${1}
    case ${DIST_TYPE} in
        snapshot)
	    RELEASE=$(date +%Y%m%d)
	    ;;
        standard)
	    RELEASE=$(get_release "${SOURCE_TREE}")
	    ;;
        *)
	    usage
	    ;;
    esac
    DIST_DIR=${PROJECT_NAME}-${RELEASE}
    echo "${RELEASE}" > "${RELEASE_OUT}"
}

get_release ()
{
    local -r SOURCE_TREE=${1}
    fgrep Release "${SOURCE_TREE}"/src/runtime/version.scm \
	| awk 'BEGIN { OFS = "." }
	       NF == 4 { print $3, $4 }
	       NF == 5 { print $3, $4, $5 }
	       NF == 6 { print $3, $4, $5, $6 }
	       NF == 7 { print $3, $4, $5, $6, $7 }' \
	| tr -d \(\)\'\"
}

restore_release_vars ()
{
    guarantee_file "${RELEASE_OUT}"
    RELEASE=$(cat "${RELEASE_OUT}")
    DIST_DIR=${PROJECT_NAME}-${RELEASE}
}

guarantee_file ()
{
    if [[ ! -f ${1} ]]; then
	echo "Missing required file: ${1}" >&2
	exit 1
    fi
}

notify ()
{
    echo "${@}" >&3
}

notify_finished ()
{
    notify "Success!"
}

cmd ()
{
    if [[ -z ${CMD_TRACE} ]]; then
	"${@}"
    else
	notify ">>>>" "${@}"
	if [[ ${CMD_TRACE} != norun ]]; then
	    "${@}"
	fi
    fi
}

reset_output_dir ()
{
    my_rm_rf "${OUTPUT_DIR}"
    my_mkdir "${OUTPUT_DIR}"
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

my_ln_s ()
{
    cmd ln -s "${@}"
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

run_command ()
{
    local OUT_FILE=${1}
    shift
    run_command_helper "${@}" &> "${OUT_FILE}"
}

run_command_append ()
{
    local OUT_FILE=${1}
    shift
    run_command_helper "${@}" >> "${OUT_FILE}" 2>&1
}

run_command_helper ()
{
    local DIR=${1}
    shift
    pushd "${DIR}" &> /dev/null
    cmd "${@}"
    popd &> /dev/null
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
    my_tar "${DIR}" "${@}" &> "${OUT_FILE}"
    make_read_only "${TAR_FILE}"
}

unpack_dist_dir ()
{
    local SOURCE=${1}
    guarantee_file "${SOURCE}""${TAR_SUFFIX}"
    my_rm_rf "${SOURCE}"
    my_untar "${SOURCE}"
    cleanup_file "${SOURCE}"
}

# Keep track of temp files and clean them up.

cleanup_file ()
{
    TEMP_FILES+=("${@}")
}

new_temp_file ()
{
    cleanup_file "${@}"
    my_rm_rf "${@}"
}

new_temp_dir ()
{
    new_temp_file "${1}"
    my_mkdir "${1}"
}

cleanup_temp_files ()
{
    if [[ -z ${PRESERVE_TEMP_FILES} ]]; then
	cd "${TL_DIR}"
	my_rm_rf "${TEMP_FILES[@]}"
	TEMP_FILES=()
    fi
}

TEMP_FILES=()
trap cleanup_temp_files EXIT SIGINT SIGQUIT SIGTERM
