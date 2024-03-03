#!/bin/sh

set -eu

HERE=$(dirname "${0}")
${MIT_SCHEME_EXE:-mit-scheme} \
    --batch-mode \
    --load "${HERE}"/standards "${HERE}"/generate-standards-doc \
    --eval "(generate-$(basename "$1") \"$1\")" '(exit)'
