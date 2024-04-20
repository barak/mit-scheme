#!/bin/sh
set -e

HERE=$(dirname "${0}")
${MIT_SCHEME_EXE:-mit-scheme} \
    --batch-mode \
    --load "${HERE}"/standards "${HERE}"/generate-standard-libs \
    --eval "(generate-standard-libs \"${1}\")" '(exit)'
