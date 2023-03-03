#!/bin/bash
set -e

HERE=$(dirname "${0}")
mit-scheme --batch-mode \
           --load "${HERE}"/standards "${HERE}"/generate-standard-libs \
           --eval "(generate-standard-libs \"${1}\")" '(exit)'
