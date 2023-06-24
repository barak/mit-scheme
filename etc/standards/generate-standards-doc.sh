#!/bin/sh

set -eu

HERE=$(dirname "${0}")
mit-scheme --batch-mode \
           --load "${HERE}"/standards "${HERE}"/generate-standards-doc \
           --eval "(generate-$(basename "$1") \"$1\")" '(exit)'
