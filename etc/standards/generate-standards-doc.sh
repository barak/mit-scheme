#!/bin/bash
set -e

HERE=$(dirname "${0}")
mit-scheme --batch-mode \
           --load "${HERE}"/standards "${HERE}"/generate-standards-doc \
           --eval "(generate-standards.texi \"${1}\" \"${2}\")" '(exit)'
