#!/bin/bash
set -e

cd "$(dirname "${0}")"

mit-scheme --batch-mode \
           --load standards generate-standards-doc \
           --eval '(generate-standards.texi "../../doc/ref-manual/")' '(exit)'
