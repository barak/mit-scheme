#!/bin/bash
set -e
find * -type f -name \*.scm | xargs etags
