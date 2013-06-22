#!/bin/sh

CURRENT_PATH="`dirname \"$0\"`"
PROJECT_ROOT="${CURRENT_PATH}/.."

LISPWORKS="/Applications/LispWorks 6.1/LispWorks.app/Contents/MacOS/lispworks-6-1-0-macos-universal"
BUILD_SCRIPT="${PROJECT_ROOT}/delivery/build.lisp"

"${LISPWORKS}" -init "${BUILD_SCRIPT}"
