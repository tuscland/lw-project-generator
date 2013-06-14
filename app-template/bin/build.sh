#!/bin/sh

ARCH="x64"
PRODUCT_BUNDLE_NAME="Leap OSC Bridge"
CODESIGN_CERTIFICATE_NAME="Developer ID Application: Wildora"
PROJECT_ROOT=".."
DELIVER_SCRIPT="${PROJECT_ROOT}/delivery/deliver.lisp"

BUILD_DIR="build"
LISPWORKS="/Applications/LispWorks\ 6.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-6-1-0-macos64-universal"
CODESIGN="/usr/bin/codesign"
COLOR="./color"
BUILD_LOG_FILE="last-build.log"
PRODUCT_BUNDLE_EXTENSION="app"
PRODUCT_BUNDLE_DIR="${PRODUCT_BUNDLE_NAME}.${PRODUCT_BUNDLE_EXTENSION}"
BUILD_ROOT="${PROJECT_ROOT}/${BUILD_DIR}"
PRODUCT_BUNDLE_ROOT="${BUILD_ROOT}/${ARCH}/${PRODUCT_BUNDLE_DIR}"
BUILD_LOG="${BUILD_ROOT}/${BUILD_LOG_FILE}"
OUTPUT_TO_LOG=" 1>>${BUILD_LOG} 0>&1"

function build_log_echo() {
    local what="$*"
    echo "#"       >> ${BUILD_LOG}
    echo "# $what" >> ${BUILD_LOG}  
}

function log_info() {
    local what="$*"
    echo "$(${COLOR} blue)${what}"
    build_log_echo ${what}
}

function log_error() {
    local what="$*"
    echo "$(${COLOR} bold red)${what}"
}

function script_cleanup() {
    trap - EXIT
    echo "$(${COLOR} off)"
}

function die() {
    local error=${1:-Undefined error}
    log_error "$0: ${error}" 
    build_log_echo ${error}
    script_cleanup
    exit 1
}

function execute() {
    typeset command="$*"
    typeset ret_code

    build_log_echo ${command}
    eval ${command}
    ret_code=$?
    if [ ${ret_code} != 0 ]; then
        die "(error=${ret_code}) Failed to execute ${command}"
    fi
}

function script_trap() {
    log_error "> Error occured, tail of ${BUILD_LOG_FILE}:"
    ${${COLOR}} off
    tail ${BUILD_LOG}
    die "Stopped."
}

function script_initialize() {
    set -e
    trap script_trap EXIT INT TERM
}

function build_deliver() {
    log_info "- DELIVER"
    execute "${LISPWORKS} -build ${DELIVER_SCRIPT} ${OUTPUT_TO_LOG}"
}

function build_codesign() {
    log_info "- CODESIGN"
    execute "${CODESIGN} -s \"${CODESIGN_CERTIFICATE_NAME}\" \"${PRODUCT_BUNDLE_ROOT}\" ${OUTPUT_TO_LOG}"
}

function build_clean() {
    log_info "- CLEAN"
    rm ${BUILD_LOG}
    rm -rf ${PRODUCT_BUNDLE_ROOT}
}

function build() {
    script_initialize
    build_clean
    build_deliver
    build_codesign
    log_info "> Success"
    script_cleanup
    exit 0
}

build
