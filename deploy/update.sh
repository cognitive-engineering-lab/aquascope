#!/bin/bash

# NOTE: this file inspired from the Rust Playground
# https://github.com/rust-lang/rust-playground/blob/main/deployment/update.sh

set -euv -o pipefail

ROOT=/home/$USER
ARTIFACT_DIR=$ROOT/artifacts
BINARY_PATH=$ARTIFACT_DIR/aquascope_serve
OWNER="cognitive-engineering-lab"
REPO="aquascope"
ZIP="artifacts.zip"

# Do all work from home directory
cd "$ROOT"

# Get the binary's hash so we know if it has changed
PREVIOUS_BINARY_HASH=""
if [[ -f "${BINARY_PATH}" ]]; then
  PREVIOUS_BINARY_HASH=$(md5sum "${BINARY_PATH}")
fi

if [[ -f "${ZIP}" ]]; then
    # Extract the artifacts
    unzip "${ZIP}" -d "${ARTIFACT_DIR}"
    docker load -i "${ARTIFACT_DIR}/image.tar"
fi

chmod +x "${BINARY_PATH}"

# Restart to get new server binary
if [[ -z "${PREVIOUS_BINARY_HASH}" ]] || ! md5sum -c <(echo "${PREVIOUS_BINARY_HASH}") --status; then
    sudo service playground stop || true
    sudo service playground start
fi
