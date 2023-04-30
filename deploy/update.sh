#!/bin/bash

# NOTE: this file was inspired by the Rust Playground deployment:
# https://github.com/rust-lang/rust-playground/blob/main/deployment/update.sh

set -euv -o pipefail

ROOT="/home/$USER"
ARTIFACT_DIR="$ROOT/artifacts"
BINARY_PATH="$ARTIFACT_DIR/aquascope_serve"
IMAGE_PATH="$ARTIFACT_DIR/image.tar"
ZIP="$ROOT/artifacts.zip"
ZIP_STASH="$ROOT/old/artifacts.zip"

# Do all work from home directory
cd "${ROOT}"

# Get the binary's hash so we know if it has changed
PREVIOUS_BINARY_HASH=""
if [[ -f "${BINARY_PATH}" ]]; then
  PREVIOUS_BINARY_HASH=$(md5sum "${BINARY_PATH}")
fi

# Unzip archive files if they exist
if [[ -f "${ZIP}" ]]; then
    unzip -o "${ZIP}" -d "${ARTIFACT_DIR}"
    docker load -i "${IMAGE_PATH}"
    chmod +x "${BINARY_PATH}"
    mv "${ZIP}" "${ZIP_STASH}"
fi

# Restart to get new server binary
if [[ -z "${PREVIOUS_BINARY_HASH}" ]] || ! md5sum -c <(echo "${PREVIOUS_BINARY_HASH}") --status; then
    sudo service playground stop || true
    sudo service playground start
    echo "Playground service restarted"
fi
