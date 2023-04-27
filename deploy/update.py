#!/usr/bin/env python3

import os

TK = os.getenv('NAME')

with open('generated.sh', 'w') as fout:
    fout.write(f'''#!/bin/bash

# NOTE: this file inspired from the Rust Playground
# https://github.com/rust-lang/rust-playground/blob/main/deployment/update.sh

set -euv -o pipefail

ROOT=/home/$USER
ARTIFACT_DIR=$ROOT/artifacts
BINARY_PATH=$ARTIFACT_DIR/aquascope_serve
OWNER="cognitive-engineering-lab"
REPO="aquascope"

# Do all work from home directory
cd $ROOT

# Get the binary's hash so we know if it has changed
PREVIOUS_BINARY_HASH=""
if [[ -f "${{BINARY_PATH}}" ]]; then
    PREVIOUS_BINARY_HASH=$(md5sum "${{BINARY_PATH}}")
fi

# Get the latest workflow run ID
curl -L \
        -H "Authorization: Bearer {TK}" \
        https://api.github.com/repos/$OWNER/$REPO/actions/workflows/pre-release.yml/runs?status=success&branch=main&event=pull_request | \
    jq -r '.workflow_runs[0].id' | \
    xargs -I ID curl -L \
        -H "Authorization: Bearer {TK}" \
        https://api.github.com/repos/$OWNER/$REPO/actions/runs/ID/artifacts | \
    jq -r '.artifacts[] | select(.name == "server-artifacts") | .archive_download_url' | \
    xargs -I URL curl -L \
        -H "Authorization: Bearer {TK}" \
        URL -o artifacts.zip

# Extract the artifacts
unzip artifacts.zip -d $ARTIFACT_DIR
docker load -i $ARTIFACT_DIR/image.tar
chmod +x "${{BINARY_PATH}}"

# Restart to get new server binary
if [[ -z "${{PREVIOUS_BINARY_HASH}}" ]] || ! md5sum -c <(echo "${{PREVIOUS_BINARY_HASH}}") --status; then
    sudo service playground stop || true
    sudo service playground start
fi
    ''')
