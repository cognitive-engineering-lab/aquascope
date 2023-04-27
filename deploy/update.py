#!/usr/bin/env python3

import os

TK = os.getenv('NAME')

with open('generated.sh', 'w') as fout:
    fout.write(f'''
#!/bin/bash

set -euv -o pipefail

ROOT=/home/$USER
ARTIFACT_DIR=$ROOT/artifacts
BINARY_PATH=$ARTIFACT_DIR/aquascope_serve
OWNER="cognitive-engineering-lab"
REPO="aquascope"

# Do all work from home directory
cd $ROOT

# Get the binary's hash so we know if it has changed
previous_binary_hash=""
if [[ -f "${{BINARY_PATH}}" ]]; then
    previous_binary_hash=$(md5sum "${{BINARY_PATH}}")
fi

# Get the latest workflow run ID
curl -L \
        -H "Authorization: Bearer {TK}" \
        https://api.github.com/repos/$OWNER/$REPO/actions/workflows/pre-release.yml/runs?status=success&branch=main&event=pull_request | \
    jq -r '.workflow_runs[0].id' | \
    xargs -I ID curl -L \
        -H "Authorization: Bearer {TK}" \
        https://api.github.com/repos/$OWNER/$REPO/actions/runs/ID/artifacts > \
    response.txt
    # | \
    # jq -r '.artifacts[] | select(.name == "server-artifacts") | .archive_download_url' | \
    # xargs -I URL curl -L \
    #     -H "Authorization: Bearer {TK}" \
    #     URL -o artifacts.zip

cat response.txt

ls -R
echo SUCCESS
    ''')

#     fout.write(f'''
# #!/bin/bash

# set -euv -o pipefail

# root=/home/$USER
# artifact_dir=$root/artifacts
# binary_path=$artifact_dir/aquascope_serve

# owner="cognitive-engineering-lab"
# repo="aquascope"

# # Do all work from home directory
# cd $root

# # Clean old docker images
# docker system prune -f || true

# # Get the binary's hash so we know if it has changed
# previous_binary_hash=""
# if [[ -f "${{binary_path}}" ]]; then
#     previous_binary_hash=$(md5sum "${{binary_path}}")
# fi

# # Get the latest workflow run ID
# run_id=$(curl -s -H "Authorization: Bearer {TK}" \
#   "https://api.github.com/repos/$owner/$repo/actions/workflows/workflow.yml/runs?status=success&branch=main&event=pull_request" \
#   | jq -r '.workflow_runs[0].id')

# # Download the artifacts for the latest workflow run
# curl -s -H "Authorization: Bearer {TK}" \
#   "https://api.github.com/repos/$owner/$repo/actions/runs/$run_id/artifacts" \
#   | jq -r '.artifacts[] | select(.name == "server-artifacts") | .archive_download_url' \
#   | xargs curl -sL -o artifacts.zip

# # Extract the artifacts
# unzip artifacts.zip -d $artifact_dir
# docker load -i $artifact_dir/image.tar

# chmod +x "${{binary_path}}"

# # Restart to get new server binary
# if [[ -z "${{previous_binary_hash}}" ]] || ! md5sum -c <(echo "${{previous_binary_hash}}") --status; then
#     sudo service playground stop || true
#     sudo service playground start
# fi
#     ''')
