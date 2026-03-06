#!/bin/bash
# Fetch reference projects as shallow clones into refs/
# These are for local reference only (e.g., AI-assisted development)
# and are gitignored.

set -e

REFS_DIR="$(cd "$(dirname "$0")/.." && pwd)/refs"
mkdir -p "$REFS_DIR"

clone_ref() {
    local name="$1"
    local url="$2"
    if [ -d "$REFS_DIR/$name" ]; then
        echo "  $name already exists, skipping"
    else
        echo "  Cloning $name..."
        git clone --depth 1 "$url" "$REFS_DIR/$name"
    fi
}

echo "Fetching reference projects into $REFS_DIR/"
clone_ref coalton       https://github.com/coalton-lang/coalton
clone_ref consensus-specs https://github.com/ethereum/consensus-specs
clone_ref ethereum_ssz  https://github.com/sigp/ethereum_ssz
clone_ref fastssz       https://github.com/ferranbt/fastssz
clone_ref py-ssz        https://github.com/ethereum/py-ssz
echo "Done."
