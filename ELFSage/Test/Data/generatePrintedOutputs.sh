#!/bin/bash

PARENT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

pushd $PARENT_DIR &> /dev/null

rm -rf PrintedHeaders
mkdir PrintedHeaders

for f in ./ELFFiles/*; do
  fname=$(basename -- "$f")
  # llvm-readobj prepends 6 lines of file metadata before all outputs. Discard this metadata
  llvm-readobj --headers ./ELFFiles/$fname | tail -n +7 > PrintedHeaders/$fname
done

popd &> /dev/null
