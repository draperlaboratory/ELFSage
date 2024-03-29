#!/bin/bash

PARENT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

pushd $PARENT_DIR &> /dev/null

rm -rf PrintedHeaders
mkdir PrintedHeaders

for f in ./ELFFiles/*; do
  fname=$(basename -- "$f")
  llvm-readobj --headers ./ELFFiles/$fname > PrintedHeaders/$fname
done

popd &> /dev/null
