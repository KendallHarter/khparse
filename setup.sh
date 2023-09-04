#!/bin/bash

# clangd setup
mkdir -p build
pushd build
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=True -DKHPARSE_BUILD_EXAMPLES=True ..
popd
ln -s build/compile_commands.json

# Hook install
./hooks/install.sh
