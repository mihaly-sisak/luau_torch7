#!/bin/bash

set -euxo pipefail

ARG1=${1:-}

clear
if [ $ARG1 == "--rst" ]; then
    rm -rf _build || true
fi
cmake -G Ninja -B _build -S . -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
cmake --build _build --config RelWithDebInfo
./_build/RelWithDebInfo/bin/luau_example example.lua
