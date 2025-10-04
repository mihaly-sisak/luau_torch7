#!/bin/bash

set -euxo pipefail

ARG1=${1:-}

clear
if [[ $ARG1 == "--rst" ]]; then
    rm -rf _build || true
fi
cd torch7/generated
xxd -i torch.lua > torch_lua.c
gperf torch_types.gperf > torch_types.c
cd ../..
cmake -G Ninja -B _build -S . -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
cmake --build _build --config RelWithDebInfo
# /RelWithDebInfo/bin
./_build/luau_example torch7/generated/luau_test.lua
#./_build/luau_example example.lua
