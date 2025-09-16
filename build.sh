#!/bin/bash

set -euxo pipefail

clear
xxd -i torch.lua > torch_lua.c
#rm -rf _build || true
cmake -G Ninja -B _build -S . -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
cmake --build _build --config RelWithDebInfo
./_build/luau_example example.lua
