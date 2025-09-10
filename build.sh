#!/bin/bash

set -euxo pipefail

clear
rm -rf _build
cmake -B _build -S . -DCMAKE_BUILD_TYPE=RelWithDebInfo
cmake --build _build --config RelWithDebInfo
./_build/luau_example example.lua
