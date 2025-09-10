#!/bin/bash

# install lua runtime
sudo apt install lua5.1 liblua5.1-0-dev
#sudo apt install luarocks

# install luarocks
mkdir -p luarocks
cd luarocks
wget https://luarocks.org/releases/luarocks-3.12.2.tar.gz
tar zxpf luarocks-3.12.2.tar.gz
cd luarocks-3.12.2
./configure
make
sudo make install

# install rock cwrap
cd ../../luarock_modules/cwrap
sudo luarocks make rocks/cwrap-scm-1.rockspec

# install rock paths
cd ../../luarock_modules/paths
sudo luarocks make rocks/paths-scm-1.rockspec

# install rock torch7
cd ../../luarock_modules/torch7
sudo luarocks make rocks/torch-scm-1.rockspec

# generate lua wrappers
# these wrappers need to be modified to compile with Luau after generation
cd ../../torch7
mkdir generated
lua ./TensorMath.lua generated/TensorMath.c
lua ./random.lua generated/random.c

