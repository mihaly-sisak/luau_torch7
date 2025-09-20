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
cd ../cwrap
sudo luarocks make rocks/cwrap-scm-1.rockspec

# install rock paths
cd ../paths
sudo luarocks make rocks/paths-scm-1.rockspec

# generate lua wrappers
# these wrappers need to be modified to compile with Luau after generation
# you need to be in the torch7 directory otherwise torchcwrap.lua can not be found
cd ../..
lua ./TensorMath.lua generated/TensorMath.c
lua ./random.lua generated/random.c

