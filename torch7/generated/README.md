Torch7 uses the [cwrap](https://github.com/torch/cwrap) and [paths](https://github.com/torch/paths) luarocks to generate C code.
This directory contains the generated code, as it needs to be modified after generation to compile with Luau.

 - `lua_cwrap_to_c.sh` Script documenting lua code generation steps
 - `random.c` Generated code from [torch7/random.lua](../random.lua), modified to work with Luau
 - `TensorMath.c` Generated code from [torch7/TensorMath.lua](../TensorMath.lua), modified to work with Luau
 - `torch.lua` Merged Torch7 lua initialization code from [torch7/init.lua](../init.lua), commented out parts that do not work with Luau, copied in file contents where `require` would have been used
 - `torch_lua.c` Transformed `torch.lua` with `xxd -i torch.lua > torch_lua.c`, to be included in C source code