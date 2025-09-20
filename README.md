# Luau_Torch7

Luau-compatible fork of Torch7.

Some features needed to be removed, for the following reasons:
 - Luau does not support files to protect user filesystem
 - Luau does not have `FFI` library
 - Luau does not have `lua_topointer` function
 - Lua `number` max representable integer is 2^53, this can hold 32 bit pointers but can not hold 64 bit ones

Removed Torch7 features:
 - Whole modules
   - File
   - DiskFile
   - MemoryFile
   - PipeFile
   - CmdLine
   - FFInterface
   - Tester
   - TestSuite
 - Partial features
   - [torch_isatty](torch7/utils.c#L67), no tty in Luau
   - [torch_Storage_(new)](torch7/generic/Storage.c#L69), allocating storage from Lua with a pointer + size
   - [luaT_pushpointer](torch7/lib/luaT/luaT.c#L1027), pushing pointer to stack
   - [luaT_lua_pointer](torch7/lib/luaT/luaT.c#L1042), getting pointer of lua object
   - [luaT_lua_pushudata](torch7/lib/luaT/luaT.c#L946), return a userdata from a C pointer

Modified Torch7 features:
 - Default tensor type is `torch.FloatTensor`

Added Torch7 features:
 - These features can be turned on or off by setting the cmake option `LUAU_TORCH7_NOISE`. You can check for `LUAU_TORCH7_NOISE` define in C and `torch.hasNoise` in Lua to see if the noise functions are avaliable.
 - `[res] torch.simplex2D([res,] scale, xstart, ystart, xsize, ysize, seed)`, simplex noise generation, returns a 2D `torch.FloatTensor`
 - `[res] torch.node2D([res,] encoded node tree string, xstart, ystart, xsize, ysize, seed)`, noise generation based on FastNoise2 Node Editor encoded node tree string, returns a 2D `torch.FloatTensor`

## Example

Use the `LUAU_TORCH7_EXAMPLE` CMake option to compile `example.cpp`, and pass `example.lua` as first argument to the exe.

Output with `LUAU_TORCH7_NOISE` enabled:
```
 1  2
 3  4
[torch.FloatTensor of size 2x2]

 1  1
 1  1
[torch.FloatTensor of size 2x2]

 2  3
 4  5
[torch.FloatTensor of size 2x2]

 1  1  1
 1  1  1
 1  1  1
[torch.FloatTensor of size 3x3]

 2  2  2
 2  2  2
 2  2  2
[torch.FloatTensor of size 3x3]

noise:
    took 0.1470989510016807 seconds
    min = -0.999998927116394, max = 0.999998927116394
    num of indexes = 281666
tracetest3
tracetest2
tracetest1
Runtime error: [string "example.lua"]:34: example error
[string "example.lua"]:34
[string "example.lua"]:39
[string "example.lua"]:44
[string "example.lua"]:47
```

## How to build

 - Check out the luau submodule
 - Build and run with `build.sh`
