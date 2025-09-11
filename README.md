# Luau_Torch7

Luau-compatible fork of Torch7.

Some features needed to be removed, for the following reasons:
 - Luau does not support files to protect user filesystem
 - Luau does not have FFI library
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

`example.lua` output:
```
 1  2
 3  4
[torch.DoubleTensor of size 2x2]

 1  1
 1  1
[torch.DoubleTensor of size 2x2]

 2  3
 4  5
[torch.DoubleTensor of size 2x2]
```

## How to build

 - Check out the luau submodule
 - Patch it with `luau.diff`
 - Build and run with `build.sh`
