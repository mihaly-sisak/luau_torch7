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
   - [luaT_lua_pushudata](torch7/lib/luaT/luaT.c#L946), return userdata from a C pointer

Modified Torch7 features:
 - Default tensor type is `torch.FloatTensor`

Added Torch7 features:
 - Coherent [noise](torch7_noise/noise.md) optional library

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
    generated and filtered 5244100 values
    took 0.1268852550019801 seconds
    min = -0.999998927116394, max = 0.999998927116394
    num of indexes = 281666

tracetest3
tracetest2
tracetest1
Runtime error: [string "example.lua"]:33: example error
[string "example.lua"]:33
[string "example.lua"]:38
[string "example.lua"]:43
[string "example.lua"]:46
```

## Documentation

__Torch__ is the main package in Torch7 where data
structures for multi-dimensional tensors and mathematical operations
over these are defined.

 - Tensor Library
   - [Tensor](torch7/doc/tensor.md) defines the _all powerful_ tensor object that provides multi-dimensional numerical arrays with type templating.
   - [Mathematical operations](torch7/doc/maths.md) that are defined for the tensor object types.
   - [Storage](torch7/doc/storage.md) defines a simple storage interface that controls the underlying storage for any tensor object.
 - Useful Utilities
   - [Timer](torch7/doc/timer.md) provides functionality for _measuring time_.
   - [Random](torch7/doc/random.md) defines a random number generator package with various distributions.
   - [Utility](torch7/doc/utility.md) functions are provided for easy handling of torch tensor types and class inheritance.
   - [Noise](torch7_noise/noise.md) optional coherent noise library

