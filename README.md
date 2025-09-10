# Luau_Torch7

Luau-compatible fork of Torch7.

Removed Torch7 features:
 - File
 - DiskFile
 - MemoryFile
 - PipeFile
 - CmdLine
 - FFInterface
 - Tester
 - TestSuite

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
