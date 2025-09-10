# Luau_Torch7

Luau-compatible fork of Torch7.

Proof of concept, be suprised if something works not if something does not.

`example.lua` runs, output:
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
