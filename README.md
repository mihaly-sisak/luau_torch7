# Luau_Torch7

Luau-compatible fork of Torch7.

Proof of concept, be suprised if something works not if something does not.

`example.lua` runs, output:
```
tensor<2x2>(1, 1, 1, 1)
tensor<2x2>(2, 2, 2, 2)
tensor<2x2>(3, 3, 3, 3)
```

## How to build

 - Check out the luau submodule
 - Patch it with `luau.diff`
 - Build and run with `build.sh`
