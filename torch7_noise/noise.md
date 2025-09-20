# Noise functions #

Luau_Torch7 provides functions for coherent noise generation, based on the [FastNoise2](https://github.com/Auburn/FastNoise2) library.

These features can be turned on or off by setting the cmake option `LUAU_TORCH7_NOISE`. You can check for `LUAU_TORCH7_NOISE` define in C and `torch.hasNoise` in Lua to see if the noise functions are avaliable.

By default, all operations allocate a new `FloatTensor` to return the result.
However, all functions also support passing the target `FloatTensor`(s) as the first argument(s), in which case the target `FloatTensor`(s) will be resized accordingly and filled with result.
This property is especially useful when one wants have tight control over when memory is allocated.

The *Torch* package adopts the same concept, so that calling a function directly on the `FloatTensor` itself using an object-oriented syntax is equivalent to passing the `FloatTensor` as the optional resulting `FloatTensor`.
The following two calls are equivalent.

```lua
torch.simplex2D(x, 100.0, 0, 0, 8, 8, 1337)
x:simplex2D(100.0, 0, 0, 8, 8, 1337)
```

The functions can be used in the following manner.

```lua
res1 = torch.simplex2D(100.0, 0, 0, 8, 8, 1337)   -- case 1

res2 = torch.Tensor()
torch.simplex2D(res2, 100.0, 0, 0, 8, 8, 1337)    -- case 2
```

The advantage of second case is, same `res2` `FloatTensor` can be used successively in a loop without any new allocation.

## Noise functions ##

### [res] torch.simplex2D([res,] scale, xstart, ystart, xsize, ysize, seed) ###

`x = torch.simplex2D(scale, xstart, ystart, xsize, ysize, seed)` returns a 2D `FloatTensor`, containing samples of Simplex noise. Sampling happens at integer values, starting at `(xstart, ystart)`, ending with `(xstart+xsize-1, ystart+ysize-1)`. The output tensor has a size of `(xsize, ysize)`.

The Simplex noise can be customized with a `scale` (float) and `seed` (integer) value. Check out the FastNoise2 Node Editor for more information.

### [res] torch.node2D([res,] encoded_node_tree_string, xstart, ystart, xsize, ysize, seed) ###

`x = torch.node2D(encoded_node_tree_string, xstart, ystart, xsize, ysize, seed)` returns a 2D `FloatTensor`, containing samples of coherent noise. Sampling happens at integer values, starting at `(xstart, ystart)`, ending with `(xstart+xsize-1, ystart+ysize-1)`. The output tensor has a size of `(xsize, ysize)`.

The encoded node tree string can be obtained by creating a noise configuration with FastNoise2 Node Editor.
