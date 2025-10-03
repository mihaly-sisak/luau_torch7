# Collecting

```c
static char* type_list[1024];

static void append_to_type_list(const char *tname)
{
  for (int i = 0; i < 1024; i++){
    char* p = type_list[i];
    if (p){
      if (strcmp(p, tname) == 0) return;
    }
    else {
      int l = strlen(tname) + 1;
      p = type_list[i] = malloc(l);
      strncpy(p, tname, l);
      return;
    }
  }
}

void luaT_printtypes()
{
  fprintf(stderr, "luaT_printtypes()\n");
  for (int i = 0; i < 1024; i++){
    char* p = type_list[i];
    if (p) fprintf(stderr, "%s\n", p);
  }
}
```

Put `append_to_type_list(tname);` in `luaT_newlocalmetatable`.

At the end of the `luau_test.lua`, dumpt the results with `luaT_printtypes()`.

Output:

```
torch.ByteStorage
torch.CharStorage
torch.ShortStorage
torch.IntStorage
torch.LongStorage
torch.FloatStorage
torch.DoubleStorage
torch.HalfStorage
torch.ByteTensor
torch.CharTensor
torch.ShortTensor
torch.IntTensor
torch.LongTensor
torch.FloatTensor
torch.DoubleTensor
torch.HalfTensor
torch.Timer
torch.Generator
torch.Allocator
```

# Generating

Check out the .gperf file based on the type strings.

Generate a perfect hash function with `gperf torch_types.gperf > torch_types.c`
