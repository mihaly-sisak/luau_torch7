#include "torch7_noise.h"
#include "TH.h"
#include "luaT.h"
#include "FastNoise.h"

struct common_args_s {
    float start [3] = {};
    int   size  [3] = {};
    float step  [3] = {};
    int   seed      = 0;
};

// processes last 7 arguments
static common_args_s get_args_2D(lua_State *L, const char* usage_str)
{
    common_args_s ret;
    int isnum = 0;

    ret.start[0] = lua_tonumberx(L, -7, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xstart is not a number");
    ret.start[1] = lua_tonumberx(L, -6, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ystart is not a number");

    ret.size[0] = lua_tointegerx(L, -5, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xsize is not an integer");
    ret.size[1] = lua_tointegerx(L, -4, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ysize is not an integer");

    ret.step[0] = lua_tonumberx(L, -3, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xstep is not a number");
    ret.step[1] = lua_tonumberx(L, -2, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ystep is not a number");

    ret.seed = lua_tointegerx(L, -1, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument seed is not an integer");

    return ret;
}

// processes last 10 arguments
static common_args_s get_args_3D(lua_State *L, const char* usage_str)
{
    common_args_s ret;
    int isnum = 0;

    ret.start[0] = lua_tonumberx(L, -10, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xstart is not a number");
    ret.start[1] = lua_tonumberx(L, -9, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ystart is not a number");
    ret.start[2] = lua_tonumberx(L, -8, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument zstart is not a number");

    ret.size[0] = lua_tointegerx(L, -7, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xsize is not an integer");
    ret.size[1] = lua_tointegerx(L, -6, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ysize is not an integer");
    ret.size[2] = lua_tointegerx(L, -5, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument zsize is not an integer");

    ret.step[0] = lua_tonumberx(L, -4, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xstep is not a number");
    ret.step[1] = lua_tonumberx(L, -3, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ystep is not a number");
    ret.step[2] = lua_tonumberx(L, -2, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument zstep is not a number");

    ret.seed = lua_tointegerx(L, -1, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument seed is not an integer");

    return ret;
}

static int torch_noise_simplex2D(lua_State *L)
{
    const char usage_str[] = "Usage: [res] torch.simplex2D([res,] xstart, ystart, xsize, ysize, xstep, ystep, seed)\nError: %s\n";
    int narg = lua_gettop(L);
    if (((narg == 7) || (narg == 8)) == false) luaL_error(L, usage_str, "bad number of arguments");
    THFloatTensor* res = NULL;
    if (narg == 8){
        res = (THFloatTensor*)luaT_toudata(L, -8, "torch.FloatTensor");
        if (res == NULL) luaL_error(L, usage_str, "argument res is not a torch.FloatTensor");
    }
    common_args_s args = get_args_2D(L, usage_str);
    if (res){
        // resize guarantees res is 2d and contiguous
        THFloatTensor_resize2d(res, args.size[0], args.size[1]);
    }
    else {
        res = THFloatTensor_newWithSize2d(args.size[0], args.size[1]);
    }
    auto fn = FastNoise::New<FastNoise::Simplex>();
    fn->GenUniformGrid2D(&res->storage->data[res->storageOffset], 
            args.start[0], args.start[1], 
            args.size [0], args.size [1], 
            args.step [0], args.step [1], args.seed);
    luaT_pushudata(L, res, "torch.FloatTensor");
    return 1;
}

static int torch_noise_simplex3D(lua_State *L)
{
    const char usage_str[] = "Usage: [res] torch.simplex3D([res,] xstart, ystart, zstart, xsize, ysize, zsize, xstep, ystep, zstep, seed)\nError: %s\n";
    int narg = lua_gettop(L);
    if (((narg == 10) || (narg == 11)) == false) luaL_error(L, usage_str, "bad number of arguments");
    THFloatTensor* res = NULL;
    if (narg == 11){
        res = (THFloatTensor*)luaT_toudata(L, -11, "torch.FloatTensor");
        if (res == NULL) luaL_error(L, usage_str, "argument res is not a torch.FloatTensor");
    }
    common_args_s args = get_args_3D(L, usage_str);
    if (res){
        // resize guarantees res is 3d and contiguous
        THFloatTensor_resize3d(res, args.size[0], args.size[1], args.size[2]);
    }
    else {
        res = THFloatTensor_newWithSize3d(args.size[0], args.size[1], args.size[2]);
    }
    auto fn = FastNoise::New<FastNoise::Simplex>();
    fn->GenUniformGrid3D(&res->storage->data[res->storageOffset], 
            args.start[0], args.start[1], args.start[2], 
            args.size [0], args.size [1], args.size [2], 
            args.step [0], args.step [1], args.step [2], args.seed);
    luaT_pushudata(L, res, "torch.FloatTensor");
    return 1;
}

static int torch_noise_node2D(lua_State *L)
{
    const char usage_str[] = "Usage: [res] torch.node2D([res,] encoded_node_tree_string, xstart, ystart, xsize, ysize, xstep, ystep, seed)\nError: %s\n";
    int narg = lua_gettop(L);
    if (((narg == 8) || (narg == 9)) == false) luaL_error(L, usage_str, "bad number of arguments");
    THFloatTensor* res = NULL;
    if (narg == 9){
        res = (THFloatTensor*)luaT_toudata(L, -9, "torch.FloatTensor");
        if (res == NULL) luaL_error(L, usage_str, "argument res is not a torch.FloatTensor");
    }
    const char* str = lua_tostring(L, -8);
    if (str == NULL) luaL_error(L, usage_str, "argument str is not a string");
    common_args_s args = get_args_2D(L, usage_str);
    if (res){
        // resize guarantees res is 2d and contiguous
        THFloatTensor_resize2d(res, args.size[0], args.size[1]);
    }
    else {
        res = THFloatTensor_newWithSize2d(args.size[0], args.size[1]);
    }
    auto fn = FastNoise::NewFromEncodedNodeTree(str);
    if (bool(fn) == false) luaL_error(L, usage_str, "can not create noise generator from str");
    fn->GenUniformGrid2D(&res->storage->data[res->storageOffset], 
            args.start[0], args.start[1], 
            args.size [0], args.size [1], 
            args.step [0], args.step [1], args.seed);
    luaT_pushudata(L, res, "torch.FloatTensor");
    return 1;
}

static int torch_noise_node3D(lua_State *L)
{
    const char usage_str[] = "Usage: [res] torch.node3D([res,] encoded_node_tree_string, xstart, ystart, zstart, xsize, ysize, zsize, xstep, ystep, zstep, seed)\nError: %s\n";
    int narg = lua_gettop(L);
    if (((narg == 11) || (narg == 12)) == false) luaL_error(L, usage_str, "bad number of arguments");
    int isnum = 0;
    THFloatTensor* res = NULL;
    if (narg == 12){
        res = (THFloatTensor*)luaT_toudata(L, -12, "torch.FloatTensor");
        if (res == NULL) luaL_error(L, usage_str, "argument res is not a torch.FloatTensor");
    }
    const char* str = lua_tostring(L, -11);
    if (str == NULL) luaL_error(L, usage_str, "argument str is not a string");
    common_args_s args = get_args_3D(L, usage_str);
    if (res){
        // resize guarantees res is 3d and contiguous
        THFloatTensor_resize3d(res, args.size[0], args.size[1], args.size[2]);
    }
    else {
        res = THFloatTensor_newWithSize3d(args.size[0], args.size[1], args.size[2]);
    }
    auto fn = FastNoise::NewFromEncodedNodeTree(str);
    if (bool(fn) == false) luaL_error(L, usage_str, "can not create noise generator from str");
    fn->GenUniformGrid3D(&res->storage->data[res->storageOffset], 
            args.start[0], args.start[1], args.start[2], 
            args.size [0], args.size [1], args.size [2], 
            args.step [0], args.step [1], args.step [2], args.seed);
    luaT_pushudata(L, res, "torch.FloatTensor");
    return 1;
}

static const struct luaL_Reg torch_noise__ [] = {
    {"simplex2D", torch_noise_simplex2D},
    {"simplex3D", torch_noise_simplex3D},
    {"node2D", torch_noise_node2D},
    {"node3D", torch_noise_node3D},
    {NULL, NULL}
};

int luaopen_libtorch_noise(lua_State *L)
{
    // torch.FloatTensor
    int ok = luaT_pushmetatable(L, "torch.FloatTensor");
    assert(ok == 1);
    luaT_setfuncs(L, torch_noise__, 0);
    lua_pop(L, -1);
    // torch
    lua_getglobal(L, "torch");
    assert(lua_istable(L, -1) == 1);
    // torch.hasNoise == 1
    lua_pushstring(L, "hasNoise");
    lua_pushinteger(L, 1);
    lua_settable(L, -3);
    // set functions
    luaT_setfuncs(L, torch_noise__, 0);
    return 1;
}

