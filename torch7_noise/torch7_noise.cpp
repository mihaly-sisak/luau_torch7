#include "torch7_noise.h"
#include "TH.h"
#include "luaT.h"
#include "FastNoise.h"

static int torch_noise_simplex2D(lua_State *L)
{
    const char usage_str[] = "Usage: [res] torch.simplex2D([res,] scale, xstart, ystart, xsize, ysize, seed)\nError: %s\n";
    int narg = lua_gettop(L);
    if (((narg == 6) || (narg == 7)) == false) luaL_error(L, usage_str, "bad number of arguments");
    int isnum = 0;
    THFloatTensor* res = NULL;
    if (narg == 7){
        res = (THFloatTensor*)luaT_toudata(L, -7, "torch.FloatTensor");
        if (res == NULL) luaL_error(L, usage_str, "argument res is not a torch.FloatTensor");
    }
    float scale = lua_tonumberx(L, -6, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument scale is not a number");
    int xstart = lua_tointegerx(L, -5, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xstart is not an integer");
    int ystart = lua_tointegerx(L, -4, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ystart is not an integer");
    int xsize = lua_tointegerx(L, -3, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xsize is not an integer");
    int ysize = lua_tointegerx(L, -2, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ysize is not an integer");
    int seed = lua_tointegerx(L, -1, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument seed is not an integer");
    if (res){
        // resize guarantees res is 2d and contiguous
        THFloatTensor_resize2d(res, xsize, ysize);
    }
    else {
        res = THFloatTensor_newWithSize2d(xsize, ysize);
    }
    auto fn = FastNoise::New<FastNoise::Simplex>();
    fn->SetScale(scale);
    fn->GenUniformGrid2D(&res->storage->data[res->storageOffset], xstart, ystart, xsize, ysize, seed);
    luaT_pushudata(L, res, "torch.FloatTensor");
    return 1;
}

static int torch_noise_node2D(lua_State *L)
{
    const char usage_str[] = "Usage: [res] torch.node2D([res,] encoded node tree string, xstart, ystart, xsize, ysize, seed)\nError: %s\n";
    int narg = lua_gettop(L);
    if (((narg == 6) || (narg == 7)) == false) luaL_error(L, usage_str, "bad number of arguments");
    int isnum = 0;
    THFloatTensor* res = NULL;
    if (narg == 7){
        res = (THFloatTensor*)luaT_toudata(L, -7, "torch.FloatTensor");
        if (res == NULL) luaL_error(L, usage_str, "argument res is not a torch.FloatTensor");
    }
    const char* str = lua_tostring(L, -6);
    if (str == NULL) luaL_error(L, usage_str, "argument str is not a string");
    int xstart = lua_tointegerx(L, -5, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xstart is not an integer");
    int ystart = lua_tointegerx(L, -4, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ystart is not an integer");
    int xsize = lua_tointegerx(L, -3, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument xsize is not an integer");
    int ysize = lua_tointegerx(L, -2, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument ysize is not an integer");
    int seed = lua_tointegerx(L, -1, &isnum);
    if (isnum == 0) luaL_error(L, usage_str, "argument seed is not an integer");
    if (res){
        // resize guarantees res is 2d and contiguous
        THFloatTensor_resize2d(res, xsize, ysize);
    }
    else {
        res = THFloatTensor_newWithSize2d(xsize, ysize);
    }
    auto fn = FastNoise::NewFromEncodedNodeTree(str);
    if (bool(fn) == false) luaL_error(L, usage_str, "can not create noise generator from str");
    fn->GenUniformGrid2D(&res->storage->data[res->storageOffset], xstart, ystart, xsize, ysize, seed);
    luaT_pushudata(L, res, "torch.FloatTensor");
    return 1;
}

static const struct luaL_Reg torch_noise__ [] = {
    {"simplex2D", torch_noise_simplex2D},
    {"node2D", torch_noise_node2D},
    {NULL, NULL}
};

int luaopen_libtorch_noise(lua_State *L)
{
    // torch.FloatTensor
    luaT_pushmetatable(L, "torch.FloatTensor");
    assert(lua_istable(L, -1) == 1);
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

