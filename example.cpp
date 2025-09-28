#include <iostream>
#include <fstream>
#include <sstream>
#include <thread>
#include <chrono>

// luau includes
#include "luacode.h"
#include "lua.h"
#include "lualib.h"

// torch7 includes
#include "torch7/torch7.h"
#ifdef LUAU_TORCH7_NOISE
#include "torch7_noise/torch7_noise.h"
#endif

// torch7 internals for custom C functions
#include "torch7/lib/TH/TH.h"
#include "torch7/lib/luaT/luaT.h"

std::string load_file_to_string(std::string& filename)
{
    std::ifstream file(filename);
    if (!file)
    {
        std::cerr << "Failed to open file: " << filename << std::endl;
        return "";
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

int exec_luau_source(lua_State* L, std::string chunkname, std::string& source)
{
    lua_CompileOptions complile_options = {};
    //// debug
    //complile_options.optimizationLevel = 0;
    //complile_options.debugLevel        = 2;
    // default
    complile_options.optimizationLevel = 1;
    complile_options.debugLevel        = 1;
    //// performance
    //complile_options.optimizationLevel = 2;
    //complile_options.debugLevel        = 0;
    
    size_t bytecodeSize = 0;
    char * bytecode     = luau_compile(source.c_str(), source.size(), &complile_options, &bytecodeSize);
    int    load_result  = luau_load(L, chunkname.c_str(), bytecode, bytecodeSize, 0);
    free(bytecode);

    // load bytecode into VM
    if (load_result != 0)
    {
        std::cerr << "Load error: " << lua_tostring(L, -1) << std::endl;
        lua_pop(L, 1); // remove error from stack
        lua_close(L);
        return 1;
    }

    /* Stack: [ ... , chunk ] */

    /* 2) push debug.traceback and remove the debug table (so stack becomes [ ... , chunk, traceback] ) */
    lua_getglobal(L, "debug");            /* pushes debug table */
    lua_getfield(L, -1, "traceback");     /* pushes debug.traceback */
    lua_remove(L, -2);                    /* remove debug table; stack: [ ... , chunk, traceback ] */

    /* 3) move traceback *below* the chunk => stack becomes [ ... , traceback, chunk ] */
    lua_insert(L, -2);

    int errfunc = lua_gettop(L) - 1;

    // execute script
    if (lua_pcall(L, 0, LUA_MULTRET, errfunc) != 0)
    {
        std::cerr << "Runtime error: " << lua_tostring(L, -1) << std::endl;
        lua_pop(L, 1); // remove error from stack
        lua_close(L);
        return 1;
    }

    lua_remove(L, errfunc);

    return 0;
}

int c_tensor_init(lua_State* L)
{
    int size_0 = 3;
    int size_1 = 3;
    THFloatTensor* t = THFloatTensor_newWithSize2d(size_0, size_1);
    for (int x1 = 0; x1 < size_1; x1++)
        for (int x0 = 0; x0 < size_0; x0++)
            THFloatTensor_set2d(t, x0, x1, 1.0f);
    luaT_pushudata(L, t, "torch.FloatTensor");
    return 1;
}

int c_tensor_modify(lua_State* L)
{
    int narg = lua_gettop(L);
    if (narg != 1) 
        luaL_error(L, "expected 1 argument");
    THFloatTensor *arg1 = (THFloatTensor*)luaT_toudata(L, 1, "torch.FloatTensor");
    if (arg1 == NULL) 
        luaL_error(L, "expected torch.FloatTensor argument");
    if (THFloatTensor_nDimension(arg1) != 2) 
        luaL_error(L, "expected 2d torch.FloatTensor argument");
    int size_0 = THFloatTensor_size(arg1, 0);
    int size_1 = THFloatTensor_size(arg1, 1);
    THFloatTensor *ret  = THFloatTensor_newWithSize2d(size_0, size_1);
    for (int x1 = 0; x1 < size_1; x1++)
        for (int x0 = 0; x0 < size_0; x0++)
            THFloatTensor_set2d(ret, x0, x1, THFloatTensor_get2d(arg1, x0, x1) * 2.0f);
    luaT_pushudata(L, ret, "torch.FloatTensor");
    return 1;
}

// needed for torch.Timer() test
int luau_sleep(lua_State* L)
{
    int narg = lua_gettop(L);
    if (narg != 1) 
        luaL_error(L, "luau_sleep(sleep_sec : number) expected 1 argument");
    int isnum = 0;
    double sleep_sec = lua_tonumberx(L, -1, &isnum);
    if (isnum == 0) luaL_error(L, "luau_sleep(sleep_sec : number) argument sleep_sec is not a number");
    auto dur = std::chrono::duration<double, std::milli>(sleep_sec * 1000.0);
    std::this_thread::sleep_for(dur);
    return 0;
}

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " script.luau" << std::endl;
        return 1;
    }

    // create a new Luau VM state
    lua_State* L = luaL_newstate();
    luaL_openlibs(L);

    // load Torch7 C library
    lua_pushcfunction(L, luaopen_libtorch, "luaopen_libtorch");
    lua_call(L, 0, 0);
    #ifdef LUAU_TORCH7_NOISE
    lua_pushcfunction(L, luaopen_libtorch_noise, "luaopen_libtorch_noise");
    lua_call(L, 0, 0);
    #endif

    // load Torch7 lua init code, this modifies global state
    {
        std::string torch7_src = std::string(reinterpret_cast<char*>(torch_lua), torch_lua_len);
        if (exec_luau_source(L, "torch7_init", torch7_src)) return 1;
    }

    // load user defined functions
    lua_pushcfunction(L, c_tensor_init, "c_tensor_init");
    lua_setglobal(L, "c_tensor_init");
    lua_pushcfunction(L, c_tensor_modify, "c_tensor_modify");
    lua_setglobal(L, "c_tensor_modify");
    lua_pushcfunction(L, luau_sleep, "luau_sleep");
    lua_setglobal(L, "luau_sleep");

    // lock global state
    luaL_sandbox(L);

    // load user lua code
    {
        std::string user_filename = std::string(argv[1]);
        std::string user_src      = load_file_to_string(user_filename);
        if (exec_luau_source(L, user_filename, user_src)) return 1;
    }

    lua_close(L);
    return 0;
}