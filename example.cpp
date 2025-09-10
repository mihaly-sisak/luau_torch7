#include <iostream>
#include <fstream>
#include <sstream>

#include "luacode.h"
#include "lua.h"
#include "lualib.h"
#include "torch7/init.h"

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " script.luau" << std::endl;
        return 1;
    }

    // Step 1: Create a new Luau VM state
    lua_State* L = luaL_newstate();
    luaL_openlibs(L);
    luaopen_libtorch(L);
    luaL_sandbox(L);

    // Step 3: Read the user’s Luau source file into a string
    std::ifstream file(argv[1]);
    if (!file)
    {
        std::cerr << "Failed to open file: " << argv[1] << std::endl;
        return 1;
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();

    lua_CompileOptions complile_options = {};
    complile_options.optimizationLevel = 2;
    complile_options.debugLevel        = 0;

    size_t bytecodeSize = 0;
    char * bytecode     = luau_compile(source.c_str(), source.size(), &complile_options, &bytecodeSize);
    int    load_result  = luau_load(L, argv[1], bytecode, bytecodeSize, 0);
    free(bytecode);

    // Load bytecode into VM
    if (load_result != 0)
    {
        std::cerr << "Load error: " << lua_tostring(L, -1) << std::endl;
        lua_pop(L, 1); // remove error from stack
        lua_close(L);
        return 1;
    }

    // Step 5: Execute script safely
    if (lua_pcall(L, 0, LUA_MULTRET, 0) != 0)
    {
        std::cerr << "Runtime error: " << lua_tostring(L, -1) << std::endl;
        lua_pop(L, 1); // remove error from stack
        lua_close(L);
        return 1;
    }

    lua_close(L);
    return 0;
}