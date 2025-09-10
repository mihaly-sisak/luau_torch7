#include <iostream>
#include <fstream>
#include <sstream>

#include "luacode.h"
#include "lua.h"
#include "lualib.h"
#include "torch7/init.h"

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

int exec_luau_source(lua_State* L, std::string& chunkname, std::string& source)
{
    lua_CompileOptions complile_options = {};
    complile_options.optimizationLevel = 2;
    complile_options.debugLevel        = 0;
    
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

    // execute script
    if (lua_pcall(L, 0, LUA_MULTRET, 0) != 0)
    {
        std::cerr << "Runtime error: " << lua_tostring(L, -1) << std::endl;
        lua_pop(L, 1); // remove error from stack
        lua_close(L);
        return 1;
    }

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
    luaopen_libtorch(L);

    // load Torch7 lua init code, this modifies global state
    {
        std::string torch7_filename = "./torch_luau.lua";
        std::string torch7_src      = load_file_to_string(torch7_filename);
        exec_luau_source(L, torch7_filename, torch7_src);
    }

    // lock global state
    luaL_sandbox(L);

    // load user lua code
    {
        std::string user_filename = std::string(argv[1]);
        std::string user_src      = load_file_to_string(user_filename);
        exec_luau_source(L, user_filename, user_src);
    }

    lua_close(L);
    return 0;
}