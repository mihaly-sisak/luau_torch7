#ifndef LUAU_TORCH7_H
#define LUAU_TORCH7_H

// LUA_API == `extern "C"`, should be only `extern` in C code
#if defined(LUA_API) && !defined(__cplusplus)
#undef LUA_API
#define LUA_API extern
#endif

#include "lua.h"

// C part
LUA_API int luaopen_libtorch(lua_State *L);

// Lua part
LUA_API unsigned char torch_lua[];
LUA_API unsigned int  torch_lua_len;

#endif // LUAU_TORCH7_H