#ifndef LUAU_TORCH7_NOISE_H
#define LUAU_TORCH7_NOISE_H

// LUA_API == `extern "C"`, should be only `extern` in C code
#if defined(LUA_API) && !defined(__cplusplus)
#undef LUA_API
#define LUA_API extern
#endif

#include "lua.h"

// C part
LUA_API int luaopen_libtorch_noise(lua_State *L);

#endif // LUAU_TORCH7_NOISE_H