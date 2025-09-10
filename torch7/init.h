#ifndef LUAU_TORCH7_INIT_H
#define LUAU_TORCH7_INIT_H

#include "lua.h"

#ifdef __cplusplus
extern "C"
#endif
int luaopen_libtorch(lua_State *L);

#endif // LUAU_TORCH7_INIT_H