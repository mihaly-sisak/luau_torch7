#ifndef TORCH_UTILS_INC
#define TORCH_UTILS_INC

// LUA_API == `extern "C"`, should be only `extern` in C code
#if defined(LUA_API) && !defined(__cplusplus)
#undef LUA_API
#define LUA_API extern
#endif

#include "luaT.h"
#include "TH.h"

#include <lua.h>
#include <lualib.h>

#ifdef _WIN32
#else
#include <unistd.h>
#endif

#ifdef __cplusplus
# define TORCH_EXTERNC extern "C"
#else
# define TORCH_EXTERNC extern
#endif

#ifdef _WIN32
# ifdef torch_EXPORTS
#  define TORCH_API TORCH_EXTERNC __declspec(dllexport)
# else
#  define TORCH_API TORCH_EXTERNC __declspec(dllimport)
# endif
#else
# define TORCH_API TORCH_EXTERNC
#endif


TORCH_API THLongStorage* torch_checklongargs(lua_State *L, int index);
TORCH_API int torch_islongargs(lua_State *L, int index);
TORCH_API const char* torch_getdefaulttensortype(lua_State *L);

#endif
