cmake_minimum_required(VERSION 3.14)
project(LuauTorch7 C)
include(FetchContent)

option(LUAU_TORCH7_EXAMPLE "Build standalone example exe"        ON)
option(LUAU_TORCH7_NOISE   "Include FastNoise2 noise generation" OFF)

message("LUAU_TORCH7_EXAMPLE = ${LUAU_TORCH7_EXAMPLE}")
message("LUAU_TORCH7_NOISE   = ${LUAU_TORCH7_NOISE}")

#add_compile_options(-fno-omit-frame-pointer -mno-omit-leaf-frame-pointer)

#add_compile_options(-fsanitize=address)
#add_link_options(-fsanitize=address)

# Luau

FetchContent_Declare(
    luau
    GIT_REPOSITORY https://github.com/luau-lang/luau.git
    GIT_TAG        origin/master
    GIT_SHALLOW    TRUE
    GIT_PROGRESS   TRUE
)

set(LUAU_BUILD_CLI   OFF CACHE BOOL "Build CLI"                      FORCE)
set(LUAU_BUILD_TESTS OFF CACHE BOOL "Build tests"                    FORCE)
set(LUAU_BUILD_WEB   OFF CACHE BOOL "Build Web module"               FORCE)
set(LUAU_WERROR      OFF CACHE BOOL "Warnings as errors"             FORCE)
set(LUAU_STATIC_CRT  OFF CACHE BOOL "Link with the static CRT (/MT)" FORCE)
set(LUAU_EXTERN_C    ON  CACHE BOOL "Use extern C for all APIs"      FORCE)

FetchContent_MakeAvailable(luau)

# Torch7

set(torch7_src
#    ./torch7/DiskFile.c
#    ./torch7/File.c
    ./torch7/Generator.c
#    ./torch7/generic/Storage.c
#    ./torch7/generic/Tensor.c
#    ./torch7/generic/TensorOperator.c
    ./torch7/init.c
    ./torch7/lib/luaT/luaT.c
    ./torch7/lib/TH/generic/simd/convolve.c
    ./torch7/lib/TH/generic/simd/convolve5x5_avx.c
    ./torch7/lib/TH/generic/simd/convolve5x5_sse.c
#    ./torch7/lib/TH/generic/THBlas.c
#    ./torch7/lib/TH/generic/THLapack.c
#    ./torch7/lib/TH/generic/THStorage.c
#    ./torch7/lib/TH/generic/THStorageCopy.c
#    ./torch7/lib/TH/generic/THTensor.c
#    ./torch7/lib/TH/generic/THTensorConv.c
#    ./torch7/lib/TH/generic/THTensorCopy.c
#    ./torch7/lib/TH/generic/THTensorLapack.c
#    ./torch7/lib/TH/generic/THTensorMath.c
#    ./torch7/lib/TH/generic/THTensorRandom.c
#    ./torch7/lib/TH/generic/THVectorDefault.c
#    ./torch7/lib/TH/generic/THVectorDispatch.c
    ./torch7/lib/TH/THAllocator.c
    ./torch7/lib/TH/THAtomic.c
    ./torch7/lib/TH/THBlas.c
#    ./torch7/lib/TH/THDiskFile.c
    ./torch7/lib/TH/THFile.c
    ./torch7/lib/TH/THGeneral.c
    ./torch7/lib/TH/THHalf.c
    ./torch7/lib/TH/THLapack.c
    ./torch7/lib/TH/THLogAdd.c
#    ./torch7/lib/TH/THMemoryFile.c
    ./torch7/lib/TH/THRandom.c
    ./torch7/lib/TH/THSize.c
    ./torch7/lib/TH/THStorage.c
    ./torch7/lib/TH/THTensor.c
    ./torch7/lib/TH/THVector.c
    ./torch7/lib/TH/vector/AVX.c
    ./torch7/lib/TH/vector/AVX2.c
#    ./torch7/lib/TH/vector/NEON.c
    ./torch7/lib/TH/vector/SSE.c
#    ./torch7/lib/TH/vector/VSX.c
#    ./torch7/MemoryFile.c
#    ./torch7/PipeFile.c
    ./torch7/Storage.c
    ./torch7/Tensor.c
    ./torch7/TensorOperator.c
    ./torch7/Timer.c
    ./torch7/utils.c
    ./torch7/generated/random.c
    ./torch7/generated/TensorMath.c
    ./torch7/generated/torch_lua.c
    ./torch7/generated/torch_types.c
)

add_library(torch7 ${torch7_src})

target_compile_definitions(torch7 PUBLIC LUA_VERSION_NUM=501)
target_compile_definitions(torch7 PUBLIC LUAU=1)

target_include_directories(torch7 PRIVATE
    ./torch7/generated
    ./torch7/lib/luaT
    ./torch7/lib/TH
    ./torch7
    ${luau_SOURCE_DIR}/VM/include
)

target_link_libraries(torch7 PRIVATE
    Luau.VM
)

# Torch7 blas, lapack

set(USE_BLAS   FALSE)
set(USE_LAPACK FALSE)

find_package(BLAS)
if(BLAS_FOUND)
    set(USE_BLAS TRUE)
    target_link_libraries(torch7 PRIVATE ${BLAS_LIBRARIES})
endif()

find_package(LAPACK)
if(LAPACK_FOUND)
    set(USE_LAPACK TRUE)
    target_link_libraries(torch7 PRIVATE ${LAPACK_LIBRARIES})
endif()

# putting configured file in source dir, needed for accessing torch types from cpp code
configure_file(./torch7/lib/TH/THGeneral.h.in ${CMAKE_CURRENT_SOURCE_DIR}/torch7/lib/TH/THGeneral.h)

# Torch7 vector extensions, has runtime CPU feature detection

target_compile_definitions(torch7 PRIVATE USE_SSE2)
target_compile_definitions(torch7 PRIVATE USE_SSE3)
target_compile_definitions(torch7 PRIVATE USE_SSSE3)
target_compile_definitions(torch7 PRIVATE USE_SSE4_1)
target_compile_definitions(torch7 PRIVATE USE_SSE4_2)
target_compile_definitions(torch7 PRIVATE USE_AVX)
target_compile_definitions(torch7 PRIVATE USE_AVX2)

set_source_files_properties(./torch7/lib/TH/generic/simd/convolve5x5_sse.c PROPERTIES COMPILE_FLAGS "-O3 -ffast-math")
set_source_files_properties(./torch7/lib/TH/generic/simd/convolve5x5_avx.c PROPERTIES COMPILE_FLAGS "-O3 -ffast-math -mavx")
set_source_files_properties(./torch7/lib/TH/vector/AVX.c PROPERTIES COMPILE_FLAGS "-O3 -mavx")
set_source_files_properties(./torch7/lib/TH/vector/AVX2.c PROPERTIES COMPILE_FLAGS "-O3 -mavx2 -mfma")

# Torch7 atomics

if (WIN32 OR MINGW)
    target_compile_definitions(torch7 PRIVATE USE_MSC_ATOMICS)
    message("Torch7 atomics: MSC")
elseif (UNIX)
    target_compile_definitions(torch7 PRIVATE USE_GCC_ATOMICS)
    message("Torch7 atomics: GCC")
else()
    message(FATAL_ERROR "Torch7 atomics: error")
endif()

# all atomics options
#target_compile_definitions(torch7 PRIVATE USE_C11_ATOMICS)
#target_compile_definitions(torch7 PRIVATE USE_MSC_ATOMICS)
#target_compile_definitions(torch7 PRIVATE USE_GCC_ATOMICS)
#target_compile_definitions(torch7 PRIVATE USE_PTHREAD_ATOMICS)

# Torch7 FastNoise2

if (LUAU_TORCH7_NOISE)

    enable_language(CXX)

    FetchContent_Declare(
        FastNoise2
        GIT_REPOSITORY https://github.com/Auburn/FastNoise2.git
        GIT_TAG        origin/NewFastSIMD
    )

    set(FASTNOISE2_TOOLS     ON  CACHE BOOL "Build \"Node Editor\" executable"                                                                          FORCE)
    set(FASTNOISE2_TESTS     OFF CACHE BOOL "Build tests"                                                                                               FORCE)
    set(FASTNOISE2_UTILITY   OFF CACHE BOOL "Build utility tools"                                                                                       FORCE)
    set(FASTNOISE2_STRICT_FP OFF CACHE BOOL "Enable strict floating point calculations to ensure output from different SIMD feature sets match EXACTLY" FORCE)

    FetchContent_MakeAvailable(FastNoise2)

    target_sources(torch7 PRIVATE
        ./torch7_noise/torch7_noise.cpp
    )

    target_include_directories(torch7 PRIVATE
        ${FastNoise2_SOURCE_DIR}/include/FastNoise
    )

    target_link_libraries(torch7 PRIVATE
        FastNoise
    )

    target_compile_definitions(torch7 PUBLIC 
        LUAU_TORCH7_NOISE
    )

endif()

# main exe

if (LUAU_TORCH7_EXAMPLE)

    enable_language(CXX)

    add_executable(luau_example example.cpp)

    target_link_libraries(luau_example PRIVATE
        Luau.VM
        Luau.Compiler
        torch7
    )

    target_include_directories(luau_example PRIVATE
        ${luau_SOURCE_DIR}/VM/include
        ${luau_SOURCE_DIR}/Compiler/include
    )

endif()
