cmake_minimum_required(VERSION 3.15)
project(LuauTorch7Example CXX C)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_compile_options(-fno-omit-frame-pointer -mno-omit-leaf-frame-pointer)

# Luau

set(LUAU_BUILD_CLI   OFF)
set(LUAU_BUILD_TESTS OFF)
set(LUAU_BUILD_WEB   OFF)
set(LUAU_WERROR      OFF)
set(LUAU_STATIC_CRT  OFF)
set(LUAU_EXTERN_C    OFF)

add_subdirectory(luau)

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
    ./torch7/lib/TH/THDiskFile.c
    ./torch7/lib/TH/THFile.c
    ./torch7/lib/TH/THGeneral.c
    ./torch7/lib/TH/THHalf.c
    ./torch7/lib/TH/THLapack.c
    ./torch7/lib/TH/THLogAdd.c
    ./torch7/lib/TH/THMemoryFile.c
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
)

add_library(torch7 ${torch7_src})

target_compile_definitions(torch7 PRIVATE LUA_VERSION_NUM=501)
target_compile_definitions(torch7 PRIVATE LUAU=1)

target_include_directories(torch7 PRIVATE
    ./torch7/lib/luaT
    ./torch7/lib/TH
    ./torch7
    ./luau/VM/include
    ${CMAKE_CURRENT_BINARY_DIR}
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

configure_file(./torch7/lib/TH/THGeneral.h.in "${CMAKE_CURRENT_BINARY_DIR}/THGeneral.h")

# Torch7 vector extensions

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

#target_compile_definitions(torch7 PRIVATE USE_C11_ATOMICS)
#target_compile_definitions(torch7 PRIVATE USE_MSC_ATOMICS)
target_compile_definitions(torch7 PRIVATE USE_GCC_ATOMICS)
#target_compile_definitions(torch7 PRIVATE USE_PTHREAD_ATOMICS)

# Torch7 mmap

#target_compile_definitions(torch7 PRIVATE _WIN32)
target_compile_definitions(torch7 PRIVATE HAVE_MMAP)

# main exe

add_executable(luau_example example.cpp)

target_link_libraries(luau_example PRIVATE
    Luau.VM
    Luau.Compiler
    torch7
)

target_include_directories(luau_example PRIVATE
    ./luau/VM/include
    ./luau/Compiler/include
)
