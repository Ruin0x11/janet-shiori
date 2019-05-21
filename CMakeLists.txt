project(janet-shiori)

cmake_minimum_required(VERSION 2.8)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

set(LINUX FALSE)
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  set(LINUX TRUE)
endif()

if(MSVC)
    add_definitions("-D_UNICODE")
    add_compile_options("/GL" "/MT$<$<CONFIG:Debug>:d>")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:CONSOLE")
else()
    add_compile_options("-O3" "-DNDEBUG" "-Wall")
endif()

add_library(shiori SHARED shiori.c)
add_executable(shiolink WIN32 shiolink.c)

target_link_libraries(shiolink shiori)
