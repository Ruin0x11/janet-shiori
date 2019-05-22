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
    add_compile_options("/GL" "/MT$<$<CONFIG:Debug>:d>" "/source-charset:utf-8")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:CONSOLE")
else()
    add_compile_options("-O3" "-DNDEBUG" "-Wall")
endif()

# add_executable(xxd WIN32 xxd.c)
#
# add_custom_command(OUTPUT ${CMAKE_SOURCE_DIR}/init.gen.c
#                    COMMAND xxd ${CMAKE_SOURCE_DIR}/init.janet ${CMAKE_SOURCE_DIR}/init.gen.c janet_gen_init
#                    MAIN_DEPENDENCY ${CMAKE_SOURCE_DIR}/init.janet
#                    DEPENDS xxd)

add_library(shiori SHARED shiori.c)
add_executable(shiolink WIN32 shiolink.c)

target_link_libraries(shiolink shiori)

add_custom_command(
        TARGET shiori POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
                ${CMAKE_SOURCE_DIR}/init.janet
                ${CMAKE_CURRENT_BINARY_DIR}/init.janet)

add_custom_command(
        TARGET shiolink POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
                ${CMAKE_SOURCE_DIR}/init.janet
                ${CMAKE_CURRENT_BINARY_DIR}/init.janet)
