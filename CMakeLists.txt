cmake_minimum_required(VERSION 3.12)

project(c64_parser)

include(ExternalProject)

ExternalProject_Add(
   expected_download
   PREFIX ${CMAKE_BINARY_DIR}/nonstd
   URL https://raw.githubusercontent.com/martinmoene/expected-lite/master/include/nonstd/expected.hpp
   DOWNLOAD_NO_EXTRACT 1
   CONFIGURE_COMMAND ""
   BUILD_COMMAND ""
   INSTALL_COMMAND ""
)

add_library(expected INTERFACE)
add_dependencies(expected_download expected_download)
target_include_directories(expected SYSTEM INTERFACE ${CMAKE_BINARY_DIR}/nonstd)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_CXX_EXTENSIONS OFF)

if (MSVC)
   add_compile_options(/W4)
else()
   add_compile_options(-Wall -Wextra -Wpedantic)
endif()

add_executable(parser
   src/main.cpp
)

target_link_libraries(parser PUBLIC expected)
add_subdirectory(benchmarks)
