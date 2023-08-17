cmake_minimum_required(VERSION 3.12)

project(c64_parser)

include(ExternalProject)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_CXX_EXTENSIONS OFF)

option(KHPARSE_BUILD_EXAMPLES "Build examples if true")

function(clone_repo name url tag)
   execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/repos/${name})
   execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/repo_versions/)
   if (EXISTS ${CMAKE_CURRENT_BINARY_DIR}/repo_versions/${name})
      file(READ ${CMAKE_CURRENT_BINARY_DIR}/repo_versions/${name} version)
      if (${version} STREQUAL ${tag})
         return()
      endif()
   endif()
   find_package(Git)
   # gcmd = git command
   function(gcmd)
      execute_process(
         COMMAND ${GIT_EXECUTABLE} ${ARGV}
         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/repos/${name}
      )
   endfunction()

   gcmd(init)
   gcmd(remote add origin ${url})
   gcmd(fetch --depth 1 origin ${tag})
   gcmd(checkout FETCH_HEAD)
   file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/repo_versions/${name} ${tag})
endfunction()

function(install_repo name #[[configure_args...]])
   list(POP_FRONT ARGV)
   execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/repos/${name}/build
   )
   execute_process(
      COMMAND ${CMAKE_COMMAND} ${ARGV} -G ${CMAKE_GENERATOR} ..
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/repos/${name}/build
   )
   execute_process(
      COMMAND ${CMAKE_COMMAND} --build ${CMAKE_CURRENT_BINARY_DIR}/repos/${name}/build --target install
   )
endfunction()

clone_repo(ctre https://github.com/hanickadot/compile-time-regular-expressions.git 5b914a62c71c6c6b336fd466a4e812db635b9fdc)
add_subdirectory(${CMAKE_CURRENT_BINARY_DIR}/repos/ctre)

clone_repo(expected https://github.com/martinmoene/expected-lite.git 6284387cb117ea78d973fb5b1cbff1651a8d5d9a)
add_library(expected INTERFACE)
target_include_directories(expected SYSTEM INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/repos/expected/include)

if (MSVC)
   add_compile_options(/W4)
else()
   add_compile_options(-Wall -Wextra -Wpedantic)
endif()

add_library(khparse INTERFACE)
target_link_libraries(khparse INTERFACE expected ctre::ctre)
target_include_directories(khparse INTERFACE src)

add_library(khparse::khparse ALIAS khparse)

if (KHPARSE_BUILD_EXAMPLES)
   add_executable(calculator examples/calculator.cpp)
   target_link_libraries(calculator PUBLIC khparse)
endif()
