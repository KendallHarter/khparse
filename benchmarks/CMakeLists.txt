include(ExternalProject)
find_package(benchmark)

if (NOT ${benchmark_FOUND})
   ExternalProject_Add(
      gbench
      SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}
      GIT_REPOSITORY https://github.com/google/benchmark.git
      GIT_TAG 5704cd4c8cea889d68f9ae29ca5aaee97ef91816
      CONFIGURE_COMMAND ""
      BUILD_COMMAND ""
      INSTALL_COMMAND ""
   )
endif()

find_package(Python3 COMPONENTS Interpreter REQUIRED)

set(KHPARSE_CHAR_NUM_PARSERS "128" CACHE STRING 
   "Number of parsers to generate for char_benchmarks")

add_custom_command(
   OUTPUT 
      ${CMAKE_BINARY_DIR}/generated/char_benchmarks
   COMMAND ${Python3_EXECUTABLE} 
      ${CMAKE_CURRENT_SOURCE_DIR}/generate_char_parsers.py
      ${KHPARSE_CHAR_NUM_PARSERS}
      ${CMAKE_BINARY_DIR}/generated/char_benchmarks
   DEPENDS
      generate_char_parsers.py
)

add_executable(char_benchmarks 
   char_benchmarks.cpp
   ${CMAKE_BINARY_DIR}/generated/char_benchmarks
)
target_link_libraries(char_benchmarks PRIVATE benchmark::benchmark)
target_include_directories(char_benchmarks PRIVATE ${CMAKE_BINARY_DIR})
