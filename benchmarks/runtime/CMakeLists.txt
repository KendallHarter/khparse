include(ExternalProject)
find_package(benchmark)

if (NOT ${benchmark_FOUND})
   find_package(Git)
   if (NOT GIT_FOUND)
      message(FATAL_ERROR Neither Google Benchmark nor Git could be found; disable benchmarks or install one)
   endif()
   
   clone_repo(benchmark https://github.com/google/benchmark.git 5704cd4c8cea889d68f9ae29ca5aaee97ef91816)
   install_repo(benchmark
      -DCMAKE_BUILD_TYPE=Release 
      -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/benchmark_source/install
      -DBENCHMARK_ENABLE_TESTING=False
   )
   find_package(
      benchmark
      HINTS ${CMAKE_BINARY_DIR}/benchmark_source/install
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
