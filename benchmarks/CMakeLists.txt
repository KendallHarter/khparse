include(ExternalProject)
find_package(benchmark)

if (NOT ${benchmark_FOUND})
   find_package(Git)
   if (NOT GIT_FOUND)
      message(FATAL_ERROR Neither Google Benchmark nor Git could be found; disable benchmarks or install one)
   endif()
   
   # gcmd = git command
   function(gcmd)
      execute_process(
         COMMAND ${GIT_EXECUTABLE} ${ARGV}
         WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/benchmark_source
      )
   endfunction()

   execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/benchmark_source
   )
   gcmd(init)
   gcmd(remote add origin https://github.com/google/benchmark.git)
   gcmd(fetch --depth 1 origin 5704cd4c8cea889d68f9ae29ca5aaee97ef91816)
   gcmd(checkout FETCH_HEAD)
   execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/benchmark_source/build
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/benchmark_source
   )
   execute_process(
      COMMAND ${CMAKE_COMMAND} 
         -DCMAKE_BUILD_TYPE=Release 
         -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/benchmark_source/install
         -DBENCHMARK_ENABLE_TESTING=False
         -G ${CMAKE_GENERATOR} 
         ..
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/benchmark_source/build
   )
   execute_process(
      COMMAND ${CMAKE_COMMAND}
         --build .
         --target install
         --configuration Release
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/benchmark_source/build
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
