#!/bin/bash

# Must set CXX environment variable beforehand
echo 'std::conditional_t'
python3 ./generate_test.py $1 0 | g++ -x c++ -std=c++2a -
time ./a.out

echo 'constexpr if'
python3 ./generate_test.py $1 1 | g++ -x c++ -std=c++2a -
time ./a.out

rm ./a.out
