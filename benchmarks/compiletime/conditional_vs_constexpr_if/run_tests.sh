#!/bin/bash

temp=$(mktemp)

# Must set CXX environment variable beforehand
echo 'std::conditional_t'
python3 ./generate_test.py $1 0 > ${temp}
time ${CXX} -x c++ -O3 -std=c++2a ${temp}

echo
echo 'constexpr if'
python3 ./generate_test.py $1 1 > ${temp}
time ${CXX} -x c++ -O3 -std=c++2a ${temp}

rm ${temp}
rm ./a.out
