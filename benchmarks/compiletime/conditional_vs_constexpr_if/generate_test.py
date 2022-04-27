import sys

size = int(sys.argv[1])
output_conditional_t = (int(sys.argv[2]) == 0)

# conditional_t<N == 0, char[0], conditional_t<N == 1, char[1]>>

print('#include <type_traits>')
print('template<std::size_t N> struct char_{};')
print('template<std::size_t N>')
print('using type =')

if not output_conditional_t:
   print('decltype([](){')

def output(num):
   if output_conditional_t:
      print(f'std::conditional_t<N == {num}, char_<{num}>, ')
   else:
      print(f'if constexpr (N == {num}) {{ return char_<{num}>{{}}; }}')

for i in range(size):
   output(i)

if output_conditional_t:
   print('int' + '>' * size + ';')
else:
   print('}());')

print('int main() {')
for i in range(size):
   print(f'type<{i}> data{i}{{}};')
print('}')
