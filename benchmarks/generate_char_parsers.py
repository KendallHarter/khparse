import sys
import random
import os

try:
   os.makedirs(os.path.dirname(sys.argv[2]))
except FileExistsError:
   pass

sys.stdout = open(sys.argv[2], 'w+')

# We are excluding backslash since that doesn't work for some reason

def generate_parsers(num):
   print(f'inline constexpr arr_char array_char{num:x}{{"',end='')
   for n in range(256):
      if num & (1 << n):
         if n == ord('\\'):
            pass
            # print('\\\\',end='')
         else:
            print(f'\\x{n:0>2x}',end='')
   print(f'"}};')
   print(f'inline constexpr bitset_char bitset_char{num:x}{{"',end='')
   for n in range(256):
      if num & (1 << n):
         if n == ord('\\'):
            pass
            # print('\\\\',end='')
         else:
            print(f'\\x{n:0>2x}',end='')
   print(f'"}};')

   print(f'inline constexpr auto one_of_char{num:x} = one_of<', end='')
   first = True
   for n in range(256):
      if num & (1 << n):
         if n == ord('\\'):
            pass
         else:
            if first:
               first = False
            else:
               print(',', end='')
            print(f"'\\x{n:0>2x}'", end='')
   print(f'>;')

num_parsers = int(sys.argv[1])
used = {}

while len(used) != num_parsers:
   val = random.randint(0, 2**256 - 1)
   used[val] = val

for num in used:
   generate_parsers(num)

print('''
void test_array()
{
   for (int i = 0; i != 256; ++i) {
''')

for num in used:
   print(f'const auto val{num:x} = array_char{num:x}.parse(i);')
   print(f'benchmark::DoNotOptimize(val{num:x});')

print('''       
   }
}''')

print('''
void test_or()
{
   for (int i = 0; i != 256; ++i) {
''')

for num in used:
   print(f'const auto val{num:x} = (i == one_of_char{num:x});')
   print(f'benchmark::DoNotOptimize(val{num:x});')

print('''       
   }
}''')

print('''
void test_bitset()
{
   for (int i = 0; i != 256; ++i) {
''')

for num in used:
   print(f'const auto val{num:x} = bitset_char{num:x}.parse(i);')
   print(f'benchmark::DoNotOptimize(val{num:x});')

print('''       
   }
}''')

