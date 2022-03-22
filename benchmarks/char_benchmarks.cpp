#include <benchmark/benchmark.h>
#include <bitset>

struct arr_char {
   using result_type = char;

   constexpr arr_char() noexcept
      : allowed{{true}}
   {}

   constexpr arr_char(unsigned char c) noexcept 
      : allowed{{false}}
   { allowed[c] = true; }

   constexpr arr_char(std::string_view init_string) noexcept 
      : allowed{{false}}
   {
      constexpr auto unescape = [=](const char* cur) {
         switch (*cur) {
            case '\'': return '\'';
            case '"':  return '\"';
            case '?':  return '\?';
            case '\\': return '\\';
            case 'a':  return '\a';
            case 'b':  return '\b';
            case 'f':  return '\f';
            case 'n':  return '\n';
            case 'r':  return '\r';
            case 't':  return '\t';
            case 'v':  return '\v';
            case ']':  return ']';
            // TODO: \NNN and \xNN
            default:   throw "Invalid escape character";
         };
      };
      for (const char* cur = init_string.data(); cur != init_string.end();) {
         if (*cur == '\\') {
            allowed[(unsigned char)unescape(cur + 1)] = true;
            cur += 2;
         }
         else if (cur[1] == '-') {
            for (char c = cur[0]; c <= cur[2]; ++c) {
               allowed[(unsigned char)c] = true;
            }
            cur += 3;
         }
         else {
            allowed[(unsigned char)*cur] = true;
            ++cur;
         }
      }
   }

   constexpr bool parse(char c) const noexcept
   {
      return allowed[(unsigned char)c];
   }

private:
   std::array<bool, 256> allowed;
};

struct bitset_char
{
public:
   constexpr bitset_char() noexcept
      : allowed{{0xFFFF'FFFF'FFFF'FFFF}}
   {}

   constexpr bitset_char(unsigned char c) noexcept 
      : allowed{{0}}
   { set(c); }

   constexpr bitset_char(std::string_view init_string) noexcept 
      : allowed{{false}}
   {
      constexpr auto unescape = [=](const char* cur) {
         switch (*cur) {
            case '\'': return '\'';
            case '"':  return '\"';
            case '?':  return '\?';
            case '\\': return '\\';
            case 'a':  return '\a';
            case 'b':  return '\b';
            case 'f':  return '\f';
            case 'n':  return '\n';
            case 'r':  return '\r';
            case 't':  return '\t';
            case 'v':  return '\v';
            case ']':  return ']';
            // TODO: \NNN and \xNN
            default:   throw "Invalid escape character";
         };
      };
      for (const char* cur = init_string.data(); cur != init_string.end();) {
         if (*cur == '\\') {
            set(unescape(cur + 1));
            cur += 2;
         }
         else if (cur[1] == '-') {
            for (char c = cur[0]; c <= cur[2]; ++c) {
               set(c);
            }
            cur += 3;
         }
         else {
            set(*cur);
            ++cur;
         }
      }
   }

   constexpr bool parse(char c) const noexcept
   {
      return check(c);
   }

private:
   constexpr bool check(unsigned char c) const
   {
      switch(c / 64) {
         case 0:  return allowed[0] & (1ull << (c - 64 * 0));
         case 1:  return allowed[1] & (1ull << (c - 64 * 1));
         case 2:  return allowed[2] & (1ull << (c - 64 * 2));
         default: return allowed[3] & (1ull << (c - 64 * 3));
      }
   }

   constexpr bool set(unsigned char c)
   {
      switch(c / 64) {
         case 0:  return allowed[0] | (1ull << (c - 64 * 0));
         case 1:  return allowed[1] | (1ull << (c - 64 * 1));
         case 2:  return allowed[2] | (1ull << (c - 64 * 2));
         default: return allowed[3] | (1ull << (c - 64 * 3));
      }
   }

   std::array<std::uint64_t, 4> allowed;
};

template<auto... values>
requires requires { typename std::common_type<decltype(values)...>::type; }
struct one_of_struct {
   friend constexpr bool operator==(const std::common_type_t<decltype(values)...>& val, one_of_struct) noexcept
   {
      return ((val == values) || ...);
   }
};

template<auto... values>
inline constexpr auto one_of = one_of_struct<values...>{};

#include "generated/char_benchmarks"

namespace {

void array_bench(benchmark::State& state)
{
   for (auto _ : state) {
      test_array();
   }    
}

void or_bench(benchmark::State& state)
{
   for (auto _ : state) {
      test_or();
   }
}

void bitset_bench(benchmark::State& state)
{
   for (auto _ : state) {
      test_bitset();
   }
}

}

BENCHMARK(array_bench);
BENCHMARK(or_bench);
BENCHMARK(bitset_bench);

BENCHMARK_MAIN();