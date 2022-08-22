#include "khparse.hpp"

#include <iostream>

// TODO: This is bugged somehow
//       It's probably in the parser framework itself :c
int main()
{
   using namespace khparse;
   fwd_parser<int64_t> math;
   // clang-format off
   const auto number = or_{
      i64,
      seq{drop<"0x">, hex64},
      seq{drop<"0b">, bin64}
   };
   const auto expression = or_{seq{with_skipper, drop<"\\s">, drop<"\\(">, math, drop<"\\)">}, number};
   const auto factor = or_{
      bind{seq{with_skipper, drop<"\\s">, expression, drop<"\\*">, expression}, [](auto vals) { const auto [a, b] = vals; return a * b; }},
      bind{seq{with_skipper, drop<"\\s">, expression, drop<"/">, expression}, [](auto vals) { const auto [a, b] = vals; return a / b; }},
      expression
   };
   math = or_{
      bind{seq{with_skipper, drop<"\\s">, factor, drop<"\\+">, factor}, [](auto vals) { const auto [a, b] = vals; return a + b; }},
      bind{seq{with_skipper, drop<"\\s">, factor, drop<"-">, factor}, [](auto vals) { const auto [a, b] = vals; return a - b; }},
      factor
   };
   // clang-format on
   assert(math.parse("2 + 3").value().value == 5);
   assert(math.parse("2 + 4 * 5").value().value == 22);
   assert(math.parse("(2 + 4) * 5").value().value == 30);
}
