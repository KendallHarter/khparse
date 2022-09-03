#include "khparse.hpp"

#include <iostream>

// This is a very limited calculator; for a more complex calculator post-processing
// of the values or repeated parsing is needed
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
   assert(math.parse("10").value().value == 10);
   assert(math.parse("5 * 5 + 5 * 10").value().value == 75);
   assert(math.parse("2 + 4 * 5").value().value == 22);
   assert(math.parse("(2 + 4) * 5").value().value == 30);
}
