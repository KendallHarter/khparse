// A more complete calculator that allows arbitrary nesting of arithmetic operators

#include "khparse.hpp"

#include <cassert>
#include <iostream>
#include <string>

int main()
{
   using namespace khparse;
   const auto number = or_{seq{drop<"0x">, hex64}, seq{drop<"0b">, bin64}, i64};
   const auto ws = drop<"\\s">;
   fwd_parser<int64_t> expression;
   const auto factor = seq{with_skipper, ws, or_{number, seq{with_skipper, ws, drop<"[(]">, expression, drop<"[)]">}}};
   const auto handle_term = [](const auto& parse_result) {
      const auto& [base_value, rest] = parse_result;
      auto return_value = base_value;
      for (const auto& [operand, value] : rest) {
         switch (operand[0]) {
         case '*': return_value *= value; break;
         case '/': return_value /= value; break;
         }
      }
      return return_value;
   };
   const auto term = bind{
      seq{with_skipper, ws, factor, repeat{with_skipper, ws, seq{with_skipper, ws, capture<"[*]|/">, factor}}},
      handle_term};
   const auto handle_expression = [](const auto& parse_result) {
      const auto& [base_value, rest] = parse_result;
      auto return_value = base_value;
      for (const auto& [operand, value] : rest) {
         switch (operand[0]) {
         case '+': return_value += value; break;
         case '-': return_value -= value; break;
         }
      }
      return return_value;
   };
   const auto expression_impl = bind{
      seq{with_skipper, ws, term, repeat{with_skipper, ws, seq{with_skipper, ws, capture<"[+]|-">, term}}},
      handle_expression};
   expression = expression_impl;
   assert(expression.parse("1 + 2 + 3 + 4 + 5").value().value == 15);
   assert(expression.parse("5 * 5 + 5 * 5 - (5 * 5 + 5 * 5)").value().value == 0);
   std::string to_parse;
   while (std::getline(std::cin, to_parse)) {
      const auto result = expression.parse(to_parse);
      if (!result) {
         std::cout << "Parse error at " << result.error().where << '\n';
      }
      else {
         if (result.value().rest != to_parse.data() + to_parse.size()) {
            std::cout << "Remaining input at " << result.value().rest << '\n';
         }
         else {
            std::cout << "Value: " << result.value().value << '\n';
         }
      }
   }
}
