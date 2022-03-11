#include <algorithm>
#include <cassert>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <string_view>
#include <type_traits>

template<std::size_t N>
struct comp_str {
   consteval comp_str(const char (&other)[N]) : data{}
   {
      for (std::size_t i = 0; i < N; ++i) {
         data[i] = other[i];
      }
   }

   char data[N];
   constexpr auto operator<=>(const comp_str&) const = default;
};

template<comp_str str>
consteval auto test()
{
   const auto& data = str.data;
   std::array<std::pair<std::string_view, std::string_view>, std::ranges::count(data, '=') + 1> rules;
   std::size_t loc = 0;

   constexpr auto ischar
      = [](int c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '-'; };
   constexpr auto not_ws = [](int c) { return c != ' ' && c != '\t' && c != '\n'; };

   auto start_loc = std::begin(data);
   while (true) {
      const auto equal_sign_loc = std::find(start_loc, std::end(data), '=');
      if (equal_sign_loc == std::end(data)) {
         break;
      }
      auto end_rule_loc = equal_sign_loc;
      while (true) {
         if (*end_rule_loc == ';') {
            break;
         }
         else if (*end_rule_loc == '\'') {
            do {
               ++end_rule_loc;
            } while (*end_rule_loc != '\'');
         }
         else if (*end_rule_loc == '"') {
            do {
               ++end_rule_loc;
            } while (*end_rule_loc != '"');
         }
         ++end_rule_loc;
      }

      const auto mri = [](auto val) { return std::make_reverse_iterator(val); };

      const auto print_start = std::find_if(start_loc, equal_sign_loc, ischar);
      const auto print_end = std::find_if(mri(start_loc), mri(equal_sign_loc), ischar) + 1;

      const auto rule_start = std::find_if(equal_sign_loc + 1, end_rule_loc, not_ws);
      const auto rule_end = std::find_if(mri(equal_sign_loc + 1), mri(end_rule_loc), not_ws) + 1;
      rules[loc] = {rule_start, rule_end.base()};
      ++loc;
      start_loc = end_rule_loc + 1;
   }

   return rules;
}

struct rule_str {
   std::string_view name;
   std::string_view def;
   int num_def_entities;
};

template<typename T, typename...>
struct head {
   using type = T;
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

constexpr auto is_ws = [](const char c) { return c == one_of<' ', '\n', '\r', '\t', '\v', '\f'>; };

template<typename E>
struct unexpected {
public:
   constexpr unexpected(E err) noexcept
      : error_{err}
   {}

   constexpr E error() const noexcept { return error_; }

private:
   E error_;
};

template<typename T, typename E>
struct const_expected {
public:
   constexpr const_expected() noexcept
      : value_{{}}
      , error_{{}}
      , has_value_{false}
   {}

   constexpr const_expected(const T& val) noexcept
      : value_{val}
      , error_{}
      , has_value_{true}
   {}

   constexpr const_expected(unexpected<E> err) noexcept
      : value_{}
      , error_{err.error()}
      , has_value_{false}
   {}

   constexpr T value() const
   {
      if (!has_value_) {
         throw "Value retrived when there was no value";
      }
      return value_;
   }

   constexpr operator T() const noexcept
   {
      return value();
   }

   constexpr E error() const noexcept
   {
      return error_;
   }

   constexpr explicit operator bool() const noexcept
   {
      return has_value_;
   }

private:
   [[no_unique_address]] T value_;
   [[no_unique_address]] E error_;
   bool has_value_;
};

struct rule_err {
   const char* where;
   const char* why;
};

// TODO: Try and find a way to make errors propagate (eventually)
constexpr auto find_rule(std::string_view v) -> const_expected<std::pair<rule_str, std::string_view>, rule_err>
{
   constexpr auto is_alpha = [](const char c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); };
   constexpr auto is_other = [](const char c) { return (c >= '0' && c <= '9') || c == '_'; };
   constexpr auto is_alpha_or_other = [=](const char c) { return is_alpha(c) || is_other(c); };
   const auto start_rule = std::ranges::find_if_not(v, is_ws);
   if (start_rule == v.end()) {
      return unexpected{rule_err{v.begin(), "Couldn't find start of rule name (unexpected end of string)"}};
   }
   else if (!is_alpha(*start_rule)) {
      return unexpected{rule_err{start_rule, "Invalid first character for rule name (must be within [a-zA-Z])"}};
   }
   const auto end_rule = std::find_if_not(start_rule, v.end(), is_alpha_or_other);
   if (end_rule == v.end()) {
      return unexpected{rule_err{start_rule, "Couldn't find end of rule name (unexpected end of string)"}};
   }
   const auto equal_sign_loc = std::find_if_not(end_rule, v.end(), is_ws);
   if (equal_sign_loc == v.end()) {
      return unexpected{rule_err{end_rule, "Couldn't find equal sign after rule name (unexpected end of string)"}};
   }
   else if (*equal_sign_loc != '=') {
      return unexpected{rule_err{equal_sign_loc, "Invalid character after rule name (must be equals sign)"}};
   }
   const auto start_def = equal_sign_loc + 1;
   auto start_next = start_def;
   int num_entities = 0;
   int paren_level = 0;
   while (true) {
      const auto end_next = std::find_if_not(start_next, v.end(), is_ws);
      if (end_next == v.end()) {
         return unexpected{rule_err{start_next, "Couldn't find end of rule (unexpected end of string; was a semi-colon forgotten?)"}};
      }
      else if (*end_next == ';') {
         return std::pair<rule_str, std::string_view>{{{start_rule, end_rule}, {start_def, end_next}, num_entities}, {end_next + 1, v.end()}};
      }
      else if (*end_next == one_of<'"', '\'', '`', '[', '{'>) {
         start_next = end_next;
         const auto [end_char, escape_allowed] = [&]() -> std::pair<char, bool> {
            switch (*end_next) {
            case '[':
               return {']', true};
            case '{':
               return {'}', false};
            default:
               return {*end_next, true};
            }
         }();
         do {
            const auto err_loc = start_next;
            start_next = std::find(start_next + 1, v.end(), end_char);
            if (start_next == v.end()) {
               return unexpected{rule_err{err_loc, "Unexpected end of string when looking for terminator character"}};
            }
         } while (escape_allowed && *(start_next - 1) == '\\');
         ++start_next;
      }
      else if (*end_next == '(') {
         paren_level += 1;
      }
      else if (*end_next == ')') {
         if (paren_level == 0) {
            return unexpected{rule_err{end_next, "Closing parenthesis with no matching opening one"}};
         }
         paren_level -= 1;
      }
      else if (is_alpha(*end_next)) {
         start_next = std::find_if_not(end_next, v.end(), is_alpha_or_other);
      }
      else if (*end_next == one_of<'-', '|', '+', '*'>) {
         start_next = end_next + 1;
      }
      else {
         return unexpected{rule_err{end_next, R"(Unexpected character in rule definition (must be within [[(a-zA-Z='"`|-+*]))"}};
      }
      ++num_entities;
   }
}

template<comp_str str>
constexpr auto decompose_rules_impl()
{
   constexpr auto v = std::string_view(str.data);
   // This is ensured to be large enough to hold all rules
   constexpr auto oversize = std::ranges::count(v, ';');

   constexpr auto num_rules_and_rules = [&]() constexpr -> const_expected<std::pair<int, std::array<rule_str, oversize>>, rule_err> {
      int num_rules = 0;
      std::array<rule_str, oversize> rules{{}};
      const char* start_next = v.begin();
      while (start_next != v.end()) {
         ++num_rules;
         const auto rule = find_rule({start_next, v.end()});
         if (rule) {
            rules[num_rules - 1] = rule.value().first;
            start_next = std::find_if_not(rule.value().second.data(), v.end(), is_ws);
         }
         else {
            return unexpected{rule.error()};
         }
      }
      return std::pair<int, std::array<rule_str, oversize>>{num_rules, rules};
   }();
   if (num_rules_and_rules) {
      constexpr auto num_rules = num_rules_and_rules.value().first;
      constexpr auto rules = num_rules_and_rules.value().second;
      std::array<rule_str, num_rules> to_ret;
      std::copy(rules.data(), rules.data() + num_rules, to_ret.data());
      return to_ret;
   }
   else {
      ;
   }
}

template<comp_str str>
constexpr auto decompose_rules = decompose_rules_impl<str>();

int main()
{
   constexpr auto rule = decompose_rules<R"(
        hi='1\'2' [123] [\][(a-zA-Z='"`]{5-9};
        borzoi = "borz"|"borzoi";
        test= hi borzoi;
    )">;
   static_assert(rule[0].name == "hi");
   static_assert(rule[0].num_def_entities == 4);
   static_assert(rule[1].name == "borzoi");
   static_assert(rule[1].num_def_entities == 3);
   static_assert(rule[2].name == "test");
   static_assert(rule[2].num_def_entities == 2);
   constexpr auto rules = decompose_rules<R"ebnf(
      letter = [a-zA-Z] ;
      digit = [0-9] ;
      symbol = [[\]{}()<>'"=|.,;] ;
      character = letter | digit | symbol | "_" ;
      identifier = letter (letter | digit | "_")+ ;
      terminal = "'" character+ "'" | '"' character+ '"' ;
      lhs = identifier ;
      rhs = identifier
          | terminal
          | "[" rhs "]"
          | "{" rhs "}"
          | "(" rhs ")"
          | rhs "|" rhs
          | rhs "," rhs ;
      rule = lhs "=" rhs ";" ;
      grammer = rule+ ;
   )ebnf">;
   for (const auto& r : rules) {
      std::cout << r.name << ": " << r.num_def_entities << '\n';
   }
   static_assert(rules[7].name == "rhs");
   std::cout << rules[7].num_def_entities << ' ' << rules[7].def.size() << '\n';
   // constexpr auto rules = test<R"ebnf(
   // letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
   //         | "H" | "I" | "J" | "K" | "L" | "M" | "N"
   //         | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
   //         | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
   //         | "c" | "d" | "e" | "f" | "g" | "h" | "i"
   //         | "j" | "k" | "l" | "m" | "n" | "o" | "p"
   //         | "q" | "r" | "s" | "t" | "u" | "v" | "w"
   //         | "x" | "y" | "z" ;
   // digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
   // symbol = "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">"
   //     | "'" | '"' | "=" | "|" | "." | "," | ";" ;
   // character = letter | digit | symbol | "_" ;

   // identifier = letter , { letter | digit | "_" } ;
   // terminal = "'" , character , { character } , "'"
   //         | '"' , character , { character } , '"' ;

   // lhs = identifier ;
   // rhs = identifier
   //     | terminal
   //     | "[" , rhs , "]"
   //     | "{" , rhs , "}"
   //     | "(" , rhs , ")"
   //     | rhs , "|" , rhs
   //     | rhs , "," , rhs ;

   // rule = lhs , "=" , rhs , ";" ;
   // grammar = { rule } ;
   // )ebnf">();
   // for (const auto& [key, val] : rules) {
   //     std::cout << key << " = " << val << '\n';
   // }
}
