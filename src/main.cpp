#include <ctre.hpp>

#include <algorithm>
#include <cassert>
#include <climits>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <string_view>
#include <type_traits>

// clang-format off
template<typename T, template<typename...> typename BaseTemplate>
concept is_instantiation_of = requires(T x) {
   { []<typename... Args>(const BaseTemplate<Args...>&){}(x) };
};

template<typename T>
concept is_std_array = requires(T x) {
   { []<typename U, std::size_t N>(const std::array<U, N>&){}(x) };
};

template<typename T, typename ElementType>
concept is_std_array_of = is_std_array<T> && requires (T x) {
   { []<std::size_t N>(const std::array<ElementType, N>){}(x) };
};
// clang-format on

struct nil_t {};
inline constexpr nil_t nil;

template<std::size_t N>
struct comp_str {
   constexpr comp_str(const char (&other)[N]) : data{} { std::ranges::copy(other, std::begin(data)); }

   constexpr comp_str(const std::array<char, N>& other) : data{} { std::ranges::copy(other, std::begin(data)); }

   constexpr comp_str(const std::string_view other) : data{} { std::ranges::copy(other, std::begin(data)); }

   char data[N];
   constexpr auto operator<=>(const comp_str&) const = default;
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
   constexpr unexpected(E err) noexcept : error_{err} {}

   constexpr E error() const noexcept { return error_; }

private:
   E error_;
};

template<typename T, typename E>
struct const_expected {
public:
   constexpr const_expected() noexcept : value_{{}}, error_{{}}, has_value_{false} {}

   constexpr const_expected(const T& val) noexcept : value_{val}, error_{}, has_value_{true} {}

   constexpr const_expected(unexpected<E> err) noexcept : value_{}, error_{err.error()}, has_value_{false} {}

   constexpr T value() const
   {
      if (!has_value_) {
         throw "Value retrived when there was no value";
      }
      return value_;
   }

   constexpr operator T() const noexcept { return value(); }

   constexpr E error() const
   {
      if (has_value_) {
         throw "Error retrived when there was no error";
      }
      return error_;
   }

   constexpr explicit operator bool() const noexcept { return has_value_; }

private:
   [[no_unique_address]] T value_;
   [[no_unique_address]] E error_;
   bool has_value_;
};

struct rule_err {
   const char* where;
   const char* why;
};

struct rule_str {
   std::string_view name;
   std::string_view def;
   int num_def_entities = 0;
};

// Would like to specify noexcept but it crashes clang-format, even if
// formatting is disabled
inline constexpr auto find_rule
   = [](std::string_view v) -> const_expected<std::pair<rule_str, std::string_view>, rule_err> {
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
   const auto start_def = std::find_if_not(equal_sign_loc + 1, v.end(), is_ws);
   auto start_next = start_def;
   int num_entities = 0;
   int paren_level = 0;
   while (true) {
      const auto end_next = std::find_if_not(start_next, v.end(), is_ws);
      if (end_next == v.end()) {
         return unexpected{rule_err{
            start_next,
            "Couldn't find end of rule (unexpected end of "
            "string; was a semi-colon forgotten?)"}};
      }
      else if (*end_next == ';') {
         if (paren_level != 0) {
            return unexpected{rule_err{end_next, "Rule has opening parenthesis left unclosed"}};
         }
         return std::pair<rule_str, std::string_view>{
            {{start_rule, end_rule}, {start_def, end_next}, num_entities}, {end_next + 1, v.end()}};
      }
      else if (*end_next == '{') {
         const auto next = std::find(start_next + 1, v.end(), '}');
         if (next == v.end()) {
            return unexpected{rule_err{
               start_next + 1,
               "Unexpected end of string when looking "
               "for terminator character }"}};
         }
         start_next = next + 1;
      }
      else if (*end_next == one_of<'"', '\'', '`'>) {
         start_next = end_next;
         const auto end_char = *end_next;
         do {
            const auto err_loc = start_next;
            if (*start_next == '[') {
               do {
                  start_next = std::find(start_next + 1, v.end(), ']');
                  if (start_next == v.end()) {
                     return unexpected{rule_err{
                        err_loc,
                        "Unexpected end of string when looking for "
                        "terminator character ]"}};
                  }
               } while (*(start_next - 1) == '\'');
            }
            start_next = std::find_if(start_next + 1, v.end(), [&](char c) { return c == end_char || c == '['; });
            if (start_next == v.end()) {
               // Do this dumb thing to get a constexpr string for more context
               const std::array<char, 4> terminators{{'"', '\'', '`'}};
               const int index = std::ranges::find(terminators, end_char) - terminators.cbegin();
               constexpr std::array<const char*, 4> err_strs{
                  {"Unexpected end of string when looking for terminator character "
                   "\"",
                   "Unexpected end of string when looking for terminator character "
                   "'",
                   "Unexpected end of string when looking for terminator character "
                   "`"}};
               return unexpected{rule_err{err_loc, err_strs[index]}};
            }
         } while (*(start_next - 1) == '\\' || *start_next != end_char);
         ++start_next;
      }
      else if (*end_next == '(') {
         paren_level += 1;
         start_next = end_next + 1;
      }
      else if (*end_next == ')') {
         if (paren_level == 0) {
            return unexpected{rule_err{end_next, "Closing parenthesis with no matching opening one"}};
         }
         paren_level -= 1;
         start_next = end_next + 1;
      }
      else if (is_alpha(*end_next)) {
         start_next = std::find_if_not(end_next, v.end(), is_alpha_or_other);
      }
      else if (*end_next == one_of<'|', '+', '*'>) {
         start_next = end_next + 1;
      }
      else {
         return unexpected{
            rule_err{end_next, R"(Unexpected character in rule definition (must be within [[(a-zA-Z='"`|+*?]))"}};
      }
      ++num_entities;
   }
};

template<comp_str line, comp_str why>
struct comp_error {
   void fail();
};

template<comp_str str>
inline constexpr is_std_array_of<rule_str> auto decompose_rules = []() {
   constexpr auto v = std::string_view(str.data);
   // This is ensured to be large enough to hold all rules
   constexpr auto oversize = std::ranges::count(v, ';');

   constexpr auto num_rules_and_rules
      = [&]() -> const_expected<std::pair<int, std::array<rule_str, oversize>>, rule_err> {
      int num_rules = 0;
      std::array<rule_str, oversize> rules{{}};
      const char* start_next = std::find_if_not(v.begin(), v.end(), is_ws);
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
   if constexpr (num_rules_and_rules) {
      constexpr auto num_rules = num_rules_and_rules.value().first;
      constexpr auto rules = num_rules_and_rules.value().second;
      std::array<rule_str, num_rules> to_ret;
      std::copy(rules.data(), rules.data() + num_rules, to_ret.data());
      return to_ret;
   }
   else {
      constexpr auto err_info = num_rules_and_rules.error();
      constexpr auto line_and_err = [&]() {
         // Create line of error
         constexpr auto mri = [](auto val) { return std::make_reverse_iterator(val); };
         constexpr auto null_or_newline = [](char c) { return c == '\n' || c == '\0'; };
         constexpr auto start_line_loc = std::find_if(mri(err_info.where), mri(v.begin()), null_or_newline).base();
         constexpr auto end_line_loc = std::find_if(err_info.where, v.end(), null_or_newline);
         std::array<char, std::distance(start_line_loc, end_line_loc)> line;
         std::copy(start_line_loc, end_line_loc, line.data());
         constexpr auto why_size = [&]() {
            int i = 0;
            for (; err_info.why[i] != '\0'; ++i) {}
            return i + 1;
         }();
         std::array<char, why_size> why;
         for (int i = 0; i < why_size; ++i) {
            why[i] = err_info.why[i];
         }
         return std::make_pair(line, why);
      }();
      comp_error<line_and_err.first, line_and_err.second>{}.fail();
      return std::array<rule_str, 1>{{}};
   }
}();

// template<int NumRules, int TotalSize>
// struct grammar {
//    // this maps from each rule
//    const_unordered_map<std::string_view, std::pair<int, int>> rule_locs;
//    // rule definitions are in here, stored in an array to minimize size
//    std::array<std::string_view, TotalSize> rule_defs;
// };

template<typename T>
struct parse_success {
   const char* rest;
   [[no_unique_address]] T value;
};

struct parse_failure {
   const char* where;
   const char* why;
};

template<comp_str str>
inline constexpr auto str_to_parser = []() {
   constexpr auto parser_str = []() {
      constexpr auto is_bounder = [](char c) { return c == one_of<'"', '`', '\''>; };
      constexpr auto v = std::string_view(str.data);
      constexpr auto start = std::find_if(v.begin(), v.end(), is_bounder);
      constexpr auto end
         = std::find_if(std::make_reverse_iterator(v.begin()), std::make_reverse_iterator(v.end()), is_bounder).base()
         - 1;
      static_assert(*start == *end);
      static_assert(start != end);
      std::array<char, std::distance(start, end)> raw_str{0};
      std::copy(start + 1, end, raw_str.begin());
      return comp_str{raw_str};
   }();
   return ctre::starts_with<parser_str.data>;
}();

enum class parser_type {
   char_,
   char_capture,
   start_group,
   end_group,
   repeat,
   rule
};

// No union so it can be used for template values
struct const_entity_def {
   parser_type type;
   const char* start_def{};
   const char* end_def{};
   std::pair<std::int64_t, std::int64_t> min_max{0, 0};

   auto operator<=>(const const_entity_def& other) const = default;
};

template<comp_str str>
constexpr inline const_entity_def str_to_entity_def = []() -> const_entity_def {
   constexpr auto v = std::string_view{str.data};
   switch (v.front()) {
   case '(': return {.type = parser_type::start_group};
   case ')': return {.type = parser_type::end_group};
   case '?': return {.type = parser_type::repeat, .min_max = {0, 1}};
   case '+': return {.type = parser_type::repeat, .min_max = {1, 0}};
   case '`': return {.type = parser_type::char_capture, .start_def = str.data, .end_def = std::end(str.data)};
   case '\'': [[fallthrough]];
   case '"': return {.type = parser_type::char_, .start_def = str.data, .end_def = std::end(str.data)};

   // TODO: Parse this
   case '{': return {.type = parser_type::repeat, .min_max = {0, 0}};
   }
   return {.type = parser_type::rule, .start_def = str.data, .end_def = std::end(str.data)};
}();

int main()
{
   constexpr auto rule = decompose_rules<R"(
        hi='1\'2' "[123]" "[\][(a-zA-Z='"`]"{5,9};
        borzoi = "borz(oi)?";
        test= hi borzoi;
    )">;
   static_assert(rule[0].name == "hi");
   static_assert(rule[0].num_def_entities == 4);
   static_assert(rule[1].name == "borzoi");
   static_assert(rule[1].num_def_entities == 1);
   static_assert(rule[2].name == "test");
   static_assert(rule[2].num_def_entities == 2);

   constexpr auto tester = str_to_entity_def<comp_str<rule[1].def.size() + 1>{rule[1].def}>;
   // constexpr auto matcher = str_to_parser<comp_str<rule[1].def.size() + 1>{rule[1].def}>;

   // std::string input;
   // while (std::cin >> input) {
   //    std::cout << matcher(input) << '\n';
   // }
}
