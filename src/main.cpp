#include <ctre.hpp>

#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <climits>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <numeric>
#include <optional>
#include <string_view>
#include <type_traits>
#include <utility>

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
struct const_str {
   constexpr const_str(const char (&other)[N]) : data{} { std::ranges::copy(other, std::begin(data)); }
   constexpr const_str(const std::array<char, N>& other) : data{} { std::ranges::copy(other, std::begin(data)); }
   constexpr const_str(const std::string_view other) : data{} { std::ranges::copy(other, std::begin(data)); }

   char data[N];
   constexpr auto operator<=>(const const_str&) const = default;
};

template<auto... Values>
   requires requires { typename std::common_type<decltype(Values)...>::type; }
struct one_of_struct {
   friend constexpr bool operator==(const std::common_type_t<decltype(Values)...>& val, one_of_struct) noexcept
   {
      return ((val == Values) || ...);
   }
};

template<auto... Values>
inline constexpr auto one_of = one_of_struct<Values...>{};

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
   using value_type = T;
   using error_type = E;

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

template<std::size_t N>
using find_rule_ret_type = std::conditional_t<
   N == 0,
   const_expected<std::pair<rule_str, std::string_view>, rule_err>,
   const_expected<std::array<std::string_view, N>, rule_err>>;

// Would like to specify noexcept but it crashes clang-format, even if
// formatting is disabled
template<std::size_t NumEntities>
inline constexpr auto find_rule = [](std::string_view v) -> find_rule_ret_type<NumEntities> {
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
   std::conditional_t<(NumEntities > 0), std::array<std::string_view, NumEntities>, nil_t> entity_defs;
   while (true) {
      const auto original_start = start_next;
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
         if constexpr (NumEntities > 0) {
            return entity_defs;
         }
         else {
            return std::pair<rule_str, std::string_view>{
               {{start_rule, end_rule}, {start_def, end_next}, {start_rule, end_next + 1}, num_entities},
               {end_next + 1, v.end()}};
         }
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
      if constexpr (NumEntities > 0) {
         entity_defs[num_entities] = std::string_view{original_start, start_next};
      }
      ++num_entities;
   }
};

template<const_str Line, const_str Why>
struct comp_error {
   void fail();
};

template<typename T>
struct parse_success {
   [[no_unique_address]] T value;
   const char* rest;
};

template<const_str Str>
inline constexpr auto str_to_parser = []() {
   constexpr auto parser_str = []() {
      constexpr auto is_bounder = [](char c) { return c == one_of<'"', '`', '\''>; };
      constexpr auto v = std::string_view(Str.data);
      constexpr auto start = std::find_if(v.begin(), v.end(), is_bounder);
      constexpr auto end = std::find_if(v.rbegin(), v.rend(), is_bounder).base() - 1;
      static_assert(*start == *end);
      static_assert(start != end);
      std::array<char, std::distance(start, end)> raw_str{0};
      std::copy(start + 1, end, raw_str.begin());
      return const_str{raw_str};
   }();
   return ctre::starts_with<parser_str.data>;
}();

struct parse_error {
   const char* where;
   const char* why;
};

template<typename T>
using parse_result = const_expected<parse_success<T>, parse_error>;

// TODO: Find a way to scale this better
//       There are many, many variants when it comes to numeric parsers
//       Probably still use this + reserve bits for int parsing
enum class parser_type {
   char_,
   char_capture,
   start_group,
   end_group,
   repeat,
   rule,
   alternate,
   int_,
   float_,
   double_,
   long_double_
};

constexpr inline std::string_view predef_parsers[]{"int"};

template<parser_type Type>
inline constexpr auto parse_with_entity = [](std::string_view) {};

template<>
inline constexpr auto parse_with_entity<parser_type::int_> = [](std::string_view v) -> parse_result<std::int64_t> {
   std::int64_t to_ret;
   const auto [rest, err] = std::from_chars(v.begin(), v.end(), to_ret);
   if (err == std::errc::invalid_argument) {
      return unexpected{parse_error{v.begin(), "Invalid character in int_"}};
   }
   else if (err == std::errc::result_out_of_range) {
      return unexpected{parse_error{v.begin(), "Value too large"}};
   }
   else {
      return parse_success<std::int64_t>{to_ret, rest};
   }
};

// No union as it really complicates things
struct temp_entity_def {
   parser_type type;
   std::string_view def{};
   std::pair<int, int> min_max{0, 0};
};

constexpr inline auto str_to_entity_def = [](std::string_view v) -> temp_entity_def {
   switch (v.front()) {
   case '(': return {.type = parser_type::start_group};
   case ')': return {.type = parser_type::end_group};
   case '|': return {.type = parser_type::alternate};
   case '?': return {.type = parser_type::repeat, .min_max = {0, 1}};
   case '+': return {.type = parser_type::repeat, .min_max = {1, 0}};
   case '`': return {.type = parser_type::char_capture, .def = v};
   case '\'': [[fallthrough]];
   case '"': return {.type = parser_type::char_, .def = v};

   // TODO: Parse this
   case '{': return {.type = parser_type::repeat, .min_max = {0, 0}};
   }
   const auto iter = std::ranges::find(predef_parsers, v);
   if (iter == std::end(predef_parsers)) {
      return {.type = parser_type::rule, .def = v};
   }
   else {
      const auto offset = std::distance(std::begin(predef_parsers), iter);
      return {.type = static_cast<parser_type>(static_cast<int>(parser_type::int_) + offset)};
   }
};

inline constexpr auto remove_nil = []<typename... T>(const std::tuple<T...>& values) {
   constexpr bool is_not_nil[]{!std::same_as<nil_t, T>...};
   constexpr auto num_values = std::accumulate(std::begin(is_not_nil), std::end(is_not_nil), 0);
   constexpr std::array indexes = [=]() {
      std::array<int, num_values> indexes;
      int write_loc = 0;
      for (int i = 0; i < std::ssize(is_not_nil); ++i) {
         if (is_not_nil[i]) {
            indexes[write_loc] = i;
            ++write_loc;
         }
      }
      return indexes;
   }();
   return [=]<std::size_t... N>(std::index_sequence<N...>)
   {
      return std::make_tuple(std::get<std::get<N>(indexes)>(values)...);
   }
   (std::make_index_sequence<num_values>{});
};

template<typename Tuple>
inline constexpr auto parser_write_indexes = []<typename... T>(const std::tuple<T...>&) {
   constexpr bool is_not_nil[]{!std::same_as<nil_t, T>...};
   constexpr std::array write_indexes = [=]() {
      std::array<int, std::size(is_not_nil)> indexes;
      int write_loc = 0;
      for (int i = 0; i < std::ssize(is_not_nil); ++i) {
         if (is_not_nil[i]) {
            indexes[i] = write_loc;
            ++write_loc;
         }
         else {
            indexes[i] = -1;
         }
      }
      return indexes;
   }();
   return write_indexes;
}(Tuple{});

template<const_str Str>
struct grammar {
private:
   // Would love to propagate this more, but due to pointers being unfriendly to template parameters,
   // it's significantly easier and cleaner to make this function do more work
   // TODO: Determine if that's still true with it being incapsulated in a class
   inline static constexpr auto decompose_rules = []() {
      constexpr auto v = std::string_view(Str.data);
      // This is ensured to be large enough to hold all rules
      constexpr auto oversize = std::ranges::count(v, ';');

      constexpr auto num_rules_and_rules
         = [&]() -> const_expected<std::pair<int, std::array<rule_str, oversize>>, rule_err> {
         int num_rules = 0;
         std::array<rule_str, oversize> rules{{}};
         const char* start_next = std::find_if_not(v.begin(), v.end(), is_ws);
         while (start_next != v.end()) {
            ++num_rules;
            const auto rule = find_rule<0>({start_next, v.end()});
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
      if constexpr (!num_rules_and_rules) {
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
      constexpr auto num_rules = num_rules_and_rules.value().first;
      constexpr auto rules_raw = num_rules_and_rules.value().second;
      constexpr auto rules = [&]() {
         std::array<rule_str, num_rules> rules;
         std::copy(rules_raw.data(), rules_raw.data() + num_rules, rules.data());
         return rules;
      }();
      constexpr auto total_size = std::accumulate(
         rules.begin(), rules.end(), 0, [](int so_far, const auto& next) { return so_far + next.num_def_entities; });
      return [=]() {
         std::array<std::pair<int, int>, rules.size()> rule_defs;
         std::array<temp_entity_def, total_size> entities_array;
         int def_loc = 0;
         auto array_loc = entities_array.begin();
         constexpr auto create_defs_from_rule = [=]<std::size_t I>() {
            constexpr auto r = std::get<I>(rules);
            std::array<temp_entity_def, r.num_def_entities> defs;
            constexpr auto str_defs = find_rule<r.num_def_entities>(r.raw_rule).value();
            std::transform(
               str_defs.begin(), str_defs.end(), defs.begin(), [](const auto& val) { return str_to_entity_def(val); });
            return defs;
         };
         auto process_defs = [&]<std::size_t I>() {
            constexpr auto defs = create_defs_from_rule.template operator()<I>();
            std::ranges::copy(defs, array_loc);
            rule_defs[def_loc] = std::make_pair(
               std::distance(entities_array.begin(), array_loc),
               std::distance(entities_array.begin(), array_loc + defs.size()));
            ++def_loc;
            array_loc += defs.size();
         };
         [&]<std::size_t... I>(std::index_sequence<I...>) { (process_defs.template operator()<I>(), ...); }
         (std::make_index_sequence<rules.size()>{});
         constexpr auto rule_names = [=]() {
            std::array<std::string_view, rules.size()> rule_names;
            for (int i = 0; i < std::ssize(rules); ++i) {
               rule_names[i] = rules[i].name;
            }
            return rule_names;
         }();
         return std::make_tuple(rule_defs, entities_array, rule_names);
      }();
   }();

   inline static constexpr auto raw_grammar_info = decompose_rules();
   inline static constexpr auto rule_defs = std::get<0>(raw_grammar_info);
   inline static constexpr auto entities_array = std::get<1>(raw_grammar_info);
   inline static constexpr auto rule_names = std::get<2>(raw_grammar_info);

public:
};
