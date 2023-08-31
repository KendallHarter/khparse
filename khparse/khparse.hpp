#ifndef KHPARSE_HPP
#define KHPARSE_HPP

// For some reason the auto-detection is failing all of a sudden?
// So force it to use the non-std version
#define nsel_CONFIG_SELECT_EXPECTED nsel_EXPECTED_NONSTD
#include <nonstd/expected.hpp>

#include <ctre.hpp>

#include <algorithm>
#include <array>
#include <charconv>
#include <concepts>
#include <functional>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

// // clang-format off
// template<typename T, template<typename...> typename BaseTemplate>
// concept is_instantiation_of = requires(T x) {
//    { []<typename... Args>(const BaseTemplate<Args...>&){}(x) };
// };

// template<typename T>
// concept is_std_array = requires(T x) {
//    { []<typename U, std::size_t N>(const std::array<U, N>&){}(x) };
// };

// template<typename T, typename ElementType>
// concept is_std_array_of = is_std_array<T> && requires (T x) {
//    { []<std::size_t N>(const std::array<ElementType, N>){}(x) };
// };
// // clang-format on

namespace khparse {

struct nil_t {};

inline constexpr nil_t nil{};

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

template<typename T>
struct parse_success {
   [[no_unique_address]] T value;
   const char* rest;
};

struct parse_error {
   const char* where;
};

template<typename T>
using parse_result = nonstd::expected<parse_success<T>, parse_error>;

// This is a mess but basically since parsing resturns a std::expected<parse_success, parse_error>
// we first need to extract out the value there.  Then, the base value type needs to be extracted.
template<typename T>
using produced_type_raw = decltype(decltype(std::declval<T>().parse(std::string_view{}))::value_type::value);

// clang-format off
template<typename T>
concept parser = requires(const T& parser, std::string_view v) {
   typename decltype(parser.parse(v))::value_type;
   { parser.parse(v) } -> std::same_as<parse_result<produced_type_raw<T>>>;
};

template<typename T>
concept is_tuple = requires(T tuple) {
   { []<typename... U>(std::tuple<U...>*){}(&tuple) };
};

template<typename T>
concept is_variant = requires(T var) {
   { []<typename... U>(std::variant<U...>*){}(&var) };
};
// clang-format on

template<parser T>
using produced_type = produced_type_raw<T>;

template<typename IntType>
struct number {
   int base = 10;

   parse_result<IntType> parse(std::string_view v) const noexcept
   {
      IntType to_ret{};
      const auto [ptr, ec] = std::from_chars(v.begin(), v.end(), to_ret, base);
      if (ec == std::errc{}) {
         return parse_success<IntType>{to_ret, ptr};
      }
      else if (ec == std::errc::invalid_argument) {
         return nonstd::make_unexpected(parse_error{v.begin()});
      }
      else /* if (ec == std::errc::result_out_of_range) */ {
         return nonstd::make_unexpected(parse_error{v.begin()});
      }
      // assert(false);
   }
};

template<parser... Parsers>
constexpr std::array inline seq_write_locs = []() {
   std::array<int, sizeof...(Parsers)> write_locations;
   int write_loc = 0;
   constexpr std::array is_nil{std::same_as<produced_type<Parsers>, nil_t>...};
   for (int i = 0; i < std::ssize(is_nil); ++i) {
      if (is_nil[i]) {
         write_locations[i] = -1;
      }
      else {
         write_locations[i] = write_loc;
         ++write_loc;
      }
   }
   return write_locations;
}();

template<std::array write_locs, parser... Parsers>
constexpr auto inline seq_ret_type = []() {
   using raw_type = std::tuple<produced_type<Parsers>...>;
   constexpr int num_values = []<std::size_t... I>(std::index_sequence<I...>) {
      return (!(std::same_as<nil_t, std::tuple_element_t<I, raw_type>>)+...);
   }(std::make_index_sequence<sizeof...(Parsers)>{});
   if constexpr (num_values == 0) {
      return nil;
   }
   else if constexpr (num_values == 1) {
      constexpr auto value_loc = std::ranges::find(write_locs, 0);
      return std::tuple_element_t<std::distance(std::begin(write_locs), value_loc), raw_type>{};
   }
   else {
      constexpr auto value_locs = []<std::size_t... I>(std::index_sequence<I...>) {
         std::array<int, num_values> value_locs;
         int write_to_values_loc = 0;
         for (int i = 0; i < std::ssize(write_locs); ++i) {
            if (write_locs[i] != -1) {
               value_locs[write_to_values_loc] = i;
               ++write_to_values_loc;
            }
         }
         return value_locs;
      }(std::index_sequence_for<Parsers...>{});

      return [=]<std::size_t... I>(std::index_sequence<I...>) {
         return std::tuple<std::tuple_element_t<value_locs[I], raw_type>...>{};
      }(std::make_index_sequence<num_values>{});
   }
};

struct always_fail {
   static auto parse(std::string_view v) noexcept -> parse_result<nil_t>
   {
      return nonstd::make_unexpected(parse_error{v.begin()});
   }
};

struct with_skipper_t {};

inline constexpr auto with_skipper = with_skipper_t{};

template<parser Skipper, parser... Parsers>
   requires(sizeof...(Parsers) > 0)
struct seq {
private:
   [[no_unique_address]] std::tuple<Parsers...> parsers_;
   [[no_unique_address]] Skipper skipper_;

   inline static constexpr auto write_locations = seq_write_locs<Parsers...>;
   using prod_type = decltype(seq_ret_type<write_locations, Parsers...>());

public:
   template<typename... ArgParsers>
   seq(ArgParsers&&... args) noexcept : parsers_{static_cast<ArgParsers&&>(args)...}, skipper_{}
   {}

   template<typename SkipperArg, typename... ArgParsers>
   seq(with_skipper_t, SkipperArg&& skipper, ArgParsers&&... args) noexcept
      : parsers_{static_cast<ArgParsers&&>(args)...}, skipper_{static_cast<SkipperArg&&>(skipper)}
   {}

   auto parse(std::string_view v) const noexcept -> parse_result<prod_type>
   {
      prod_type to_ret{};
      const char* parse_loc = v.begin();
      parse_error err{};
      const auto handle_parser = [&]<std::size_t I>() {
         if (!parse_loc) {
            return;
         }
         while (true) {
            const auto skip_result = skipper_.parse({parse_loc, v.end()});
            if (!skip_result) {
               break;
            }
            parse_loc = skip_result->rest;
         }
         const auto result = std::get<I>(parsers_).parse({parse_loc, v.end()});
         if (result) {
            if constexpr (write_locations[I] != -1) {
               if constexpr (is_tuple<prod_type>) {
                  std::get<write_locations[I]>(to_ret) = result->value;
               }
               else {
                  to_ret = result->value;
               }
            }
            parse_loc = result->rest;
         }
         else {
            parse_loc = nullptr;
            err = result.error();
         }
      };

      [&]<std::size_t... I>(std::index_sequence<I...>) {
         (handle_parser.template operator()<I>(), ...);
      }(std::index_sequence_for<Parsers...>{});

      if (parse_loc) {
         return parse_success<prod_type>{to_ret, parse_loc};
      }
      else {
         return nonstd::make_unexpected(err);
      }
   }
};

template<parser... Parsers>
seq(Parsers...) -> seq<always_fail, Parsers...>;

template<parser Skipper, parser... Parsers>
seq(with_skipper_t, Skipper, Parsers...) -> seq<Skipper, Parsers...>;

inline constexpr auto u8 = number<std::uint8_t>{};
inline constexpr auto u16 = number<std::uint16_t>{};
inline constexpr auto u32 = number<std::uint32_t>{};
inline constexpr auto u64 = number<std::uint64_t>{};

inline constexpr auto i8 = number<std::int8_t>{};
inline constexpr auto i16 = number<std::int16_t>{};
inline constexpr auto i32 = number<std::int32_t>{};
inline constexpr auto i64 = number<std::int64_t>{};

inline constexpr auto uhex8 = number<std::uint8_t>{16};
inline constexpr auto uhex16 = number<std::uint16_t>{16};
inline constexpr auto uhex32 = number<std::uint32_t>{16};
inline constexpr auto uhex64 = number<std::uint64_t>{16};

inline constexpr auto hex8 = number<std::int8_t>{16};
inline constexpr auto hex16 = number<std::int16_t>{16};
inline constexpr auto hex32 = number<std::int32_t>{16};
inline constexpr auto hex64 = number<std::int64_t>{16};

inline constexpr auto ubin8 = number<std::uint8_t>{2};
inline constexpr auto ubin16 = number<std::uint16_t>{2};
inline constexpr auto ubin32 = number<std::uint32_t>{2};
inline constexpr auto ubin64 = number<std::uint64_t>{2};

inline constexpr auto bin8 = number<std::int8_t>{2};
inline constexpr auto bin16 = number<std::int16_t>{2};
inline constexpr auto bin32 = number<std::int32_t>{2};
inline constexpr auto bin64 = number<std::int64_t>{2};

template<const_str Regex, bool DropResult>
struct regex_impl {
private:
   using value_type = std::conditional_t<DropResult, nil_t, std::string_view>;
   static inline constexpr auto matcher = ctre::match<Regex.data>;

public:
   static auto parse(std::string_view v) noexcept -> parse_result<value_type>
   {
      const auto result = matcher.starts_with(v);
      if (!result) {
         return nonstd::make_unexpected(parse_error{v.begin()});
      }
      if constexpr (DropResult) {
         return parse_success<nil_t>{nil, v.begin() + result.to_view().size()};
      }
      else {
         return parse_success<std::string_view>{result.to_view(), v.begin() + result.to_view().size()};
      }
   }
};

template<const_str Regex>
inline constexpr auto drop = regex_impl<Regex, true>{};

template<const_str Regex>
inline constexpr auto capture = regex_impl<Regex, false>{};

template<typename Skipper, parser Parser>
struct repeat {
private:
   int min_;
   int max_;
   [[no_unique_address]] Parser parser_;
   [[no_unique_address]] Skipper skipper_;

   using base_type = produced_type<Parser>;
   static inline constexpr bool nil_type = std::same_as<base_type, nil_t>;
   using value_type = std::conditional_t<nil_type, nil_t, std::vector<produced_type<Parser>>>;

public:
   template<std::constructible_from<Parser> ArgParser>
   repeat(ArgParser&& p) noexcept : min_{0}, max_{0}, parser_{static_cast<ArgParser&&>(p)}, skipper_{}
   {}

   template<std::constructible_from<Parser> ArgParser>
   repeat(ArgParser&& p, int min, int max) noexcept
      : min_{min}, max_{max}, parser_{static_cast<ArgParser&&>(p)}, skipper_{}
   {}

   template<std::constructible_from<Skipper> ArgSkipper, std::constructible_from<Parser> ArgParser>
   repeat(with_skipper_t, ArgSkipper&& s, ArgParser&& p) noexcept
      : min_{0}, max_{0}, parser_{static_cast<ArgParser&&>(p)}, skipper_{static_cast<ArgSkipper&&>(s)}
   {}

   template<std::constructible_from<Skipper> ArgSkipper, std::constructible_from<Parser> ArgParser>
   repeat(with_skipper_t, ArgSkipper&& s, ArgParser&& p, int min, int max) noexcept
      : min_{min}, max_{max}, parser_{static_cast<ArgParser&&>(p)}, skipper_{static_cast<ArgSkipper&&>(s)}
   {}

   auto parse(std::string_view v) const noexcept -> parse_result<value_type>
   {
      // There's probably a better way to handle nil types cleanly, but this works...
      const char* parse_loc = v.begin();
      std::conditional_t<nil_type, int, value_type> to_ret{};
      for (int i = 0; i < max_ || max_ == 0; ++i) {
         while (true) {
            const auto skip_result = skipper_.parse({parse_loc, v.end()});
            if (!skip_result) {
               break;
            }
            parse_loc = skip_result->rest;
         }
         const auto result = parser_.parse({parse_loc, v.end()});
         if (!result) {
            break;
         }
         else {
            if constexpr (nil_type) {
               ++to_ret;
            }
            else {
               to_ret.push_back(result->value);
            }
            parse_loc = result->rest;
         }
      }
      // Only need to check min_ here as max_ is handled by the loop above
      if constexpr (nil_type) {
         if (to_ret < min_) {
            return nonstd::make_unexpected(parse_error{parse_loc});
         }
         return parse_success<value_type>{nil, parse_loc};
      }
      else {
         if (std::ssize(to_ret) < min_) {
            return nonstd::make_unexpected(parse_error{parse_loc});
         }
         return parse_success<value_type>{to_ret, parse_loc};
      }
   }
};

template<parser Parser>
repeat(Parser) -> repeat<always_fail, Parser>;

template<parser Parser>
repeat(Parser, int, int) -> repeat<always_fail, Parser>;

template<parser Parser, parser Skipper>
repeat(with_skipper_t, Skipper, Parser) -> repeat<Skipper, Parser>;

template<parser Parser, parser Skipper>
repeat(with_skipper_t, Skipper, Parser, int, int) -> repeat<Skipper, Parser>;

template<typename FirstResType, typename... RestResType>
auto calc_or_type() -> std::conditional_t<
   (std::same_as<FirstResType, RestResType> && ...) && !std::same_as<FirstResType, nil_t>,
   FirstResType,
   std::variant<FirstResType, RestResType...>>;

// TODO: Add variant that doesn't compress all types into the same thing in case that's wanted
template<parser... Parsers>
struct or_ {
private:
   [[no_unique_address]] std::tuple<Parsers...> parsers_;

public:
   template<std::constructible_from<Parsers>... ArgParsers>
   or_(ArgParsers&&... args) noexcept : parsers_{static_cast<ArgParsers&&>(args)...}
   {}

   using value_type = decltype(calc_or_type<produced_type<Parsers>...>());
   auto parse(std::string_view v) const noexcept -> parse_result<value_type>
   {
      value_type to_ret;
      const char* rest = nullptr;
      const auto handle_parser = [&]<std::size_t I>() {
         if (rest) {
            return;
         }
         const auto result = std::get<I>(parsers_).parse(v);
         if (result) {
            if constexpr (is_variant<value_type>) {
               to_ret.template emplace<I>(result->value);
            }
            else {
               to_ret = result->value;
            }
            rest = result->rest;
         }
      };

      [&]<std::size_t... I>(std::index_sequence<I...>) {
         (handle_parser.template operator()<I>(), ...);
      }(std::index_sequence_for<Parsers...>{});

      if (rest) {
         return parse_success<value_type>{to_ret, rest};
      }
      else {
         return nonstd::make_unexpected(parse_error{v.begin()});
      }
   }
};

template<parser... Parsers>
or_(Parsers...) -> or_<Parsers...>;

template<typename T>
using nilify = std::conditional_t<std::same_as<T, void>, nil_t, T>;

// TODO: constrain Callable
// TODO: seq and or_ specialization
//       seq uses std::apply
//       or_ uses N callable arguments for each alternative
// TODO: Allow returning of errors from the bound function
template<parser Parser, typename Callable>
struct bind {
private:
   [[no_unique_address]] Parser parser_;
   [[no_unique_address]] Callable callable_;

   using value_type = nilify<decltype(callable_(parser_.parse(std::string_view{})->value))>;

public:
   template<std::constructible_from<Parser> ArgParser, std::constructible_from<Callable> ArgCallable>
   bind(ArgParser&& parser, ArgCallable&& callable) noexcept
      : parser_{static_cast<ArgParser&&>(parser)}, callable_{static_cast<ArgCallable&&>(callable)}
   {}

   auto parse(std::string_view v) const noexcept -> parse_result<value_type>
   {
      const auto result = parser_.parse(v);
      if (!result) {
         return nonstd::make_unexpected(result.error());
      }
      else {
         if constexpr (std::same_as<value_type, nil_t>) {
            callable_(result->value);
            return parse_success<nil_t>{nil, result->rest};
         }
         else {
            return parse_success<value_type>{callable_(result->value), result->rest};
         }
      }
   }
};

template<parser Parser, typename Callable>
bind(Parser, Callable) -> bind<Parser, Callable>;

template<typename RetType>
struct fwd_parser {
public:
   template<parser Parser>
   // clang-format off
      // TODO: Make "typed parser" concept instead of this
      requires requires (Parser p, std::string_view v) { {p.parse(v)} -> std::same_as<parse_result<RetType>>; }
   // clang-format on
   // Only take references because an rvalue will dangle
   // There might be a better way around this but this was the best I could come up with
   fwd_parser(Parser& p) noexcept : parser_{[&p](std::string_view v) { return p.parse(v); }}
   {}

   constexpr fwd_parser() noexcept = default;

   parse_result<RetType> parse(std::string_view v) const { return parser_(v); }

private:
   std::function<parse_result<RetType>(std::string_view)> parser_;
};

} // namespace khparse

#endif // KHPARSE_HPP
