// PRG format is incredibly simple
// First 2 bytes are where to load the data, then the binary data follows

// This worked, but compile times were slow and it prodced a lot of code

#include <bitset>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <string_view>
#include <optional>
#include <limits>
#include <utility>
#include <system_error>
#include <charconv>
#include <tuple>
#include <type_traits>
#include <variant>
#include <iostream>

// This is a very basic parser framework
// It's probably not super efficient or anything, but it's simple enough
// Would probably want to not return optionals and use string_view or something
// Dunno, this isn't going to be improved unless it's very, very slow

using void_parse_result = std::pair<bool, std::string_view>;

template<typename T>
using parse_result = std::pair<std::optional<T>, std::string_view>;

struct literal {
public:
   constexpr literal(const char* match) noexcept
      : match_{match}
   {}

   void_parse_result parse(std::string_view v) const noexcept
   {
      if (v.starts_with(match_)) {
         return {true, v.substr(match_.size())};
      }
      return {false, v};
   }

private:
   std::string_view match_;
};

struct number_ {
public:
   parse_result<std::int64_t> parse(std::string_view v) const noexcept
   {
      const auto [start, end, base] = [&]() -> std::tuple<const char*, const char*, int> {
         const auto get_end = [&](std::size_t loc) -> const char* {
            if (loc == v.npos) {
               return v.data() + v.size();
            }
            return v.data() + loc;
         };
         if (v.starts_with("0x")) {
            const auto start = v.substr(2);
            const auto non_num_index = start.find_first_not_of("0123456789AaBbCcDdEeFf");
            return {start.data(), get_end(non_num_index), 16};
         }
         else if (v.starts_with("$")) {
            const auto start = v.substr(1);
            const auto non_num_index = start.find_first_not_of("0123456789AaBbCcDdEeFf");
            return {start.data(), get_end(non_num_index), 16};
         }
         else if (v.starts_with("0b")) {
            const auto start = v.substr(2);
            const auto non_num_index = start.find_first_not_of("01");
            return {start.data(), get_end(non_num_index), 2};
         }
         else {
            const auto non_num_index = v.find_first_not_of("0123456789");
            return {v.data(), get_end(non_num_index), 10};
         }
      }();
      if (start == end) {
         return {{}, v};
      };
      std::int64_t value;
      const auto [ptr, ec] = std::from_chars(start, end, value, base);
      if (ec == std::errc{} && ptr == end) {
         return {value, {ptr, v.data() + v.size()}};
      }
      else {
         return {{}, v};
      }
   }
};

constexpr number_ number{};

// FEATURE: Use non-bitset based implementation because it's still not constexpr
// for some dumb reason.  Grr.
struct char_ {
public:
   struct char_range {
      constexpr char_range(char lower, char upper) noexcept
         : lower{static_cast<unsigned char>(lower)}
         , upper{static_cast<unsigned char>(upper)}
      {
         assert(lower <= upper);
      }

      unsigned char lower;
      unsigned char upper;
   };

   char_(std::initializer_list<char> c_list) noexcept
      : allowed_{}
   {
      for (const auto c : c_list) {
         allowed_[c] = true;
      }
   }

   char_(std::initializer_list<char_range> range_list) noexcept
   {
      for (const auto [low, high] : range_list) {
         for (int i = low; i <= high; ++i) {
            allowed_[i] = true;
         }
      }
   }

   parse_result<char> parse(std::string_view v) const noexcept
   {
      if (v.empty() || !allowed_[v.front()]) {
         return {std::nullopt, v};
      }
      else {
         return {v.front(), v.substr(1)};
      }
   }

private:
   std::bitset<256> allowed_;
};

namespace detail {
   // work around
   template<typename... T>
   struct head {
      using type = void;
   };

   template<typename T, typename... Rest>
   struct head<T, Rest...> {
      using type = T;
   };

   template<typename... T>
   struct seq_type_impl;

   template<typename... SoFar>
   struct seq_type_impl<std::tuple<SoFar...>> {
      using type =
         std::conditional_t<sizeof...(SoFar) == 0,
            void_parse_result,
            parse_result<std::tuple<SoFar...>>>;
   };

   template<typename... SoFar, typename T, typename... Rest>
   struct seq_type_impl<std::tuple<SoFar...>, parse_result<T>, Rest...> {
      using type = typename seq_type_impl<std::tuple<SoFar..., T>, Rest...>::type;
   };

   template<typename... SoFar, typename... Rest>
   struct seq_type_impl<std::tuple<SoFar...>, void_parse_result, Rest...> {
      using type = typename seq_type_impl<std::tuple<SoFar...>, Rest...>::type;
   };

   template<typename T>
   struct wrap_const_char {
      using type = T;
   };

   template<>
   struct wrap_const_char<const char*> {
      using type = literal;
   };

   template<typename T>
   struct generated_type_impl {
      using type = void;
   };

   template<typename T>
   struct generated_type_impl<parse_result<T>> {
      using type = T;
   };

   template<typename T>
   struct repeat_type_impl {
      using type = parse_result<T>;
   };

   template<>
   struct repeat_type_impl<void> {
      using type = void_parse_result;
   };
}

template<typename T>
using generated_type = typename detail::generated_type_impl<T>::type;

template<typename T>
using repeat_type = typename detail::repeat_type_impl<generated_type<T>>::type;

template<typename... T>
using seq_type = typename detail::seq_type_impl<
   std::tuple<>,
   decltype(std::declval<T>().parse(std::declval<std::string_view>()))...
>::type;

template<typename... Parsers>
struct seq {
public:
   constexpr seq(const Parsers&... parsers) noexcept
      : parsers_{parsers...}
   {}

   seq_type<Parsers...> parse(std::string_view v) const noexcept
   {
      seq_type<Parsers...> to_ret = [&]() -> seq_type<Parsers...> {
         if constexpr (is_void_parse_result) {
            return {true, v};
         }
         else {
            return {typename seq_type<Parsers...>::first_type{std::in_place}, v};
         }
      }();
      std::apply(handle_parser<0, Parsers...>, std::tuple_cat(std::forward_as_tuple(to_ret), parsers_));
      return to_ret;
   }

private:
   static inline constexpr auto is_void_parse_result = std::is_same_v<seq_type<Parsers...>, void_parse_result>;

   template<std::size_t N, typename CurParser, typename... Rest>
   static void handle_parser(seq_type<Parsers...>& to_ret, const CurParser& sub_parser, const Rest&... rest) noexcept {
      const auto parse_result = sub_parser.parse(to_ret.second);
      if (!parse_result.first) {
         if constexpr (is_void_parse_result) {
            to_ret.first = false;
         }
         else {
            to_ret.first = std::nullopt;
         }
         return;
      }
      to_ret.second = parse_result.second;
      if constexpr (std::is_same_v<decltype(parse_result), const void_parse_result>) {
         if constexpr (sizeof...(rest) > 0) {
            handle_parser<N>(to_ret, rest...);
         }
      }
      else {
         std::get<N>(*to_ret.first) = *parse_result.first;
         if constexpr (sizeof...(rest) > 0) {
            handle_parser<N + 1>(to_ret, rest...);
         }
      }
   }

   std::tuple<Parsers...> parsers_;
};

template<typename... Parsers>
seq(Parsers...) -> seq<typename detail::wrap_const_char<Parsers>::type...>;

// Extract actual types generated and store into a variant
template<typename... T>
using or_type = parse_result<
   std::variant<typename decltype(std::declval<T>().parse(std::declval<std::string_view>()))::first_type...>
>;

template<typename... Parsers>
struct or_{
public:
   constexpr or_(const Parsers&... parsers) noexcept
      : parsers_{parsers...}
   {}

   or_type<Parsers...> parse(std::string_view v) const noexcept
   {
      or_type<Parsers...> to_ret{std::nullopt, v};
      std::apply(handle_parser<0, Parsers...>, std::tuple_cat(std::forward_as_tuple(to_ret), parsers_));
      return to_ret;
   }

private:
   template<std::size_t N, typename CurParser, typename... Rest>
   static void handle_parser(or_type<Parsers...>& to_ret, const CurParser& sub_parser, const Rest&... rest) noexcept {
      const auto parse_result = sub_parser.parse(to_ret.second);
      if (parse_result.first) {
         using variant_type = typename or_type<Parsers...>::first_type::value_type;
         to_ret.first = variant_type{std::in_place_index<N>, parse_result.first};
         to_ret.second = parse_result.second;
         return;
      }
      if constexpr (sizeof...(rest) > 0) {
         handle_parser<N + 1>(to_ret, rest...);
      }
   };

   std::tuple<Parsers...> parsers_;
};

template<typename... Parsers>
or_(Parsers...) -> or_<typename detail::wrap_const_char<Parsers>::type...>;

template<typename Parser>
struct repeat {
public:
   constexpr repeat(const Parser& parser, int min_count = 0, int max_count = std::numeric_limits<int>::max()) noexcept
      : parser_{parser}
      , min_count_{min_count}
      , max_count_{max_count}
   {
      assert(min_count >= 0);
      assert(max_count >= min_count);
   }

   repeat_type<Parser> parse(std::string_view v) const noexcept
   {
      if constexpr (std::is_same_v<repeat_type<Parser>, void_parse_result>) {
         void_parse_result to_ret{true, v};
         int num_parsed = 0;
         while (num_parsed < max_count_) {
            const auto result = parser_.parse(to_ret.second);
            to_ret.second = result.second;
            if (!result.first) {
               break;
            }
            ++num_parsed;
         }
         if (num_parsed < min_count_) {
            to_ret.first = false;
         }
         return to_ret;
      }
      else {
         using vec_type = typename repeat_type<Parser>::first_type;
         repeat_type<Parser> to_ret{vec_type{}, v};
         to_ret.first = {};
         auto& vec = *to_ret.first;
         while (std::ssize(vec) < max_count_) {
            const auto result = parser_.parse(to_ret.second);
            to_ret.second = result.second;
            if (!result.first) {
               break;
            }
            vec.push_back(to_ret->front());
         }
         if (std::ssize(vec) < min_count_) {
            to_ret.first = std::nullopt;
         }
         return to_ret;
      }
   }

private:
   Parser parser_;
   int min_count_;
   int max_count_;
};

// BEGIN SEPARATE CODE(?)

// constexpr or_ instr{
//    "adc", "and", "asl", "bcc", "bcs", "beq", "bit", "bmi", "bne", "bpl", "brk", "bvc", "bvs", "clc",
//    "cld", "cli", "clv", "cmp", "cpx", "cpy", "dec", "dex", "dey", "eor", "inc", "inx", "iny", "jmp",
//    "jsr", "lda", "ldx", "ldy", "lsr", "nop", "ora", "pha", "php", "pla", "plp", "rol", "ror", "rti",
//    "rts", "sbc", "sec", "sed", "sei", "sta", "stx", "sty", "tax", "tay", "tsx", "txa", "txs", "tya"
// };

int main()
{
   seq test{
      repeat{char_{{'A', 'Z'}}},
      "(",
      or_{number, repeat{char_{{'A', 'Z'}}}},
      ");"
   };
   assert(test.parse("FUNC(PARAM);").first);
   assert(test.parse("BIGFUNC(128);").first);
   assert(!test.parse("HECK(z);").first);
}
