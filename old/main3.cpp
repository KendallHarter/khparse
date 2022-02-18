#include <cassert>
#include <charconv>
#include <cstdint>
#include <optional>
#include <string_view>
#include <utility>
#include <tuple>
#include <variant>
#include <vector>

#include <iostream>

namespace khparse {

struct literal;
struct void_{};
template<typename> struct repeat;

namespace detail {

struct number_parser {
public:
   constexpr number_parser(int base, std::string_view v) noexcept
      : base_{base}
      , charset_{v}
   {}

   /* constexpr */ const char* parse(std::string_view v) const noexcept
   {
      // Make sure the first character matches int the character set
      // !charset_.contains(v.front())
      if (charset_.find_first_of(v.front()) == charset_.npos) {
         return nullptr;
      }
      const auto end_loc = v.find_first_not_of(charset_);
      if (end_loc != v.npos) {
         return v.substr(end_loc).data();
      }
      else {
         return v.data() + v.size();
      }
   }

   /* constexpr */ std::int64_t produce_value(std::string_view v) const noexcept
   {
      if (!std::is_constant_evaluated()) {
         std::int64_t value;
         const auto [ptr, ec] = std::from_chars(v.begin(), v.data() + v.size(), value, base_);
         // assert(ptr == v.data() + v.size());
         assert(ec == std::errc{});
         return value;
      }
      else {
         [[maybe_unused]] std::int64_t value = 0;
         // TODO: implement this in a constexpr context
         [[maybe_unused]] extern int constexpr_produce_value_not_implemented;
         return 0;
      }
   }

private:
   int base_;
   std::string_view charset_;
};

template<typename T>
struct propagate_void {
   template<template<typename...> typename Template>
   struct result { using type = Template<T>; };
};

template<>
template<template<typename...> typename Template>
struct propagate_void<void>::result<Template> {
   using type = void;
};

// clang-format off
template<typename T> struct wrap_void       { using type = T;     };
template<          > struct wrap_void<void> { using type = void_; };

template<typename T> struct wrap_str              { using type = T; };
template<          > struct wrap_str<const char*> { using type = literal; };
// clang-format on

template<std::size_t N>
struct constexpr_size_t {};

template<typename...> struct TD;

template<bool IsOr, typename Skipper, typename... Parsers>
struct seq_or_impl {
private:
   using parser_tuple_type = std::tuple<Parsers...>;
   using gen_type = std::conditional_t<IsOr,
      std::variant<typename detail::wrap_void<decltype(std::declval<Parsers>().produce_value(""))>::type...>,
      std::tuple<typename detail::wrap_void<decltype(std::declval<Parsers>().produce_value(""))>::type...>
   >;

public:
   explicit constexpr seq_or_impl(const Skipper& skipper, const Parsers&... parsers) noexcept
      : skipper_{skipper}
      , parser_tuple_{parsers...}
   {}

   /* constexpr */ const char* parse(std::string_view v) const noexcept
   {
      const char* to_ret = IsOr ? nullptr : handle_skipper(v.data(), v.data() + v.size());
      const auto handle_parser = [&](const auto& p) {
         if constexpr (IsOr) {
            if (to_ret) { return; }
            to_ret = p.parse(v);
         }
         else {
            if (!to_ret) { return; }
            to_ret = handle_skipper(p.parse({to_ret, v.data() + v.size()}), v.data() + v.size());
         }
      };

      // clang-format off
      // [&]<std::size_t... N>(std::index_sequence<N...>) {
      //    (handle_parser(std::get<N>(parser_tuple_)), ...);
      // }(std::make_index_sequence<sizeof...(Parsers)>{});
      // clang-format on

      return to_ret;
   }

   /* constexpr */ gen_type produce_value(std::string_view v) const noexcept
   {
      // Use an optional so we know when it's created/in case it's not default constructable
      if constexpr (IsOr) {
         std::optional<gen_type> to_ret;
         const auto handle_parser = [&]<std::size_t N>(const auto& p, detail::constexpr_size_t<N>) {
            if (to_ret) { return; }
            if (p.parse(v)) {
               if constexpr (std::is_same_v<decltype(p.produce_value("")), void>) {
                  to_ret = gen_type{std::in_place_index_t<N>{}};
               }
               else {
                  to_ret = gen_type{std::in_place_index_t<N>{}, p.produce_value(v)};
               }
            }
         };

         // clang-format off
         // [&]<std::size_t... N>(std::index_sequence<N...>) {
         //    (handle_parser(std::get<N>(parser_tuple_), detail::constexpr_size_t<N>{}), ...);
         // }(std::make_index_sequence<sizeof...(Parsers)>{});
         // clang-format on

         assert(to_ret.has_value());
         return *to_ret;
      }
      else {
         std::string_view next{handle_skipper(v.data(), v.data() + v.size()), v.data() + v.size()};
         const auto handle_parser = [&](const auto& parser) {
            const auto value_string = next;
            const auto rest = handle_skipper(parser.parse(next), v.data() + v.size());
            next = std::string_view{rest, v.data() + v.size()};
            if constexpr (std::is_same_v<decltype(parser.produce_value("")), void>) {
               return void_{};
            }
            else {
               return parser.produce_value(value_string);
            }
         };

         // clang-format off
         // return [&]<std::size_t... N>(std::index_sequence<N...>) -> gen_type {
         //    return std::tuple{handle_parser(std::get<N>(parser_tuple_))...};
         // }(std::make_index_sequence<sizeof...(Parsers)>{});
         // clang-format on
      }
   }

private:
   constexpr const char* handle_skipper(const char* rest, const char* end) const noexcept
   {
      if constexpr (!std::is_same_v<Skipper, void_>) {
         if (rest) {
            const auto after_skip = repeat{skipper_}.parse({rest, end});
            if (after_skip) {
               rest = after_skip;
            }
         }
      }
      return rest;
   }

   [[no_unique_address]] Skipper skipper_;
   parser_tuple_type parser_tuple_;
};

} // namespace detail

struct char_ {
public:
   /* constexpr */ explicit char_(std::initializer_list<std::pair<char, char>> ranges) noexcept
      : ranges_{ranges}
   {}

   constexpr const char* parse(std::string_view v) const noexcept
   {
      if (v.empty()) {
         return nullptr;
      }
      else if (ranges_.empty()) {
         return v.data() + 1;
      }
      else {
         bool okay = false;
         for (const auto& r : ranges_) {
            const auto c = v.front();
            if (c >= r.first && c <= r.second) {
               okay = true;
            }
         }
         return okay ? v.data() + 1 : nullptr;
      }
   }

   constexpr char produce_value(std::string_view v) const noexcept
   {
      return v.front();
   }

private:
   std::vector<std::pair<char, char>> ranges_;
};

struct literal {
public:
   constexpr literal(const char* v) noexcept
      : match_text_{v}
   {}

   constexpr literal(std::string_view v) noexcept
      : match_text_{v}
   {}

   constexpr const char* parse(std::string_view v) const noexcept
   {
      if (v.starts_with(match_text_)) {
         return v.substr(match_text_.size()).data();
      }
      return nullptr;
   }

   constexpr void produce_value(std::string_view) const noexcept
   {}

private:
   std::string_view match_text_;
};

constexpr auto hex_number = detail::number_parser{16, "0123456789AaBbCcDdEeFf"};
constexpr auto dec_number = detail::number_parser{10, "0123456789"};
constexpr auto bin_number = detail::number_parser{2, "01"};

template<typename Parser>
struct repeat {
private:
   using base_type = decltype(std::declval<Parser>().produce_value(""));
   using gen_type = typename detail::propagate_void<base_type>::template result<std::vector>::type;

public:
   /* constexpr */ repeat(const Parser& parser, int min = 1, int max = std::numeric_limits<int>::max()) noexcept
      : parser_{parser}
      , min_{min}
      , max_{max}
   {
      assert(min <= max);
   }

   constexpr const char* parse(std::string_view v) const noexcept
   {
      int num_parsed = 0;
      std::string_view rest = v;
      while (num_parsed < max_) {
         const auto result = parser_.parse(rest);
         if (result == nullptr) {
            break;
         }
         rest = std::string_view{result, v.data() + v.size()};
         ++num_parsed;
      }
      if (num_parsed < min_) {
         return nullptr;
      }
      return rest.data();
   }

   /* constexpr */ gen_type produce_value(std::string_view v) const noexcept
   {
      if constexpr (std::is_same_v<void, gen_type>) {
         return;
      }
      else {
         gen_type result;
         std::string_view rest = v;
         while (!rest.empty()) {
            result.push_back(parser_.produce_value(rest));
            rest = std::string_view{parser_.parse(rest), v.data() + v.size()};
         }
         return result;
      }
   }

private:
   Parser parser_;
   int min_;
   int max_;
};

template<typename Parser>
repeat(Parser) -> repeat<typename detail::wrap_str<Parser>::type>;

template<typename... Parsers>
struct or_ : detail::seq_or_impl<true, void_, Parsers...> {
   using base = detail::seq_or_impl<true, void_, Parsers...>;

   constexpr or_(const Parsers&... p) noexcept
      : base{void_{}, p...}
   {}
};

template<typename... Parsers>
or_(Parsers...) -> or_<typename detail::wrap_str<Parsers>::type...>;

template<typename... Parsers>
struct sequence : detail::seq_or_impl<false, void_, Parsers...> {
   using base = detail::seq_or_impl<false, void_, Parsers...>;

   constexpr sequence(const Parsers&... p) noexcept
      : base{void_{}, p...}
   {}
};

template<typename... Parsers>
sequence(Parsers...) -> sequence<typename detail::wrap_str<Parsers>::type...>;

template<typename... Parsers>
using seq = sequence<Parsers...>;

template<typename Skipper, typename... Parsers>
struct seq_with_skipper : detail::seq_or_impl<false, Skipper, Parsers...> {
   using base = detail::seq_or_impl<false, Skipper, Parsers...>;
   using base::base;
};

template<typename Skipper, typename... Parsers>
seq_with_skipper(Skipper, Parsers...) -> seq_with_skipper<typename detail::wrap_str<Skipper>::type, typename detail::wrap_str<Parsers>::type...>;

} // namespace khparse

// Ideas:
// inline variables - are just encoded within instructions and so don't take any additional RAM
//                    Will need to write to every location that it is used though
//                    Constants and variables that are only used once can always be treated like this
// mmio - have on_read/on_write functions that act as bit masks etc. so can know which bits having meaning/are read
// irq - Have as own function type; also need to have pointer thing for it for MMIO

// To figure out:
// Good way to handle stuff like paging, stuff that isn't always resident in RAM, etc.

// This code compiles horribly slowly (types get horrible large very quickly) and currently doesn't work anyways
// Need to do one with type erasure of some sort, probably

int main()
{
   using namespace khparse;
   // clang-format off
   // basic parsers
   const auto number = or_{
      seq{
         or_{"0x", "$"},
         hex_number
      },
      seq{
         or_{"0b", "%"},
         bin_number
      },
      dec_number
   };
   const auto letter = char_{{'a', 'z'}, {'A', 'Z'}};
   const auto name   = repeat{letter};
   const auto value  = or_{number, name};
   // directive parsers
   const auto org         = seq_with_skipper{" ", ".org", number};
   const auto label       = seq{name, ":"};
   const auto opcode_name = repeat{letter, 3, 3};
   // addressing modes
   const auto immediate   = seq{"#", value};
   const auto abs_or_zp   = value;
   const auto abs_or_zp_x = seq{value, ",x"};
   const auto abs_y       = seq{value, ",y"};
   const auto indirect_x  = seq{"(", value, ",x)"};
   const auto indirect_y  = seq{"(", value, "),y"};
   const auto indirect    = seq{"(", value, ")"};
   const auto addr_mode = or_{
      immediate,
      abs_y,
      indirect_x,
      indirect_y,
      indirect,
      abs_or_zp_x,
      abs_or_zp
   };
   // putting everything together
   const auto parser = seq_with_skipper{char_{{' ', ' '}, {'\n', '\n'}},
      repeat{or_{
         org,
         label,
         // special case opcodes (implied addressing mode)
         "nop",
         "brk",
         "rts",
         "rti",
         "txs",
         "tsx",
         "pha",
         "pla",
         "php",
         "plp",
         // general case opcodes
         seq_with_skipper{char_{{' ', ' '}, {'\n', '\n'}},
            opcode_name, addr_mode
         }
      }}
   };
   // supported addressing modes
   // clang-format on
   const char* dumb_prog = R"(
   .org 0xC000

   loop:
      inc $D020
      jmp loop
   )";
   assert(parser.parse(dumb_prog));
   const auto result = parser.produce_value(dumb_prog);
   (void)value;
}
