#include <cassert>
#include <charconv>
#include <cstdint>
#include <optional>
#include <string_view>
#include <utility>
#include <tuple>
#include <variant>
#include <vector>

// TODO: Probably rework this design
//       Not entirely sure why I encapsulated all of the parsers in lambdas
//       and had them return internal types.  It works but it's weird.

namespace khparse {

// Should make the return types have their data be private but lazy
constexpr auto char_ = [](std::vector<std::pair<char, char>> ranges = {}) {
   struct ret_type {
   public:
      const char* parse(std::string_view v) const noexcept
      {
         if (v.empty()) {
            return nullptr;
         }
         else if (ranges.empty()) {
            return v.data() + 1;
         }
         else {
            for (const auto& r : ranges) {
               const auto c = v.front();
               if (c < r.first || c > r.second) {
                  return nullptr;
               }
            }
            return v.data() + 1;
         }
      }

      char produce_value(std::string_view v) const noexcept
      {
         return v.front();
      }

      std::vector<std::pair<char, char>> ranges;
   };

   return ret_type{ranges};
};

constexpr auto lit_ = [](std::string_view match_text) {
   struct ret_type {
   public:
      const char* parse(std::string_view v) const noexcept
      {
         if (v.starts_with(match_text)) {
            return v.substr(match_text.size()).data();
         }
         return nullptr;
      }

      void produce_value(std::string_view) const noexcept
      {}

      std::string_view match_text;
   };

   return ret_type{match_text};
};

namespace detail {
   constexpr auto make_number_parser = [](int base, const char* charset) {
      struct ret_type {
      public:
         const char* parse(std::string_view v) const noexcept
         {
            // Make sure the first character matches int the character set
            // !charset.contains(v.front())
            if (charset.find_first_of(v.front()) != 0) {
               return nullptr;
            }
            const auto end_loc = v.find_first_not_of(charset);
            return v.substr(end_loc).data();
         }

         std::int64_t produce_value(std::string_view v) const noexcept
         {
            std::int64_t value;
            const auto [ptr, ec] = std::from_chars(v.begin(), v.data() + v.size(), value, base);
            assert(ptr == v.data() + v.size());
            assert(ec == std::errc{});
            // TODO: Error checking?  For now we just assume that produce_value won't fail for any parser
            return value;
         }

         int base;
         std::string_view charset;
      };

      return ret_type{base, charset};
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

   struct void_{};
   template<typename T> struct wrap_void       { using type = T;     };
   template<          > struct wrap_void<void> { using type = void_; };

   template<std::size_t N>
   struct constexpr_size_t {};
}

template<typename T>
using wrap_void = typename detail::wrap_void<T>::type;

constexpr auto hex_number = detail::make_number_parser(16, "0123456789AaBbCcDdEeFf");
constexpr auto dec_number = detail::make_number_parser(10, "0123456789");
constexpr auto bin_number = detail::make_number_parser(2, "01");

// TODO: Probably figure out how to carry data around so this doesn't have to be
// re-parsed when it comes to producing values.
// But it's fine (for now?)
constexpr auto repeat = [](const auto& parser, int min = 0, int max = std::numeric_limits<int>::max()) {
   using base_type = decltype(parser.produce_value(""));
   using gen_type = typename detail::propagate_void<base_type>::template result<std::vector>::type;
   using parser_type = std::remove_cv_t<decltype(parser)>;

   assert(min <= max);

   struct ret_type {
   public:
      const char* parse(std::string_view v) const noexcept
      {
         int num_parsed = 0;
         std::string_view rest = v;
         while (num_parsed < max) {
            const auto result = parser.parse(rest);
            if (result == nullptr) {
               break;
            }
            rest = std::string_view{result, v.data() + v.size()};
            ++num_parsed;
         }
         if (num_parsed < min) {
            return nullptr;
         }
         return rest.data();
      }

      gen_type produce_value(std::string_view v) const noexcept
      {
         if constexpr (std::is_same_v<void, gen_type>) {
            return;
         }
         else {
            gen_type result;
            std::string_view rest = v;
            while (!rest.empty()) {
               result.push_back(parser.produce_value(rest));
               rest = std::string_view{parser.parse(rest), v.data() + v.size()};
            }
            return result;
         }
      }

      parser_type parser;
      int min;
      int max;
   };

   return ret_type{parser, min, max};
};

constexpr auto or_ = []<typename... Parsers>(const Parsers&... parsers) {
   using parser_tuple_type = std::tuple<Parsers...>;
   using gen_type = std::variant<wrap_void<decltype(parsers.produce_value(""))>...>;

   struct ret_type {
   public:
      const char* parse(std::string_view v) const noexcept
      {
         const char* to_ret = nullptr;
         const auto handle_parser = [&](const auto& p) {
            if (to_ret) { return; }
            // Can just assign the value since we check if it's nullptr
            to_ret = p.parse(v);
         };

         [&]<std::size_t... N>(std::index_sequence<N...>) {
            (handle_parser(std::get<N>(parser_tuple)), ...);
         }(std::make_index_sequence<sizeof...(parsers)>{});

         return to_ret;
      }

      gen_type produce_value(std::string_view v) const noexcept
      {
         // Use an optional so we know when it's created/in case it's not default constructable
         std::optional<gen_type> to_ret;
         const auto handle_parser = [&]<std::size_t N>(const auto& p, detail::constexpr_size_t<N>) {
            if (to_ret) { return; }
            if (p.parse(v)) {
               if constexpr (std::is_same_v<decltype(p.produce_value("")), void>) {
                  to_ret = gen_type{std::in_place_index_t<N>{}};
               }
               else {
                  to_ret = p.produce_value(v);
               }
            }
         };

         [&]<std::size_t... N>(std::index_sequence<N...>) {
            (handle_parser(std::get<N>(parser_tuple), detail::constexpr_size_t<N>{}), ...);
         }(std::make_index_sequence<sizeof...(parsers)>{});

         assert(to_ret.has_value());
         return *to_ret;
      }

      parser_tuple_type parser_tuple;
   };

   return ret_type{{parsers...}};
};

constexpr auto always_fail = []() {
   struct ret_type {
      const char* parse(std::string_view) const noexcept { return nullptr; }
      void produce_value(std::string_view) const noexcept {}
   };

   return ret_type{};
};

} // namespace khparse

int main()
{
   // assert(hex_number.produce_value("10") == 0x10);
   // assert(repeat(lit_("a")).parse("aaaaaaaaaaaaaaaaa"));
   // assert(char_().produce_value("a") == 'a');
   // assert(repeat(char_()).produce_value("aaa") == (std::vector<char>{'a', 'a', 'a'}));

   // const auto test = or_(lit_("a"), lit_("b"));
   // assert(test.parse("a"));
   // assert(test.parse("b"));
   // assert(!test.parse("c"));
   // assert(test.produce_value("b").index() == 1);
}
