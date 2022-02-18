#include <bitset>
#include <cassert>
#include <concepts>
#include <charconv>
#include <cstring>
#include <cstdint>
#include <string_view>
#include <variant>
#include <memory>
#include <vector>
#include <limits>

// OKAYOKAYOKAY
// WE'RE ALMOST THERE
// separate parsing and producing values
// can then type erase
// can unambiguously interpret values then
// which means the vast majority of the code isn't actually templated
// just the interpretation of the results

struct parse_failure {
   const char* reason;
};

template<typename T>
struct parse_success {
   [[no_unique_address]] T value;
   const char* rest;
};

struct void_{};

template<>
struct parse_success<void> {
   parse_success(const char* rest) noexcept
      : rest{rest}
   {}

   [[no_unique_address]] void_ dummy;
   const char* rest;
};

template<typename T>
using parse_result = std::variant<parse_failure, parse_success<T>>;

template<typename T>
using base_result_type = decltype(std::variant_alternative_t<1, decltype(std::declval<T>().parse(""))>::value);

template<typename T>
bool is_success(const parse_result<T>& p) noexcept
{
   return p.index() == 1;
}

// clang-format off
template<typename T, typename R>
concept is_parser_r = requires (const T& parser) {
   { parser.parse("") } -> std::same_as<parse_result<R>>;
};
// clang-format on

template<typename Result>
struct parser_erasure {
private:
   using func_ptr = parse_result<Result> (*)(const parser_erasure&, std::string_view) noexcept;
   using deleter_ptr = void (*)(void*) noexcept;
   inline static constexpr auto storage_size = sizeof(void*) * 2;
   static_assert(sizeof(void*) == 8);

public:
   parser_erasure(const parser_erasure&) = delete;
   parser_erasure& operator=(const parser_erasure&) = delete;

   // This could be releveant/implemented eventually
   parser_erasure(parser_erasure&&) = delete;
   parser_erasure& operator=(parser_erasure&&) = delete;

   template<is_parser_r<Result> Parser>
   // clang-format off
      requires (sizeof(Parser) <= storage_size
             && std::is_trivially_destructible_v<Parser>
             && !std::same_as<std::remove_cvref_t<Parser>, parser_erasure>)
   // clang-format on
   parser_erasure(Parser&& p) noexcept
      : invoker_{mark_static([](const parser_erasure& p, std::string_view v) noexcept {
         return std::launder(reinterpret_cast<const std::remove_cvref_t<Parser>*>(&p.storage_))->parse(v);
      })}
   {
      new (&storage_) std::remove_cvref_t<Parser>{std::forward<Parser>(p)};
   }

   template<is_parser_r<Result> Parser>
   // clang-format off
      requires (!std::same_as<std::remove_cvref_t<Parser>, parser_erasure>)
   // clang-format on
   parser_erasure(Parser&& p) noexcept
      : ptrs_{
         new std::remove_cvref_t<Parser>{std::forward<Parser>(p)},
         [](void* ptr) noexcept {
            delete static_cast<std::remove_cvref_t<Parser>*>(ptr);
         }
      }
      , invoker_{mark_dynamic([](const parser_erasure& p, std::string_view v) noexcept {
         const auto parser_ptr = static_cast<const std::remove_cvref_t<Parser>*>(p.ptrs_.alloc_);
         return parser_ptr->parse(v);
      })}
   {
   }

   ~parser_erasure()
   {
      if (is_dynamic()) {
         ptrs_.deleter_(ptrs_.alloc_);
      }
   }

   parse_result<Result> parse(std::string_view v) const noexcept
   {
      return get_ptr()(*this, v);
   }

private:
   static std::uintptr_t mark_static(func_ptr ptr) noexcept
   {
      assert(!(reinterpret_cast<std::uintptr_t>(ptr) & 0x8000'0000'0000'0000));
      return reinterpret_cast<std::uintptr_t>(ptr);
   }

   static std::uintptr_t mark_dynamic(func_ptr ptr) noexcept
   {
      assert(!(reinterpret_cast<std::uintptr_t>(ptr) & 0x8000'0000'0000'0000));
      return reinterpret_cast<std::uintptr_t>(ptr) | 0x8000'0000'0000'0000;
   }

   func_ptr get_ptr() const noexcept
   {
      return reinterpret_cast<func_ptr>(invoker_ & 0x7FFF'FFFF'FFFF'FFFF);
   }

   bool is_dynamic() const noexcept
   {
      return invoker_ & 0x8000'0000'0000'0000;
   }

   struct ptrs_struct {
      void* alloc_;
      deleter_ptr deleter_;
   };

   union {
      alignas(sizeof(void*) * 2) char storage_[sizeof(void*) * 2];
      ptrs_struct ptrs_;
   };
   std::uintptr_t invoker_;

   static_assert(sizeof(storage_) == storage_size);
};

struct literal {
public:
   constexpr literal(const char* match) noexcept
      : match_{match}
   {}

   parse_result<void> parse(std::string_view v) const noexcept
   {
      if (v.starts_with(match_)) {
         return parse_success<void>{v.substr(match_.size()).data()};
      }
      return parse_failure{""};
   }

private:
   std::string_view match_;
};

struct number {
   constexpr number(int base) noexcept
      : base_{base}
   {}

   parse_result<std::int64_t> parse(std::string_view v) const noexcept
   {
      std::int64_t value;
      const auto [ptr, ec] = std::from_chars(v.begin(), v.data() + v.size(), value, base_);
      if (ec != std::errc{}) {
         if (ec == std::errc::invalid_argument) {
            return parse_failure{"Could not parse number"};
         }
         else if (ec == std::errc::result_out_of_range) {
            return parse_failure{"Number too large"};
         }
         else {
            return parse_failure{"Unknown error"};
         }
      }
      return parse_success{value, ptr};
   }

private:
   int base_;
};

struct char_ {
   char_() noexcept
   {
      // Set all to true
      allowed.flip();
   }

   char_(std::string_view init_string) noexcept
   {
      handle_init(init_string);
   }

   parse_result<char> parse(std::string_view v) const noexcept
   {
      if (v.empty()) {
         return parse_failure{"Empty string"};
      }
      else if (!allowed[v.front()]) {
         return parse_failure{"Character did not match"};
      }
      return parse_success{v.front(), v.substr(1).data()};
   }

private:
   void handle_init(std::string_view rest) noexcept
   {
      if (rest.empty()) {
         return;
      }
      else if (rest.size() == 1) {
         allowed[rest.front()] = true;
      }
      else if (rest[1] == '-') {
         assert(rest.size() >= 3);
         assert(rest[0] <= rest[2]);
         for (char c = rest[0]; c <= rest[2]; ++c) {
            allowed[c] = true;
         }
         handle_init(rest.substr(3));
      }
      else {
         allowed[rest.front()] = true;
         handle_init(rest.substr(1));
      }
   }

   std::bitset<256> allowed;
};

template<typename ProducedType>
struct repeat {
public:
   template<is_parser_r<ProducedType> T>
   explicit repeat(T&& parser, int min = 1, int max = std::numeric_limits<int>::max()) noexcept
      : parser_{parser}
      , min_{min}
      , max_{max}
   {
      assert(min >= 0);
      assert(max >= 0);
      assert(min <= max);
   }

   parse_result<std::vector<ProducedType>> parse(std::string_view v) const noexcept
   {
      const char* const end_ptr = v.data() + v.size();
      const char* rest = v.data();
      std::vector<ProducedType> to_ret;
      for (unsigned i = 0; i < static_cast<unsigned>(max_); ++i) {
         const auto result = parser_.parse({rest, end_ptr});
         if (const auto* data = std::get_if<parse_success<ProducedType>>(&result)) {
            rest = data->rest;
            to_ret.push_back(data->value);
         }
         else {
            if (i < static_cast<unsigned>(min_)) {
               return *std::get_if<parse_failure>(&result);
            }
            return parse_success{to_ret, rest};
         }
      }
      return parse_success{to_ret, rest};
   }

private:
   parser_erasure<ProducedType> parser_;
   int min_;
   int max_;
};

// clang-format off
template<typename T> repeat(T)           -> repeat<base_result_type<T>>;
template<typename T> repeat(T, int)      -> repeat<base_result_type<T>>;
template<typename T> repeat(T, int, int) -> repeat<base_result_type<T>>;
// clang-format on

template<typename... ProducedTypes>
struct or_ {
public:
   template<is_parser_r<ProducedTypes>... Ts>
   explicit or_(Ts&&... parsers) noexcept
      : parsers_{std::forward<Ts>(parsers)...}
   {}

   parse_result<std::variant<ProducedTypes...>> parse(std::string_view v) const noexcept
   {
      const auto partial = [&](const auto&... parsers) noexcept {
         return or_parse_impl(v, parsers...);
      };
      return std::apply(partial, parsers_);
   }

private:
   using base_type = std::variant<ProducedTypes...>;

   template<typename T, typename... Rest>
   static parse_result<base_type> or_parse_impl(std::string_view v, const T& first, const Rest&... rest) noexcept
   {
      using parser_result_type = base_result_type<T>;
      const auto result = first.parse(v);
      if (const auto* data = std::get_if<parse_success<parser_result_type>>(&result)) {
         constexpr std::size_t loc = sizeof...(ProducedTypes) - sizeof...(Rest) - 1;
         return parse_success<base_type>{base_type{std::in_place_index<loc>, data->value}, data->rest};
      }
      else if constexpr (sizeof...(Rest) > 1) {
         return or_parse_impl(v, rest...);
      }
      else {
         return *std::get_if<parse_failure>(&result);
      }
   }

   std::tuple<parser_erasure<ProducedTypes>...> parsers_;
};

template<typename... Ts>
or_(Ts...) -> or_<base_result_type<Ts>...>;

int main()
{
   const char_ tester{"a-zA-Z"};
   assert(is_success(tester.parse("A")));
   assert(!is_success(tester.parse("0")));
   const auto name = repeat{tester};
   assert(is_success(name.parse("AAAaaabbioguweSDJIPDFNGi")));
   assert(!is_success(name.parse("2AAAaaabbioguweSDJIPDFNGi")));
   const auto dumb = or_{
      tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester,
      tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester,
      tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester,
      tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester, tester};
   dumb.parse("a");
}
