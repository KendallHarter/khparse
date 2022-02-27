#include <cassert>
#include <cstdint>
#include <string_view>
#include <type_traits>
#include <vector>
#include <variant>
#include <concepts>
#include <bitset>
#include <iostream>
#include <cstring>
#include <array>
#include <functional>
#include <optional>
#include <charconv>

template<typename...> struct TD;

struct nil_t {};
inline constexpr nil_t nil{};

using parse_value = std::variant<nil_t, char, std::int64_t, std::string_view, std::vector<std::int64_t>, std::vector<std::string_view>>;

struct parse_success {
   parse_value value;
   const char* rest;
};

struct parse_failure {
   const char* where;
   const char* err_str;
};

using parse_result = std::variant<parse_success, parse_failure>;

template<std::size_t loc, typename T, typename... Rest, std::size_t... I>
constexpr auto get_value_indexes(std::index_sequence<I...> locs)
{
   constexpr auto next_val = [&]() {
      if constexpr (std::same_as<T, nil_t>) {
         return locs;
      }
      else {
         return std::index_sequence<I..., loc>{};
      }
   }();
   if constexpr (sizeof...(Rest) == 0) {
      return next_val;
   }
   else {
      return get_value_indexes<loc + 1, Rest...>(next_val);
   }
}

template<typename... T, std::size_t... I>
auto get_base_type(std::index_sequence<I...>) -> std::tuple<std::tuple_element_t<I, std::tuple<T...>>...>;

template<typename... T>
auto get_base_type() -> decltype(get_base_type<typename T::result_type...>(get_value_indexes<0, T...>({})));

// clang-format off
template<typename T, typename... Set>
concept one_of = (std::same_as<T, Set> || ...);

template<typename First, typename... Rest>
using head_type = First;

template<typename T>
concept base_parser = requires(const T& parser, std::string_view str) {
   { parser.parse(str) } -> std::same_as<parse_result>;
};

template<typename T>
concept wrapable_parser = requires(const T& parser, std::string_view str) {
   requires std::copy_constructible<T>;
   requires one_of<typename std::remove_cvref_t<T>::result_type, nil_t, char, std::int64_t, std::string_view>;
   { parser.parse(str) } -> std::same_as<parse_result>;
};

template<typename...>
struct sequence_result;

template<typename... T>
   requires (std::same_as<nil_t, T> && ...)
struct sequence_result<T...>
{
   using type = nil_t;
};
   
// clang-format on

struct char_ {
   using result_type = char;

   char_() noexcept
   {
      // Set all to true
      allowed.flip();
   }

   char_(unsigned char c) noexcept
   {
      allowed[c] = true;
   }

   char_(std::string_view init_string) noexcept
   {
      handle_init(init_string);
   }

   parse_result parse(std::string_view str) const noexcept
   {
      if (str.empty()) {
         return parse_failure{"Empty string", str.data()};
      }
      else if (!allowed[str.front()]) {
         return parse_failure{"Character did not match", str.data()};
      }
      return parse_success{str.front(), str.substr(1).data()};
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

struct always_succeed_t
{
   using result_type = nil_t;

   parse_result parse(std::string_view str) const noexcept
   {
      return parse_success{nil, str.data()};
   }
};

struct always_fail_t
{
   using result_type = nil_t;

   parse_result parse(std::string_view str) const noexcept
   {
      return parse_failure{str.data(), "always_fail::parse"};
   }
};

inline constexpr always_succeed_t always_succeed{};
inline constexpr always_fail_t always_fail{};

// Don't allow unbound variant/sequence in variant/sequence because that makes things complicated
struct parser
{
public:
   template<wrapable_parser T>
   parser(T&& base_parser) noexcept
      : storage_{make_storage(static_cast<T&&>(base_parser))}
      , parse_impl_{make_parser_ptr<T>()}
      , deleter_{[](void* raw, bool dynamic) {
         using t_noref = std::remove_cvref_t<T>;
         t_noref& value = *std::launder(static_cast<t_noref*>(raw));
         value.~t_noref();
         if (dynamic) {
            delete &value;
         }
      }}
   {}

   parse_result parse(std::string_view s) const noexcept
   {
      const auto ptr = reinterpret_cast<parse_ptr>(parse_impl_ & (~is_dynamic_bit));
      return ptr(storage_, s);
   }

   ~parser()
   {
      if (is_dynamic()) {
         deleter_(storage_, true);
      }
      else {
         deleter_(&storage_, false);
      }
   }

private:
   inline static constexpr auto is_dynamic_bit = 1ull << (sizeof(void*) * 8 - 1);
   
   template<typename T>
   static void* make_storage(T&& base_parser) noexcept
   {
      if constexpr (sizeof(T) > sizeof(storage_)) {
         return new T{static_cast<T&&>(base_parser)};
      }
      else {
         using t_noref = std::remove_cvref_t<T>;
         void* to_ret;
         new (&to_ret) t_noref{static_cast<T&&>(base_parser)};
         return to_ret;
      }
   }

   template<typename T>
   std::uintptr_t make_parser_ptr() noexcept
   {
      constexpr bool is_dynamic = sizeof(T) > sizeof(storage_);
      constexpr auto func = [](const void* raw, std::string_view str) -> parse_result {
         if constexpr (is_dynamic) {
            return static_cast<const T*>(raw)->parse(str);
         }
         else {
            using t_noref = std::remove_cvref_t<T>;
            return std::launder(reinterpret_cast<const t_noref*>(&raw))->parse(str);
         }
      };
      auto ptr = reinterpret_cast<std::uintptr_t>(func.operator parse_ptr());
      assert((ptr & is_dynamic_bit) == 0);
      return is_dynamic ? ptr | is_dynamic_bit : ptr;
   }

   bool is_dynamic() const noexcept
   {
      return parse_impl_ & is_dynamic_bit;
   }

   using parse_ptr = parse_result (*)(const void*, std::string_view);
   using delete_ptr = void (*)(void*, bool);

   void* storage_;
   std::uintptr_t parse_impl_;
   delete_ptr deleter_;
};

// clang-format off
// TODO: Add ability to manipulate parse_result within the binding
template<typename Callable, typename... Parsers>
concept seq_bindable = requires(const Callable& c, const decltype(get_base_type<Parsers...>())& values) {
   { std::apply(c, values) } -> one_of<void, nil_t>;
};

template<typename Callable, typename... Parsers>
concept or_bindable = requires(const Callable& c, const std::variant<typename Parsers::result_type...>& value) {
   { c(value) } -> one_of<void, nil_t>;
};
// clang-format on

struct number {
   using result_type = std::int64_t;

   constexpr number(int base) noexcept
      : base_{base}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      std::int64_t value;
      const auto [ptr, ec] = std::from_chars(v.begin(), v.data() + v.size(), value, base_);
      if (ec != std::errc{}) {
         if (ec == std::errc::invalid_argument) {
            return parse_failure{"Could not parse number", v.data()};
         }
         else if (ec == std::errc::result_out_of_range) {
            return parse_failure{"Number too large", v.data()};
         }
         else {
            return parse_failure{"Unknown error", v.data()};
         }
      }
      return parse_success{value, ptr};
   }

private:
   int base_;
};

inline constexpr auto hex_number = number{16};
inline constexpr auto dec_number = number{10};
inline constexpr auto bin_number = number{2};

// This should probably be a function with the callable passed in since we're
// always going to want to bind it (it is rather useless otherwise)
constexpr auto seq = []<wrapable_parser... Parsers>(wrapable_parser auto&& skipper, seq_bindable<Parsers...> auto&& binder, Parsers&&... parsers) -> parser {
   struct seq_parser {
      std::remove_cvref_t<decltype(skipper)> skipper_;
      std::remove_cvref_t<decltype(binder)> binder_;
      std::tuple<Parsers...> parsers_;

      // the [[maybe_unused]] is to prevent false positive warnings
      using result_type [[maybe_unused]] = nil_t;

      parse_result parse(std::string_view str) const noexcept
      {
         using data_type = decltype(get_base_type<Parsers...>());

         parse_result to_ret;
         const char* last_result = str.data();
         data_type call_data;

         auto handle_parser = [&]<std::size_t I>() {
            if (!last_result) { return; }

            while (true) {
               const auto loc_raw = skipper_.parse({last_result, str.end()});
               if (const auto loc = std::get_if<parse_success>(&loc_raw)) {
                  last_result = loc->rest;
               }
               else {
                  break;
               }
            }

            const auto result = std::get<I>(parsers_).parse({last_result, str.end()});
            if (const auto res = std::get_if<parse_success>(&result)) {
               last_result = res->rest;
               std::get<I>(call_data) = *std::get_if<std::tuple_element_t<I, data_type>>(&res->value);
            }
            else {
               last_result = nullptr;
               to_ret = *std::get_if<parse_failure>(&result);
            }
         };

         [&]<std::size_t... I>(std::index_sequence<I...>) {
            (handle_parser.template operator()<I>(), ...);
         }(std::index_sequence_for<Parsers...>{});

         if (last_result) {
            std::apply(binder_, call_data);
         }

         return to_ret;
      }
   };
   return seq_parser{static_cast<decltype(skipper)&&>(skipper), static_cast<decltype(binder)&&>(binder), std::forward_as_tuple(static_cast<Parsers&&>(parsers)...)};
};

constexpr auto or_ = []<wrapable_parser... Parsers>(or_bindable<Parsers...> auto&& binder, Parsers&&... parsers) -> parser {
   struct or_parser {
      std::remove_cvref_t<decltype(binder)> binder_;
      std::tuple<Parsers...> parsers_;
      mutable std::string err_str;

      // the [[maybe_unused]] is to prevent false positive warnings
      using result_type [[maybe_unused]] = nil_t;

      parse_result parse(std::string_view str) const & noexcept
      {
         using data_type = std::variant<typename Parsers::result_type...>;
         std::optional<data_type> call_data;
         parse_result to_ret;
         std::string temp_err_str;

         auto handle_parser = [&]<std::size_t I>() {
            if (call_data) { return; }
            
            auto result = std::get<I>(parsers_).parse(str);
            if (const auto res = std::get_if<parse_success>(&result)) {
               call_data = data_type{std::in_place_index<I>, *std::get_if<std::variant_alternative_t<I, data_type>>(&res->value)};
               res->value = nil;
               to_ret = *res;
            }
            else {
               if (!temp_err_str.empty()) {
                  temp_err_str.push_back('\n');
               }
               temp_err_str += std::get_if<parse_failure>(&result)->err_str;
            }
         };

         [&]<std::size_t... I>(std::index_sequence<I...>) {
            (handle_parser.template operator()<I>(), ...);
         }(std::index_sequence_for<Parsers...>{});

         if (!call_data) {
            err_str = temp_err_str;
            to_ret = parse_failure{str.data(), err_str.c_str()};
         }
         else {
            binder_(*call_data);
         }

         return to_ret;
      }
   };
   
   return or_parser{static_cast<decltype(binder)&&>(binder), {static_cast<Parsers&&>(parsers)...}, ""};
};

inline constexpr auto make_final_parser = [](base_parser auto&& skipper, base_parser auto&&... parsers) -> base_parser auto {
   return seq([](auto...){}, static_cast<decltype(skipper)&&>(skipper), static_cast<decltype(parsers)&&>(parsers)...);
};

int main()
{
   parser p{char_{}};
   const auto val = p.parse("hi");
   std::cout << std::get<char>(std::get<parse_success>(val).value) << '\n';
   const auto heck = seq(char_{'b'}, [](char a, char b){ std::cout << a << ' ' << b << '\n'; }, char_{}, char_{});
   const auto heck2 = or_([](auto v) { std::cout << v.index() << '\n'; }, char_{'a'}, char_{'b'});
   heck.parse("abbbbbc");
   heck2.parse("a");
   heck2.parse("b");
   // const auto test = make_final_parser()
}
