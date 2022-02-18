#include <cassert>
#include <cstdint>
#include <string_view>
#include <vector>
#include <variant>
#include <concepts>
#include <bitset>
#include <iostream>
#include <cstring>
#include <array>
#include <functional>

struct nil_t {};

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
auto get_base_type() -> decltype(get_base_type<T...>(get_value_indexes<0, T...>(std::index_sequence_for<T...>{})));

// clang-format off
template<typename T, typename... Set>
concept one_of = (std::same_as<T, Set> || ...);

template<typename First, typename... Rest>
using head_type = First;

template<typename T>
concept wrappable_parser = requires(const T& parser, std::string_view str) {
   requires std::copy_constructible<T>;
   requires one_of<typename T::result_type, nil_t, char, std::int64_t, std::string_view>;
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

// Don't allow unbound variant/sequence in variant/sequence because that makes things complicated
struct parser
{
public:
   template<wrappable_parser T>
   parser(T&& base_parser) noexcept
      : storage_{make_storage(static_cast<T&&>(base_parser))}
      , parse_impl_{make_parser_ptr<T>()}
      , deleter_{[](void* raw, bool dynamic) {
         T& value = *std::launder(static_cast<T*>(raw));
         value.~T();
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
         void* to_ret;
         new (&to_ret) T{static_cast<T&&>(base_parser)};
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
            return std::launder(reinterpret_cast<const T*>(&raw))->parse(str);
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

template<wrappable_parser... T>
struct sequence
{
private:
   using prod_type = decltype(get_base_type<T...>());

public:
   sequence(T...) noexcept
   {

   }

   // clang-format off
   template<typename Callable>
      requires requires (Callable c) { { std::apply(c, prod_type{}) }; }
   // clang-format on
   parser bind(Callable callable) && noexcept
   {
      ;
   }
};

struct char_ {
   using result_type = char;

   char_() noexcept
   {
      // Set all to true
      allowed.flip();
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

public:
   std::bitset<256> allowed;
};



int main()
{
   parser p{char_{}};
   const auto val = p.parse("hi");
   std::cout << std::get<char>(std::get<parse_success>(val).value) << '\n';
}
