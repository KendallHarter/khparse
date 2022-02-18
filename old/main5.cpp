#include <bitset>
#include <cassert>
#include <charconv>
#include <cstring>
#include <string>
#include <variant>
#include <vector>
#include <memory>
#include <functional>
#include <iostream>
#include <limits>

namespace khparse {

struct with_skipper_t{};
inline constexpr auto with_skipper = with_skipper_t{};

struct parse_value;
using parse_value_vec = std::unique_ptr<std::vector<parse_value>>;
struct nil_t{};
struct parse_failure_t{};
inline constexpr auto nil = nil_t{};
inline constexpr auto parse_failure = parse_failure_t{};
struct parse_value : std::variant<
   parse_failure_t,
   nil_t,
   std::int64_t,
   char,
   std::string_view,
   parse_value_vec
> {};

struct parse_result {
   parse_value value;
   const char* rest;
   int index;
};

} // namespace khparse

namespace khparse::impl {

// clang-format off
template<typename T>
concept is_parser = requires (const T& parser) {
   { parser.parse(std::string_view{}) } -> std::same_as<parse_result>;
};

template<typename T>
concept is_typed_parser = is_parser<T> && requires {
   typename T::base_type;
};

template<typename T>
concept is_parser_or_str = is_parser<T> || std::convertible_to<T, const char*>;
// clang-format on

struct parser {
public:
   template<is_parser Parser>
      requires (!std::same_as<std::remove_cvref_t<Parser>, parser>)
   explicit parser(Parser&& parser) noexcept
      : storage_{[p=std::forward<Parser>(parser)](std::string_view v) noexcept {
         return p.parse(v);
      }}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      return storage_(v);
   }

private:
   std::function<parse_result(std::string_view)> storage_;
};

// struct parser {
// private:
//    using func_ptr = parse_result (*)(const parser&, std::string_view) noexcept;
//    using deleter_ptr = void (*)(void*) noexcept;
//    inline static constexpr auto storage_size = sizeof(void*) * 2;
//    static_assert(sizeof(void*) == 8);

// public:
//    parser(const parser&) = delete;
//    parser& operator=(const parser&) = delete;

//    parser(parser&& rhs) noexcept
//    {
//       std::memcpy(this, &rhs, sizeof(parser));
//       std::fill_n(&rhs, sizeof(rhs), 0);
//    }
//    parser& operator=(parser&& rhs) noexcept
//    {
//       std::memcpy(this, &rhs, sizeof(parser));
//       std::fill_n(&rhs, sizeof(rhs), 0);
//       return *this;
//    }

//    template<is_parser Parser>
//    // clang-format off
//       requires (sizeof(Parser) <= storage_size
//              && std::is_trivially_destructible_v<Parser>
//              && !std::same_as<std::remove_cvref_t<Parser>, parser>)
//    // clang-format on
//    parser(Parser&& p) noexcept
//       : invoker_{mark_static([](const parser& p, std::string_view v) noexcept {
//          return std::launder(reinterpret_cast<const std::remove_cvref_t<Parser>*>(&p.storage_))->parse(v);
//       })}
//    {
//       new (&storage_) std::remove_cvref_t<Parser>{std::forward<Parser>(p)};
//    }

//    template<is_parser Parser>
//    // clang-format off
//       requires (sizeof(Parser) > storage_size && !std::same_as<std::remove_cvref_t<Parser>, parser>)
//    // clang-format on
//    parser(Parser&& p) noexcept
//       : ptrs_{
//          new std::remove_cvref_t<Parser>{std::forward<Parser>(p)},
//          [](void* ptr) noexcept {
//             delete static_cast<std::remove_cvref_t<Parser>*>(ptr);
//          }
//       }
//       , invoker_{mark_dynamic([](const parser& p, std::string_view v) noexcept {
//          const auto parser_ptr = static_cast<const std::remove_cvref_t<Parser>*>(p.ptrs_.alloc_);
//          return parser_ptr->parse(v);
//       })}
//    {
//    }

//    ~parser()
//    {
//       if (is_dynamic()) {
//          ptrs_.deleter_(ptrs_.alloc_);
//       }
//    }

//    parse_result parse(std::string_view v) const noexcept
//    {
//       return get_ptr()(*this, v);
//    }

// private:
//    static std::uintptr_t mark_static(func_ptr ptr) noexcept
//    {
//       assert(!(reinterpret_cast<std::uintptr_t>(ptr) & 0x8000'0000'0000'0000));
//       return reinterpret_cast<std::uintptr_t>(ptr);
//    }

//    static std::uintptr_t mark_dynamic(func_ptr ptr) noexcept
//    {
//       assert(!(reinterpret_cast<std::uintptr_t>(ptr) & 0x8000'0000'0000'0000));
//       return reinterpret_cast<std::uintptr_t>(ptr) | 0x8000'0000'0000'0000;
//    }

//    func_ptr get_ptr() const noexcept
//    {
//       return reinterpret_cast<func_ptr>(invoker_ & 0x7FFF'FFFF'FFFF'FFFF);
//    }

//    bool is_dynamic() const noexcept
//    {
//       return invoker_ & 0x8000'0000'0000'0000;
//    }

//    struct ptrs_struct {
//       void* alloc_;
//       deleter_ptr deleter_;
//    };

//    union {
//       alignas(sizeof(void*) * 2) char storage_[sizeof(void*) * 2];
//       ptrs_struct ptrs_;
//    };
//    std::uintptr_t invoker_;

//    static_assert(sizeof(storage_) == storage_size);
// };

struct literal {
public:
   using base_type = nil_t;

   constexpr literal(const char* match) noexcept
      : match_{match}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      if (v.starts_with(match_)) {
         return {nil, v.data() + match_.size(), 0};
      }
      return {parse_failure, match_.data(), 0};
   }

private:
   std::string_view match_;
};

struct char_ {
   using base_type = char;

   char_() noexcept
   {
      // Set all to true
      allowed.flip();
   }

   char_(std::string_view init_string) noexcept
   {
      handle_init(init_string);
   }

   parse_result parse(std::string_view v) const noexcept
   {
      if (v.empty()) {
         return {parse_failure, "Empty string", 0};
      }
      else if (!allowed[v.front()]) {
         return {parse_failure, "Character did not match", 0};
      }
      return {v.front(), v.substr(1).data(), 0};
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

struct string {
public:
   using base_type = std::string_view;

   string(char_ base_parser = {}, int min = 1, int max = std::numeric_limits<int>::max()) noexcept
      : allowed{base_parser.allowed}
      , min_{min}
      , max_{max}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      const char* end_loc = v.data();
      const char* end_ptr = v.data() + v.size();
      int i = 0;
      while(end_loc < end_ptr && allowed[*end_loc] && i < max_) {
         ++end_loc;
         ++i;
      }
      if (i >= min_) {
         return {std::string_view{v.data(), end_loc}, end_loc, 0};
      }
      else {
         return {parse_failure, v.data(), 0};
      }
   }

private:
   std::bitset<256> allowed;
   int min_;
   int max_;
};

struct number {
   using base_type = std::int64_t;

   constexpr number(int base) noexcept
      : base_{base}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      std::int64_t value;
      const auto [ptr, ec] = std::from_chars(v.begin(), v.data() + v.size(), value, base_);
      if (ec != std::errc{}) {
         if (ec == std::errc::invalid_argument) {
            return {parse_failure, "Could not parse number", 0};
         }
         else if (ec == std::errc::result_out_of_range) {
            return {parse_failure, "Number too large", 0};
         }
         else {
            return {parse_failure, "Unknown error", 0};
         }
      }
      return {value, ptr, 0};
   }

private:
   int base_;
};

parser wrap_types(const char* str) { return parser{literal{str}}; }
template<typename T> parser wrap_types(T&& val) { return parser{std::forward<T>(val)}; }

struct fail_always_t {
   parse_result parse(std::string_view) const noexcept
   {
      return {parse_failure, "always fails", 0};
   }
};

constexpr auto fail_always = fail_always_t{};

const char* apply_skipper(const parser& skipper, const char* start_loc, const char* end_loc) noexcept
{
   const char* rest_ptr = start_loc;
   while (true) {
      auto [value, rest, dummy] = skipper.parse({rest_ptr, end_loc});
      (void)dummy;
      if (std::holds_alternative<parse_failure_t>(value)) {
         return rest_ptr;
      }
      rest_ptr = rest;
   }
}

struct repeat {
public:
   template<is_parser_or_str Skipper, is_parser Parser>
   repeat(with_skipper_t, Skipper&& skipper, Parser&& p, int min = 1, int max = std::numeric_limits<int>::max()) noexcept
      : skipper_{wrap_types(std::forward<Skipper>(skipper))}
      , parser_{wrap_types(std::forward<Parser>(p))}
      , min_{min}
      , max_{max}
   {
      assert(min >= 0);
      assert(min <= max);
   }

   template<is_parser Parser>
   repeat(Parser&& p, int min = 1, int max = std::numeric_limits<int>::max()) noexcept
      : repeat{with_skipper, fail_always, std::forward<Parser>(p), min, max}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      const auto end_ptr = v.data() + v.size();
      const char* rest_ptr = apply_skipper(skipper_, v.data(), end_ptr);
      auto to_ret = std::make_unique<std::vector<parse_value>>();
      for (unsigned i = 0; i < static_cast<unsigned>(max_); ++i) {
         auto [value, rest, dummy] = parser_.parse({rest_ptr, end_ptr});
         (void)dummy;
         if (std::holds_alternative<parse_failure_t>(value)) {
            break;
         }
         rest_ptr = apply_skipper(skipper_, rest, end_ptr);
         to_ret->push_back(std::move(value));
      }
      if (to_ret->size() >= static_cast<unsigned>(min_)) {
         return {std::move(to_ret), rest_ptr, 0};
      }
      return {parse_failure, "", 0};
   }

private:
   parser skipper_;
   parser parser_;
   int min_;
   int max_;
};

struct seq {
public:
   template<is_parser_or_str Skipper, is_parser_or_str... Parsers>
   seq(with_skipper_t, Skipper&& skipper, Parsers&&... parsers) noexcept
      : skipper_{wrap_types(std::forward<Skipper>(skipper))}
      , parsers_{wrap_types(std::forward<Parsers>(parsers))...}
   {}

   template<is_parser_or_str... Parsers>
      requires (sizeof...(Parsers) > 1)
   seq(Parsers&&... parsers) noexcept
      : skipper_{parser{fail_always}}
      , parsers_{wrap_types(std::forward<Parsers>(parsers))...}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      auto to_ret = std::make_unique<std::vector<parse_value>>();
      const auto end_ptr = v.data() + v.size();
      const char* rest_ptr = apply_skipper(skipper_, v.data(), end_ptr);
      for (const auto& p : parsers_) {
         auto [value, rest, dummy] = p.parse({rest_ptr, end_ptr});
         (void)dummy;
         if (std::holds_alternative<parse_failure_t>(value)) {
            return {parse_failure, rest, 0};
         }
         to_ret->push_back(std::move(value));
         rest_ptr = apply_skipper(skipper_, rest, end_ptr);
      }
      return {std::move(to_ret), rest_ptr, 0};
   }

private:
   parser skipper_;
   std::vector<parser> parsers_;
};

struct or_ {
public:
   template<is_parser_or_str... Parsers>
      requires (sizeof...(Parsers) > 1)
   or_(Parsers&&... parsers) noexcept
      : parsers_{wrap_types(std::forward<Parsers>(parsers))...}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      int i = 0;
      for (const auto& p : parsers_) {
         auto [value, rest, dummy] = p.parse(v);
         (void)dummy;
         if (!std::holds_alternative<parse_failure_t>(value)) {
            return {std::move(value), rest, i};
         }
         ++i;
      }
      return {parse_failure, "", 0};
   }

private:
   std::vector<parser> parsers_;
};

} // khparse::impl

namespace khparse {

namespace detail {

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

template<typename... T, std::size_t... I>
auto get_func_type(std::index_sequence<I...>) -> std::function<void(std::tuple_element_t<I, std::tuple<T...>>...)>;

// This is all kind of a mess, but it's to get common types to fold together
template<typename T>
struct extract_tuple {
   using type = T;
};

template<typename T>
struct extract_tuple<std::tuple<T>> {
   using type = T;
};

template<typename T, typename...>
struct first_type_impl {
   using type = T;
};

template<typename... T>
using first_type = typename extract_tuple<typename first_type_impl<T...>::type>::type;

template<typename T, typename... Rest>
inline constexpr bool all_types_same = (std::same_as<T, Rest> && ...);

template<typename T>
concept decays_to_text = requires(const T& x) {
   static_cast<const char*>(x);
};

template<typename T>
using str_wrapper = std::conditional_t<decays_to_text<T>, ::khparse::impl::literal, T>;

constexpr auto do_nothing = [](std::string_view, parse_failure_t err) noexcept {
   return err;
};

} // namespace khparse::detail

using literal = impl::literal;
using char_ = impl::char_;
using number = impl::number;
using string = impl::string;
static inline constexpr auto hex_number = impl::number{16};
static inline constexpr auto dec_number = impl::number{10};
static inline constexpr auto bin_number = impl::number{2};
using err_handler_t = std::function<parse_failure_t(std::string_view, parse_failure_t)>;

// TODO: Need to move stuff like folding out nil_t for seq into the base parsers
//       since they won't otherwise work when bound
template<typename TypeTuple, typename GenType, std::size_t... ValueIndexes>
struct bound_parser {
private:
   using func_type = std::function<GenType(std::tuple_element_t<ValueIndexes, TypeTuple>...)>;

public:
   using base_type = std::conditional_t<std::same_as<GenType, void>, nil_t, GenType>;

   bound_parser(const impl::parser& parser, const func_type& func, const err_handler_t& on_err) noexcept
      : parser_{parser}
      , func_{func}
      , on_err_{on_err}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      const auto [value, rest, index] = parser_.parse(v);
      (void)index;
      if (const auto value_ptr = std::get_if<parse_value_vec>(&value)) {
         const auto& values = **value_ptr;
         // TODO: these are not correct & will crash
         [](auto x, auto...) { std::cout << x << '\n'; }(values[ValueIndexes].index()...);
         std::cout << values[1].index() << '\n';
         std::cout << "sz " << sizeof...(ValueIndexes) << '\n';
         std::cout << std::get<5>(values[1])->size() << '\n';
         if constexpr (!std::same_as<GenType, void>) {
            return {func_(std::get<std::tuple_element_t<ValueIndexes, TypeTuple>>(values[ValueIndexes])...), rest, 0};
         }
         else {
            func_(std::get<std::tuple_element_t<ValueIndexes, TypeTuple>>(values[ValueIndexes])...);
            return {nil, rest, 0};
         }
      }
      else {
         const auto err = std::get_if<parse_failure_t>(&value);
         assert(err);
         return {on_err_({rest, v.data() + v.size()}, *err), rest, 0};
      }
   }

private:
   impl::parser parser_;
   func_type func_;
   err_handler_t on_err_;
};

template<typename GenType, typename... T>
struct bound_parser<GenType, std::variant<T...>>
{
private:
   using var_type = std::variant<typename T::base_type...>;
   using func_type = std::function<GenType(const var_type&)>;

public:
   using base_type = std::conditional_t<std::same_as<GenType, void>, nil_t, GenType>;

   bound_parser(const impl::parser& parser, const func_type& func, const err_handler_t& on_err) noexcept
      : parser_{parser}
      , func_{func}
      , on_err_{on_err}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      const auto [value, rest, index] = parser_.parse(v);
      if (const auto err_ptr = std::get_if<parse_failure_t>(&value)) {
         return {on_err_({rest, v.data() + v.size()}, *err_ptr), rest, 0};
      }
      else {
         if constexpr (!std::same_as<GenType, void>) {
            return {func_(make_variant(value, index, std::index_sequence_for<T...>{})), rest, 0};
         }
         else {
            func_(make_variant(value, index, std::index_sequence_for<T...>{}));
            return {nil, rest, 0};
         }
      }
   }

private:
   template<std::size_t I>
   static auto get_value(const parse_value& value) noexcept
   {
      return std::get<std::variant_alternative_t<I, var_type>>(value);
   }

   template<std::size_t I, std::size_t... Rest>
   static var_type make_variant_impl(const parse_value& value, std::size_t index) noexcept
   {
      if (I == index) {
         return var_type{std::in_place_index<I>, get_value<I>(value)};
      }
      else {
         if constexpr (sizeof...(Rest) > 0) {
            return make_variant_impl<Rest...>(value, index);
         }
         else {
            // This is unreachable
            assert(false);
            std::exit(10);
         }
      }
   }

   template<std::size_t... I>
   static var_type make_variant(const parse_value& value, std::size_t index, std::index_sequence<I...>) noexcept {
      return make_variant_impl<I...>(value, index);
   };

   impl::parser parser_;
   func_type func_;
   err_handler_t on_err_;
};

// vector version
template<typename BaseType, typename GenType>
struct bound_parser<BaseType, GenType>
{
private:
   using func_type = std::conditional_t<std::same_as<BaseType, nil_t>, std::function<GenType()>, std::function<GenType(const BaseType&)>>;

public:
   using base_type = BaseType;

   bound_parser(const impl::parser& parser, const func_type& func, const err_handler_t& on_err) noexcept
      : parser_{parser}
      , func_{func}
      , on_err_{on_err}
   {}

   parse_result parse(std::string_view v) const noexcept
   {
      const auto [value, rest, index] = parser_.parse(v);
      (void)index;
      if (const auto err_ptr = std::get_if<parse_failure_t>(&value)) {
         return {on_err_({rest, v.data() + v.size()}, *err_ptr), rest, 0};
      }
      else {
         if constexpr (std::same_as<nil_t, base_type>) {
            if constexpr (std::same_as<GenType, void>) {
               func_();
               return {nil, rest, 0};
            }
            else {
               return {func_(), rest, 0};
            }
         }
         else {
            assert(std::holds_alternative<parse_value_vec>(value));
            base_type to_ret;
            for (const auto& val = ***std::get_if<parse_value_vec*>(&value); const auto& x : val) {
               assert(std::holds_alternative<BaseType>(x));
               to_ret.push_back(std::get<BaseType>(x));
            }
            if constexpr (std::same_as<GenType, void>) {
               func_(to_ret);
               return {nil, rest, 0};
            }
            else {
               return {func_(to_ret), rest, 0};
            }
         }
      }
   }

private:
   impl::parser parser_;
   func_type func_;
   err_handler_t on_err_;
};

template<typename ...> struct TD;

template<impl::is_typed_parser... T>
struct seq : impl::seq {
public:
   using base = impl::seq;
   using base::base;
   using base::parse;

   static inline constexpr auto value_indexes = detail::get_value_indexes<0, typename T::base_type...>(std::index_sequence<>{});
   using base_type = decltype(detail::get_base_type<typename T::base_type...>(value_indexes));
   using func_type = decltype(detail::get_func_type<typename T::base_type...>(value_indexes));

   auto bind(const auto& f, const err_handler_t& err) && noexcept
   {
      return bind_impl(std::move(*this), std::function{f}, err, value_indexes);
   }

   auto bind(const auto& f) && noexcept
   {
      return std::move(*this).bind(f, detail::do_nothing);
   }

private:
   template<typename RetType, typename... Args, std::size_t... I>
   static bound_parser<std::tuple<typename T::base_type...>, RetType, I...> bind_impl(impl::seq self, const std::function<RetType(Args...)>& f, const err_handler_t& on_err, std::index_sequence<I...>) noexcept
   {
      return {impl::parser{static_cast<impl::seq&&>(self)}, f, on_err};
   }
};

template<typename... T>
   requires (!std::same_as<T, with_skipper_t> && ...)
seq(const T&...) -> seq<detail::str_wrapper<T>...>;

template<typename Skipper, typename... T>
seq(const with_skipper_t&, const Skipper&, const T&...) -> seq<detail::str_wrapper<T>...>;

template<impl::is_typed_parser... T>
struct or_ : impl::or_ {
private:
   using var_type = std::variant<typename T::base_type...>;

public:
   using base = impl::or_;
   using base::base;
   using base::parse;

   using base_type = std::conditional_t<
      !std::same_as<detail::first_type<T...>, nil_t> && detail::all_types_same<detail::first_type<typename T::base_type>...>, 
      detail::first_type<typename T::base_type...>, 
      std::variant<detail::first_type<typename T::base_type...>>
   >;

   parse_result parse(std::string_view v) const noexcept
   {
      
   }

   auto bind(const auto& f, const err_handler_t& err) && noexcept
   {
      using res_type = decltype(f(std::declval<std::variant<T...>>()));
      return bind_impl(std::move(*this), std::function<res_type(const var_type&)>{f}, err);
   }

   auto bind(const auto& f) && noexcept
   {
      return std::move(*this).bind(f, detail::do_nothing);
   }

private:
   template<typename RetType>
   static bound_parser<RetType, std::variant<T...>> bind_impl(impl::or_ self, const std::function<RetType(const var_type&)>& f, const err_handler_t& on_err) noexcept
   {
      return {impl::parser{static_cast<impl::or_&&>(self)}, f, on_err};
   }
};

template<typename... T>
or_(const T&...) -> or_<detail::str_wrapper<T>...>;

template<impl::is_typed_parser T>
struct repeat : impl::repeat {
public:
   using base = impl::repeat;
   using base::parse;

   template<typename Skipper>
   repeat(with_skipper_t, Skipper&& skipper, T&& p, int min = 1, int max = std::numeric_limits<int>::max()) noexcept
      : base(with_skipper, std::forward<Skipper>(skipper), std::forward<T>(p), min, max)
   {}

   repeat(const T& p, int min = 1, int max = std::numeric_limits<int>::max()) noexcept
      : base{p, min, max}
   {}

   repeat(T&& p, int min = 1, int max = std::numeric_limits<int>::max()) noexcept
      : base{std::move(p), min, max}
   {}

   using base_type = std::conditional_t<std::same_as<typename T::base_type, nil_t>, nil_t, std::vector<typename T::base_type>>;

private:
   auto bind(const auto& f, const err_handler_t& err) && noexcept
   {
      if constexpr (std::same_as<base_type, nil_t>) {
         using res_type = decltype(f());
         return bound_parser<base_type, res_type>{impl::parser{static_cast<impl::repeat&&>(std::move(*this))}, f, err};
      }
      else {
         using res_type = decltype(f(std::declval<base_type>()));
         return bound_parser<base_type, res_type>{impl::parser{static_cast<impl::repeat&&>(std::move(*this))}, f, err};
      }
   }

   auto bind(const auto& f) && noexcept
   {
      return std::move(*this).bind(f, detail::do_nothing);
   }
};

template<typename First, typename... Rest>
repeat(First, Rest...) -> repeat<detail::str_wrapper<First>>;

template<typename Skipper, typename First, typename... Rest>
repeat(const with_skipper_t&, Skipper, First, Rest...) -> repeat<detail::str_wrapper<First>>;

} // khparse

int main()
{
   using namespace khparse;
   const auto prefixed_number = or_{
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

   static_assert(std::same_as<decltype(prefixed_number)::base_type, std::int64_t>);

   const auto letter = char_{"a-zA-Z"};
   const auto name   = string{letter};
   const auto value  = or_{prefixed_number, name};
   // directive parsers
   const auto org         = seq{with_skipper, " ", ".org", prefixed_number}.bind([](std::int64_t val) {
      std::cout << "org " << val << '\n';
   });
   const auto label       = seq{name, ":"}.bind([](std::string_view v) { 
      std::cout << "hello " << v << '\n'; 
   });
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
   const auto special_opcodes = or_ {
      "nop",
      "brk",
      "rts",
      "rti",
      "txs",
      "tsx",
      "pha",
      "pla",
      "php",
      "plp"
   }.bind([](const auto& var) -> void {
      std::cout << "special " << var.index() << '\n';
   });
   const auto x = repeat(" ");
   // putting everything together
   const auto parser = repeat{with_skipper, char_{" \n"},
      or_{
         org,
         label,
         special_opcodes,
         // general case opcodes
         seq{with_skipper, char_{" \n"},
            opcode_name, addr_mode
         }
      }
   };
   const char* dumb_prog = R"(
   .org 0xC000

   loop:
   hoop:
   goop:
   
      inc $D020
      jmp loop
      
      nop
      brk
      rts
      rti
      txs
      tsx
      pha
      pla
      php
      plp
   )";
   const auto result = parser.parse(dumb_prog).value;
   assert(!std::holds_alternative<khparse::parse_failure_t>(result));
   assert(std::holds_alternative<std::unique_ptr<std::vector<khparse::parse_value>>>(result));

   std::cout << std::get<std::unique_ptr<std::vector<khparse::parse_value>>>(result)->size() << '\n';
}