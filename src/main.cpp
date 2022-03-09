#include <compare>
#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <iostream>
#include <cassert>
#include <iterator>
#include <string_view>
#include <type_traits>

template<std::size_t N>
struct comp_str { 
    consteval comp_str(const char (&other)[N])
        : data{}
    {
        for (std::size_t i = 0; i < N; ++i) {
            data[i] = other[i];
        }
    }

    char data[N];
    constexpr auto operator<=>(const comp_str&) const = default;
};

template<comp_str str>
consteval auto test()
{
    const auto& data = str.data;
    std::array<std::pair<std::string_view, std::string_view>, std::ranges::count(data, '=') + 1> rules;
    std::size_t loc = 0;

    constexpr auto ischar = [](int c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '-'; };
    constexpr auto not_ws = [](int c) { return c != ' ' && c != '\t' && c != '\n'; };

    auto start_loc = std::begin(data);
    while (true) {
        const auto equal_sign_loc = std::find(start_loc, std::end(data), '=');
        if (equal_sign_loc == std::end(data)) { break; }
        auto end_rule_loc = equal_sign_loc;
        while (true) {
            if (*end_rule_loc == ';') {
                break;
            }
            else if (*end_rule_loc == '\'') {
                do { ++end_rule_loc; } while (*end_rule_loc != '\'');
            }
            else if (*end_rule_loc == '"') {
                do { ++end_rule_loc; } while (*end_rule_loc != '"');
            }
            ++end_rule_loc;
        }

        const auto mri = [](auto val) { return std::make_reverse_iterator(val); };

        const auto print_start = std::find_if(start_loc, equal_sign_loc, ischar);
        const auto print_end = std::find_if(mri(start_loc), mri(equal_sign_loc), ischar) + 1;

        const auto rule_start = std::find_if(equal_sign_loc + 1, end_rule_loc, not_ws);
        const auto rule_end = std::find_if(mri(equal_sign_loc + 1), mri(end_rule_loc), not_ws) + 1;
        rules[loc] = {rule_start, rule_end.base()};
        ++loc;
        start_loc = end_rule_loc + 1;
    }
    
    return rules;
}

struct rule_str {
    std::string_view name;
    std::string_view def;
    int num_def_entities;
};

template<typename T, typename...>
struct head { using type = T; };

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

consteval auto find_rule(std::string_view v) -> std::pair<rule_str, std::string_view>
{
    constexpr auto is_alpha = [](const char c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); };
    constexpr auto is_other = [](const char c) { return (c >= '0' && c <= '9') || c == '_'; };
    constexpr auto is_alpha_or_other = [=](const char c) { return is_alpha(c) || is_other(c); };
    const auto start_rule = std::ranges::find_if_not(v, is_ws);
    if (start_rule == v.end()) {
        throw "Couldn't find start of rule name (unexpected end of string)";
    }
    else if (!is_alpha(*start_rule)) {
        throw "Invalid first character for rule name (must be within [a-zA-Z])";
    }
    const auto end_rule = std::find_if_not(start_rule, v.end(), is_alpha_or_other);
    if (end_rule == v.end()) {
        throw "Couldn't find end of rule name (unexpected end of string)";
    }
    const auto equal_sign_loc = std::find_if_not(end_rule, v.end(), is_ws);
    if (equal_sign_loc == v.end()) {
        throw "Couldn't find equal sign after rule name (unexpected end of string)";
    }
    else if (*equal_sign_loc != '=') {
        throw "Invalid character after rule name (must be equals sign)";
    }
    const auto start_def = equal_sign_loc + 1;
    auto start_next = start_def;
    int num_entities = 0;
    while (true) {
        const auto end_next = std::find_if_not(start_next, v.end(), is_ws);
        if (end_next == v.end()) {
            throw "Couldn't find end of rule (unexpected end of string; was a semi-colon forgotten?)";
        }
        else if (*end_next == ';') {
            return {{{start_rule, end_rule}, {start_def, end_next}, num_entities}, {end_next + 1, v.end()}};
        }
        else if (*end_next == one_of<'"', '\'', '`', '[', '{'>) {
            start_next = end_next;
            const auto [end_char, escape_allowed] = [&]() -> std::pair<char, bool> {
                switch (*end_next) {
                    case '[': return {']',       true};
                    case '{': return {'}',       false};
                    default:  return {*end_next, true};
                }
            }();
            do {
                start_next = std::find(start_next + 1, v.end(), end_char);
                if (start_next == v.end()) {
                    throw "Unexpected end of string when looking for terminator character";
                }
            } while (escape_allowed && *(start_next - 1) == '\\');
            ++start_next;
        }
        else if (is_alpha(*end_next)) {
            start_next = std::find_if_not(end_next, v.end(), is_alpha_or_other);
        }
        else if (*end_next == one_of<'-', '|'>) {
            ++start_next;
        }
        else {
            throw R"(Unexpected character in rule definition (must be within [[(a-zA-Z='"`|-]))";
        }
        ++num_entities;
    }
}

template<std::size_t NumRules>
consteval auto decompose_rules_impl(std::string_view v) -> std::conditional_t<NumRules == 0, int, std::array<rule_str, NumRules>>
{
    rule_str* rules = new rule_str[0];
    int num_rules = 0;
    const char* start_next = v.begin();
    while (start_next != v.end()) {
        // Allocate more room
        ++num_rules;
        rule_str* rules_tmp = new rule_str[num_rules];
        for (int i = 0; i < num_rules - 1; ++i) {
            rules_tmp[i] = rules[i];
        }
        delete[] rules;
        rules = rules_tmp;
        // Acutally parse now
        const auto [rule, rest] = find_rule({start_next, v.end()});
        rules[num_rules - 1] = rule;
        start_next = std::find_if_not(rest.data(), v.end(), is_ws);
    }
    if constexpr (NumRules == 0) {
        delete[] rules;
        return num_rules;
    }
    else {
        std::array<rule_str, NumRules> to_ret;
        std::copy(rules, rules + NumRules, to_ret.data());
        delete[] rules;
        return to_ret;
    }
}

template<comp_str str>
constexpr auto decompose_rules = decompose_rules_impl<decompose_rules_impl<0>(str.data)>(str.data);

int main()
{
    constexpr auto rule = decompose_rules<R"(
        hi='1\'2' [123] [\][(a-zA-Z='"`]{5-9};
        borzoi = "borz"|"borzoi";
        test= hi borzoi;
    )">;
    static_assert(rule[0].name == "hi");
    static_assert(rule[0].num_def_entities == 4);
    static_assert(rule[1].name == "borzoi");
    static_assert(rule[1].num_def_entities == 3);
    static_assert(rule[2].name == "test");
    static_assert(rule[2].num_def_entities == 2);
    // constexpr auto rules = test<R"ebnf(
    // letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
    //         | "H" | "I" | "J" | "K" | "L" | "M" | "N"
    //         | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
    //         | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
    //         | "c" | "d" | "e" | "f" | "g" | "h" | "i"
    //         | "j" | "k" | "l" | "m" | "n" | "o" | "p"
    //         | "q" | "r" | "s" | "t" | "u" | "v" | "w"
    //         | "x" | "y" | "z" ;
    // digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
    // symbol = "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">"
    //     | "'" | '"' | "=" | "|" | "." | "," | ";" ;
    // character = letter | digit | symbol | "_" ;
    
    // identifier = letter , { letter | digit | "_" } ;
    // terminal = "'" , character , { character } , "'" 
    //         | '"' , character , { character } , '"' ;
    
    // lhs = identifier ;
    // rhs = identifier
    //     | terminal
    //     | "[" , rhs , "]"
    //     | "{" , rhs , "}"
    //     | "(" , rhs , ")"
    //     | rhs , "|" , rhs
    //     | rhs , "," , rhs ;

    // rule = lhs , "=" , rhs , ";" ;
    // grammar = { rule } ;
    // )ebnf">();
    // for (const auto& [key, val] : rules) {
    //     std::cout << key << " = " << val << '\n';
    // }
}
