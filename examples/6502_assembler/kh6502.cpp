// A more complex 6502 assembler

#include "khparse.hpp"

#include <algorithm>
#include <array>
#include <bitset>
#include <cstdint>
#include <format>
#include <iostream>
#include <optional>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <variant>

constexpr auto sorted_keywords(auto container) noexcept
{
   std::ranges::sort(container, {}, [](const auto& c) { return std::get<0>(c); });
   return container;
}

enum class addr_mode : std::uint8_t {
   implied,
   accumulator,
   immediate,
   zero_page,
   zero_page_x,
   zero_page_y,
   // relative and absolute are indistinguishable
   // relative,
   absolute,
   absolute_x,
   absolute_y,
   indirect,
   indexed_indirect,
   indirect_indexed,
};

template<typename CharT>
struct std::formatter<addr_mode, CharT> {
   constexpr auto parse(std::format_parse_context& ctx) const noexcept { return ctx.begin(); }

   template<typename FormatContext>
   constexpr auto format(addr_mode mode, FormatContext& ctx) const noexcept
   {
      constexpr const char* modes[]{
         "implied",
         "accumulator",
         "immediate",
         "zero page",
         "zero page, x",
         "zero page, y",
         "absolute",
         "absolute, x",
         "absolute, y",
         "indirect",
         "indexed indirect",
         "indirect indexed"};
      const auto index = static_cast<int>(mode);
      if (index < 0 || index > std::ssize(modes)) {
         return std::format_to(ctx.out(), "[Invalid value]");
      }
      return std::format_to(ctx.out(), "{}", modes[index]);
   }
};

// http://6502.org/users/obelisk/6502/reference.html
// 0xFF is used for illegal combinations, third element is if it's a branch instruction
// Note that for opcodes like ASL that have accumulator addressing, immediate is also used
// so that the A is optional
constexpr auto opcodes
   = sorted_keywords(std::to_array<std::tuple<std::string_view, std::array<std::uint8_t, 12>, bool>>(
      {//       Impl. Acc.  Imme. zp    zp_x  zp_y  abs   abs_x abs_y indir indrx indry
       {"ADC", {0xFF, 0xFF, 0x69, 0x65, 0x75, 0xFF, 0x6D, 0x7D, 0x79, 0xFF, 0x61, 0x71}, false},
       {"AND", {0xFF, 0xFF, 0x29, 0x25, 0x35, 0xFF, 0x2D, 0x3D, 0x39, 0xFF, 0x21, 0x31}, false},
       {"ASL", {0x0A, 0x0A, 0xFF, 0x0A, 0x16, 0xFF, 0x0E, 0x1E, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"BCC", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x90, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, true},
       {"BCS", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xB0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, true},
       {"BEQ", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xF0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, true},
       {"BIT", {0xFF, 0xFF, 0xFF, 0x24, 0xFF, 0xFF, 0x2C, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"BMI", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x30, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, true},
       {"BNE", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xD0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, true},
       {"BPL", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x10, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, true},
       {"BRK", {0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"BVC", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x50, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, true},
       {"BVS", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x70, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, true},
       {"CLC", {0x18, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"CLD", {0xD8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"CLI", {0x58, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"CLV", {0xB8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"CMP", {0xFF, 0xFF, 0xC9, 0xC5, 0xD5, 0xFF, 0xCD, 0xDD, 0xD9, 0xFF, 0xC1, 0xD1}, false},
       {"CPX", {0xFF, 0xFF, 0xE0, 0xE4, 0xFF, 0xFF, 0xEC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"CPY", {0xFF, 0xFF, 0xC0, 0xC4, 0xFF, 0xFF, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"DEC", {0xFF, 0xFF, 0xFF, 0xC6, 0xD6, 0xFF, 0xCE, 0xDE, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"DEX", {0xCA, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"DEY", {0x88, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"EOR", {0xFF, 0xFF, 0x49, 0x45, 0x55, 0xFF, 0x4D, 0x5D, 0x59, 0xFF, 0x41, 0x51}, false},
       {"INC", {0xFF, 0xFF, 0xFF, 0xE6, 0xF6, 0xFF, 0xEE, 0xFE, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"INX", {0xE8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"INY", {0xC8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"JMP", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x4C, 0xFF, 0xFF, 0x6C, 0xFF, 0xFF}, false},
       {"JSR", {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x20, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"LDA", {0xFF, 0xFF, 0xA9, 0xA5, 0xB5, 0xFF, 0xAD, 0xBD, 0xB9, 0xFF, 0xA1, 0xB1}, false},
       {"LDX", {0xFF, 0xFF, 0xA2, 0xA6, 0xFF, 0xB6, 0xAE, 0xFF, 0xBE, 0xFF, 0xFF, 0xFF}, false},
       {"LDY", {0xFF, 0xFF, 0xA0, 0xA4, 0xB4, 0xFF, 0xAC, 0xBC, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"LSR", {0x4A, 0x4A, 0xFF, 0x46, 0x56, 0xFF, 0x4E, 0x5E, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"NOP", {0xEA, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"ORA", {0xFF, 0xFF, 0x09, 0x05, 0x15, 0xFF, 0x0D, 0x1D, 0x19, 0x00, 0x01, 0x11}, false},
       {"PHA", {0x48, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"PHP", {0x08, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"PLA", {0x68, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"PLP", {0x28, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"ROL", {0x2A, 0x2A, 0xFF, 0x26, 0x36, 0xFF, 0x2E, 0x3E, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"ROR", {0x6A, 0x6A, 0xFF, 0x66, 0x76, 0xFF, 0x6E, 0x7E, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"RTI", {0x40, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"RTS", {0x60, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"SBC", {0xFF, 0xFF, 0xE9, 0xE5, 0xF5, 0xFF, 0xED, 0xFD, 0xF9, 0xFF, 0xE1, 0xF1}, false},
       {"SEC", {0x38, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"SED", {0xF8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"SEI", {0x78, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"STA", {0xFF, 0xFF, 0xFF, 0x85, 0x95, 0xFF, 0x8D, 0x9D, 0x99, 0xFF, 0x81, 0x91}, false},
       {"STX", {0xFF, 0xFF, 0xFF, 0x86, 0xFF, 0x96, 0x8E, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"STY", {0xFF, 0xFF, 0xFF, 0x84, 0x94, 0xFF, 0x8C, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"TAX", {0xAA, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"TAY", {0xA8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"TSX", {0xBA, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"TXA", {0x8A, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"TXS", {0x9A, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"TYA", {0x98, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false}}));

struct address_mode_data {
   addr_mode mode;
   std::variant<std::string_view, std::int64_t> value;
};

struct assembler_state {
   struct string_like_hasher {
      using hash_type = std::hash<std::string_view>;
      using is_transparent = void;

      std::size_t operator()(const char* str) const noexcept { return hash_type{}(str); };
      std::size_t operator()(const std::string& str) const noexcept { return hash_type{}(str); };
      std::size_t operator()(std::string_view str) const noexcept { return hash_type{}(str); };
   };

   // String instead of string_view because of synthesized labels
   std::unordered_map<std::string, std::int64_t, string_like_hasher, std::equal_to<>> labels;
   std::unordered_map<std::string, std::vector<std::tuple<std::uint16_t, addr_mode, bool>>> unresolved_labels;

   std::uint64_t label_id = 0;
   std::uint64_t line_no = 1;

   // TODO: Handle other formats such as CRT
   std::array<std::uint8_t, 65536> code;
   std::bitset<65536> code_written;

   // 32 bits as the range is non-inclusive
   std::uint32_t earliest_address = 0x10000;
   std::uint32_t last_address = 0;
   std::uint32_t current_address = 0;

   bool errored = false;

   void emit_error(const std::string_view reason) noexcept
   {
      errored = true;
      std::cerr << "Error on line " << line_no << ":\n\t" << reason << '\n';
   }

   void emit_byte(std::uint8_t byte) noexcept
   {
      if (code_written[current_address]) {
         std::cerr << "Fatal error on line " << line_no << ":\n\tAddress " << std::hex << current_address
                   << " written to twice.\n";
         std::exit(1);
      }
      code_written[current_address] = true;
      code[current_address] = byte;
      ++current_address;
   }
};

int main()
{
   assembler_state state;
   using namespace khparse;

   const auto ws_sub_nl = drop<"[ \t]">;

   const auto integer
      = or_{seq{drop<"0x">, hex64}, seq{drop<"0b">, bin64}, seq{drop<"[$]">, hex64}, seq{drop<"%">, bin64}, i64};

   const auto uinteger8
      = or_{seq{drop<"0x">, uhex8}, seq{drop<"0b">, ubin8}, seq{drop<"[$]">, uhex8}, seq{drop<"%">, ubin8}, u8};

   const auto identifier = capture<"[a-zA-Z_][a-zA-Z0-9_]*">;

   // This requires the label to be able to be resolved
   const auto value = or_{integer, bind{identifier, [&](const auto& label) -> std::int64_t {
                                           const auto loc = state.labels.find(label);
                                           if (loc == state.labels.end()) {
                                              state.emit_error(std::format("Unknown label \"{}\"", label));
                                              return 0;
                                           }
                                           else {
                                              return loc->second;
                                           }
                                        }}};

   fwd_parser<int64_t> math;

   // These go from order of most priority to least
   const auto term0
      = seq{with_skipper, ws_sub_nl, or_{value, seq{with_skipper, ws_sub_nl, drop<"[(]">, math, drop<"[)]">}}};

   const auto term1 = bind{
      seq{
         with_skipper,
         ws_sub_nl,
         term0,
         repeat{with_skipper, ws_sub_nl, seq{with_skipper, ws_sub_nl, capture<">>|<<|&|[|]|^">, term0}}},
      [](const auto& parse_result) {
         const auto& [base_value, rest] = parse_result;
         auto return_value = base_value;
         for (const auto& [operand, value] : rest) {
            switch (operand[0]) {
            // We only need to check the first character for << and >> since they are unique
            case '<': return_value <<= value; break;
            case '>': return_value >>= value; break;
            case '&': return_value &= value; break;
            case '|': return_value |= value; break;
            case '^': return_value ^= value; break;
            }
         }
         return return_value;
      }};

   const auto term2 = bind{
      seq{
         with_skipper,
         ws_sub_nl,
         term1,
         repeat{with_skipper, ws_sub_nl, seq{with_skipper, ws_sub_nl, capture<"[*]|/">, term1}}},
      [](const auto& parse_result) {
         const auto& [base_value, rest] = parse_result;
         auto return_value = base_value;
         for (const auto& [operand, value] : rest) {
            switch (operand[0]) {
            case '*': return_value *= value; break;
            case '/': return_value /= value; break;
            }
         }
         return return_value;
      }};

   const auto term3 = bind{
      seq{
         with_skipper,
         ws_sub_nl,
         term2,
         repeat{with_skipper, ws_sub_nl, seq{with_skipper, ws_sub_nl, capture<"[+]|-">, term2}}},
      [](const auto& parse_result) {
         const auto& [base_value, rest] = parse_result;
         auto return_value = base_value;
         for (const auto& [operand, value] : rest) {
            switch (operand[0]) {
            case '+': return_value += value; break;
            case '-': return_value -= value; break;
            }
         }
         return return_value;
      }};

   math = term3;

   const auto comment_or_end = drop<"$|;.*$">;
   const auto label_eol = seq{with_skipper, ws_sub_nl, identifier, comment_or_end};
   const auto label_or_math = or_{label_eol, term3};
   // Addressing modes
   const auto implied = bind{seq{with_skipper, ws_sub_nl, comment_or_end}, []() {
                                return address_mode_data{addr_mode::implied, {}};
                             }};
   const auto accumulator = bind{seq{with_skipper, ws_sub_nl, drop<"[Aa]">, comment_or_end}, []() {
                                    return address_mode_data{addr_mode::accumulator, {}};
                                 }};
   const auto immediate
      = bind{seq{with_skipper, ws_sub_nl, drop<"#">, label_or_math, comment_or_end}, [](const auto& val) {
                return address_mode_data{addr_mode::immediate, val};
             }};
   // These three differ depending on if the value is 16-bit or 8-bit
   const auto resolve_zp_abs = [](const auto& val, addr_mode zp, addr_mode abs) {
      if (const auto number = std::get_if<std::int64_t>(&val)) {
         if (*number < 0x100) {
            return address_mode_data{zp, *number};
         }
      }
      return address_mode_data{abs, val};
   };
   const auto zp_abs = bind{seq{with_skipper, ws_sub_nl, label_or_math, comment_or_end}, [&](const auto& val) {
                               return resolve_zp_abs(val, addr_mode::zero_page, addr_mode::absolute);
                            }};
   const auto zp_abs_x = bind{
      seq{with_skipper, ws_sub_nl, label_or_math, drop<",">, drop<"[xX]">, comment_or_end},
      [&](const auto& val) { return resolve_zp_abs(val, addr_mode::zero_page_x, addr_mode::absolute_x); }};
   const auto zp_abs_y = bind{
      seq{with_skipper, ws_sub_nl, label_or_math, drop<",">, drop<"[yY]">, comment_or_end},
      [&](const auto& val) { return resolve_zp_abs(val, addr_mode::zero_page_y, addr_mode::absolute_y); }};

   const auto indirect = bind{
      seq{with_skipper, ws_sub_nl, drop<"[(]">, label_or_math, drop<"[)]">, comment_or_end}, [&](const auto& val) {
         return address_mode_data{addr_mode::indirect, val};
      }};
   const auto indexed_indirect = bind{
      seq{with_skipper, ws_sub_nl, drop<"[(]">, label_or_math, drop<",">, drop<"[xX]">, drop<"[)]">, comment_or_end},
      [&](const auto& val) {
         return address_mode_data{addr_mode::indexed_indirect, val};
      }};
   const auto indirect_indexed = bind{
      seq{with_skipper, ws_sub_nl, drop<"[(]">, label_or_math, drop<"[)]">, drop<",">, drop<"[yY]">, comment_or_end},
      [&](const auto& val) {
         return address_mode_data{addr_mode::indirect_indexed, val};
      }};
   const auto address_mode
      = or_{implied, accumulator, immediate, zp_abs, zp_abs_x, zp_abs_y, indirect, indexed_indirect, indirect_indexed};

   const auto label
      = bind{seq{with_skipper, ws_sub_nl, seq{identifier, drop<":">}, comment_or_end}, [&](const auto& label) {
                if (state.labels.count(label) != 0) {
                   state.emit_error(std::format("Label \"{}\" defined twice", label));
                }
                state.labels.emplace(std::string{label}, state.current_address);
             }};
   const auto opcode = bind{capture<"[a-zA-Z]+">, [&](const auto& op_str) {
                               const int index = [&]() {
                                  if (op_str.size() != 3) {
                                     return -1;
                                  }
                                  char buffer[3];
                                  const auto buffer_view = std::string_view{buffer, 3};
                                  std::transform(op_str.begin(), op_str.end(), std::begin(buffer), [](unsigned char c) {
                                     return static_cast<char>(std::toupper(c));
                                  });
                                  const auto loc = std::lower_bound(
                                     opcodes.begin(), opcodes.end(), buffer_view, [](const auto& tup, const auto& str) {
                                        return std::get<0>(tup) < str;
                                     });
                                  if (loc != opcodes.end() && std::get<0>(*loc) == buffer_view) {
                                     return static_cast<int>(std::distance(opcodes.begin(), loc));
                                  }
                                  return -1;
                               }();
                               if (index == -1) {
                                  state.emit_error(std::format("Unknown opcode \"{}\"", op_str));
                               }
                               return index;
                            }};

   const auto opcode_line = bind{
      seq{with_skipper, ws_sub_nl, opcode, address_mode}, [&](const auto& op_info) {
         const auto [op_index, mode_data] = op_info;
         if (op_index != -1) {
            const auto& [op_name, hex_array, is_branch] = opcodes[op_index];
            if (hex_array[op_index] == 0xFF) {
               state.emit_error(std::format("Invalid addressing mode {} for opcode {}", mode_data.mode, op_name));
            }
            else {
               state.emit_byte(hex_array[op_index]);
               // TODO: Emit bytes/mark unresolved labels based on mode_data
            }
         }
      }};

   const auto line_assembler = or_{label, opcode_line};

   std::string to_parse;
   while (std::getline(std::cin, to_parse)) {
      const auto result = line_assembler.parse(to_parse);
      if (!result) {
         std::cout << "Parse error at " << result.error().where << '\n';
      }
      else {
         if (result.value().rest != to_parse.data() + to_parse.size()) {
            std::cout << "Remaining input at " << result.value().rest << '\n';
         }
         else {
            std::cout << "Parse successful\n";
         }
      }
   }
}
