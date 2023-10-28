// A simple 6502 assembler that assembles to Commodore PRG format
// It reads from STDIN, reports errors/info on STDERR, and outputs the PRG on STDOUT

#include "khparse.hpp"

#include <algorithm>
#include <array>
#include <bitset>
#include <cstdint>
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

std::ostream& operator<<(std::ostream& out, addr_mode mode) noexcept
{
   using am = addr_mode;
   switch (mode) {
   case am::implied: return out << "implied";
   case am::accumulator: return out << "accumulator";
   case am::immediate: return out << "immediate";
   case am::zero_page: return out << "zero page";
   case am::zero_page_x: return out << "zero page, x";
   case am::zero_page_y: return out << "zero page, y";
   case am::absolute: return out << "absolute";
   case am::absolute_x: return out << "absolute, x";
   case am::absolute_y: return out << "absolute, y";
   case am::indirect: return out << "indirect";
   case am::indexed_indirect: return out << "indexed indirect";
   case am::indirect_indexed: return out << "indirect indexed";
   }
   return out;
}

// http://6502.org/users/obelisk/6502/reference.html
// 0xFF is used for illegal combinations, third element is if it's a branch instruction
constexpr auto opcodes
   = sorted_keywords(std::to_array<std::tuple<std::string_view, std::array<std::uint8_t, 12>, bool>>(
      {//       Impl. Acc.  Imme. zp    zp_x  zp_y  abs   abs_x abs_y indir indrx indry
       {"ADC", {0xFF, 0xFF, 0x69, 0x65, 0x75, 0xFF, 0x6D, 0x7D, 0x79, 0xFF, 0x61, 0x71}, false},
       {"AND", {0xFF, 0xFF, 0x29, 0x25, 0x35, 0xFF, 0x2D, 0x3D, 0x39, 0xFF, 0x21, 0x31}, false},
       {"ASL", {0xFF, 0x0A, 0xFF, 0x0A, 0x16, 0xFF, 0x0E, 0x1E, 0xFF, 0xFF, 0xFF, 0xFF}, false},
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
       {"LSR", {0xFF, 0x4A, 0xFF, 0x46, 0x56, 0xFF, 0x4E, 0x5E, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"NOP", {0xEA, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"ORA", {0xFF, 0xFF, 0x09, 0x05, 0x15, 0xFF, 0x0D, 0x1D, 0x19, 0x00, 0x01, 0x11}, false},
       {"PHA", {0x48, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"PHP", {0x08, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"PLA", {0x68, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"PLP", {0x28, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"ROL", {0xFF, 0x2A, 0xFF, 0x26, 0x36, 0xFF, 0x2E, 0x3E, 0xFF, 0xFF, 0xFF, 0xFF}, false},
       {"ROR", {0xFF, 0x6A, 0xFF, 0x66, 0x76, 0xFF, 0x6E, 0x7E, 0xFF, 0xFF, 0xFF, 0xFF}, false},
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
   std::variant<std::uint16_t, std::string_view> value;
};

struct assembler_state {
   // 32 bits as the range is non-inclusive
   std::uint32_t earliest_address = 0xFFFF;
   std::uint32_t last_address = 0;
   std::uint32_t current_address = 0;
   bool errored = false;
   int line_no = 1;
   std::unordered_map<std::string_view, std::uint16_t> labels;
   std::unordered_map<std::string_view, std::vector<std::tuple<std::uint16_t, addr_mode, bool>>> unresolved_labels;
   std::array<std::uint8_t, 65536> code;
   std::bitset<65536> code_written;

   // Returns false if the byte is already written to
   bool try_write_byte(std::uint8_t byte) noexcept
   {
      if (code_written[current_address]) {
         return false;
      }
      code_written[current_address] = true;
      code[current_address] = byte;
      if (current_address < earliest_address) {
         earliest_address = current_address;
      }
      current_address += 1;
      if (current_address > last_address) {
         last_address = current_address;
      }
      return true;
   }
};

const auto create_parser = [](assembler_state& state) {
   using namespace khparse;

   const auto ws_sub_nl = drop<"[ \t]">;

   const auto integer
      = or_{seq{drop<"0x">, uhex16}, seq{drop<"0b">, ubin16}, seq{drop<"[$]">, uhex16}, seq{drop<"%">, ubin16}, u16};

   const auto uinteger8
      = or_{seq{drop<"0x">, uhex8}, seq{drop<"0b">, ubin8}, seq{drop<"[$]">, uhex8}, seq{drop<"%">, ubin8}, u8};

   const auto identifier = capture<"[a-zA-Z_][a-zA-Z0-9_]*">;

   const auto int_or_label = or_{integer, identifier};

   const auto comment_or_endl = drop<"\\n|(;[^\\n]*\\n)">;

   const auto implied = seq{with_skipper, ws_sub_nl, comment_or_endl};
   const auto accumulator = seq{with_skipper, ws_sub_nl, drop<"[Aa]">, comment_or_endl};
   const auto immediate = seq{with_skipper, ws_sub_nl, drop<"#">, int_or_label, comment_or_endl};
   // These three depend only differ depending on if the value is 16-bit or 8-bit
   const auto zp_abs = seq{with_skipper, ws_sub_nl, int_or_label, comment_or_endl};
   const auto zp_abs_x = seq{with_skipper, ws_sub_nl, int_or_label, drop<",">, drop<"[xX]">, comment_or_endl};
   const auto zp_abs_y = seq{with_skipper, ws_sub_nl, int_or_label, drop<",">, drop<"[yY]">, comment_or_endl};
   const auto indirect = seq{with_skipper, ws_sub_nl, drop<"[(]">, int_or_label, drop<"[)]">, comment_or_endl};
   const auto indexed_indirect
      = seq{with_skipper, ws_sub_nl, drop<"[(]">, int_or_label, drop<",">, drop<"[xX]">, drop<"[)]">, comment_or_endl};
   const auto indirect_indexed
      = seq{with_skipper, ws_sub_nl, drop<"[(]">, int_or_label, drop<"[)]">, drop<",">, drop<"[yY]">, comment_or_endl};

   const auto address_mode = bind{
      or_{accumulator, immediate, zp_abs_x, zp_abs_y, zp_abs, indexed_indirect, indirect_indexed, indirect, implied},
      [](const auto& result) -> address_mode_data {
         if (std::get_if<0>(&result)) {
            return {addr_mode::accumulator, {}};
         }
         else if (const auto val = std::get_if<1>(&result)) {
            return {addr_mode::immediate, *val};
         }
         else if (const auto val = std::get_if<2>(&result)) {
            if (const auto number = std::get_if<std::uint16_t>(val)) {
               if (*number < 0x100) {
                  return {addr_mode::zero_page_x, *number};
               }
            }
            return {addr_mode::absolute_x, *val};
         }
         else if (const auto val = std::get_if<3>(&result)) {
            if (const auto number = std::get_if<std::uint16_t>(val)) {
               if (*number < 0x100) {
                  return {addr_mode::zero_page_y, *number};
               }
            }
            return {addr_mode::absolute_y, *val};
         }
         else if (const auto val = std::get_if<4>(&result)) {
            if (const auto number = std::get_if<std::uint16_t>(val)) {
               if (*number < 0x100) {
                  return {addr_mode::zero_page, *number};
               }
            }
            return {addr_mode::absolute, *val};
         }
         else if (const auto val = std::get_if<5>(&result)) {
            return {addr_mode::indexed_indirect, *val};
         }
         else if (const auto val = std::get_if<6>(&result)) {
            return {addr_mode::indirect_indexed, *val};
         }
         else if (const auto val = std::get_if<7>(&result)) {
            return {addr_mode::indirect, *val};
         }
         else if (std::get_if<8>(&result)) {
            return {addr_mode::implied, {}};
         }
         // This should never be reached
         assert(false);
         return {addr_mode::implied, {}};
      }};

   const auto label = seq{with_skipper, ws_sub_nl, identifier, drop<":">, comment_or_endl};

   // Returns the index of the opcode, or -1 if an invalid opcode is provided
   const auto output_opcode_index = [](const auto& str) {
      if (str.size() != 3) {
         return -1;
      }
      char buffer[3];
      const auto buffer_view = std::string_view{buffer, 3};
      std::transform(str.begin(), str.end(), std::begin(buffer), [](unsigned char c) {
         return static_cast<char>(std::toupper(c));
      });
      const auto loc
         = std::lower_bound(opcodes.begin(), opcodes.end(), buffer_view, [](const auto& tup, const auto& str) {
              return std::get<0>(tup) < str;
           });
      if (loc != opcodes.end() && std::get<0>(*loc) == buffer_view) {
         return static_cast<int>(std::distance(opcodes.begin(), loc));
      }
      return -1;
   };

   const auto opcode = bind{capture<"[a-zA-Z]+">, output_opcode_index};

   const auto handle_directive = [&state](const auto& dirs) {
      if (const auto org_val = std::get_if<0>(&dirs)) {
         state.current_address = *org_val;
      }
      else if (const auto val_opt = std::get_if<1>(&dirs)) {
         // This was copied from below; probably just want to have this be the body of the function?
         const auto attempt_byte = [&](const std::uint8_t byte) {
            if (!state.try_write_byte(byte)) {
               std::cerr << "Fatal error on line " << state.line_no << ":\n\tAddress " << state.current_address
                         << " written to twice\n";
               std::exit(1);
            }
         };
         const auto& [val, vals] = *val_opt;
         attempt_byte(val);
         for (const auto& rest : vals) {
            attempt_byte(rest);
         }
      }
      else if (const auto val_opt = std::get_if<2>(&dirs)) {
         const auto& [label, value] = *val_opt;
         if (!state.labels.try_emplace(label, value).second) {
            std::cerr << "Error on line " << state.line_no << ":\n\t\"" << label << "\" already defined.\n";
            state.errored = true;
         }
      }
      else if (const auto val_opt = std::get_if<3>(&dirs)) {
         const auto& [label, value] = *val_opt;
         const auto loc = state.labels.find(label);
         if (loc == state.labels.end()) {
            std::cerr << "Error on line " << state.line_no << ":\n\t\"" << label << "\" in assert not defined.\n";
            state.errored = true;
         }
         else if (loc->second != value) {
            std::cerr << "Error on line " << state.line_no << ":\n\tAssert failure; " << label << " = " << loc->second
                      << "\n";
            state.errored = true;
         }
      }
   };

   const auto directive = bind{
      or_{
         seq{with_skipper, ws_sub_nl, drop<"[.][Oo][Rr][Gg]">, integer},
         seq{
            with_skipper,
            ws_sub_nl,
            drop<"[.][Bb][Yy][Tt][Ee]">,
            uinteger8,
            repeat{seq{with_skipper, ws_sub_nl, drop<",">, uinteger8}}},
         seq{with_skipper, ws_sub_nl, drop<"[.][Dd][Ee][Ff][Ii][Nn][Ee]">, identifier, integer},
         seq{with_skipper, ws_sub_nl, drop<"[.][Aa][Ss][Ss][Ee][Rr][Tt]">, identifier, drop<"=">, integer}},
      handle_directive};

   const auto handle_line = [&state](const auto& var) {
      if (const auto label = std::get_if<0>(&var)) {
         if (state.labels.count(*label) != 0) {
            std::cerr << "Error on line " << state.line_no << ":\n\tLabel \"" << *label << "\" defined twice\n";
            state.errored = true;
         }
         else {
            state.labels.emplace(*label, state.current_address);
         }
      }
      else if (const auto op_info = std::get_if<1>(&var)) {
         const auto& [op_index, addr] = *op_info;
         if (op_index == -1) {
            std::cerr << "Error on line " << state.line_no << ":\n\tInvalid opcode\n";
            state.errored = true;
         }
         else {
            const auto& [op_name, hex_array, is_branch] = opcodes[op_index];
            const auto addr_index = static_cast<int>(addr.mode);
            if (hex_array[addr_index] == 0xFF) {
               std::cerr << "Error on line " << state.line_no << ":\n\tInvalid addressing mode \"" << addr.mode
                         << "\" for opcode \"" << op_name << "\"\n";
               state.errored = true;
            }
            else {
               const auto attempt_byte = [&](const std::uint8_t byte) {
                  if (!state.try_write_byte(byte)) {
                     std::cerr << "Fatal error on line " << state.line_no << ":\n\tAddress " << state.current_address
                               << " written to twice\n";
                     std::exit(1);
                  }
               };
               attempt_byte(hex_array[addr_index]);
               const std::optional<std::uint16_t> value_opt = [&]() -> std::optional<std::uint16_t> {
                  if (const auto int_val = std::get_if<std::uint16_t>(&addr.value)) {
                     return *int_val;
                  }
                  else if (const auto str_val = std::get_if<std::string_view>(&addr.value)) {
                     const auto label_loc = state.labels.find(*str_val);
                     if (label_loc != state.labels.end()) {
                        return label_loc->second;
                     }
                     else {
                        state.unresolved_labels[*str_val].emplace_back(state.current_address, addr.mode, is_branch);
                        return {};
                     }
                  }
                  // This should never be reached
                  return {};
               }();
               using am = addr_mode;
               switch (addr.mode) {
               case am::implied: break;
               case am::accumulator: break;
               case am::immediate:
                  if (value_opt) {
                     if (*value_opt > 0xFF) {
                        std::cerr << "Error on line " << state.line_no << ":\n\t16-bit value given to immediate mode\n";
                        state.errored = true;
                     }
                     attempt_byte(*value_opt & 0xFF);
                  }
                  else {
                     attempt_byte(0);
                  }
                  break;
               case am::zero_page: [[fallthrough]];
               case am::zero_page_x: [[fallthrough]];
               case am::zero_page_y:
                  if (value_opt) {
                     attempt_byte(*value_opt & 0xFF);
                  }
                  else {
                     std::cerr << "INTERNAL ERROR:\n\tZero page value has unknown value";
                     std::exit(1);
                  }
                  break;
               case am::absolute: [[fallthrough]];
               case am::absolute_x: [[fallthrough]];
               case am::absolute_y: [[fallthrough]];
               case am::indirect: [[fallthrough]];
               case am::indexed_indirect: [[fallthrough]];
               case am::indirect_indexed:
                  if (is_branch) {
                     if (value_opt) {
                        // Only +1 here because we already wrote the opcode byte
                        const int16_t diff = *value_opt - (state.current_address + 1);
                        if (diff > 127 || diff < -128) {
                           std::cerr << "Error on line " << state.line_no << ":\n\tOffset of " << diff
                                     << " is too large\n";
                           state.errored = true;
                        }
                        attempt_byte(static_cast<int8_t>(diff));
                     }
                     else {
                        attempt_byte(0);
                     }
                  }
                  else {
                     if (value_opt) {
                        attempt_byte(*value_opt & 0xFF);
                        attempt_byte(*value_opt >> 8);
                     }
                     else {
                        attempt_byte(0);
                        attempt_byte(0);
                     }
                  }
                  break;
               }
            }
         }
      }
      // TODO: This reports wrong line numbers for some reason... figure out why
      state.line_no += 1;
   };
   return repeat{bind{
      seq{
         with_skipper,
         ws_sub_nl,
         or_{label, seq{with_skipper, ws_sub_nl, opcode, address_mode}, directive, comment_or_endl}},
      handle_line}};
};

int main()
{
   // Is there a better way of reading until EOL? (Almost certainly using read but this works for now)
   std::string full_input;
   std::string line;
   while (std::getline(std::cin, line)) {
      full_input += line + '\n';
   }
   assembler_state state;
   const auto main_parser = create_parser(state);
   const auto result = main_parser.parse(full_input);
   if (result.has_value()) {
      if (result.value().rest != full_input.data() + full_input.size()) {
         const auto rest = std::string_view{result.value().rest};
         const auto newline_index = rest.find('\n');
         const auto line = rest.substr(0, newline_index);
         std::cerr << "Parse Failure:\n\tCurrent Line: " << line << '\n';
      }
      else {
         // Resolve labels (this code is fairly repetitive of previous code, should probably fix that)
         for (const auto& [label_to_resolve, loc_mode_branch_vec] : state.unresolved_labels) {
            const auto loc = state.labels.find(label_to_resolve);
            if (loc == state.labels.end()) {
               std::cerr << "Error:\n\tUnresolved label \"" << label_to_resolve << "\" not found";
               state.errored = true;
            }
            else {
               const auto value = loc->second;
               for (const auto& [loc, mode, is_branch] : loc_mode_branch_vec) {
                  using am = addr_mode;
                  switch (mode) {
                  // These shouldn't appear at all
                  case am::implied: break;
                  case am::accumulator: break;
                  case am::immediate:
                     if (value > 0xFF) {
                        std::cerr << "Error:\n\tLabel \"" << label_to_resolve << "\" too large for immediate mode\n";
                        state.errored = true;
                     }
                     state.code[loc] = value;
                     break;
                  // these should never appear either
                  case am::zero_page: break;
                  case am::zero_page_x: break;
                  case am::zero_page_y: break;
                  case am::absolute: [[fallthrough]];
                  case am::absolute_x: [[fallthrough]];
                  case am::absolute_y: [[fallthrough]];
                  case am::indirect: [[fallthrough]];
                  case am::indexed_indirect: [[fallthrough]];
                  case am::indirect_indexed:
                     if (is_branch) {
                        const int16_t diff = value - (loc + 1);
                        if (diff > 127 || diff < -128) {
                           std::cerr << "Error on line " << state.line_no << ":\n\tOffset of " << diff
                                     << " is too large\n";
                           state.errored = true;
                        }
                        state.code[loc] = static_cast<int8_t>(diff);
                     }
                     else {
                        state.code[loc] = value & 0xFF;
                        state.code[loc + 1] = value >> 8;
                     }
                     break;
                  }
               }
            }
         }
         if (!state.errored) {
            std::cerr << "Code from " << std::hex << state.earliest_address << " to " << state.last_address << '\n';
            std::cout << (std::uint8_t)(state.earliest_address & 0xFF) << (std::uint8_t)(state.earliest_address >> 8);
            for (auto loc = state.earliest_address; loc < state.last_address; ++loc) {
               std::cout << state.code[loc];
            }
         }
      }
   }
}
