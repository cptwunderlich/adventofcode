
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <functional>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

uint8_t hex_to_uint(char c) {
    if (c < 'A') return c - '0';
    else         return c - 'A' + 10;
}

using std::string;
using std::vector;
using bitvec_it = vector<bool>::const_iterator;

// WARNING: should raise error when end is reached before n bits read,
// but this is toy code!
class Bitstream {
    private:
        vector<bool> bitvec_{};
        bitvec_it it;

    public:
        Bitstream(const string &line) {
            for (char c : line) {
                uint8_t nibble = hex_to_uint(c);
                // Push lower 4 bits
                bitvec_.push_back(nibble & 8u);
                bitvec_.push_back(nibble & 4u);
                bitvec_.push_back(nibble & 2u);
                bitvec_.push_back(nibble & 1u);
            }

            it = bitvec_.cbegin();
        }

        uint32_t take_bits(size_t n) {
            assert(n >= 1);

            uint32_t res = 0;
            for (size_t cnt = 0; it != bitvec_.cend() && cnt < n; ++cnt, ++it) {
                res = res << 1;
                res |= *it;
            }

            return res;
        }

        uint8_t take_nibble() {
            return static_cast<uint8_t>(take_bits(4));
        }

        uint64_t take_literal() {
            uint64_t res = 0;
            constexpr size_t nibbles_per_int = sizeof(uint64_t)*2;
            bool more = true;
            for (size_t nibbles = 0; it != bitvec_.cend() && more; ++nibbles) {
                assert(nibbles < nibbles_per_int);

                more = *it++;
                uint8_t b = take_nibble();
                res = res << 4;
                res |= b;
            }

            return res;
        }

        bool end() { return it == bitvec_.cend(); }

        ptrdiff_t offset() { return it - bitvec_.cbegin(); }

        void print() {
            for (bool b : bitvec_) {
                std::cout << b;
            }
            std::cout << '\n';
        }
};

void parse_packet(Bitstream &bitstream, vector<uint64_t> &stack, size_t &version_sum) {
    // Version
    version_sum += bitstream.take_bits(3);

    uint32_t type_id = bitstream.take_bits(3);
    if (type_id == 4) {
        // Literal
        stack.push_back(bitstream.take_literal());
        return;
    } else {
        // Operator
        bool len_type_id = bitstream.take_bits(1);
        uint32_t num_packets = 0;
        if (len_type_id) {
            num_packets = bitstream.take_bits(11);
            for (size_t cnt = 0; cnt < num_packets; ++cnt) {
                parse_packet(bitstream, stack, version_sum);
            }
        } else {
            uint32_t num_bits = bitstream.take_bits(15);
            ptrdiff_t start = bitstream.offset();
            while (bitstream.offset() - start < num_bits) {
                parse_packet(bitstream, stack, version_sum);
                ++num_packets;
            }
        }

        // Reduce operands with operator
        std::function<uint64_t(uint64_t, uint64_t)> op;
        switch (type_id) {
        case 0:
            op = std::plus<>();
            break;

        case 1:
            op = std::multiplies<>();
            break;

        case 2:
            op = [](uint64_t x, uint64_t y){ return std::min<>(x, y); };
            break;

        case 3:
            op = [](uint64_t x, uint64_t y){ return std::max<>(x, y); };
            break;

        // case 4 is literal!

        case 5:
            // wow, this will silently convert bool to uint64_t. Gotta love C++!
            op = std::greater<>();
            break;

        case 6:
            op = std::less<>();
            break;

        case 7:
            op = std::equal_to<>();
            break;

        default:
            // "impossible"
            assert(false);
        }

        uint64_t res = std::accumulate(
            stack.cend()-(num_packets-1), // Begin with second elem
            stack.cend(),                 // End
            *(stack.cend()-num_packets),  // Start with first elem
            op);
        stack.erase(stack.end()-num_packets, stack.end());
        stack.push_back(res);
    }
}

int main() {

    std::string line;
    std::getline(std::cin, line);

    Bitstream bitstream{line};
    // bitstream.print();

    vector<uint64_t> stack;
    size_t sum = 0;
    parse_packet(bitstream, stack, sum);

    assert(stack.size() > 0);

    std::cout << "Part 1: " << sum << '\n';

    std::cout << "Part 2: " << stack.back() << '\n';

    return 0;
}
