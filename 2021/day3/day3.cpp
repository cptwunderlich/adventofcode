#include <algorithm>
#include <bitset>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

// Input sizes
constexpr size_t len = 1000; // 12;
constexpr size_t width = 12; // 5;

bool find_most_common(std::bitset<len> &vec, size_t actual_size) {
    auto ones = vec.count();
    auto zeroes = actual_size - ones;
    return (ones >= zeroes) ? true : false;
}

std::vector<bool> find_match(std::vector<std::bitset<len>> &matrix, bool most_common) {
    std::bitset<len> idx_mask;
    idx_mask.set(); // set all to 1
    for (size_t i = 0; i < matrix.size() && idx_mask.count() > 1; ++i) {
        std::bitset<len> vec = matrix[i] & idx_mask;
        bool mc = find_most_common(vec, idx_mask.count());
        if (!most_common) mc = !mc;
        std::bitset<len> xor_mask;
        if (mc) xor_mask.set();
        xor_mask ^= matrix[i];
        idx_mask &= xor_mask;
    }

    size_t idx = 0;
    for (; idx < len; ++idx) {
        if (idx_mask.test(idx)) break;
    }

    std::vector<bool> res;
    for (size_t i = 0; i < matrix.size(); ++i) {
        res.push_back(matrix[i][idx]);
    }

    return res;
}

long bitvec_to_long(std::vector<bool> &&vec) {
    long res = 0;
    for (size_t i = vec.size() - 1; i != (size_t) -1; --i) {
        size_t shift = vec.size() - 1 - i;
        res += static_cast<long>(vec[i]) << shift;
    }

    return res;
}

int main() {

    std::vector<std::bitset<len>> matrix;
    matrix.reserve(width);
    for (size_t i = 0; i < width; ++i) {
        matrix.emplace_back();
    }

    std::string line;
    size_t line_no = 0;
    while (std::getline(std::cin, line)) {
        for (size_t i = 0; i < width; ++i) {
            if (line[i] == '1') matrix[i].set(line_no);
        }

        ++line_no;
    }

    // Part 1
    long gamma_res = 0, epsilon_res = 0;
    std::vector<bool> most_commons;
    most_commons.reserve(width);
    for (size_t i = matrix.size() - 1; i != (size_t)-1; --i) {
        bool mc = find_most_common(matrix[i], matrix[i].size());

        size_t shift = matrix.size() - 1 - i;
        gamma_res += static_cast<long>(mc) << shift;
        epsilon_res += static_cast<long>(!mc) << shift;
        most_commons.push_back(mc);
    }

    std::cout << "Part 1: " << gamma_res * epsilon_res << '\n';

    // Part 2
    long oxy_rate = bitvec_to_long(find_match(matrix, true));
    long co2_rate = bitvec_to_long(find_match(matrix, false));

    std::cout << "Part 2: " << oxy_rate * co2_rate << '\n';

    return 0;
}
