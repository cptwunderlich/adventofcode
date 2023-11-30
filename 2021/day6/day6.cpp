#include <algorithm>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

using ulong = unsigned long;

// Making a copy of counts, bc. we modify it!
ulong calc_population(std::vector<ulong> counts, int lifetime) {
    for (; lifetime > 0; --lifetime) {
        ulong zeroes = counts[0];
        std::rotate(counts.begin(), counts.begin()+1, counts.end());
        counts[6] += zeroes;
    }

    return std::accumulate(counts.cbegin(), counts.cend(), 0uL);
}

int main() {

    std::string token;
    std::vector<ulong> counts;
    counts.resize(9);
    while (std::getline(std::cin, token, ',')) {
        size_t i = std::stoul(token);
        ++counts[i];
    }

    std::cout << "Part 1: " << calc_population(counts, 80) << '\n';

    std::cout << "Part 2: " << calc_population(counts, 256) << '\n';

    return 0;
}
