#include <algorithm>
#include <iostream>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>

using std::string;
using std::vector;
using insertion_rules = std::unordered_map<string, char>;
using poly_state = std::unordered_map<string, size_t>;

size_t apply_rules(const string &poly, const insertion_rules &insns, size_t iters) {
    poly_state state;
    std::unordered_map<char, size_t> letter_cnts;

    // Build pairs from template and count letters
    for (size_t i = 0; i < poly.size()-1; ++i) {
        ++state[string{poly[i], poly[i+1]}];
        ++letter_cnts[poly[i]];
    }

    // Account for last letter
    ++letter_cnts[poly[poly.size()-1]];

    for (size_t i = 0; i < iters; ++i) {
        poly_state tmp;
        for (const auto& [k, cnt] : state) {
            if (cnt == 0) continue;

            auto sit = insns.find(k);
            if (sit != insns.end()) {
                tmp[{k[0], sit->second}] += cnt;
                tmp[{sit->second, k[1]}] += cnt;
                letter_cnts[sit->second] += cnt;
            }
        }

        state = tmp;
    }

    auto [minit, maxit] = std::minmax_element(letter_cnts.cbegin(), letter_cnts.cend(),
        [](auto &l, auto &r){ return l.second < r.second; });

    // std::cout << "Min: " << minit->first << " (" << minit->second << "), Max: "
    //     << maxit->first << " (" << maxit->second << ")\n";

    return (maxit->second) - (minit->second);
}

int main() {

    string poly;
    // Read template
    std::getline(std::cin, poly);

    string line;
    // Empty line
    std::getline(std::cin, line);

    // Insertions
    insertion_rules insns;
    while (std::getline(std::cin, line)) {
        insns.emplace(string{line[0], line[1]}, line[6]);
    }

    // Part 1
    std::cout << "Part 1: " << apply_rules(poly, insns, 10) << '\n';

    // Part 2
    std::cout << "Part 2: " << apply_rules(poly, insns, 40) << '\n';

    return 0;
}
