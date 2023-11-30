#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>

using strlist = std::vector<std::string>;

struct display {
    strlist patterns;
    strlist output;
};

std::string letter_diff(const std::string &s, std::string other) {
    other.erase(std::remove_if(other.begin(), other.end(),
        [s, &other](char c){ return s.find(c) != std::string::npos; }),
        other.end());

    return other;
}

bool contains_all(const std::string &needle, const std::string &haystack) {
    return std::all_of(needle.cbegin(), needle.cend(),
        [haystack](char c){ return haystack.find(c) != std::string::npos; });
}

bool contains_one_of(const std::string &s, const std::string &one) {
    assert((one.size() == 2));

    int found_fst = s.find(one[0]) == std::string::npos;
    int found_snd = s.find(one[1]) == std::string::npos;

    return ((found_fst + found_snd) == 1);
}

int main() {

    std::string line;
    std::vector<display> displays;
    while (std::getline(std::cin, line)) {
        std::istringstream iss{line};
        std::string tmp;
        displays.emplace_back();
        while (iss) {
            iss >> tmp;
            if (tmp == "|") break;
            displays.back().patterns.push_back(tmp);
        }

        for (int i = 0; i < 4; ++i) {
            iss >> tmp;
            displays.back().output.push_back(tmp);
        }

    }

    int cnt_unique = 0;
    long sum = 0;
    for (display &d : displays) {
        std::string one, four, seven, eight;
        for (auto &s : d.patterns) {
            if (s.length() == 2) {
                one = s;
            } else if (s.length() == 3) {
                seven = s;
            } else if (s.length() == 4) {
                four = s;
            } else if (s.length() == 7) {
                eight = s;
            }
        }

        std::string left_bottom = letter_diff(seven, letter_diff(four, eight));

        std::string digits;
        for (auto &s : d.output) {
            if (s.length() == 2) {
                digits += '1';
                ++cnt_unique;
            } else if (s.length() == 3) {
                digits += '7';
                ++cnt_unique;
            } else if (s.length() == 4) {
                digits += '4';
                ++cnt_unique;
            } else if (s.length() == 5) {
                if (contains_all(left_bottom, s)) {
                    digits += '2';
                } else if (contains_all(one, s)) {
                    digits += '3';
                } else {
                    digits += '5';
                }
            } else if (s.length() == 6) {
                auto maybe_nine = letter_diff(s, eight);
                if (left_bottom.find(maybe_nine[0]) != std::string::npos) {
                    digits += '9';
                } else if (contains_one_of(s, one)) {
                    digits += '6';
                } else {
                    digits += '0';
                }
            } else if (s.length() == 7) {
                digits += '8';
                ++cnt_unique;
            }
        }

        sum += std::stol(digits);
    }

    std::cout << "Part 1: " << cnt_unique << '\n';

    // Part 2
    std::cout << "Part 2: " << sum << '\n';

    return 0;
}
