#pragma once

#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
#include <utility>

namespace aoc {

std::vector<std::string> split(std::string s, char delim = ' ') {
    std::vector<std::string> res;
    std::string::const_iterator last = s.cbegin();
    std::string::const_iterator it = s.cbegin();

    do {
        it = std::find(it, s.cend(), delim);
        if (it != last) res.push_back(std::string(last, it));
        if (it != s.cend()) it++;
        last = it;
    } while (it != s.cend());

    return res;
}

template <typename T> void read_input(std::vector<T> &container) {
    T in;
    for (std::cin >> in; !std::cin.eof(); std::cin >> in) {
        container.push_back(in);
    }
}

struct point {
    int x, y;

    point(int x, int y) : x(x), y(y) {}

    bool operator<(const point &other) const noexcept {
        return (this->y == other.y) ? (this->x < other.x) : (this->y < other.y);
    }

    bool operator==(const point &other) const noexcept {
        return other.x == this->x && other.y == this->y;
    }
};

struct PointHash {
    // Assuming 32 bit int and 64 bit size_t
    std::size_t operator()(point const& p) const noexcept {
        return static_cast<size_t>(p.x) | (static_cast<size_t>(p.y) << 32);
    }
};

}