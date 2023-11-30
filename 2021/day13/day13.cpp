#include <algorithm>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

#include "../include/aoc.hpp"

using aoc::point;
using std::string;
using std::vector;
using std::tuple;
using pointvec = std::vector<point>;

tuple<int, int> max(const pointvec &points) {
    int ymax = 0, xmax = 0;

    for (auto [x, y] : points) {
        xmax = std::max(x, xmax);
        ymax = std::max(y, ymax);
    }

    return {ymax, xmax};
}

void print(const pointvec &points) {
    auto [ymax, xmax] = max(points);

    vector<vector<bool>> m;
    m.resize(static_cast<size_t>(ymax)+1);

    size_t size_x = static_cast<size_t>(xmax)+1;
    std::for_each(m.begin(), m.end(),
        [size_x](auto &v){ v.resize(size_x); });

    for (auto [x, y] : points) {
        size_t x_sz = static_cast<size_t>(x);
        size_t y_sz = static_cast<size_t>(y);
        m[y_sz][x_sz] = true;
    }

    for (auto &row : m) {
        for (bool col : row) {
            std::cout << (col ? '#' : '.');
        }

        std::cout << '\n';
    }
}

size_t do_fold(pointvec &points, tuple<char, int> fold) {
    char dir = std::get<char>(fold);
    int foldi = std::get<int>(fold);

    std::for_each(points.begin(), points.end(),
        [dir, foldi](point &p){
            if (dir == 'y' && p.y > foldi) {
                p.y -= 2 * (p.y - foldi);
            } else if (dir == 'x' && p.x > foldi) {
                p.x -= 2 * (p.x - foldi);
            }
        });

    std::sort(points.begin(), points.end());
    auto end_unique = std::unique(points.begin(), points.end());
    points.erase(end_unique, points.end());

    return points.size();
}

int main() {
    pointvec points;
    std::string line;
    while (std::getline(std::cin, line) && !line.empty()) {
        auto fst_end = line.find(',');
        string xstr = line.substr(0, fst_end);
        string ystr = line.substr(fst_end+1);
        points.emplace_back(std::stoi(xstr), std::stoi(ystr));
    }

    // Folds
    vector<tuple<char, int>> folds;
    while (std::getline(std::cin, line)) {
        char dir = line[11];
        int pos = std::stoi(line.substr(13));
        folds.emplace_back(dir, pos);
    }

    std::cout << "Part 1: " << do_fold(points, folds[0]) << '\n';

    // Part 2
    for (size_t i = 1; i < folds.size(); ++i) {
        do_fold(points, folds[i]);
    }

    std::cout << "Part 2:\n";
    print(points);
    std::cout << '\n';

    return 0;
}
