#include <algorithm>
#include <cassert>
#include <iostream>
#include <numeric>
#include <stdint.h>
#include <string>
#include <vector>

using std::vector;
using matrix = vector<vector<int>>;

struct point {
    size_t row, col;

    bool operator<(const point& other) const {
        return (this->row < other.row) ||
            (this->row == other.row && this->col < other.col);
    }
};

vector<point> neighbors(const matrix &m, const point &p) {
    assert((m.size() < __LONG_MAX__));
    assert((!m.empty()));
    assert((m[0].size() < __LONG_MAX__));
    assert((p.row < __LONG_MAX__));
    assert((p.col < __LONG_MAX__));

    long rows = static_cast<long>(m.size());
    long cols = static_cast<long>(m[0].size());
    long row = static_cast<long>(p.row);
    long col = static_cast<long>(p.col);
    vector<point> res;
    for (long i = row -1; i <= row+1; ++i) {
        if (i < 0 || i >= rows) continue;

        for (long j = col - 1; j <= col+1; ++j) {
            if (j < 0 || j >= cols) continue;

            size_t i_sz = static_cast<size_t>(i);
            size_t j_sz = static_cast<size_t>(j);
            if (i_sz == p.row && j_sz == p.col) continue;

            res.push_back({i_sz, j_sz});
        }
    }

    return res;
}

// void flash(matrix &m, const point &p) {
//     vector<point> neighs = neighbors(m, p);
//     for
// }

void print(const matrix &m) {
    for (auto &row : m) {
        for (auto col : row) {
            std::cout << col;
        }

        std::cout << '\n';
    }
}

int main() {

    matrix lvls;
    std::string line;
    while (std::getline(std::cin, line)) {
        lvls.emplace_back();
        for (char c : line) {
            lvls.back().push_back(static_cast<int>(c) - 48);
        }
    }

    size_t cnt = 0;
    size_t final_step = 0;
    for (size_t iters = 0; iters < SIZE_MAX; ++iters) {
        // Increase
        std::for_each(lvls.begin(), lvls.end(),
            [](auto &r){ std::for_each(r.begin(), r.end(),
                [](int &i){ ++i; });
        });

        bool flashes;
        do {
            flashes = false;
            for (size_t i = 0; i < lvls.size(); ++i) {
                vector<int> &row = lvls[i];

                for (size_t j = 0; j < row.size(); ++j) {
                    if (row[j] == 10) {
                        if (iters < 100) ++cnt;
                        flashes = true;
                        ++row[j]; // don't flash again

                        point cur{i, j};
                        vector<point> n = neighbors(lvls, cur);
                        for (point &p : n) {
                            if (lvls[p.row][p.col] <= 9) {
                                ++lvls[p.row][p.col];
                            }
                        }
                    }
                }
            }
        } while (flashes);

        // Reset to zero
        std::for_each(lvls.begin(), lvls.end(),
            [](auto &r){ std::for_each(r.begin(), r.end(),
                [](int &i){ if (i >= 10) i = 0; });
        });

        if (iters >= 100) {
            if (std::all_of(lvls.cbegin(), lvls.cend(), [](auto &row){
                    return std::all_of(row.cbegin(), row.cend(), [](int i){ return i == 0; });
                })) {

                final_step = iters + 1;
                break;
            }
        }

        // print(lvls);
        // std::cout << '\n';
    }

    std::cout << "Part 1: " << cnt << '\n';

    // Part 2
    std::cout << "Part 2: " << final_step << '\n';

    return 0;
}
