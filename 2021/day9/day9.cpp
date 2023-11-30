#include <algorithm>
#include <array>
#include <functional>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>
#include <unordered_set>

using std::string;
using std::vector;
using std::unordered_set;

struct point {
    long x, y;

    point(long x, long y) : x(x), y(y) {}

    bool operator==(const point &other) const {
        return other.x == this->x && other.y == this->y;
    }
};

template<>
struct std::hash<point> {
    std::size_t operator()(point const& p) const noexcept {
        return static_cast<size_t>(p.x ^ (p.y << 1));
    }
};

template <typename T> int signum(T val) {
    return (T(0) < val) - (val < T(0));
}

const std::array<point, 4> dirs{ point(-1, 0), {0, -1}, {1, 0}, {0, 1} };

bool visited(const unordered_set<point> &basin, const point &p) {
    return basin.count(p) > 0;
}

bool valid(const vector<vector<int>> &heightmap, const point &p) {
    return p.y >= 0 && p.y < heightmap.size() && p.x >= 0 && p.x < heightmap[p.y].size();
}

size_t find_basin_size(const vector<vector<int>> &heightmap, const point p) {

    vector<point> stack;
    unordered_set<point> basin;

    // Start with low point
    stack.push_back(p);

    while (!stack.empty()) {
        point cur = stack.back();
        stack.pop_back();
        int curval = heightmap[cur.y][cur.x];

        if (curval == 9 || visited(basin, cur)) continue;

        basin.insert(cur);

        for (const point &dir : dirs) {
            point n{cur.x + dir.x, cur.y + dir.y};
            if (valid(heightmap, n) && heightmap[n.y][n.x] > curval && !visited(basin, n)) {
                stack.push_back(n);
            }
        }
    }

    return basin.size();
}

int main() {

    string line;
    vector<vector<int>> heightmap;
    while (std::getline(std::cin, line)) {
        heightmap.emplace_back();
        for (char c : line) {
            heightmap.back().push_back(static_cast<int>(c) - 48);
        }
    }

    long sum = 0;
    vector<size_t> basin_sizes;

    for (size_t i = 0; i < heightmap.size(); ++i) {
        vector<int> &row = heightmap[i];
        for (size_t j = 0; j < row.size(); ++j) {
            std::vector<int> tmp;
            tmp.reserve(8);
            int cur = heightmap[i][j];

            // Row above
            if (i > 0) {
                if (j > 0) tmp.push_back(heightmap[i-1][j-1]);
                tmp.push_back(heightmap[i-1][j]);
                if (j < row.size()-1) tmp.push_back(heightmap[i-1][j+1]);
            }

            // Same Row
            if (j > 0) tmp.push_back(heightmap[i][j-1]);
            if (j < row.size()-1) tmp.push_back(heightmap[i][j+1]);

            // Row below
            if (i < heightmap.size()-1) {
                if (j > 0) tmp.push_back(heightmap[i+1][j-1]);
                tmp.push_back(heightmap[i+1][j]);
                if (j < row.size()-1) tmp.push_back(heightmap[i+1][j+1]);
            }

            if (std::all_of(tmp.cbegin(), tmp.cend(), [cur](int x){ return cur < x; })) {
                sum += (cur + 1);
                size_t basinsz = find_basin_size(heightmap, point{j, i});
                basin_sizes.push_back(basinsz);
            }
        }
    }

    std::cout << "Part 1: " << sum << '\n';

    // Part 2
    std::sort(basin_sizes.begin(), basin_sizes.end());
    size_t p2 = basin_sizes.back() * basin_sizes[basin_sizes.size()-2]
        * basin_sizes[basin_sizes.size()-3];

    std::cout << "Part 2: " << p2 << '\n';

    return 0;
}
