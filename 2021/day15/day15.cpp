#include <algorithm>
#include <cstdint>
#include <iostream>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>

struct node {
    size_t x, y;
    uint8_t weight;

    node() : x(0), y(0), weight(0) {}

    node(size_t x, size_t y, uint8_t weight) : x(x), y(y), weight(weight) {}

    bool operator>(const node &other) const noexcept {
        return this->weight > other.weight;
    }

    bool operator==(const node &other) const noexcept {
        return this->x == other.x && this->y == other.y;
    }
};

template<>
struct std::hash<node> {
    std::size_t operator()(const node &n) const noexcept {
        return n.x ^ (n.y << 1);
    }
};

struct msize {
    size_t x, y;
};

using std::vector;
using matrix = vector<vector<uint8_t>>;
using cost_map = std::unordered_map<node, size_t>;

// Admissible heuristic should return cost always less-equal
// to actual cost. So we take manhattan distance with cost of 1
// per move (minimal cost).
size_t estimate_rest(const node &from, const node &to) {
    return (to.x - from.x) + (to.y - from.y);
}

uint8_t weight(const matrix &map, size_t x, size_t y) {
    size_t yorig = y % map.size();
    size_t xorig = x % map[yorig].size();
    uint8_t w = map[yorig][xorig];

    uint8_t yadd = static_cast<uint8_t>(y / map.size());
    uint8_t xadd = static_cast<uint8_t>(x / map[yorig].size());
    uint8_t sum = w + yadd + xadd;

    return (sum > 9) ? sum % 10 + 1 : sum;
}

vector<node> neighbors(const matrix &map, msize sz, const node &n) {
    vector<node> res;
    if (n.y != 0) res.emplace_back(n.x, n.y-1, weight(map, n.y-1, n.x));
    if (n.x != 0) res.emplace_back(n.x-1, n.y, weight(map, n.y, n.x-1));
    if (n.y < sz.y-1) res.emplace_back(n.x, n.y+1, weight(map, n.y+1, n.x));
    if (n.x < sz.x-1) res.emplace_back(n.x+1, n.y, weight(map, n.y, n.x+1));

    return res;
}

size_t cost_or_default(const cost_map &costs, const node &n) {
    auto it = costs.find(n);
    if (it != costs.end()) return it->second;
    else return std::numeric_limits<size_t>::max();
}

size_t astar(const matrix &map, msize sz) {
    vector<node> open;
    const node start{0, 0, map[0][0]};
    size_t xlast = sz.x-1;
    size_t ylast = sz.y-1;
    const node goal{xlast, ylast, weight(map, xlast, ylast)};
    open.push_back(start);

    cost_map from_start;
    from_start[start] = 0;

    cost_map to_goal;
    to_goal[start] = estimate_rest(start, goal);

    auto fscore = [&to_goal](const node &l, const node &r){
        return cost_or_default(to_goal, l) > cost_or_default(to_goal, r);
    };

    while (!open.empty()) {
        std::pop_heap(open.begin(), open.end(), fscore);
        node cur = open.back();

        if (cur == goal) {
            return from_start[goal];
        }

        open.pop_back();
        size_t cur_cost = cost_or_default(from_start, cur);
        for (node &n : neighbors(map, sz, cur)) {
            size_t tmp_cost = cur_cost + n.weight;
            if (tmp_cost < cost_or_default(from_start, n)) {
                from_start[n] = tmp_cost;
                to_goal[n] = tmp_cost + estimate_rest(n, goal);
                if (std::find(open.cbegin(), open.cend(), n) == open.cend()) {
                    open.push_back(n);
                    std::push_heap(open.begin(), open.end(), fscore);
                }
            }
        }
    }

    return std::numeric_limits<size_t>::max();
}

int main() {

    std::string line;
    matrix map;
    while (std::getline(std::cin, line)) {
        // Interpret single digit chars as int and subtract ASCII offset
        map.emplace_back(line.data(), line.data() + line.size());
        std::for_each(map.back().begin(), map.back().end(),
            [](uint8_t &i){ i -= 48; });
    }

    std::cout << "Part 1: " << astar(map, {map.size(), map[0].size()}) << '\n';

    // Part 2
    std::cout << "Part 2: " << astar(map, {map.size()*5, map[0].size()*5}) << '\n';

    return 0;
}
