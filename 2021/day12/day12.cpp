#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using std::string;
using uset = std::unordered_set<int>;
using umap = std::unordered_map<int, uset>;

constexpr int start = 0x7B;
constexpr int end = 0x7C;

int id_to_int(const std::string &s) {
    if (s == "start") {
        return start;
    } else if (s == "end") {
        return end;
    } else {
        int res = 0;
        for (size_t i = 0; i < s.size(); ++i) {
            char c = s[i];
            res |= static_cast<int>(c) << (8*i);
        }

        return res;
    }
}

string int_to_id(int i) {
    if (i == start) {
        return "start";
    } else if (i == end) {
        return "end";
    } else {
        string s{""};

        for (int i = 0; true; ++i) {
            int b = i & (0xFF << (8*i));
            if (b == 0) return s;
            s += static_cast<char>(b);
        }
    }
}

bool lower(int i) {
    return (i & 0xFF) >= 0x61;
}

void print_path(const std::vector<int> &v) {
    for (int i : v) {
        std::cout << int_to_id(i) << ", ";
    }

    std::cout << '\n';
}

size_t dfs(umap &g, int cur, size_t cnt, uset &visited,
    /* Debug std::vector<int> path, */
    bool duplicates)
{
    // Debug path.push_back(cur);
    visited.insert(cur);

    if (cur == end) {
        // Debug print_path(path);
        return ++cnt;
    }

    for (int n : g[cur]) {
        if (!lower(n) || visited.count(n) == 0) {
            cnt = dfs(g, n, cnt, visited, /*path,*/ duplicates);

            if (n != start) visited.erase(n);
        } else if (duplicates && n != start) {
            cnt = dfs(g, n, cnt, visited, /*path,*/ false);
        }
    }

    return cnt;
}

int main() {

    std::string line;
    umap g;
    while (std::getline(std::cin, line)) {
        auto pos = std::find(line.cbegin(), line.cend(), '-');
        string left{line.cbegin(), pos};
        string right{pos+1, line.cend()};
        int vl = id_to_int(left);
        int vr = id_to_int(right);

        g[vl].insert(vr);
        g[vr].emplace(vl);
    }

    uset visited;

    // Part 1
    std::cout << "Part 1: " << dfs(g, start, 0, visited, false) << '\n';

    // Part 2
    visited.clear();
    std::cout << "Part 2: " << dfs(g, start, 0, visited, true) << '\n';

    return 0;
}
