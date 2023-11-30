#include <algorithm>
#include <charconv>
#include <cstdint>
#include <iostream>
#include <string>
#include <vector>

struct line_segment {
    size_t x1, y1, x2, y2;

    line_segment(size_t x1, size_t y1, size_t x2, size_t y2)
        : x1(x1), y1(y1), x2(x2), y2(y2) {}

    friend bool operator<(const line_segment& lhs, const line_segment& rhs) {
        return lhs.x1 < rhs.x1;
    }
};

int main() {

    std::string line;
    std::vector<line_segment> segments;
    size_t max_x = 0, max_y = 0;

    while (std::getline(std::cin, line)) {
        size_t x1,x2,y1,y2;
        char *end = line.data() + line.size();
        auto [snd, ec1] { std::from_chars(line.data(), end, x1) };
        // Skip ","
        auto [third, ec2] { std::from_chars(snd+1, end, y1) };
        // skip " -> "
        auto [fourth, ec3] { std::from_chars(third+4, end, x2) };
        // Skip ","
        std::from_chars(fourth+1, end, y2);
        segments.emplace_back(x1, y1, x2, y2);
        max_x = std::max(std::max(x1, x2), max_x);
        max_y = std::max(std::max(y1, y2), max_y);
    }

    // Part 1

    std::vector<std::vector<uint8_t>> overlaps;
    for (size_t i = 0; i <= max_y; ++i) {
        std::vector<uint8_t> tmp;
        tmp.resize(max_x+1);
        overlaps.push_back(tmp);
    }

    for (auto seg_it = segments.begin(); seg_it != segments.end();) {
        const line_segment &s = *seg_it;

        if (s.x1 == s.x2) {
            for (size_t y = std::min(s.y1, s.y2); y <= std::max(s.y1, s.y2); ++y) {
                ++overlaps[y][s.x1];
            }
            seg_it = segments.erase(seg_it);
        } else if (s.y1 == s.y2) {
            for (size_t x = std::min(s.x1, s.x2); x <= std::max(s.x1, s.x2); ++x) {
                ++overlaps[s.y1][x];
            }
            seg_it = segments.erase(seg_it);
        } else {
            ++seg_it;
        }
    }

    // count overlaps
    int count = 0;
    for (auto row : overlaps) {
        count += std::count_if(row.cbegin(), row.cend(), [](uint8_t cnt){ return cnt > 1; });
    }

    std::cout << "Part 1: " << count << '\n';

    // Part 2
    for (auto s : segments) {
        size_t xstart, xend, y;
        int step = 1;
        if (s.x1 < s.x2) {
            xstart = s.x1;
            xend = s.x2;
            y = s.y1;
            step = (s.y1 < s.y2) ? 1 : -1;
        } else {
            xstart = s.x2;
            xend = s.x1;
            y = s.y2;
            step = (s.y2 < s.y1) ? 1 : -1;
        }

        for (size_t x = xstart; x <= xend; ++x) {
            ++overlaps[y][x];
            y = static_cast<size_t>(static_cast<long>(y) + step);
        }
    }

    count = 0;
    for (auto row : overlaps) {
        count += std::count_if(row.cbegin(), row.cend(), [](uint8_t cnt){ return cnt > 1; });
    }

    std::cout << "Part 2: " << count << '\n';

    return 0;
}
