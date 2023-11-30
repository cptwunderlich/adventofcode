#include <array>
#include <cassert>
#include <cmath>
#include <iterator>
#include <iostream>
#include <regex>
#include <string>

using std::string;

struct Target {
    long xmin, xmax, ymin, ymax;
};

long gausssum(long n) {
    return (n*(n+1))/2;
}

long min_xspeed(long target_x_min) {
    return std::floor(std::sqrt(2*target_x_min));
}

bool check_hit(const Target &target, long vx, long vy) {

    for (long xpos=0, ypos=0; xpos <= target.xmax && ypos >= target.ymax;) {
        if (target.xmin <= xpos && xpos <= target.xmax
            && target.ymin >= ypos && ypos >= target.ymax)
        {
            return true;
        }

        xpos += vx;
        ypos += vy;

        vx = (vx == 0) ? 0 : (vx-1);
        --vy;
    }

    return false;
}

size_t speed_combos(const Target &target, long y_speed_max) {
    // x from sqrt(2*xmin) - xmax
    // y from ymax - from part 1
    // enumerate all and check
    size_t cnt = 0;
    for (long vx = min_xspeed(target.xmin); vx <= target.xmax; ++vx) {
        for (long vy = target.ymax; vy <= y_speed_max; ++vy) {
            if (check_hit(target, vx, vy)) {
                //std::cout << '(' << vx << ',' << vy << ")\n";
                ++cnt;
            }
        }
    }

    return cnt;
}

int main() {

    std::string line;
    std::getline(std::cin, line);

    std::regex nums_pattern("-?[0-9]+");
    auto nums_begin =
        std::sregex_iterator(line.begin(), line.end(), nums_pattern);
    auto nums_end = std::sregex_iterator();
    std::array<long, 4> nums;
    size_t i=0;
    for (auto it = nums_begin; it != nums_end; ++it, ++i) {
        assert(i < 4);
        std::smatch match = *it;
        std::string match_str = match.str();
        nums[i] = std::stol(match_str);
    }

    // annoyingly, y-coords are in order of less-than,
    // not distance from 0.
    Target target{nums[0], nums[1], nums[3], nums[2]};

    // Part 1
    long yspeed = std::abs(target.ymax)-1;
    long zenith = gausssum(yspeed);

    std::cout << "Part 1: " << zenith << '\n';

    // Part 2
    std::cout << "Part 2: " << speed_combos(target, yspeed) << '\n';

    return 0;
}
