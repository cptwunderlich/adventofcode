#include <algorithm>
#include <cmath>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

long calc_fuel2(const std::vector<int> &positions, long avg) {
    return std::accumulate(positions.cbegin(), positions.cend(), 0L,
        [avg](long acc, int r){
            long steps = std::abs(static_cast<long>(r) - avg);
            long fuel = (steps * (steps+1)) / 2;
            return acc + fuel;
    });
}

int main() {

    std::string token;
    std::vector<int> positions;
    while (std::getline(std::cin, token, ',')) {
        positions.push_back(std::stoi(token));
    }

    // Part 1: Distance from median
    std::sort(positions.begin(), positions.end());

    int median = positions[positions.size()/2];
    // std::cout << "Median: " << median << "\n";

    int fuel = std::accumulate(positions.cbegin(), positions.cend(), 0,
        [median](int acc, int r){ return acc + std::abs(r - median); });

    std::cout << "Part 1: " << fuel << '\n';

    // Part 2: Calc average, sum distance from gaussian sum of pos
    // to average. Try with both floored and ceiled avg to find minimum.
    long sum = std::accumulate(positions.cbegin(), positions.cend(), 0L);
    double avg = static_cast<double>(sum) / positions.size();
    long upper_limit = std::ceil(avg);
    long lower_limit = std::floor(avg);
    // std::cout << "avg/upper/lower: " << avg << '/' << upper_limit << '/' << lower_limit << '\n';

    long fuel2 = std::min(
        calc_fuel2(positions, upper_limit),
        calc_fuel2(positions, lower_limit));

    std::cout << "Part 2: " << fuel2 << '\n';

    return 0;
}
