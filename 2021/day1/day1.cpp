#include <iostream>
#include <iterator>
#include <numeric>
#include <vector>

using vec_input_it = std::vector<int>::const_iterator;

int count_increases(vec_input_it begin, vec_input_it end, ptrdiff_t window=1) {
    return std::transform_reduce(begin, end - window,
        begin + window, 0,
        std::plus<>(),
        std::less<>());
}

int main() {

    std::vector<int> inp{std::istream_iterator<int>(std::cin), std::istream_iterator<int>()};

    std::cout << "Part 1: " << count_increases(inp.cbegin(), inp.cend()) << '\n';
    std::cout << "Part 2: " << count_increases(inp.cbegin(), inp.cend(), 3) << '\n';

    return 0;
}
