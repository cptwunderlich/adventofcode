#include <vector>
#include <iostream>
#include <iterator>
#include <fstream>
#include <functional>

/* Advent of Code Day 5
 * (improved version using istream_iterator to initialize vector,
 * inspiration taken from Reddit's u/willkill07)
 *
 * Benjamin Maurer (maurer.benjamin@gmail.com)
 * GitHub: github.com/cptwunderlich
 */

using intvec = std::vector<int>;

struct state {
    intvec v;
    int ip = 0;
    int steps = 0;

    state(intvec v_arg) : v(v_arg) {}
};

bool step(state &st, std::function<int(int, intvec&)> f) {
    bool cont{st.ip >= 0 && st.ip < st.v.size()};
    if (cont) {
        st.ip += f(st.ip, st.v);
        st.steps++;
    }

    return cont;
}

int main() {
    std::ifstream f{"./input.txt"};
    std::vector<int> v {std::istream_iterator<int>{f}, {}};
    f.close();

    // Make two copies of v, as it will be modified
    state st1{v};
    state st2{v};
    v.clear();

    while ( step(st1, [](int i, intvec &vec) { return vec[i]++; }) ) {}

    while ( step(st2, [](int i, intvec &vec) { return (vec[i] >= 3)? vec[i]-- : vec[i]++; }) ) {}

    std::cout << "Steps part1: " << st1.steps << "\nSteps part2: " << st2.steps << std::endl;

    return 0;
}

