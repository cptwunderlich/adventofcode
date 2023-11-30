#include <iostream>
#include <string>
#include <vector>

int main() {

    std::string cmd;
    int steps;
    int h = 0, d = 0;
    int h2 = 0, d2 = 0, aim = 0;

    while (std::cin >> cmd >> steps) {
        if (cmd == "forward") {
            h += steps;
            h2 += steps;
            d2 += (steps * aim);
        } else if (cmd == "down") {
            d += steps;
            aim += steps;
        } else if (cmd == "up") {
            d -= steps;
            aim -= steps;
        }
    }

    std::cout << "Part 1: " << h*d << '\n'
        << "Part 2: " << h2*d2 << '\n';

    return 0;
}
