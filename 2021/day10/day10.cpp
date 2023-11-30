#include <algorithm>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

using std::vector;
using std::string;
using ulong = unsigned long;

bool open(char c) {
    return (c == '(' || c == '[' || c == '<' || c == '{');
}

char get_closing(char open) {
    switch (open) {
    case '{':
        return '}';

    case '[':
        return ']';

    case '(':
        return ')';

    case '<':
        return '>';

    default:
        return 'x';
    }
}

bool match(char open, char close) {
    return get_closing(open) == close;
}

int points1(char c) {
    switch (c) {
    case ')': return 3;
    case ']': return 57;
    case '}': return 1197;
    case '>': return 25137;

    default:
        return 0;
    }
}

ulong points2(char c) {
    switch (c) {
    case '(': return 1;
    case '[': return 2;
    case '{': return 3;
    case '<': return 4;

    default:
        return 0;
    }
}

int main() {

    string line;
    vector<ulong> scores;
    int sum = 0;
    while (std::getline(std::cin, line)) {
        vector<char> stack;
        bool corrupted = false;

        for (char c : line) {
            if (open(c)) {
                stack.push_back(c);
            } else {
                char other = stack.back();
                stack.pop_back();
                if (!match(other, c)) {
                    sum += points1(c);
                    corrupted = true;
                    break;
                }
            }
        }

        if (!corrupted) {
            ulong score = 0;
            while (!stack.empty()) {
                char c = stack.back();
                stack.pop_back();
                score *= 5;
                score += points2(c);
            }

            scores.push_back(score);
        }
    }

    std::cout << "Part 1: " << sum << '\n';

    // Part 2
    size_t m = static_cast<size_t>(scores.size()/2);
    auto middle = scores.begin() + static_cast<ptrdiff_t>(scores.size()/2);
    std::nth_element(scores.begin(), middle, scores.end());

    std::cout << "Part 2: " << scores[m] << '\n';

    return 0;
}
