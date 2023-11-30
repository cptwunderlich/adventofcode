#include <algorithm>
#include <iostream>
#include <iterator>
#include <numeric>
#include <string>
#include <sstream>
#include <vector>

#include "../include/aoc.hpp"

using board = std::vector<std::vector<int>>;

int score_board(const board &b) {
    int res = 0;
    for (auto row : b) {
        res += std::accumulate(row.cbegin(), row.cend(), 0,
            [](int acc, int x){ return (x <= 0) ? acc : acc + x; });
    }

    return res;
}

void mark(std::vector<board> &boards, int num) {
    for (board &b : boards) {
        for (auto &row : b) {
            // mark
            std::replace(row.begin(), row.end(), num, -1);
        }
    }
}

int check_and_remove(std::vector<board> &boards, int num) {
    // Check boards
    for (auto board_it = boards.begin(); board_it != boards.end(); ++board_it) {
        board &b = *board_it;

        // Check columns
        for (size_t col = 0; col < b[0].size(); ++col) {
            bool match = true;
            for (size_t row = 0; row < b.size(); ++row) {
                // Check row
                if (std::all_of(b[row].cbegin(), b[row].cend(), [](int x){ return x == -1; })) {
                    match = true;
                    break;
                }

                if (b[row][col] != -1) {
                    match = false;
                }
            }

            if (match) {
                int score = score_board(b) * num;
                boards.erase(board_it);
                return score;
            }
        }
    }

    return -1;
}

int part1(std::vector<int> &nums, std::vector<board> &boards) {
    for (auto num_it = nums.begin(); num_it != nums.end(); ++num_it) {
        int num = *num_it;
        // Mark all boards
        mark(boards, num);

        // Check boards
        int score = check_and_remove(boards, num);
        if (score >= 0) {
            nums.erase(nums.begin(), num_it);
            return score;
        }
    }

    return -1;
}

int part2(std::vector<int> &nums, std::vector<board> &boards) {
    int res = -1;
    for (auto num_it = nums.begin(); num_it != nums.end() && !boards.empty(); ++num_it) {
        mark(boards, *num_it);
        int score = 0;
        while ((score = check_and_remove(boards, *num_it)) >= 0) {
            res = score;
        }
    }

    return res;
}

int main() {

    std::string line;
    std::getline(std::cin, line);
    auto num_strs = aoc::split(line, ',');
    std::vector<int> nums(num_strs.size());
    std::transform(num_strs.begin(), num_strs.end(), nums.begin(),
        [](const std::string &s){ return std::stoi(s);});

    // Read boards
    std::vector<board> boards;

    while (std::cin) {
        // Skip empty line
        std::getline(std::cin, line);

        board b;
        for (int i = 0; i < 5; ++i) {
            std::getline(std::cin, line);
            if (line.empty()) break;
            std::istringstream ss(line);
            b.emplace_back(std::istream_iterator<int>{ss}, std::istream_iterator<int>{});
        }

        if (!b.empty()) boards.push_back(b);
    }

    // Part 1
    std::cout << "Part 1: " << part1(nums, boards) << '\n';

    // Part 2
    std::cout << "Part 2: " << part2(nums, boards) << '\n';

    return 0;
}
