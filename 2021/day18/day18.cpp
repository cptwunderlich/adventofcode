#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <string>

#include "day18.hpp"

/* This one turned out really ugly.
   But I'm just happy that it works now, I'll just leave it like this... */

using std::string;

vector<Token> parse_input(std::istream &is) {
    std::string line;
    vector<Token> num;
    int lvl = 0;
    while (std::getline(is, line)) {
        for (char c : line) {
            if (c == '[') {
                num.emplace_back(LEVEL, ++lvl);
            } else if (c == ']') {
                num.emplace_back(LEVEL, --lvl);
            } else if (c >= '0' && c <= '9') {
                num.emplace_back(NUM, c - '0');
            }
        }
    }

    return num;
}

void print_num(vector<Token>::const_iterator begin,
               vector<Token>::const_iterator end) {
    int lvl = 0;
    for (; begin != end; ++begin) {
        const Token &t = *begin;
        if (t.first == LEVEL) {
            if (t.second > lvl) {
                std::cout << '[';
            } else {
                std::cout << ']';
            }

            lvl = t.second;
        } else {
            std::cout << t.second << ' ';
        }
    }

    std::cout << '\n';
}

void explode(vector<Token> &vec, size_t begin, size_t &last, size_t cur) {
    // ?, [1,2], ?
    //     ^--cur
    // Find prev number (if any)
    assert(cur != 0);
    assert(vec[cur].first == NUM);
    for (size_t prev = cur-1; prev > begin; --prev) {
        if (vec[prev].first == NUM) {
            vec[prev].second += vec[cur].second;
            break;
        }
    }

    // Move to second number in pair
    // ?, [1,2], ?
    //       ^--cur
    ++cur;
    assert(vec[cur].first == NUM);

    // Find next number (if any)
    for (size_t next = cur+1; next <= last; ++next) {
        if (vec[next].first == NUM) {
            vec[next].second += vec[cur].second;
            break;
        }
    }

    // Replace pair with zero
    // ?, 0, ?
    size_t pair_start = cur-2;
    ++cur;
    assert( vec[pair_start].first == LEVEL && vec[cur].first == LEVEL );
    vec[cur].first = NUM;
    vec[cur].second = 0;
    vec.erase(vec.cbegin()+pair_start, vec.cbegin()+cur);
    last -= 3;
}

void split(vector<Token> &vec, int lvl, size_t pos) {
    assert( vec[pos].first == NUM && vec[pos].second >= 10 );

    float q = vec[pos].second / 2.0f;
    vector<Token> v{{NUM, std::floor(q)}, {NUM, std::ceil(q)}, {LEVEL, lvl}};
    vec[pos].first = LEVEL;
    vec[pos].second = lvl + 1;
    vec.insert(vec.cbegin()+pos+1, v.cbegin(), v.cend());
}

void add(vector<Token> &vec, size_t begin, size_t end) {
    assert(begin < end && end != 0); // Bounds
    assert(vec[begin].first == LEVEL && vec[begin].second == 1);
    assert(vec[end].first == LEVEL && vec[end].second == 0);

    for (size_t i = begin; i <= end; ++i) {
        if (vec[i].first == LEVEL) {
            ++vec[i].second;
        }
    }

    vec.insert(vec.cbegin()+begin, std::make_pair(LEVEL, 1));
    vec.insert(vec.cbegin()+end+2, std::make_pair(LEVEL, 0));
}

void explode_all(vector<Token> &vec, size_t begin, size_t &last) {
    int lvl = 0;
    while (true) {
        size_t i = 0;
        for (i = begin; i <= last; ++i) {
            if (vec[i].first == LEVEL) {
                lvl = vec[i].second;
                continue;
            }

            if (lvl == 5) {
                // Explode
                explode(vec, begin, last, i);
                break; // Start from beginning
            }
        }

        if (i > last) break;
    }
}

void reduce(vector<Token> &vec, size_t begin, size_t &last) {
    assert(vec[begin].first == LEVEL);
    assert(vec[last].first == LEVEL);

    bool reducible = true;
    int lvl = 0;
    while (reducible) {
        explode_all(vec, begin, last);

        size_t i = 0;
        for (i = begin; i <= last; ++i) {
            if (vec[i].first == LEVEL) {
                lvl = vec[i].second;
                continue;
            }

            if (vec[i].second >= 10) {
                // Split
                split(vec, lvl, i);
                last += 3;
                break; // Start from beginning
            }
        }

        if (i > last) reducible = false;
    }
}

size_t find_end(vector<Token> &vec, size_t begin) {
    assert(begin < vec.size());

    size_t end = begin;
    for (size_t j = begin+1; j < vec.size(); ++j) {
            if (vec[j].first == LEVEL && vec[j].second == 0) {
                end = j;
                break;
            }
    }

    return end;
}

int magnitude(vector<Token> &vec, size_t begin, size_t &end) {
    int lvl = 99999;
    for (size_t i = begin; i <= end; ++i) {
        if (vec[i].first == LEVEL) {
            if (vec[i].second < lvl && i > 2) {
                assert(vec[i-2].first == NUM && vec[i-1].first == NUM);
                int mag = 3*vec[i-2].second + 2*vec[i-1].second;
                vec[i-3].first = NUM;
                vec[i-3].second = mag;
                vec.erase(vec.cbegin()+i-2, vec.cbegin()+i+1);
                end -= 3;
                --lvl;
                i -= 3;
            } else {
                lvl = vec[i].second;
            }
        }
    }

    assert( (end - begin) == 0 );
    return vec[begin].second;
}

int sum(vector<Token> &nums) {
    while (nums.size() > 1) {
        size_t i = 0;

        // Find end
        size_t end = find_end(nums, i);
        if (end == i) break;

        reduce(nums, i, end);

        if (end + 1 == nums.size()) break;

        // Find another one to add
        size_t snd_begin = end + 1;
        size_t snd_end = find_end(nums, snd_begin);
        reduce(nums, snd_begin, snd_end);

        add(nums, i, snd_end);
    }

    size_t last = nums.size()-1;
    // print_num(nums.cbegin(), nums.cend());
    return magnitude(nums, 0, last);
}

#ifndef CATCH_CONFIG_MAIN
int main() {

    vector<Token> num = parse_input(std::cin);
    vector<vector<Token>> part2vec;
    vector<std::pair<size_t, size_t>> bounds;
    size_t begin = 0;
    for (size_t i = 0; i < num.size(); ++i) {
        int lvl = 0;
        if (num[i].first == LEVEL) {
            lvl = num[i].second;
            if (lvl == 0) {
                bounds.push_back(std::make_pair(begin, i));
                begin = i+1;
            }
        }
    }

    for (auto &x : bounds) {
        for (auto &y : bounds) {
            if (x == y) continue;
            part2vec.emplace_back(num.cbegin()+x.first, num.cbegin()+x.second+1);
            part2vec.back().insert(part2vec.back().cend(), num.cbegin()+y.first, num.cbegin()+y.second+1);
        }
    }

    std::cout << "Part 1: " << sum(num) << '\n';

    // Part 2
    int mag = 0;
    for (auto &snail_num : part2vec) {
        int tmp = sum(snail_num);
        mag = std::max(mag, tmp);
    }
    std::cout << "Part 2: " << mag << '\n';

    return 0;
}
#endif
