#include <vector>
#include <iostream>
#include <fstream>
#include <unordered_map>
#include <algorithm>

/* Advent of Code Day 7
 *
 * This code assumes well-formed input.
 *
 * Benjamin Maurer (maurer.benjamin@gmail.com)
 * GitHub: github.com/cptwunderlich
 */

using strsize = std::string::size_type;

std::unordered_map<std::string, size_t> names{};
std::vector<std::vector<bool> > adjmatrix{};
std::vector<int> weights{};

std::string find_name(size_t val) {
    for (auto entry : names) {
        if (entry.second == val) return entry.first;
    }

    return "";
}

std::pair<size_t, std::string> find_root() {
    for (size_t col=0; col < adjmatrix[0].size(); ++col) {
        size_t row;
        for (row=0; row < adjmatrix.size(); ++row) {
            if (adjmatrix[row][col]) break;
        }

        if (row >= adjmatrix.size()) return std::make_pair(col, find_name(col));
    }

    return std::make_pair(std::string::npos, "ERROR");
}

size_t insert(size_t &i, const std::string &name, int weight, ssize_t parent) {
    auto num = i;
    auto p = names.insert({name, num});
    if (p.second) {
        i++;
    
        if (weights.size() <= num) {
            weights.resize(num+1);
        }

        if (adjmatrix.size() <= num) {
            adjmatrix.resize(num+1);
            for (auto &v : adjmatrix) {
                v.resize(num+1);
            }
        }
    }
    
    num = p.first->second;
    if (weights[num] < weight) weights[num] = weight;
    if (parent >= 0) {
        adjmatrix[parent][num] = true;
    }

    return num;
}

void parseLine(const std::string &s) {
    static size_t i = 0;

    strsize pos = s.find(' ');
    std::string node = s.substr(0, pos);
    pos += 2;
    strsize pos2 = s.find(')', pos);
    std::string weight = s.substr(pos, pos2-(pos));

    size_t p = insert(i, node, std::stoi(weight), -1);

    //std::cout << "Node: " << node << " weight: " << weight;
    
    pos = s.find("->", pos2);
    if (pos != std::string::npos) {
        std::string neighbor;

        while (pos2 < s.length()) {
            pos = s.find(' ', pos);
            pos2 = s.find(',', pos);
            if (pos2 == std::string::npos) pos2 = s.length();
            pos++;
            neighbor = s.substr(pos, (pos2)-pos);
            insert(i, neighbor, 0, p);
            pos = pos2;
        }
    }

    //std::cout << '\n';
}

void print_mat() {
    std::cout << "\n\n\n";

    for (size_t i = 0; i < adjmatrix.size(); ++i) {
        std::cout << find_name(i) << " " << i << " ";
    
        for (bool c : adjmatrix[i]) {
            std::cout << ((c)? 1 : 0) << " ";
        }
        std::cout << "\n";
    }
}

void print_weights() {
    std::cout << "\n\n";
    for (size_t i=0; i < weights.size(); ++i) {
        std::cout << find_name(i) << " w: " << weights[i] << "\n";
    }
}

int sum(size_t pos) {
    int res = 0;
    std::vector<std::pair<size_t, int> > vals{};

    for (size_t i=0; i < adjmatrix[pos].size(); ++i) {
        if (adjmatrix[pos][i]) vals.emplace_back(std::make_pair(i, sum(i)));
    }

    if (vals.size() >= 3) {
        std::sort(begin(vals), end(vals), 
            [](std::pair<size_t, int> a, std::pair<size_t, int> b)
            { return a.second > b.second;});
        if (vals.front().second != vals.back().second) {
            if (vals[0].second != vals[1].second) {
                std::cout << "weight: " 
                    << (vals[1].second - vals[0].second + weights[vals[0].first]) 
                    << "\n";
            } else {
                std::cout << "weight: " 
                    << (vals[0].second - vals.back().second + weights[vals.back().first]) 
                    << "\n";
            }  
        }

        for (auto p : vals) {
            res += p.second;
        }
    }

    return res + weights[pos];
}

int calc_corrected_weight(size_t root) {
    std::vector<std::pair<size_t, int> > vals{};
    vals.reserve(3);

    for (size_t i=0; i < adjmatrix[root].size(); ++i) {
        if (adjmatrix[root][i]) {
            int s = sum(i);
            if (vals.size() < 2) {
                vals.emplace_back(std::make_pair(i, s));
            } else {    
                if (vals[0].second == vals[1].second) {
                    if (vals[0].second != s) {
                        return vals[0].second - s + weights[i];
                    }
                } else {
                    if (vals[0].second != s) {
                        return s - vals[0].second + weights[vals[0].first];
                    } else {
                        return s - vals[1].second + weights[vals[1].first];
                    }
                }
            }
        }
    }

    return -1; 
}

int main() {
    std::ifstream f{"./input.txt"};
    for (std::string s; std::getline(f, s);) {
        parseLine(s);
    }
    f.close();

    auto root = find_root();
    std::cout << "Root: " << root.second << '\n';

    std::cout << "Weight: " << calc_corrected_weight(root.first) << std::endl;
    
    //print_mat();
   // print_weights();

    return 0;
}
