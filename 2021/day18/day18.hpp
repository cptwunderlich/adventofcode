#pragma once

#include <istream>
#include <utility>
#include <vector>

enum TokenType {
    LEVEL, NUM
};

using std::vector;
using Token = std::pair<TokenType, int>;

vector<Token> parse_input(std::istream &is);
void explode(vector<Token> &vec, size_t begin, size_t &last, size_t cur);
void split(vector<Token> &vec, int lvl, size_t pos);
void reduce(vector<Token> &vec, size_t begin, size_t &last);
void add(vector<Token> &vec, size_t begin, size_t end);
int sum(vector<Token> &nums);
int magnitude(vector<Token> &vec, size_t begin, size_t &end);