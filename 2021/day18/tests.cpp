#include <sstream>

#include "../include/catch.hpp"

#include "./day18.hpp"

using Catch::Matchers::Equals;

Token mk(TokenType tt, int i) { return std::make_pair(tt, i); }

namespace Catch {
    template<>
    struct StringMaker<Token> {
        static std::string convert( Token const& v ) {
            std::string s;
            if (v.first == LEVEL) {
                s += "(LVL,";
            } else {
                s += "(NUM,";
            }

            s += std::to_string(v.second);
            s += ")";

            return s;
        }
    };
}

TEST_CASE( "Parser test", "[parse_input]" ) {
    std::istringstream in{"[1,2]"};
    REQUIRE_THAT(parse_input(in),
        Equals(vector<Token>{ mk(LEVEL, 1), mk(NUM, 1), mk(NUM, 2), mk(LEVEL, 0) }));

    in.str("[9,[8,7]]");
    in.clear();
    REQUIRE_THAT(parse_input(in),
        Equals(vector<Token>{ mk(LEVEL, 1), mk(NUM, 9), mk(LEVEL, 2), mk(NUM, 8), mk(NUM, 7),
            mk(LEVEL, 1), mk(LEVEL, 0)}));

    in.str("[[9,[[5,3],3]],9]");
    in.clear();
    {
        vector<Token> tmp = parse_input(in);
        REQUIRE( tmp.size() == 13 );
        REQUIRE( tmp.back() == mk(LEVEL, 0) );
        REQUIRE( tmp[4] == mk(LEVEL, 4) );
    }

    in.str("[1,2][3,4]");
    in.clear();
    REQUIRE_THAT(parse_input(in),
        Equals(vector<Token>{ mk(LEVEL, 1), mk(NUM, 1), mk(NUM, 2), mk(LEVEL, 0),
            mk(LEVEL, 1), mk(NUM, 3), mk(NUM, 4), mk(LEVEL, 0)}));
}

TEST_CASE( "Explode snail number - inc num right", "[explode]" ) {
    std::istringstream in{"[[[[[9,8],1],2],3],4]"}, out{"[[[[0,9],2],3],4]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    explode(in_vec, 0, last, 5);
    REQUIRE_THAT(
        in_vec,
        Equals(parse_input(out))
    );
}

TEST_CASE( "Explode snail number - inc num left", "[explode]" ) {
    std::istringstream in{"[7,[6,[5,[4,[3,2]]]]]"}, out{"[7,[6,[5,[7,0]]]]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    explode(in_vec, 0, last, 9);
    REQUIRE_THAT(
        in_vec,
        Equals(parse_input(out))
    );
}

TEST_CASE( "Explode snail number - inc far right", "[explode]" ) {
    std::istringstream in{"[[6,[5,[4,[3,2]]]],1]"}, out{"[[6,[5,[7,0]]],3]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    explode(in_vec, 0, last, 8);
    REQUIRE_THAT(
        in_vec,
        Equals(parse_input(out))
    );
}

TEST_CASE( "Explode snail number - explode right", "[explode]" ) {
    std::istringstream in{"[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"},
        out{"[[3,[2,[8,0]]],[9,[5,[7,0]]]]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    explode(in_vec, 11, last, 18);
    REQUIRE_THAT(
        in_vec,
        Equals(parse_input(out))
    );
}

TEST_CASE( "split", "[split]" ) {
    vector<Token> in{mk(LEVEL, 1), mk(NUM, 10), mk(NUM, 3), mk(LEVEL, 0)};
    vector<Token> out{mk(LEVEL, 1), mk(LEVEL, 2), mk(NUM, 5), mk(NUM, 5), mk(LEVEL, 1), mk(NUM, 3), mk(LEVEL, 0)};
    split(in, 1, 1);
    REQUIRE_THAT(in, Equals(out));
}

TEST_CASE( "reduce") {
    std::istringstream in{"[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"},
        out{"[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    reduce(in_vec, 0, last);
    REQUIRE_THAT(in_vec, Equals(parse_input(out)));
}

TEST_CASE( "reduce 2") {
    std::istringstream in{"[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"},
        out{"[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    reduce(in_vec, 0, last);
    REQUIRE_THAT(in_vec, Equals(parse_input(out)));
}

TEST_CASE( "add 1") {
    std::istringstream in{"[[[[4,3],4],4],[7,[[8,4],9]]]\n[1,1]"},
        out{"[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"};
    vector<Token> in_vec = parse_input(in);
    add(in_vec, 0, in_vec.size()-1);
    REQUIRE_THAT(in_vec, Equals(parse_input(out)));
}

TEST_CASE( "sum") {
    std::istringstream in{"[[[[4,3],4],4],[7,[[8,4],9]]]\n[1,1]"};
    vector<Token> in_vec = parse_input(in);
    REQUIRE(sum(in_vec) == 1384);
}

TEST_CASE( "sum longer") {
    std::istringstream in{"[1,1]\n[2,2]\n[3,3]"};
    vector<Token> in_vec = parse_input(in);
    REQUIRE(sum(in_vec) == 135);
}

TEST_CASE( "sum 2") {
    std::istringstream in{"[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"},
        out{"[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"};
    vector<Token> in_vec = parse_input(in);
    sum(in_vec);
    REQUIRE_THAT(in_vec, Equals(parse_input(out)));
}

TEST_CASE( "magnitude 1") {
    std::istringstream in{"[[9,1],[1,9]]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    REQUIRE( magnitude(in_vec, 0, last) == 129);
}

TEST_CASE( "magnitude 2") {
    std::istringstream in{"[[1,2],[[3,4],5]]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    REQUIRE( magnitude(in_vec, 0, last) == 143);
}

TEST_CASE( "magnitude 3") {
    std::istringstream in{"[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    REQUIRE( magnitude(in_vec, 0, last) == 1384);
}

TEST_CASE( "magnitude 4") {
    std::istringstream in{"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"};
    vector<Token> in_vec = parse_input(in);
    size_t last = in_vec.size()-1;
    REQUIRE( magnitude(in_vec, 0, last) == 3488);
}