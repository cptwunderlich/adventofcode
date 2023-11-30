#include <algorithm>
#include <cmath>
#include <iostream>
#include <iterator>
#include <numeric>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

#include "Eigen/Dense"

using namespace std;
using countmap = std::unordered_map<int, int>;
using Matrix4xXi = Eigen::Matrix<int, 4, Eigen::Dynamic>;
using Eigen::MatrixXi;
using Eigen::Matrix4i;
using Eigen::Vector4i;

constexpr float pi_half = 1.57079f;
constexpr int min_overlap = 12;
const Eigen::Matrix4i none = Matrix4i::Zero();

int roundi(float f) {
    return static_cast<int>(lroundf(f));
}

vector<Matrix4i> make_rotations() {
    vector<Matrix4i> res;
    float t = M_PI / 2;

    for (int i = 0; i < 4; ++i) {
        // Y axis
        Matrix4i rotY{
            {roundi(cos(t*i)), 0, roundi(sin(t*i)), 0},
            {0, 1, 0, 0},
            {-roundi(sin(t*i)), 0, roundi(cos(t*i)), 0},
            {0, 0, 0, 1} };

        for (int j = 0; j < 4; ++j) {
            Matrix4i rotZ{
                {roundi(cos(t*j)), -roundi(sin(t*j)), 0, 0},
                {roundi(sin(t*j)), roundi(cos(t*j)), 0, 0},
                {0, 0, 1, 0},
                {0, 0, 0, 1} };

            res.push_back(rotY * rotZ);
        }

        for (int j = 1; j < 4; j+=2) {
            Matrix4i rotX{ {1, 0, 0, 0},
                {0, roundi(cos(t*j)), -roundi(sin(t*j)), 0},
                {0, roundi(sin(t*j)), roundi(cos(t*j)), 0},
                {0, 0, 0, 1} };

            res.push_back(rotX * rotY);
        }
    }

    return res;
}

MatrixXi distance(const MatrixXi &base, const MatrixXi &other) {
    MatrixXi dists{base.cols(), other.cols()};

    for (long i = 0; i < base.cols(); ++i) {
        dists.col(i) =
            (other.colwise() - base.col(i)).matrix().colwise().norm();
    }

    return dists;
}

countmap bincount(const MatrixXi &m) {
    countmap res;
    std::for_each(m.reshaped().cbegin(), m.reshaped().cend(), [&res](int i){ ++res[i]; });
    return res;
}

std::pair<int, int> max_count(const countmap &m) {
    if (m.empty()) return {-1, -1};

    auto res = std::max_element(m.cbegin(), m.cend(),
        [](const auto &l, const auto& r){ return l.second < r.second; });

    return *res;
}

Matrix4i find_translation(const Matrix4xXi &base, const Matrix4xXi &other, const MatrixXi &dist,
    int most_freq_dist)
{
    for (Eigen::Index i = 0; dist.cols(); ++i) {
        for (Eigen::Index j = 0; dist.rows(); ++j) {
            if (dist(j, i) == most_freq_dist) {
                Vector4i diff = base.col(i) - other.col(j);
                Matrix4i tx{ {1, 0, 0, diff(0)},
                             {0, 1, 0, diff(1)},
                             {0, 0, 1, diff(2)},
                             {0, 0, 0, 1} };
                Matrix4xXi other_trans = tx * other;
                MatrixXi dist_trans = distance(base, other_trans);
                int matches = (dist_trans.array() == 0).count();
                if (matches >= min_overlap) {
                    return tx;
                }
            }
        }
    }

    return none;
}

size_t find_unique_beacons(const vector<Matrix4xXi> &scans) {
    const Matrix4xXi &base = scans.front();
    const vector<Matrix4i> rotations = make_rotations();
    vector<Vector4i> beacon_vecs;

    for (auto it = std::next(scans.cbegin()); it != scans.cend(); ++it) {
        const Matrix4xXi &beacons = *it;

        for (const Matrix4i &rot : rotations) {
            const Matrix4xXi beacons_rot = rot * beacons;
            const MatrixXi dists = distance(base, beacons_rot);
            auto dist_counts = bincount(dists);
            auto mc = max_count(dist_counts);
            if (mc.second >= min_overlap) {
                const Matrix4i tx = find_translation(base, beacons_rot, dists, mc.first);
                if (tx != none) {
                    const Matrix4xXi beacons_tx = tx * beacons_rot;
                    for (Vector4i v : beacons_tx.colwise()) {
                        beacon_vecs.push_back(v);
                    }

                    break; // Stop iterating rotations
                }
            }
        }
    }

    std::sort(beacon_vecs.begin(), beacon_vecs.end(),
        [](const Vector4i &lhs, const Vector4i &rhs) {
            return std::tie(lhs(0), lhs(1), lhs(2))
                < std::tie(rhs(0), rhs(1), rhs(2));
    });
    std::unique(beacon_vecs.begin(), beacon_vecs.end());

    return beacon_vecs.size();
}

#ifndef CATCH_CONFIG_MAIN
int main() {

    std::string line;
    vector<Matrix4xXi> scans;
    while (std::getline(std::cin, line)) {
        if (line.size() > 1 && line[0] == '-') {
            vector<int> coeffs;
            while (std::getline(std::cin, line) && !line.empty()) {
                size_t start = 0;
                size_t end = line.find(',');
                int x = std::stoi(line.substr(0, end));
                coeffs.push_back(x);
                start = end+1;
                end = line.find(',', start);
                int y = std::stoi(line.substr(start, end));
                coeffs.push_back(y);
                start = end+1;
                int z = std::stoi(line.substr(start));
                coeffs.push_back(z);
                coeffs.push_back(1);
            }

            scans.emplace_back(Eigen::Map<Matrix4xXi>(coeffs.data(), 4, coeffs.size()/4));
            scans.back().transpose();
        }
    }

    size_t unique_beacons = find_unique_beacons(scans);

    std::cout << "Part 1: " << unique_beacons << '\n';

    // Part 2
    std::cout << "Part 2: " << "" << '\n';

    return 0;
}
#endif
