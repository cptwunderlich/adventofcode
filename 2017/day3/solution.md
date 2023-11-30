# Day 3

http://adventofcode.com/2017/day/3

I was too busy to get to it, but then I couldn't sleep and took a look at the
solution thread. I did try to somewhat figure out the solution for part one
and I calculated my result. But I got the idea from someone else.

## Part 1:
    
    17  16  15  14  13
    18   5   4   3  12
    19   6   1   2  11
    20   7   8   9  10
    21  22  23  24  25

Since the lower righter diagonal is the series of perfect uneven squares,
i.e., 1^2, 3^2, 5^2, ..., n^2, we can use that to calculate the distance.
The distance from the uneven square n^2 to the center is n-1.
The length of the edge of the current spiral is n.
We can find the uneven squares smaller and larger then our number and use
that to calculate the distance to the uneven square (corner).

Example:

Distance from 18 to 1.

3^2 = 9 < 18 < 5^2 = 25

## Part 2:

That sequence can be found on https://oeis.org/A141481,
so you can look the solution up.

