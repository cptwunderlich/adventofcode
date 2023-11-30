# Advent of Code 2020

As usual, I won't compete for the leaderboards.
I just want to improve my Haskell (might use another language as well)
and do it for fun.

For the first time ever, I'm streaming my attempts live on Discord.

## Day 1

Should've been rather straight forward, as the problem is a mix between
combinatorics and arithmetic.
I tried to create an "efficient" solution for part 1, but it looks a bit
messy. It would be even more efficient if I would just check and add elements
one at a time, so that I'd only have to iterate over the list once.

For part 2 I just did it the naive way. I tried an optimization,
but it made no difference. I've seen a solution, that uses the same approach
as I did in part 1, by simply iteratively adding `2020-x` to the set too...

One solution on reddit looked really good and terse. For both parts,
a list comprehension with tails etc.

The second part seems to be a variation of the [3SUM problem](https://en.wikipedia.org/wiki/3SUM)

## Day 2

Spent half the time figuring out Cabal and Parsec.
The problem itself was straight forward: counting repetions in a string,
then checking letters at given positions.

My design of part 1 let me reuse most of it.

The only thing I'm not really happy with, is in part 1, counting occurrences
of `char` will not short-curcuit after `max` occurences - I think.
Never too sure how to analyze Haskell programs and what happens under the surface.

## Day 3

I thought this one would be really easy and I solved the first part quickly.
But I had a bug in the second part which took me a while to find.

Basically, I immediately had the imperative solution in mind, using modular
arithmetic. But also for a more "functional" approach, I though of `cycle`.

I always for get about `sum` and `product`. I went back to change that in
part 2, where I had a `foldl' (*) 1 fxs`, but I didn't change it for part 1.
A very pretty and terse solution reminded me of these functions.
(actually, it might be better to forget sum/product, according to a
[blog post by Michael Snoyman](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1).)

A more efficient solution would probably use `Data.Vector` and modular arithmetic.
I think Haskell's TypeClasses could shine for the latter, [like in this post](https://byorgey.wordpress.com/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1/).

## Day 4

A parsing and data validation task.
I think that would have been even easier with RegEx, but I actually haven't taken
the time yet to get and learn a regex library for Haskell.
That's why I stuck with parsec. The first part was done quickly, for the second
one I refactored a bit to make it "nicer".
But then it struck me - I had a bug and it took me forever to find it...
My solution counted one match too many - one parser matched the input, but didn't
consume all of it. I cheated and used another solution to find the culprit.
Parser combinators are deceptively simple... <* EOF

## Day 5

Went very well. For the first part, I created an interval type and a function
that would halve an interval, returning a pair of the halves.
The input can than directly be mapped to `fst` and `snd`.

The second part was even easier. We are simply looking for two numbers x and y
where `y - x = 2` and the result is `x + 1`.

Edit: After looking at the solutions, I'm kind of upset. You can look at the
problem as a binary string... I've updated my solution accordingly.

Note that I have seen a solution for `bstrToDec` like this:

    binToDec = foldr (\x y -> x + 2 * y) 0

But the above blog post says to avoid `foldr` for functions which are not lazy
in their second argument, like `(*)`.

## Day 6

My first instinct was to throw every group into a set. But I figured, I could
solve that with some text/list manipulation, i.e., `length . group . sort`.
That worked and was short and sweet, but sure enough, the second part required
a Set. So now my solution contains both approaches, but I don't want to rewrite
part 1.

## Day 7

I hated the parsing. This was 90% parsing. Debugging parsec is no fun.
But yeah, there it is.

## Day 8

Was pretty straight forward. I guess I could make the solution more succinct
and beautiful, but I won't bother. E.g., reusing more machinery between
part1 and part2. We'll see whether this is like last year and this instruction
set will get reused.

I would have expected part2 to be slower, since I'm basically doing a brute-
force approach. I added the loop detection from part1 for quicker termination
from the get go and it is very fast.

## Day 9

Found this one pretty OK as well, although I'm sure that a prettier/more terse
version would be possible.
I wasted so much time on part 2, bc. I didn't read the docs of the function
`subsequences` from Data.List propertly. That lead the code being too slow
to find a solution and I was scratching my head trying all sorts of things...
Once I figured that out, I replaced it in 5 mins.

Variation of the [subset sum problem](https://en.wikipedia.org/wiki/Subset_sum_problem).

## Day 10

Part 1 was easy, but I had my difficulties with part 2. It was late so
I postponed it to day 11 and I have to admit, I did look at the solutions thread.
That's how I found out that I need to look at the Tribonacci series.

## Day 11

This was generally fun, except that I had a bug in `inView` that took me a while.
My first attempt to fix it failed: Note to self: `scanl` on infinite lists with
an eager function doesn't work...

## Day 12

Pretty straight forward, I just had to refreshen my geometry skills a little bit...

So, the "trick" to the puzzle is, that all rotations are multiple of 90°. That would
make everything easier, but since I am used to striving for general solutions,
it didn't even cross my mind. So I had to muck around with vector rotation etc...
Apparently there is a simple solution using immaginary nubmers.

## Day 13

How did I not know `Data.Function.on`?? I've been looking for something like that for
a while! I should also take a closer look at the Bifunctor package.

I did look at explanations of the chinese remainder theorem and used
https://rosettacode.org/wiki/Chinese_remainder_theorem

## Day 14

