# Advent of Code 2017

Usually I'm not a fan of coding competitions and puzzles. After all I'm already programming
for Work and University, so when I code in my spare time, I'd rather do something useful or
informative.
But I stumpled upon this and tried the first puzzle and got hooked.

My Rules: I'll try to solve them all, but I have no intention of getting up at 6 AM to try
and get on the leader board. Furthermore I'm not playing code golf.
But I'll try to solve the problems in a more "functional" way and using languages I might
not be using that often.

I'll document my progress here in this document, while the solutions are in their respective
directories.

## Day 1 & 2

I only started putting this up on Github on day 4.
For these two I used Python 3 and tried to use no raw loops. It took me way too long to
solve these and my Python 3 is woefully rusty. I use it maybe once a year and I don't
think I've used functools before.

Anyway, my solutions are hideous and way too complicated. Take a look at the solution
megathreads at https://www.reddit.com/r/adventofcode/ for some nicer ones.

## Day 3

I was actually too busy that day and didn't get to look at the problem, until I was lying
awake at 3 AM. I took a look at the solution megathread of the day and "solved" it the
next day. Many people figured out, that you didn't need code for that one.
You might consider it cheating, but meh.

## Day 4

For this one I broke out Java 8/9. I'm mostly stuck with 7 at work, so I didn't have a
chance yet to play with streams. This one seemed fairly easy, easier than days 1 and 2
and I solved it rather quickly. I'm also fairly pleased with how it looks.

I did incorporate one piece of feedback that I got from reddit after posting my solution
there. Namely using `.map(s -> s.split(" "))`, where I wasn't sure how to get rid of
two "manual" splits in filter.

## Day 5

The task is to follow the 'jump' instructions until you exit the array bounds.
I used C++ and at first I made a simple iterative approach and solved the puzzle.
Then I wanted to make it "prettier". To me it seems, like this is a problem more
suited for an iterative approach as opposed to a functional one. The solutions in
functional languages on Reddit also don't seem as succinct and elegant as usually.

## Day 6

I was busy all day, so I solved this one on the evening of Day 7.
While I used Kotlin for the solution, this program is absolutely ugly.
I just wanted to get it done. It's not idiomatic, it's a complete hack.
But it did the job.

## Day 7

Went again for C++, thinking I could do it in a very fast way. In the end,
I didn't use any fancy <algorithms> or anything. I thought using an
adjecency matrix would make checking for the root really easy
(no connections leading to the root), but it did make the construction
more complicated.
I hate to say it, but I really had my difficulties with this one.

## Day 8

Executing some simple conditional instructions on registers.
This one was super easy. I used python3 again, so I'd have an easy time
parsing the input.
I basically just rearranged the parts of the string and added a defaultdict
on top, so I could just "eval" (exec) the strings.

## Day 9

Parsing of nested braces with ignores, negation etc.
Another simple, nice one. Used Python again.
For part 1 I had a nice structure, where I put handlers into a dict, using a
NOP-lambda as default for get. So basically a "one function per symbol" approach.
I did have some duplication with regards to negation and ignores in the functions
though, but the main loop was as simple as possible.
I had to change that somewhat for part 2, where the ignored characters had to be
counted. So in this version, there is an if-block with three "modes" and the
final one uses the simplified "method in a dictionary per symbol" approach from
before.

## Day 10

Calculate an esoteric hash function.
The first part involves reversing substrings of the endless cycle of the input.
It did sound like a perfect fit for itertools.cycle and co. In the end, it didn't
turn out to be as straight forward as I had hoped. So I did end up throwing some
raw loops and modulo arithmetic on the problem.
For the second part, I lost half an hour, because I only had time to solve this
at night and I was too tired, not noticing that I used the wrong array for part 2.
That's also why you shouldn't use names like "ns" and "ns2", gah.
When I solved the puzzle, my solution was extremely slow. I used to get current
islice by just alway iterating from the beginning (i to i+n). I knew this wasn't
optimal, but for part 1 it didn't make a difference. But I added an "optimization"
after the fact.

## Day 12

I fell a bit off the wagon here. Too much going on right now.
But this one seemed easy and so it was. Some basic graph problem, i.e., finding
connected components.
