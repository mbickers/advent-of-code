import functools
import aoc
import tqdm


def solve(input, count=False):
    towels, designs = input.split("\n\n")
    towels = set(towels.split(", "))
    longest = max(len(t) for t in towels)
    designs = designs.splitlines()

    @functools.cache
    def ways(d):
        if not d:
            return 1
        return sum(
            ways(d[len + 1 :])
            for len in range(min(longest, len(d)))
            if d[: len + 1] in towels
        )

    return sum(ways(d) if count else ways(d) > 0 for d in tqdm.tqdm(designs))


test_input = """\
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"""

assert solve(test_input) == 6
print(
    solve(aoc.input(19)),
)
assert solve(test_input, True) == 16
print(solve(aoc.input(19), True))
