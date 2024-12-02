import collections
import pathlib


def parse_input(raw_input):
    return [tuple(int(x) for x in line.split("   ")) for line in raw_input.splitlines()]


def part1(input):
    list1, list2 = zip(*input)
    return sum(abs(x1 - x2) for x1, x2 in zip(sorted(list1), sorted(list2)))


def part2(input):
    left, right = zip(*input)
    counts = collections.Counter(right)
    return sum(x * counts.get(x, 0) for x in left)


def solve(raw_input):
    input = parse_input(raw_input)
    return part1(input), part2(input)


test_input = """\
3   4
4   3
2   5
1   3
3   9
3   3"""
test_solutions = solve(test_input)
assert test_solutions == (11, 31)

raw_input = pathlib.Path("day_01_input.txt").read_text()
solutions = solve(raw_input)
print(solutions)
