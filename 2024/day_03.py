import pathlib
import re


def part1(input):
    return sum(
        int(match.groups()[0]) * int(match.groups()[1])
        for match in re.finditer(r"mul\((\d+),(\d+)\)", input)
    )


def part2(input):
    s = 0
    enabled = True
    for match in re.finditer(
        r"(?P<mul>mul\((?P<x1>\d+),(?P<x2>\d+)\))|(?P<do>do\(\))|(?P<dont>don't\(\))",
        input,
    ):
        if match.group("do"):
            enabled = True
        elif match.group("dont"):
            enabled = False
        elif match.group("mul") and enabled:
            s += int(match.group("x1")) * int(match.group("x2"))

    return s


test_solutions = (
    part1("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"),
    part2("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"),
)
print(test_solutions)
assert test_solutions == (161, 48)

raw_input = pathlib.Path("day_03_input.txt").read_text()
solutions = part1(raw_input), part2(raw_input)
print(solutions)
