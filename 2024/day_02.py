import itertools
import pathlib


def parse_input(raw_input):
    return [tuple(int(x) for x in line.split()) for line in raw_input.splitlines()]


def part1(input):
    count = 0
    for line in input:
        is_inc = None
        for x, y in itertools.pairwise(line):
            if is_inc is None:
                is_inc = x < y

            if not (is_inc == (x < y) and 1 <= abs(x - y) <= 3):
                break
        else:
            count += 1

    return count


def sign(x):
    if x == 0:
        return 0
    return x / abs(x)


def part2(input):
    def valid(line):
        pairs = [
            sign(y - x) * (1 <= abs(x - y) <= 3) for x, y in itertools.pairwise(line)
        ]
        return abs(sum(pairs)) == len(pairs)

    count = 0
    for line in input:
        if valid(line):
            count += 1
        else:
            for index_to_exclude in range(len(line)):
                if valid(line[0:index_to_exclude] + line[index_to_exclude + 1 :]):
                    count += 1
                    break

    return count


def solve(raw_input):
    input = parse_input(raw_input)
    return part1(input), part2(input)


test_input = """\
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""
test_solutions = solve(test_input)
print(test_solutions)
assert test_solutions == (2, 4)

raw_input = pathlib.Path("day_02_input.txt").read_text()
solutions = solve(raw_input)
print(solutions)
