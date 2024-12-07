import aoc


def parse_input(input):
    equations = []
    for line in input.splitlines():
        left, right = line.split(": ")
        equations.append((int(left), tuple(int(num) for num in right.split(" "))))
    return equations


def solvable(left, right, allow_concat):
    if len(right) == 1:
        return left == right[0]

    return (
        (
            left % right[-1] == 0
            and solvable(left // right[-1], right[:-1], allow_concat=allow_concat)
        )
        or (
            left - right[-1] >= 0
            and solvable(left - right[-1], right[:-1], allow_concat=allow_concat)
        )
        or (
            allow_concat
            and str(left).endswith(str(right[-1]))
            and len(str(left)) > len(str(right[-1]))
            and solvable(
                int(str(left).removesuffix(str(right[-1]))),
                right[:-1],
                allow_concat=allow_concat,
            )
        )
    )


def part1(input):
    equations = parse_input(input)

    return sum(
        left for left, right in equations if solvable(left, right, allow_concat=False)
    )


def part2(input):
    equations = parse_input(input)
    return sum(
        left for left, right in equations if solvable(left, right, allow_concat=True)
    )


test_input = """\
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

assert part1(test_input) == 3749
print(part1(aoc.input(7)))

assert part2(test_input) == 11387
print(part2(aoc.input(7)))
