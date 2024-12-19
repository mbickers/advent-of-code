import itertools
import re
import aoc
import tqdm


def simulate(fallen, size):
    end = size + size * 1j
    frontier = {(0)}
    visited = set()
    for step in itertools.count():
        if not frontier:
            return None
        if end in frontier:
            return step
        visited |= frontier
        frontier = {
            p + d
            for p in frontier
            for d in [1, 1j, -1j, -1]
            if (p + d) not in visited
            and (p + d) not in fallen
            and 0 <= (p + d).real <= size
            and 0 <= (p + d).imag <= size
        }


def part1(input, size, fallen_count):
    bytes = [int(m[1]) + int(m[2]) * 1j for m in re.finditer(r"(\d+),(\d+)", input)]
    return simulate(set(bytes[:fallen_count]), size)


def part2(input, size):
    bytes = [int(m[1]) + int(m[2]) * 1j for m in re.finditer(r"(\d+),(\d+)", input)]
    for fallen_count in tqdm.tqdm(range(len(bytes))):
        if simulate(set(bytes[:fallen_count]), size) is None:
            block = bytes[fallen_count - 1]
            return f"{block.real:.0f},{block.imag:.0f}"


test_input = """\
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"""

assert part1(test_input, 6, 12) == 22
print(
    part1(aoc.input(18), 70, 1024),
)
assert part2(test_input, 6) == "6,1"
print(part2(aoc.input(18), 70))
