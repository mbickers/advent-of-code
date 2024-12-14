from itertools import count
from math import prod
import re
from typing import Counter
import aoc


def parse_input(input):
    return [
        ((int(m[1]), int(m[2])), (int(m[3]), int(m[4])))
        for m in re.finditer(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)$", input, re.MULTILINE)
    ]


def part1(input, size):
    sx, sy = size
    robots = parse_input(input)
    final_pos = [
        ((px + 100 * vx) % sx, (py + 100 * vy) % sy) for (px, py), (vx, vy) in robots
    ]
    quadrants = [
        sum((px - sx // 2) * qx > 0 and (py - sy // 2) * qy > 0 for px, py in final_pos)
        for qx in [1, -1]
        for qy in [1, -1]
    ]
    return prod(quadrants)


def part2(input, size):
    sx, sy = size
    robots = parse_input(input)
    for sec in count(1):
        robots = [
            (((px + vx) % sx, (py + vy) % sy), (vx, vy))
            for (px, py), (vx, vy) in robots
        ]
        quadrants = [
            sum(
                (px - sx // 2) * qx > 0 and (py - sy // 2) * qy > 0
                for (px, py), _ in robots
            )
            for qx in [1, -1]
            for qy in [1, -1]
        ]
        if prod(quadrants) < 110_000_000 and all(
            v == 1 for v in Counter(p for p, _ in robots).values()
        ):
            r = set(p for p, _ in robots)
            for x in range(sx):
                for y in range(sy):
                    print("X" if (x, y) in r else ".", end="")
                print()
            return sec


test_input = """\
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"""

assert part1(test_input, (11, 7)) == 12
print(
    part1(aoc.input(14), (101, 103)),
)
print(part2(aoc.input(14), (101, 103)))
