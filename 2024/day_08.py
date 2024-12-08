import collections
import itertools
import aoc


def parse_input(input):
    antennas = collections.defaultdict(list)
    for r, line in enumerate(input.splitlines()):
        for c, char in enumerate(line):
            if char == ".":
                continue
            antennas[char].append(r + c * 1j)
    return antennas, (r + 1, c + 1)


def part1(input):
    antennas, (rows, cols) = parse_input(input)
    antinodes = {
        x - (y - x)
        for locs in antennas.values()
        for x, y in itertools.permutations(locs, r=2)
    }
    antinodes = {
        loc for loc in antinodes if 0 <= loc.real < rows and 0 <= loc.imag < cols
    }
    return len(antinodes)


def part2(input):
    antennas, (rows, cols) = parse_input(input)
    antinodes = set()
    for locs in antennas.values():
        for x, y in itertools.combinations(locs, r=2):
            d = y - x
            t_boundary = [
                int(boundary / d_part)
                for boundary, d_part in [
                    (x.real, -d.real),
                    ((rows - x.real), d.real),
                    (x.imag, -d.imag),
                    ((cols - x.imag), d.imag),
                ]
                if d_part != 0
            ]
            for t in range(
                min(t for t in t_boundary if t <= 0),
                max(t for t in t_boundary if t >= 0) + 1,
            ):
                loc = x + t * d
                if 0 <= loc.real < rows and 0 <= loc.imag < cols:
                    antinodes.add(loc)
    return len(antinodes)


test_input = """\
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""

assert part1(test_input) == 14
print(part1(aoc.input(8)))

assert part2(test_input) == 34
print(part2(aoc.input(8)))
