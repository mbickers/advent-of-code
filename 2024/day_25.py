import aoc


def part1(input):
    schematics = [
        (
            "lock" if schematic[0] == "#" else "key",
            [sum(schematic[6 * r + c] == "#" for r in range(7)) - 1 for c in range(5)],
        )
        for schematic in input.split("\n\n")
    ]
    locks = [item[1] for item in schematics if item[0] == "lock"]
    keys = [item[1] for item in schematics if item[0] == "key"]
    return sum(
        all(lock_col + key_col < 6 for lock_col, key_col in zip(lock, key))
        for lock in locks
        for key in keys
    )


test_input = """\
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"""

assert part1(test_input) == 3
print(part1(aoc.input(25)))
