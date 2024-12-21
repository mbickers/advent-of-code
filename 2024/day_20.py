import aoc


def solve(input, save_at_least, max_cheat_length):
    map = {
        r + c * 1j: char
        for r, line in enumerate(input.splitlines())
        for c, char in enumerate(line)
    }
    (start,) = (pos for pos, char in map.items() if char == "S")
    (end,) = (pos for pos, char in map.items() if char == "E")
    path = {start: 0}
    last = start
    dirs = [-1, 1j, 1, -1j]
    while last != end:
        (next,) = (
            last + d
            for d in dirs
            if map[last + d] in {".", "E"} and (last + d) not in path
        )
        path[next] = len(path)
        last = next
    cheats = 0
    for cheat_start, start_idx in path.items():
        for cheat_end, end_idx in path.items():
            cheat_length = abs(cheat_start.real - cheat_end.real) + abs(
                cheat_start.imag - cheat_end.imag
            )
            saved = end_idx - start_idx - cheat_length
            if cheat_length <= max_cheat_length and saved >= save_at_least:
                cheats += 1
    return cheats


test_input = """\
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"""

assert solve(test_input, 20, 2) == 5
print(
    solve(aoc.input(20), 100, 2),
)
assert solve(test_input, 74, 20) == 7
print(solve(aoc.input(20), 100, 20))
