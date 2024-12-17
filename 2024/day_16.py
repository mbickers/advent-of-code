import aoc
import heapq


def parse_input(input: str):
    map = {
        (r, c): ch
        for r, row in enumerate(input.splitlines())
        for c, ch in enumerate(row)
    }
    start = next(pos for pos, ch in map.items() if ch == "S"), (0, 1)
    return map, start


def solve(input, part1):
    map, start = parse_input(input)
    pq = [(0, start, None)]
    tiles_along_best_path = {None: set()}
    while True:
        score = pq[0][0]
        tiles_along_best_path_update = {}
        while pq[0][0] == score:
            _, (pos, dir), source = heapq.heappop(pq)
            if (pos, dir) in tiles_along_best_path:
                continue

            if (pos, dir) in tiles_along_best_path_update:
                tiles_along_best_path_update[(pos, dir)].update(
                    tiles_along_best_path[source]
                )

            else:
                tiles_along_best_path_update[(pos, dir)] = {
                    pos
                } | tiles_along_best_path[source]

                for new_dir in [(-dir[1], dir[0]), (dir[1], -dir[0])]:
                    if (pos, new_dir) not in tiles_along_best_path:
                        heapq.heappush(pq, (1000 + score, (pos, new_dir), (pos, dir)))
                new_pos = pos[0] + dir[0], pos[1] + dir[1]
                if map[new_pos] != "#" and (new_pos, dir) not in tiles_along_best_path:
                    heapq.heappush(pq, (1 + score, (new_pos, dir), (pos, dir)))
        for (pos, _), tiles in tiles_along_best_path_update.items():
            if map[pos] == "E":
                return score if part1 else len(tiles)
        tiles_along_best_path = tiles_along_best_path | tiles_along_best_path_update


test_input = """\
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"""


assert solve(test_input, True) == 7036
print(solve(aoc.input(16), True))
assert solve(test_input, False) == 45
print(solve(aoc.input(16), False))
