from collections import defaultdict
import aoc


def part1(input):
    map = [[int(x) for x in line] for line in input.splitlines()]
    scores = 0
    for tr in range(len(map)):
        for tc in range(len(map[0])):
            if map[tr][tc] != 0:
                continue

            q = [(tr, tc)]
            visited = set()
            while q:
                r, c = q.pop()
                if (r, c) in visited:
                    continue
                visited.add((r, c))
                for qr, qc in [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]:
                    if (
                        (0 <= qr < len(map))
                        and (0 <= qc < len(map[0]))
                        and map[qr][qc] == map[r][c] + 1
                        and (qr, qc) not in visited
                    ):
                        q.append((qr, qc))

            score = sum(map[r][c] == 9 for (r, c) in visited)
            scores += score

    return scores


def part2(input):
    map = [[int(x) for x in line] for line in input.splitlines()]
    ratings = 0
    for tr in range(len(map)):
        for tc in range(len(map[0])):
            if map[tr][tc] != 0:
                continue

            ways_to_reach_prev_height = {(tr, tc): 1}
            for height in range(1, 10):
                ways_to_reach_height = defaultdict(int)
                for (r, c), ways in ways_to_reach_prev_height.items():
                    for qr, qc in [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]:
                        if (
                            (0 <= qr < len(map))
                            and (0 <= qc < len(map[0]))
                            and map[qr][qc] == height
                        ):
                            ways_to_reach_height[(qr, qc)] += ways
                ways_to_reach_prev_height = ways_to_reach_height

            rating = sum(ways_to_reach_prev_height.values())
            ratings += rating

    return ratings


test_input = """\
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"""

assert part1(test_input) == 36
print(part1(aoc.input(10)))

assert part2(test_input) == 81
print(part2(aoc.input(10)))
