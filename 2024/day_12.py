import aoc


def find_regions(map):
    regions = []
    unassigned = set(map.keys())
    while unassigned:
        root = unassigned.pop()
        border = set()
        region = set()
        queue = [root]
        while queue:
            p = queue.pop()
            if p in region:
                continue
            region.add(p)
            for dir in [-1, 1j, 1, -1j]:
                adj = p + dir
                if map.get(adj, None) != map[root]:
                    border.add((p, adj))
                    continue
                if adj not in region:
                    queue.append(adj)
        unassigned -= region
        regions.append((region, border))
    return regions


def part1(input):
    map = {
        (r + 1j * c): plant
        for r, row in enumerate(input.splitlines())
        for c, plant in enumerate(row)
    }
    return sum(len(region) * len(border) for region, border in find_regions(map))


def part2(input):
    map = {
        (r + 1j * c): plant
        for r, row in enumerate(input.splitlines())
        for c, plant in enumerate(row)
    }
    regions = find_regions(map)
    score = 0
    for region, border in regions:
        unassigned = set(border)
        sides = []
        while unassigned:
            root = unassigned.pop()
            side = set()
            queue = [root]
            while queue:
                f = queue.pop()
                if f in side:
                    continue
                side.add(f)
                normal = f[1] - f[0]
                for adj in [
                    (f[0] + normal * 1j, f[1] + normal * 1j),
                    (f[0] + normal * -1j, f[1] + normal * -1j),
                ]:
                    if adj in border and adj not in side:
                        queue.append(adj)
            unassigned -= side
            sides.append(side)
        score += len(region) * len(sides)
    return score


test_input = """\
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"""

assert part1(test_input) == 1930
print(part1(aoc.input(12)))

assert part2(test_input) == 1206
print(part2(aoc.input(12)))
