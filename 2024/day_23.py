from collections import defaultdict
import aoc


def part1(input):
    connections = [sorted([line[:2], line[3:]]) for line in input.splitlines()]
    connected = defaultdict(set)
    for a, b in connections:
        connected[a].add(b)
    sets = [
        (a, b, c)
        for a, bs in connected.items()
        for b in bs
        for c in connected.get(b, set()) & bs
    ]
    return sum(any(x[0] == "t" for x in xs) for xs in sets)


def part2(input):
    edges = [sorted([line[:2], line[3:]]) for line in input.splitlines()]
    neighbors = defaultdict(set)
    for a, b in edges:
        neighbors[a].add(b)
        neighbors[b].add(a)

    # Bron-Kerbosch
    maximal_cliques = []

    def find_maximal_cliques(includes_all, can_include, includes_none):
        if not can_include and not includes_none:
            maximal_cliques.append(includes_all)
        for v in can_include:
            find_maximal_cliques(
                includes_all=includes_all | {v},
                can_include=can_include & neighbors[v],
                includes_none=includes_none & neighbors[v],
            )
            can_include = can_include - {v}
            includes_none = includes_none | {v}

    find_maximal_cliques(
        includes_all=set(), can_include=set(neighbors), includes_none=set()
    )

    return ",".join(sorted(max(maximal_cliques, key=len)))


test_input = """\
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"""

assert part1(test_input) == 7
print(part1(aoc.input(23)))

assert part2(test_input) == "co,de,ka,ta"
print(part2(aoc.input(23)))
