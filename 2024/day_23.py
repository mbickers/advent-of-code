from collections import defaultdict
import itertools
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
    connections = {
        frozenset(sorted([line[:2], line[3:]])) for line in input.splitlines()
    }
    # slow but works
    ks = connections
    x = 1
    while len(ks) > 1:
        x += 1
        print(len(ks), x)
        ks = {
            k1 | k2
            for k1, k2 in itertools.combinations(ks, r=2)
            if len(t := k1 ^ k2) == 2
            if frozenset(t) in connections
        }
    return ",".join(sorted(*ks))


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
