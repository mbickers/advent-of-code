import functools
import re
import aoc


def part1(input):
    initial, gates = input.split("\n\n")
    initial = {m[1]: m[2] == "1" for m in re.finditer(r"(...): (.)", initial)}
    gates = {
        m[4]: (m[1], m[2], m[3])
        for m in re.finditer(r"(...) (...?) (...) -> (...)", gates)
    }

    @functools.cache
    def value(wire):
        if wire in initial:
            return initial[wire]
        i1, gate, i2 = gates[wire]
        if gate == "AND":
            return value(i1) and value(i2)
        elif gate == "OR":
            return value(i1) or value(i2)
        elif gate == "XOR":
            return value(i1) != value(i2)

    return sum(
        value(gate) * (2**i)
        for i, gate in enumerate(g for g in sorted(gates) if g[0] == "z")
    )


def part2(input):
    gates = input.split("\n\n")[1]
    gates = {
        m[4]: (i1, m[2], i2)
        for m in re.finditer(r"(...) (...?) (...) -> (...)", gates)
        for i1, i2 in [sorted([m[1], m[3]])]
    }

    # manually figured out swaps from looking at output generated below
    swaps = [("z18", "skf"), ("wkr", "nvr"), ("z07", "bjm"), ("hsw", "z13")]
    for a, b in swaps:
        gates[a], gates[b] = gates[b], gates[a]
    seen = set()

    def get(wire, d):
        if wire not in gates or d == 0:
            return (wire,)
        i1, g, i2 = gates[wire]
        if i1[0] == "x" and i2[0] == "y" and i1[-2:] == i2[-2:] and g == "AND":
            return (f"{wire}=", f"carry_{i1[-2:]}")
        if i1[0] == "x" and i2[0] == "y" and i1[-2:] == i2[-2:] and g == "XOR":
            return (f"{wire}=", f"x_{i1[-2:]}")
        if wire in seen:
            return (wire,)
        seen.add(wire)
        return (
            f"{wire}=",
            g,
            *sorted([get(i1, d - 1), get(i2, d - 1)], key=lambda x: x[1:]),
        )

    for z in sorted(gates):
        if z[0] == "z":
            print(get(z, 8))

    return ",".join(sorted([wire for swap in swaps for wire in swap]))


test_input = """\
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02"""

assert part1(test_input) == 4
print(part1(aoc.input(24)))
print(part2(aoc.input(24)))
