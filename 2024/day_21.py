import functools
import itertools
import aoc


def solve(input, robots):
    numeric = """\
789
456
123
 0A"""
    numeric = {
        ch: r + c * 1j
        for r, line in enumerate(numeric.splitlines())
        for c, ch in enumerate(line)
    }
    directional = """\
 ^A
<v>"""
    directional = {
        ch: r + c * 1j
        for r, line in enumerate(directional.splitlines())
        for c, ch in enumerate(line)
    }

    @functools.cache
    def sequence_cost(seq, level, human_level):
        if level == human_level:
            return len(seq)

        costs = 0
        keypad = numeric if level == 0 else directional
        for start, press in itertools.pairwise("A" + seq):
            d = keypad[press] - keypad[start]
            routes = []
            rp = ("^" if d.real < 0 else "v") * abs(int(d.real))
            cp = ("<" if d.imag < 0 else ">") * abs(int(d.imag))
            if (
                keypad[start].real != keypad[" "].real
                or keypad[press].imag != keypad[" "].imag
            ):
                routes += [cp + rp + "A"]
            if (
                keypad[start].imag != keypad[" "].imag
                or keypad[press].real != keypad[" "].real
            ):
                routes += [rp + cp + "A"]
            routes = set(routes)
            cost = min(sequence_cost(route, level + 1, human_level) for route in routes)
            costs += cost
        return costs

    complexities = 0
    for code in input.splitlines():
        complexities += sequence_cost(code, 0, robots) * int(code[:-1])
    return complexities


test_input = """\
029A
980A
179A
456A
379A"""

assert solve(test_input, 3) == 126384
print(solve(aoc.input(21), 3))
print(solve(aoc.input(21), 26))
