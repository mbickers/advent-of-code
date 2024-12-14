import re
import aoc


def parse_input(input):
    pattern = r"""Button A: X\+(\d+), Y\+(\d+)
Button B: X\+(\d+), Y\+(\d+)
Prize: X=(\d+), Y=(\d+)"""
    return [
        (
            (int(match[1]), int(match[2])),
            (int(match[3]), int(match[4])),
            (int(match[5]), int(match[6])),
        )
        for match in re.finditer(pattern, input, re.MULTILINE)
    ]


def solve(input, offset=0):
    claw_machines = parse_input(input)
    tokens = 0
    for (u0, u1), (v0, v1), (w0, w1) in claw_machines:
        w0, w1 = w0 + offset, w1 + offset
        # [w] = [u|v] * [t], want to solve for t
        det = u0 * v1 - v0 * u1
        t0_times_det = v1 * w0 - v0 * w1
        t1_times_det = -u1 * w0 + u0 * w1
        if t0_times_det % det != 0 or t1_times_det % det != 0:
            continue

        t0, t1 = t0_times_det // det, t1_times_det // det
        tokens += t0 * 3 + t1
    return tokens


test_input = """\
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""


assert solve(test_input) == 480
print(solve(aoc.input(13)))
print(solve(aoc.input(13), 10000000000000))
