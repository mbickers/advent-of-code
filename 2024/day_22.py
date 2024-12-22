from collections import defaultdict
from itertools import pairwise
import aoc


def evolve(secret):
    m = 16777216
    secret = ((secret * 64) ^ secret) % m
    secret = (secret // 32 ^ secret) % m
    secret = (secret * 2048 ^ secret) % m
    return secret


def part1(input):
    initial_secrets = [int(x) for x in input.splitlines()]
    ans = 0
    for s in initial_secrets:
        for _ in range(2000):
            s = evolve(s)
        ans += s
    return ans


def part2(input):
    initial_secrets = [int(x) for x in input.splitlines()]
    total_seq_values = defaultdict(int)
    for secret in initial_secrets:
        seq_values = {}
        prices = [secret % 10]
        for _ in range(2000):
            secret = evolve(secret)
            prices.append(secret % 10)
            if len(prices) >= 4:
                seq = tuple(n - p for p, n in pairwise(prices[-5:]))
                if seq not in seq_values:
                    seq_values[seq] = prices[-1]
        for seq, value in seq_values.items():
            total_seq_values[seq] += value
    return max(total_seq_values.values())


test_input1 = """\
1
10
100
2024"""

assert part1(test_input1) == 37327623
print(part1(aoc.input(22)))

test_input2 = """\
1
2
3
2024"""
assert part2(test_input2) == 23
print(part2(aoc.input(22)))
