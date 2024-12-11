import functools
import aoc


def run(input, total_steps):
    stones = [int(x) for x in input.split()]

    @functools.cache
    def simulate(stone, steps):
        if steps == 0:
            return 1

        if stone == 0:
            return simulate(1, steps - 1)
        elif (str_stone := str(stone)) and len(str_stone) % 2 == 0:
            return simulate(
                int(str_stone[: len(str_stone) // 2]), steps - 1
            ) + simulate(int(str_stone[len(str_stone) // 2 :]), steps - 1)
        else:
            return simulate(stone * 2024, steps - 1)

    return sum([simulate(x, total_steps) for x in stones])


def part2(input):
    return None


test_input = "125 17"

assert run(test_input, 25) == 55312
print(run(aoc.input(11), 25))
print(run(aoc.input(11), 75))
