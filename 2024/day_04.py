import itertools
import pathlib


def part1(input):
    input_lines = input.splitlines()
    rows, cols = len(input_lines), len(input_lines[0])
    count = 0
    for d_r in [-1, 0, 1]:
        for d_c in [-1, 0, 1]:
            for start_r, start_c in itertools.product(range(rows), range(cols)):
                for offset in range(4):
                    r = start_r + (d_r * offset)
                    c = start_c + (d_c * offset)
                    if (
                        (not 0 <= r < rows)
                        or (not 0 <= c < cols)
                        or input_lines[r][c] != "XMAS"[offset]
                    ):
                        break
                else:
                    count += 1

    return count


def part2(input):
    input_lines = input.splitlines()
    rows, cols = len(input_lines), len(input_lines[0])
    count = 0
    dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    for start in itertools.product(range(1, rows - 1), range(1, cols - 1)):
        if input_lines[start[0]][start[1]] != "A":
            continue

        for shared_x_offset, unique_x_offset in zip(dirs, [dirs[-1], *dirs[:-1]]):
            for (letter, shared_sign), unique_sign in itertools.product(
                [("M", 1), (("S"), -1)], [1, -1]
            ):
                r, c = [
                    start[i]
                    + shared_sign * shared_x_offset[i]
                    + unique_sign * unique_x_offset[i]
                    for i in [0, 1]
                ]
                if input_lines[r][c] != letter:
                    break
            else:
                count += 1
                break
    return count


test_input = """\
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""
assert part1(test_input) == 18

input = pathlib.Path("day_04_input.txt").read_text()
print(part1(input))

assert part2(test_input) == 9
print(part2(input))
