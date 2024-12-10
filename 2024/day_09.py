import itertools

import tqdm
import aoc


def part1(input):
    disk_map = [int(x) for x in input.rstrip()]
    blocks = [
        (id_times_2 // 2) if is_file else None
        for id_times_2, (is_file, run_len) in enumerate(
            zip(itertools.cycle([True, False]), disk_map)
        )
        for _ in range(run_len)
    ]
    left = 0
    right = len(blocks) - 1
    while left < right:
        if blocks[left] is not None:
            left += 1
        elif blocks[right] is None:
            right -= 1
        else:
            blocks[left], blocks[right] = blocks[right], None
    checksum = sum(
        pos * file_id for pos, file_id in enumerate(blocks) if file_id is not None
    )
    return checksum


def part2(input):
    disk_map = [int(x) for x in input.rstrip()]
    blocks = [
        (id_times_2 // 2) if is_file else None
        for id_times_2, (is_file, run_len) in enumerate(
            zip(itertools.cycle([True, False]), disk_map)
        )
        for _ in range(run_len)
    ]
    src_length = 0
    cur_id = None
    for right in tqdm.tqdm(range(len(blocks) - 1, -1, -1)):
        if cur_id is not None and blocks[right] != cur_id:
            dst_length = 0
            for left in range(0, right + 1):
                if blocks[left] is None:
                    dst_length += 1
                else:
                    dst_length = 0

                if dst_length == src_length:
                    for i in range(0, src_length):
                        blocks[left - src_length + 1 + i] = cur_id
                        blocks[right + 1 + i] = None
                    break
            src_length = 0

        if blocks[right] is None:
            src_length = 0
            cur_id = None
        else:
            src_length += 1
            cur_id = blocks[right]

    checksum = sum(
        pos * file_id for pos, file_id in enumerate(blocks) if file_id is not None
    )
    return checksum


test_input = "2333133121414131402"

assert part1(test_input) == 1928
print(part1(aoc.input(9)))

assert part2(test_input) == 2858
print(part2(aoc.input(9)))
