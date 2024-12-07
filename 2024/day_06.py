import aoc


def parse_input(input):
    start = None
    obstacles = []
    for row, line in enumerate(input.splitlines()):
        for col, char in enumerate(line):
            if char == "#":
                obstacles += [(row, col)]
            elif char == "^":
                start = (row, col)

    return start, set(obstacles), (row + 1, col + 1)


def sim(pos, dir, obstacles, size, on_state_update):
    visited = set()
    while (0 <= pos[0] < size[0]) and (0 <= pos[1] < size[1]):
        if (pos, dir) in visited:
            return "loop"
        on_state_update(pos, dir)
        visited |= {(pos, dir)}
        maybe_next = (pos[0] + dir[0], pos[1] + dir[1])
        if maybe_next in obstacles:
            dir = dir[1], -dir[0]
        else:
            pos = maybe_next

    return "leave"


def part1(input):
    pos, obstacles, size = parse_input(input)
    visited = set()
    sim(pos, (-1, 0), obstacles, size, lambda pos, _: (visited.add(pos)))
    return len(visited)


def part2(input):
    start_pos, permanent_obstacles, size = parse_input(input)
    loop_causing_obstacles = set()
    visited = set()

    def on_state_update(cur_pos, cur_dir):
        nonlocal loop_causing_obstacles, visited
        visited |= {cur_pos}
        proposed_obstacle = cur_pos[0] + cur_dir[0], cur_pos[1] + cur_dir[1]
        if (
            proposed_obstacle in visited
            or proposed_obstacle in loop_causing_obstacles
            or proposed_obstacle in permanent_obstacles
            or not (0 <= proposed_obstacle[0] < size[0])
            or not (0 <= proposed_obstacle[1] < size[1])
        ):
            return

        outcome = sim(
            cur_pos,
            cur_dir,
            permanent_obstacles | {proposed_obstacle},
            size,
            on_state_update=lambda pos, dir: None,
        )

        if outcome == "loop":
            loop_causing_obstacles |= {proposed_obstacle}

    sim(start_pos, (-1, 0), permanent_obstacles, size, on_state_update)

    return len(loop_causing_obstacles)


test_input = """\
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

assert part1(test_input) == 41
print(part1(aoc.input(6)))

assert part2(test_input) == 6
print(part2(aoc.input(6)))
