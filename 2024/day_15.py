import aoc


def parse_input(input: str):
    map, steps = input.split("\n\n")
    map = {
        r + c * 1j: ch
        for r, row in enumerate(map.splitlines())
        for c, ch in enumerate(row)
        if ch != "."
    }
    walls = {pos for pos, ch in map.items() if ch == "#"}
    boxes = {pos for pos, ch in map.items() if ch == "O"}
    start = next(pos for pos, ch in map.items() if ch == "@")
    steps = [{"^": -1, ">": 1j, "v": 1, "<": -1j}[ch] for ch in steps if ch != "\n"]

    return walls, boxes, start, steps


def part1(input):
    walls, boxes, start, steps = parse_input(input)
    robot = start
    for step in steps:
        target = robot + step
        while target in boxes:
            target += step
        if target in walls:
            continue
        robot = robot + step
        if robot in boxes:
            boxes.remove(robot)
            boxes.add(target)
    return sum(100 * int(box.real) + int(box.imag) for box in boxes)


def part2(input):
    walls, boxes, start, steps = parse_input(input)
    walls = {
        pos.real + (2 * pos.imag + offset) * 1j for pos in walls for offset in [0, 1]
    }
    boxes = {pos.real + (2 * pos.imag) * 1j for pos in boxes}
    start = start.real + (2 * start.imag) * 1j
    robot = start
    for step in steps:
        if robot + step in walls:
            continue
        if {robot + step - 1j, robot + step} & boxes:
            visited = set()
            blocked = False
            q = list({robot + step - 1j, robot + step} & boxes)
            while q:
                box = q.pop()
                if box in visited:
                    continue
                visited.add(box)
                if box + step in walls or box + step + 1j in walls:
                    blocked = True
                    break
                for pushable_box_pos in [box + step - 1j, box + step, box + step + 1j]:
                    if pushable_box_pos in boxes and pushable_box_pos not in visited:
                        q.append(pushable_box_pos)
            if blocked:
                continue
            boxes -= visited
            boxes.update([box + step for box in visited])
        robot = robot + step
    return sum(100 * int(box.real) + int(box.imag) for box in boxes)


test_input = """\
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"""


assert part1(test_input) == 10092
print(part1(aoc.input(15)))
assert part2(test_input) == 9021
print(part2(aoc.input(15)))
