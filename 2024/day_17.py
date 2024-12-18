import itertools
import re
import aoc


def step(program, state):
    ip, r, out = state
    opcode, operand = program[ip], program[ip + 1]
    combo_operand = r[operand - 4] if 4 <= operand <= 6 else operand
    # print(ip, (opcode, operand), inst, (operand, combo_operand), r)
    if opcode == 0:  # adv
        r[0] = r[0] >> combo_operand
    elif opcode == 1:  # bxl
        r[1] = r[1] ^ operand
    elif opcode == 2:  # bst
        r[1] = combo_operand % 8
    elif opcode == 3:  # jnz
        if r[0] != 0:
            ip = operand - 2
    elif opcode == 4:  # bxc
        r[1] = r[1] ^ r[2]
    elif opcode == 5:  # out
        out.append(combo_operand % 8)
    elif opcode == 6:  # bdv
        r[1] = r[0] >> combo_operand
    elif opcode == 7:  # cdv
        r[2] = r[0] >> combo_operand
    ip += 2
    return ip, r, out


def parse_input(input):
    registers, program = input.split("\n\n")
    registers = tuple(int(x) for x in re.findall(r"\d+", registers))
    program = tuple(int(x) for x in re.findall(r"\d+", program))
    return registers, program


def part1(input):
    registers, program = parse_input(input)
    state = 0, list(registers), []
    while 0 <= state[0] < len(program) - 1:
        state = step(program, state)
    return ",".join(str(x) for x in state[2])


def pretty_print(program):
    for opcode, operand in itertools.batched(program, 2):
        combo_operand = "abc"[operand - 4] if 4 <= operand <= 6 else f"0b{operand:b}"
        if opcode == 0:  # adv
            print(f"a = a >> {combo_operand}")
        elif opcode == 1:  # bxl
            print(f"b = b ^ 0b{operand:b}")
        elif opcode == 2:  # bst
            print(f"b = {combo_operand} % 8")
        elif opcode == 3:  # jnz
            print(f"if a != 0: goto {operand}")
        elif opcode == 4:  # bxc
            print("b = b ^ c")
        elif opcode == 5:  # out
            print(f"out.append({combo_operand} % 8)")
        elif opcode == 6:  # bdv
            print(f"b = a >> {combo_operand}")
        elif opcode == 7:  # cdv
            print(f"c = a >> {combo_operand}")


def part2(input):
    registers, program = parse_input(input)
    # my input program
    # - has one jump
    # - shifts a right 3 digits each loop
    # - does not preserve any state in b or c
    #
    # this solver attemps to greedily find 'chunks' of octets of a that correspond
    # to trailing parts of the input program. Once a distinct new chunk is found, move on
    # to next chunk.
    pretty_print(program)
    initial_a_leading_digits = 0
    while True:
        for octets_to_add in itertools.count(1):
            valid = []
            for new_trailing_octets in range(1 << (octets_to_add * 3)):
                reg_a = (
                    initial_a_leading_digits << (octets_to_add * 3)
                ) + new_trailing_octets
                state = 0, [reg_a, *registers[1:]], []
                while 0 <= state[0] < len(program) - 1:
                    state = step(program, state)
                if all(o == p for o, p in zip(reversed(state[2]), reversed(program))):
                    if len(state[2]) == len(program):
                        return reg_a
                    valid.append(reg_a)
            if len(valid) == 1:
                initial_a_leading_digits = valid[0]
                break


test_input = """\
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"""

assert part1(test_input) == "4,6,3,5,6,3,5,2,1,0"
print(
    part1(aoc.input(17)),
)

print(part2(aoc.input(17)))
