import re
import aoc


def simulate(input):
    registers, program = input.split("\n\n")
    registers = tuple(int(x) for x in re.findall(r"\d+", registers))
    program = tuple(int(x) for x in re.findall(r"\d+", program))
    instructions = ["adv", "bxl", "bst", "jnz", "bxc", "out", "bdv", "cdv"]
    ip = 0
    r = list(registers)
    out = []
    while 0 <= ip < len(program) - 1:
        opcode, operand = program[ip], program[ip + 1]
        inst = instructions[opcode]
        combo_operand = r[operand - 4] if 4 <= operand <= 6 else operand
        # print(ip, (opcode, operand), inst, (operand, combo_operand), r)
        if inst == "adv":
            r[0] = r[0] >> combo_operand
        elif inst == "bxl":
            r[1] = r[1] ^ operand
        elif inst == "bst":
            r[1] = combo_operand % 8
        elif inst == "jnz":
            if r[0] != 0:
                ip = operand
                continue
        elif inst == "bxc":
            r[1] = r[1] ^ r[2]
        elif inst == "out":
            out.append(combo_operand % 8)
        elif inst == "bdv":
            r[1] = r[0] >> combo_operand
        elif inst == "cdv":
            r[2] = r[0] >> combo_operand
        ip += 2
    return ",".join(str(x) for x in out)


test_input = """\
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"""

assert simulate(test_input) == "4,6,3,5,6,3,5,2,1,0"
print()
# not 6
print(
    simulate(aoc.input(17)),
)
print(simulate(aoc.input(17)))
