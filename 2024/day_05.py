import collections
import pathlib


def parse_input(input):
    order_rules, updates = input.split("\n\n")
    order_rules = [
        tuple([int(page) for page in rule.split("|")])
        for rule in order_rules.splitlines()
    ]
    updates = [
        [int(page) for page in update.split(",")] for update in updates.splitlines()
    ]
    return order_rules, updates


def part1(input):
    order_rules, updates = parse_input(input)
    required_after = collections.defaultdict(set)
    for before, after in order_rules:
        required_after[before] |= {after}

    def valid_update(update):
        seen_in_update = set()
        for next in update:
            if seen_in_update & required_after[next]:
                return False
            seen_in_update |= {next}
        return True

    valid_updates = [update for update in updates if valid_update(update)]
    return sum(update[len(update) // 2] for update in valid_updates)


def part2(input):
    order_rules, updates = parse_input(input)
    required_after = collections.defaultdict(set)
    for before, after in order_rules:
        required_after[before] |= {after}

    def valid_update(update):
        seen_in_update = set()
        for next in update:
            if seen_in_update & required_after[next]:
                return False
            seen_in_update |= {next}
        return True

    invalid_updates = [update for update in updates if not valid_update(update)]
    answer = 0
    for update in invalid_updates:
        unplaced = set(update)
        placed_rev = []
        for _ in range(0, len(update) // 2 + 1):
            for choice in unplaced:
                if not required_after[choice] & unplaced:
                    placed_rev.append(choice)
                    unplaced -= {choice}
                    break
            else:
                assert False
        answer += placed_rev[-1]

    return answer


test_input = """\
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""
assert part1(test_input) == 143

input = pathlib.Path("day_05_input.txt").read_text()
print(part1(input))

assert part2(test_input) == 123
print(part2(input))
