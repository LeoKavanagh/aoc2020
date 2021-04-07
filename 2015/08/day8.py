import sys
from ast import literal_eval


def get_lengths(lines):
    for line in lines:
        yield len(line) - len(literal_eval(line))


def get_lengths2(lines):
    for line in lines:
        yield (2 + len(line
            .replace('\\', '\\\\')
            .replace('"', '\\"'))
        ) - len(line)


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        lines = [a.strip() for a in f]

    print('Part 1: {}'.format(sum(get_lengths(lines))))
    print('Part 2: {}'.format(sum(get_lengths2(lines))))

