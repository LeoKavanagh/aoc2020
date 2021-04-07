import sys
from collections import namedtuple, Counter

Point = namedtuple('Point', 'x y')

INSTRUCTIONS = {'^': Point(0, 1),
                '>': Point(1, 0),
                'v': Point(0, -1),
                '<': Point(-1, 0)}


def move(pt1, pt2):
    return Point(pt1.x + pt2.x, pt1.y + pt2.y)


def visit_houses(start, dat, instruction_map):
    curr = start
    for x in dat:
        step = instruction_map[x]
        new_house = move(curr, step)
        curr = new_house
        yield curr



def main(args):
    with open(sys.argv[1], 'r') as f:
        dat = f.read().strip()

    origin = Point(0, 0)

    counts = Counter(visit_houses(origin, dat, INSTRUCTIONS))
    part1 = len(counts.items())

    # The YAGNI Aunt says you don't need to generalise this to n Santas
    counts2 = Counter(visit_houses(origin, dat[::2], INSTRUCTIONS)) + \
              Counter(visit_houses(origin, dat[1::2], INSTRUCTIONS))

    part2 = len(counts2.items())

    print('Part 1: {}'.format(part1))
    print('Part 2: {}'.format(part2))

if __name__ == '__main__':
    main(sys.argv)

