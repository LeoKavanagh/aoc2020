import sys
import functools


def product(iterable):
    return functools.reduce(lambda x, y: x * y, iterable)


def parse_dims(row):
    return map(int, row.strip().split('x'))


def paper_required(l, w, h):
    return  2*l*w + 2*w*h + 2*h*l + product(sorted((l, w, h))[:2])


def ribbon_required(l, w, h):
    return functools.reduce(lambda x, y: 2*x + 2*y, sorted((l, w, h))[:2]) + product((l, w, h))


def part1(dat):
    return sum(map(lambda x: paper_required(*parse_dims(x)), dat))


def part2(dat):
    return sum(map(lambda x: ribbon_required(*parse_dims(x)), dat))


with open(sys.argv[1], 'r') as f:
    dat = f.read().strip().splitlines()
    print(part1(dat))
    print(part2(dat))

